#' Knit And Post A Rmarkdown Document to Qiita
#'
#' @param input path to a .Rmd file. If `NULL`, use the activedocument.
#'
#' @export
qiitaddin_knit <- function(input = NULL) {
  if(is.null(input) && rstudioapi::isAvailable()){
    input <- rstudioapi::getActiveDocumentContext()$path
  }

  if(tolower(tools::file_ext(input)) != "rmd") {
    stop(sprintf("%s is not .Rmd file!", basename(input)))
  }

  md_file <- rmarkdown::render(
    input = input,
    output_format = rmarkdown::md_document(variant = "markdown_github"),
    encoding = "UTF-8"
  )

  front_matter <- rmarkdown::yaml_front_matter(input, "UTF-8")

  qiitaddin_upload(
    md_file = md_file,
    title   = front_matter$title,
    tags    = front_matter$tags
  )
}

qiitaddin_upload <- function(md_file, title, tags) {
  md_text <- read_utf8(md_file)
  imgs <- qiitaddin_extract_image_paths(md_text)

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(4, shiny::textInput("title", "title", "")),
        shiny::column(1, shiny::checkboxInput("coediting", "coediting")),
        shiny::column(1, shiny::checkboxInput("private", "private", TRUE)),
        shiny::column(1, shiny::checkboxInput("gist", "gist")),
        shiny::column(1, shiny::checkboxInput("tweet", "tweet"))
      ),
      # TODO: tag
      shiny::hr(),
      shiny::div(shiny::includeMarkdown(md_file))
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::updateTextInput(session, "title", value = title)

    shiny::observeEvent(input$done, {
      # check credentials for Imgur and Qiita
      imgur_token <- imguR::imgur_login()

      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- rstudioapi::askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      progress <- shiny::Progress$new(session, min=0, max=2)
      on.exit(progress$close())

      # Step 1) Upload to Imgur
      progress$set(message = "Uploading the images to Imgur...")
      num_imgs <- length(imgs)

      for (i in 1:num_imgs) {
        progress$set(detail = imgs[i])
        res <- imguR::upload_image(imgs[i], key = NULL, token = imgur_token)
        progress$set(value = i/num_imgs)

        md_text <- stringr::str_replace_all(md_text, stringr::fixed(imgs[i]), res$link)
      }

      # Write the modified Markdown text to another file
      writeLines(md_text, paste0(md_file,".uploaded"))

      # Step 2) Upload to Qiita
      progress$set(message = "Uploading the document to Qiita...", detail = "")
      result <- qiitr::qiita_post_item(
        title = input$title,
        body = md_text,
        tags = qiitr::qiita_util_tag("R"),
        # TODO: post without any tag is not permitted.
        # tags = lapply(tags, qiitr::qiita_util_tag),
        coediting = input$coediting,
        private   = input$private,
        gist      = input$gist,
        tweet     = input$tweet
      )

      progress$set(value = 2, message = "Done!")
      Sys.sleep(2)

      invisible(stopApp())
      return(result)
    })
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}

read_utf8 <- function(x) {
  paste(readLines(x, encoding = "UTF-8"), collapse = "\n")
}

qiitaddin_extract_image_paths <- function(md_text) {
  html_doc <- xml2::read_html(commonmark::markdown_html(md_text))
  img_nodes <- xml2::xml_find_all(html_doc, ".//img")
  xml2::xml_attr(img_nodes, "src")
}
