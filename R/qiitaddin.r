#' Publish R Markdown Document to Qiita
#'
#' Knit and post a given R Markdown file to Qiita. Images will be uploaded to Imgur or Gyazo.
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
  imgs <- extract_image_paths(md_text)

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
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput("upload_method", label = "Upload images to",
                                            choices = c("Imgur", "Gyazo", "Imgur(anonymous)")))
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
      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- rstudioapi::askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      progress <- shiny::Progress$new(session, min=0, max=2)
      on.exit(progress$close())

      # Step 1) Upload to Imgur or Gyazo
      progress$set(message = sprintf("Uploading the images to %s...", input$upload_method))
      num_imgs <- length(imgs)

      upload_image <- switch(input$upload_method,
                             "Imgur" = upload_image_imgur,
                             "Gyazo" = upload_image_gyazo,
                             "Imgur(anonymous)" = upload_image_imgur_anonymously,
                             stop("invalid choice", input$upload_method))

      for (i in 1:num_imgs) {
        progress$set(detail = imgs[i])

        image_url <- upload_image(imgs[i])
        md_text <- stringr::str_replace_all(md_text, stringr::fixed(imgs[i]), image_url)

        progress$set(value = i/num_imgs)
      }

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

      utils::browseURL(result$url)

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

extract_image_paths <- function(md_text) {
  html_doc <- xml2::read_html(commonmark::markdown_html(md_text))
  img_nodes <- xml2::xml_find_all(html_doc, ".//img")
  xml2::xml_attr(img_nodes, "src")
}
