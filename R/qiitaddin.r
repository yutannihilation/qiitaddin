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

  qiitaddin_post_qiita(
    md_file = md_file,
    title   = front_matter$title,
    tags    = front_matter$tags
  )
}

qiitaddin_post_qiita <- function(md_file, title, tags) {
  md_text <- read_utf8(md_file)
  imgs <- qiitaddin_extract_image_paths(md_text)

  imgur_token <- imguR::imgur_login()

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
      shiny::div(
        shiny::actionButton("upload", "Upload to imgur"),
        shiny::textOutput("upload_result")
      ),
      shiny::hr(),
      shiny::div(shiny::includeMarkdown(md_file))
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::updateTextInput(session, "title", value = title)

    shiny::observeEvent(input$upload, {
      progress <- shiny::Progress$new(session, min=0, max=length(imgs))
      on.exit(progress$close())

      progress$set(message = "Uploading",
                   detail  = "...")

      for (i in 1:length(imgs)) {
        progress$set(value = i, detail = imgs[i])
        res <- imguR::upload_image(imgs[i], key = NULL, token = imgur_token)
        md_text <- stringr::str_replace_all(md_text, stringr::fixed(imgs[i]), res$link)
        Sys.sleep(0.5)
      }

      writeLines(md_text, md_file)
      output$upload_result <- shiny::renderText("Done")
      shiny::updateActionButton(session, "upload", icon = shiny::icon("check"))
    })

    shiny::observeEvent(input$done, {
      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- rstudioapi::askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      result <- qiitr::qiita_post_item(
        title = input$title,
        body = read_utf8(md_file),
        tags = qiitr::qiita_util_tag("R"),
        # TODO: post without any tag is not permitted.
        # tags = lapply(tags, qiitr::qiita_util_tag),
        coediting = input$coediting,
        private   = input$private,
        gist      = input$gist,
        tweet     = input$tweet
      )

      invisible(stopApp())
      return(result)
    })
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}


qiitaddin_upload_images <- function(imgs) {

}

read_utf8 <- function(x) {
  paste(readLines(x, encoding = "UTF-8"), collapse = "\n")
}

qiitaddin_extract_image_paths <- function(md_text) {
  html_doc <- xml2::read_html(commonmark::markdown_html(md_text))
  img_nodes <- xml2::xml_find_all(html_doc, ".//img")
  xml2::xml_attr(img_nodes, "src")
}
