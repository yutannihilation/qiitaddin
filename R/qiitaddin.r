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
  body <- paste(readLines(md_file, encoding = "UTF-8"), collapse = "\n")

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview"),
    miniUI::miniContentPanel(
      shiny::tableOutput('table'),
      shiny::hr(),
      shiny::div(shiny::includeMarkdown(md_file))
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    output$table <- shiny::renderTable(head(iris))
    shiny::observeEvent(input$done, {

      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- .rs.askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      result <- qiitr::qiita_post_item(
        title = title,
        body = body,
        tags = qiitr::qiita_util_tag("R"),
        # TODO: post without any tag is not permitted.
        # tags = lapply(tags, qiitr::qiita_util_tag),
        coediting = FALSE,
        private   = TRUE,
        gist      = FALSE,
        tweet     = FALSE
      )

      invisible(stopApp())
      return(result)
    })
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}


qiitaddin_upload_images <- function(md_file) {
  md_text <- paste(readLines(md_file, encoding = "UTF-8"), collapse = "\n")
  imgs <- qiitaddin_extract_image_paths(md_text)

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview"),
    miniUI::miniContentPanel(
      shiny::tableOutput(iris)
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$upload, {})
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}

qiitaddin_extract_image_paths <- function(md_text) {
  html_doc <- xml2::read_html(commonmark::markdown_html(md_text))
  img_nodes <- xml2::xml_find_all(html_doc, ".//img")
  xml2::xml_attr(img_nodes, "src")
}
