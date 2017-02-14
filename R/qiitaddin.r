#' Knit the active document and display it
#'
#' @export
qiitaddin_knit <- function() {
  input <- rstudioapi::getActiveDocumentContext()$path

  if(tolower(tools::file_ext(input)) != "rmd")
    stop(sprintf("active document %s is not .Rmd file!", basename(input)))

  output_file <- rmarkdown::render(
    input = input,
    output_format = rmarkdown::md_document(variant = "markdown_github"),
    encoding = "UTF-8"
  )

  # TODO: avoid unexported functions if possible
  front_matter <- rmarkdown:::parse_yaml_front_matter(
    rmarkdown:::read_lines_utf8(input, "UTF-8"))

  body <- paste(readLines(output_file, encoding = "UTF-8"), collapse = "\n")

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview"),
    miniUI::miniContentPanel(
      shiny::div(shiny::includeMarkdown(output_file))
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {

      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- .rs.askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      result <- qiitr::qiita_post_item(
        title = front_matter$title,
        body = body,
        tags = lapply(front_matter$tags, qiitr::qiita_util_tag),
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
