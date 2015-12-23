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
  ui <- shinygadgets::gadgetPage(
    shinygadgets::titlebar("Preview"),
    shinygadgets::contentPanel(
      shiny::div(shiny::includeMarkdown(output_file))
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      token <- .rs.askForPassword("Input Qiita access token:")

      result <- qiitr::qiita_post_item(
        url = "https://qiita.com",
        token = token,
        title = front_matter$title,
        tags = lapply(front_matter$tags, qiitr::qiita_tag),
        body = body,
        private = TRUE
      )

      invisible(stopApp())
      return(result)
    })
  }

  viewer <- shinygadgets::dialogViewer("Preview", width = 1000, height = 800)
  shinygadgets::runGadget(ui, server, viewer = viewer)
}
