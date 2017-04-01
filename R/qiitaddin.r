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
  md_dir <- dirname(md_file)
  imgs <- extract_image_paths(md_text)

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(2,
                      shiny::checkboxInput("coediting", "coediting"),
                      shiny::checkboxInput("private", "private", TRUE)),
        shiny::column(4,
                      shiny::selectInput("upload_method", label = "Upload images to",
                                         choices = c("Imgur", "Gyazo", "Imgur(anonymous)"))),
        shiny::column(2,
                      shiny::checkboxInput("update", "update an existing item")),
        shiny::column(4,
                      shiny::conditionalPanel(
                        condition = "input.update == true",
                        shiny::textInput("item_id", label = "Item ID")
                      )
        )
      ),
      # TODO: tag
      shiny::hr(),
      shiny::h1(title, align = "center"),
      shiny::div(
        shiny::HTML(
          markdown::markdownToHTML(md_file,
                                   fragment.only = TRUE,
                                   encoding = "UTF-8")
        )
      )
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      if(identical(Sys.getenv("QIITA_ACCESSTOKEN"), "")) {
        token <- rstudioapi::askForPassword("Input Qiita access token:")
        Sys.setenv(QIITA_ACCESSTOKEN = token)
        return(FALSE)
      }

      if(input$update) {
        shiny::validate(
          shiny::need(input$item_id, "item ID is empty!"),
          shiny::need(is_valid_item_id(input$item_id), "No such item!")
        )
      }


      progress <- shiny::Progress$new(session, min=0, max=2)
      on.exit(progress$close())

      # Step 1) Upload to Imgur or Gyazo
      progress$set(message = sprintf("Uploading the images to %s...", input$upload_method))

      upload_image <- switch(input$upload_method,
                             "Imgur" = upload_image_imgur,
                             "Gyazo" = upload_image_gyazo,
                             "Imgur(anonymous)" = upload_image_imgur_anonymously,
                             stop("invalid choice", input$upload_method))

      num_imgs <- length(imgs)
      for (i in seq_along(imgs)) {
        progress$set(detail = imgs[i])

        # attempt to avoid rate limits
        Sys.sleep(0.2)

        image_url <- upload_image(file.path(md_dir, imgs[i]))
        md_text <- stringr::str_replace_all(md_text, stringr::fixed(imgs[i]), image_url)

        progress$set(value = i/num_imgs)
      }

      # Step 2) Upload to Qiita
      if(input$update) {
        progress$set(message = "Updating the document on Qiita...",
                     detail = sprintf("Item ID: %s", input$item_id))

        result <- qiitr::qiita_update_item(
          item_id = input$item_id,
          title = title,
          body = md_text,
          tags = qiitr::qiita_util_tag("R"), # TODO: tags
          # coediting = input$coediting,     # TODO: This is a bug filed as qiitr#5
          private   = input$private
        )
      } else {
        progress$set(message = "Uploading the document to Qiita...",
                     detail = "")

        result <- qiitr::qiita_post_item(
          title = title,
          body = md_text,
          tags = qiitr::qiita_util_tag("R"), # TODO: tags
          coediting = input$coediting,
          private   = input$private
        )
      }

      progress$set(value = 2, message = "Done!")
      Sys.sleep(2)

      utils::browseURL(result$url)

      invisible(shiny::stopApp())
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

is_valid_item_id <- function(item_id) {
  # check if the item exists
  tryCatch(
    {
      qiitr::qiita_get_items(item_id = item_id)
      return(TRUE)
    },
    error = function(e) {
      warning("No such item: ", item_id, "\n Abort.")
      return(FALSE)
    }
  )
}
