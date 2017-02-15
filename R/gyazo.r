#' Upload an image to Gyazo
#'
#' Uploads a given image to Gyazo.
#'
#' @param file Path to an image file.
#'
#' @return A URL of the uploaded image.
upload_image_gyazo <- function(file) {
  gyazo_token <- httr::oauth2.0_token(
    endpoint = gyazo_endpoint(),
    app      = gyazo_app(),
    cache    = TRUE
  )

  res <- httr::POST("https://upload.gyazo.com/api/upload",
                    body = list(
                      imagedata = httr::upload_file(file)
                    ),
                    httr::config(token = gyazo_token))

  httr::stop_for_status(res)

  httr::content(res)$url
}

gyazo_app <- function() {
  httr::oauth_app(
    appname = "Gyazo",
    key     = .gyazo_client_id,
    secret  = .gyazo_client_secret
  )
}


gyazo_endpoint <- function() {
  httr::oauth_endpoint(
    authorize = "/oauth/authorize",
    access    = "/oauth/token",
    base_url  = "https://api.gyazo.com"
  )
}
