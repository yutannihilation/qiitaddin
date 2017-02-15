#' Upload an image to Imgur
#'
#' Uploads a given image file to Imgur by imguR package.
#'
#' @param file Path to an image file.
#'
#' @return A URL of the uploaded image.
#' @export
upload_image_imgur <- function(file) {
  # Get OAuth token for Imgur
  imgur_token <- imguR::imgur_login()

  res <- imguR::upload_image(file, key = NULL, token = imgur_token)

  if(is.null(res$link)) {
    stop("No URL is returned: %s", res)
  }
  res$link
}
