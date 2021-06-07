
#' Validate a postcode/huisnummer with the Postcode API
#' @details Set the API key with `options(postcode_api_key = "abcdef")` (see 1Password).
#' @param postcode E.g. '1234AB'
#' @param huisnummer Integer
#' @export
#' @importFrom glue glue
#' @importFrom httr GET add_headers content
#' @examples
#' @return A list with address, location information, or NULL if postcode/huisnummer not found.
#' \dontrun{
#' postcode_validate("5617BD", 42)
#' }
postcode_validate <- function(postcode, huisnummer, key = getOption("postcode_api_key")){

  r <- httr::GET(
    glue::glue("https://api.postcodeapi.nu/v3/lookup/{postcode}/{huisnummer}"),
    httr::add_headers(`X-Api-Key` = key)
  )

  out <- httr::content(r)

  if(!is.list(out))out <- NULL

  out
}

