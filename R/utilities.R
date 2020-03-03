#  Various utilities

#' Set filename for authorization tokens.
#'
#' @param filename Name of file storing keys.
#' @param service Name of service key to return (google, opencage, etc).
#'
#' @details
#' The authorization file should be of the form:
#'
#' service authorization_key
#'
#' Supported services are opencage and googlemaps.
#'
#' So a line like
#'
#' googlemaps IOUYGUBKJKBKNOlknlnlnlknLnkl
#'
#' is what is required.
#'
#' Blank lines and lines beginning with # are skipped
#'

set_keyfile <- function(filename, service) {

    value <- NULL
    #print("======= running set_keyfile ========")
  #  does file exist?

    if (!fs::file_exists(filename)) {
      stop(paste("File", filename, "does not exist. Do you need a full path?"))
    }

  #  read file in and clean up

    keys <- readr::read_lines(filename,
                       skip_empty_rows=TRUE,
                       progress=FALSE)
    keys <- tibble::as.tibble(keys)

    keys <- keys %>%
      dplyr::filter(!stringr::str_detect(value, "^#")) # remove comments

    for (i in 1:nrow(keys)) {
        if (stringr::str_split(keys[i,], "\\s+")[[1]][1] == "opencage") {
          opencage_key <- stringr::str_split(keys[i,], "\\s+")[[1]][2]}
        if (stringr::str_split(keys[i,], "\\s+")[[1]][1] == "googlemaps") {
          google_key <- stringr::str_split(keys[i,], "\\s+")[[1]][2]}
    }
    #print(paste("opencage_key =", opencage_key))
    #print(paste("google_key =", google_key))
    if (service=="opencage") {return(opencage_key)}
    if (service=="google") {return(google_key)}
  }
