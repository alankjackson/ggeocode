#  Various utilities

#' Set filename for authorization tokens.
#'
#' @param filename Name of file storing keys.
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

set_keyfile <- local({
  opencage_key <- NULL
  google_key <- NULL
  function(filename) {

  #  does file exist?

    if (!fs::file_exists(filename)) {
      stop(paste("File", filename, "does not exist. Do you need a full path?"))
    }

  #  read file in and clean up

    keys <- read_lines(filename,
                       skip_empty_rows=TRUE,
                       progress=FALSE)
    keys <- as.tibble(keys)

    keys <- keys %>%
      filter(!str_detect(value, "^#")) # remove comments

    for (i in 1:nrow(keys)) {
        if (str_split(keys[i,], "\\s+")[[1]][1] == "opencage") {
          opencage_key <- str_split(keys[i,], "\\s+")[[1]][2]}
        if (str_split(keys[i,], "\\s+")[[1]][1] == "googlemaps") {
          google_key <- str_split(keys[i,], "\\s+")[[1]][2]}
    }
  }
})
