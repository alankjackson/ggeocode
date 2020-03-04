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
#' Supported services are opencage, here, and googlemaps.
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
      svsc <- stringr::str_split(keys[i,], "\\s+")[[1]][1]
      key <- stringr::str_split(keys[i,], "\\s+")[[1]][2]
        if (svsc == "opencage")     {opencage_key <- key}
        if (svsc == "googlemaps")   {google_key <- key}
        if (svsc == "here_api_key") { here_api_key <- key}
    }
    #print(paste("opencage_key =", opencage_key))
    #print(paste("google_key =", google_key))
    if (service=="opencage") {return(opencage_key)}
    if (service=="google") {return(google_key)}
    if (service=="here") {return(here_api_key)}
}

# --------------------------------------------------------------------
#' Compare addresses for similarity.
#'
#' @param address_1 First address for comparison.
#' @param address_2 Second address for comparison.
#'
#' @details
#' Expect addresses of the form:
#'
#' number street, city, state, zip, country
#'
#' Each component will be compared in turn, and the number of
#' discrepancies will be noted. A vector of the number of
#' discrepancies will be returned. Components are each string
#' separated by comma. Case insensitive. For zipcodes, we will only
#' compare the first 5 digits and ignore the rest.
#' If one of the components is missing, -1 will be returned.
#'
#' For example:
#'
#' 1111 Main St, Anycity, ST 11111, USA compared to 1113 Main Rd, Anycity, ST
#'
#' returns
#'
#' 3,0,0,-1,-1

compare_addys <- function(address_1, address_2) {
  # Split on comma and remove leading and trailing whitespace
  add_1 <- stringr::str_trim(stringr::str_split(address_1, ",")[[1]])
  # Split state and zipcode
  add_1 <- c(add_1[1:2],stringr::str_split(add_1[3], " ")[[1]], add_1[4] )
  # Remove extra 4 digits from zip if present
  if (length(add_1>3)) {
    add_1[4] <- stringr::str_extract(add_1[4], "^\\d+")
  }
  # Repeat for other address
  add_2 <- stringr::str_trim(stringr::str_split(address_2, ",")[[1]])
  if (length(add_2)==4){
    add_2 <- c(add_2[1:2],stringr::str_split(add_2[3], " ")[[1]], add_2[4] )
  } else {
    add_2 <- c(add_2[1:2],stringr::str_split(add_2[3], " ")[[1]] )
  }
  # Remove extra 4 digits from zip if present
  if (length(add_2)>3) {
    add_2[4] <- stringr::str_extract(add_2[4], "^\\d+")
  }
  #   What if one is shorter?
  ret_vector <- c(-1,-1,-1,-1,-1)

  # Street
  ret_vector[1] <- adist(add_1[1], add_2[1], counts = TRUE, ignore.case=TRUE)[[1]]
  # City
  ret_vector[2] <- adist(add_1[2], add_2[2], counts = TRUE, ignore.case=TRUE)[[1]]
  # State
  ret_vector[3] <- adist(add_1[3], add_2[3], counts = TRUE, ignore.case=TRUE)[[1]]
  # Zip
  if ((length(add_1)>3) & (length(add_2)>3)) {
    ret_vector[4] <- adist(add_1[4], add_2[4], counts = TRUE, ignore.case=TRUE)[[1]]
  }
  # Country
  if (length(add_1)>4 & length(add_2)>4) {
    ret_vector[5] <- adist(add_1[5], add_2[5], counts = TRUE, ignore.case=TRUE)[[1]]
  }

  return(ret_vector)

}

