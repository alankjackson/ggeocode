#' Geocode using the Census website
#'
#'
#' @param placename Placename.
#' @param benchmark A numerical ID or name that references what
#'   version of the locator should be searched. (default = Public_AR_Current)
#' @param vintage a numerical ID or name that references what vintage of geography is desired (default = Current_Current)
#' @details Access the US Census Bureau geocoding service. Return a data frame.
#'
#' This function typically returns multiple results because of placename ambiguity
#'
#' A dataframe will be returned with
#' \itemize{
#'   \item status
#'   \item matched address
#'   \item Latitude
#'   \item Longitude
#'   \item Census Tract
#'   \item Census Block
#' }
#'

census <- function(placename,
                   benchmark = "Public_AR_Current",
                   vintage = "Current_Current" ){
  url <- "https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?"

  # prepare the address
  address <- stringr::str_replace_all(placename, ",", "%2C")
  address <- stringr::str_replace_all(address, " ", "+")
  urlreq <- paste0(url, "address=", address, "&benchmark=", benchmark,
                   "&vintage=", vintage, "&format=json")
  # query server
  result <- .GetResult(urlreq)

  # did we succeed?
  if (result$status_code != 200) { # failure
    return(data.frame(
      status=paste("fail_code: ",result$status_code),
      match_address=NA,
      lat=NA,
      long=NA,
      tract=NA,
      block=NA
    ))
  } else {
    result <- httr::content(result)
    Num_matches <- length(result[["result"]][["addressMatches"]])

    if (Num_matches <= 0) { # failed to find address
      return(data.frame(
        status="fail_no_result",
        match_address=NA,
        lat=NA,
        long=NA,
        tract=NA,
        block=NA
      ))
    }

    # prepare data frame with all results
    foo <- data.frame(
      status=NA,
      match_address=NA,
      lat=NA,
      long=NA,
      tract=NA,
      block=NA
    )
    for (i in 1:Num_matches) {
      temp <- result[["result"]][["addressMatches"]][[i]]
      tract <- temp[["geographies"]][[
                     "2010 Census Blocks"]][[1]][["TRACT"]]
    if (is.null(tract)){
      tract <- NA
    }
    #if (is.null(tract)){
    #  return(data.frame(
    #    status="fail_no_tract",
    #    match_address=NA,
    #    lat=NA,
    #    long=NA,
    #    tract=NA,
    #    block=NA
    #  ))
    #}
    status <- "success"
    match_address=temp[["matchedAddress"]]
    lat=temp[["coordinates"]][["y"]]
    long=temp[["coordinates"]][["x"]]
    block=temp[["geographies"]] [[
                "2010 Census Blocks"]][[1]][["BLOCK"]]

      if (i==1) {
        foo$status <- status
        foo$match_address <- match_address
        foo$lat <- lat
        foo$long <- long
        foo$tract <- tract
        foo$block <- block

      } else {
        dplyr::add_row(foo, status=status,
                          match_address=match_address,
                          lat=lat,
                          long=long,
                          tract= tract,
                          block=block)
      } # end if/else
    } # end for loop
    return(foo)
  }
}

.GetResult <- function(urlreq) {

  #   set up to retry twice on server type error (which usually works)
  attempt <- 1
  result <- data.frame(status_code=0)
  while(result$status_code!=200 && attempt<=3 ) {
    if (attempt>1){print(paste("attempted", attempt))}
    attempt <- attempt + 1
    try(
      #     Go get result
      result <- httr::GET(urlreq)
    )
  }
  return(result)
}

