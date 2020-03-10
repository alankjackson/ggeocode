#' Run ggmap::geocode geocoding function.
#'
#' This is just a wrapper around the ggmap::geocode
#' function, with a few smarts added.
#'
#' @param placename Placename.
#' @param keyfile File (full path) to read api key from.
#' @param bounds Provides the geocoder with a hint to the region that
#' the query resides in. This value will restrict the possible results
#' to the supplied region. The bounds parameter should be specified as
#' 4 coordinate points forming the south-west and north-east corners of
#' a bounding box. For example, \code{bounds = c("-118.604794","34.172684",
#' "-118.500938","34.236144")} (min long, min lat, max long, max lat).
#' @param endcheck Is address off end of street? (default = TRUE)
#'
#' @details To get an API key to access google geocoding, register
#'   at \url{https://cloud.google.com/console/google/maps-apis/overview}.
#'   The free API key provides up to 40,000 calls a month.
#'
#' This function typically returns multiple results because of placename ambiguity.
#'

google <- function(placename,
                   keyfile=NULL,
                   bounds=NULL,
                   endcheck = TRUE){

  # Do I have any free queries left?

  if (track_usage("google", "Query")<2) {
    return(tibble::tibble(Type="No free queries remain",lat=NA,long=NA,
                   match_address=NA,LocType=NA ))
  }

  #   Test for keyfile filled in ***** TODO *****
  google_key <- set_keyfile(keyfile, "google")
  print(google_key)
  ggmap::register_google(key = google_key)
 ### bounds=34.172684,-118.604794|34.236144,-118.500938

  # Bounds present, format correctly
  if (!is.null(bounds)){
    BoundBox <-paste0(bounds[2],",",bounds[1],"|", bounds[4],",", bounds[3])
    Results <- ggmap::geocode(location=placename, output="all",
                                    inject=BoundBox)
  } else {
    Results <- ggmap::geocode(location=placename, output="all",
                              override_limit = TRUE)
  }
  track_usage(service="google", action="Post")

  # parse json output

  Results <- .getfields(Results)

  #   Did we error?

  if (is.na(Results$Type) |
      Results$Type == "ZERO_RESULTS"){
    Results$status <- "no result"
    return(Results)
  }

  #   Are we within the city?

  if (!dplyr::between(Results$lat, bounds[2], bounds[4]) |
      !dplyr::between(Results$long, bounds[1], bounds[3])){
    Results$status <- "outside boundary"
    return(Results)
  }

  #   Is it a bogus location at center of long street?

  if (Results$LocType == "GEOMETRIC_CENTER") { # bad location
    Results$status <- "Road only, street number ignored"
    return(Results)
  }

  # REALLY bogus location - don't want this

  if (Results$LocType == "APPROXIMATE" ) { # bad location
    Results$status <- "City only, street ignored"
    return(Results)
  }

  # endcheck turned on and location not a real digitized point
  if (endcheck & Results$LocType != "ROOFTOP") {
    addrnumber <- trimws(stringr::str_extract(placename, "^[0-9]+ "))
    if (as.numeric(addrnumber)>50){
      addrnumber2 <- as.character(as.numeric(addrnumber)-16)
    } else {
      addrnumber2 <- as.character(as.numeric(addrnumber)+16)
    }
    address <- stringr::str_replace(placename, addrnumber, addrnumber2)
    perturbedResult <- ggmap::geocode(address, output="all")
    perturbedResult <- .getfields(perturbedResult)
    track_usage(service="google", action="Post")

    #   Are the answers the same? If so, we have a problem
    distance <- stats::dist(rbind(c(Results$lat, Results$long),
                           c(perturbedResult$lat, perturbedResult$long))) *
      69*5280
    if (distance < 10){# could be almost anywhere, reject
      Results$status <- paste("Distance =",as.character(distance))
    }
  }

  #   return result

  return(Results)

}

#   Function for pulling fields out of nested lists returned by geocode
.getfields <- function(x){
  if(! is.na(x) && length(x$results)>0)  {tibble::tibble(
    lat=as.numeric(x$results[[1]]$geometry$location$lat),
    long=as.numeric(x$results[[1]]$geometry$location$lng),
    match_address=x$results[[1]]$formatted_address,
    LocType=x$results[[1]]$geometry$location_type,
    Type=unlist(x$results[[1]]$types[1])
  )
  } else if ("status" %in% names(x) &&
             x$status=="ZERO_RESULTS"){
    tibble::tibble(Type=x$status,lat=NA,long=NA,
           match_address=NA,LocType=NA )
  } else{
    tibble::tibble(Type=NA,lat=NA,long=NA,
           match_address=NA,LocType=NA )
  }
}

