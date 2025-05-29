#' Assign a NEON API token to the global NEON_TOKEN variable
#'
#' @param token API token string
#' @param file alternatively, a text file containing the API token string, 
#' accessed with \code{readLines(file)}.
#'
#' @returns nothing, assigns the NEON_TOKEN global variable
.setNeonTokenGlobal <- function(token = NA, file = NULL){
  if (!missing(file) && !any(is.na(file)) && !is.null(file)) {
    token = readLines(file)
  } else if (missing(token) | any(is.na(token))) {
    token = NA
  } 
  assign("NEON_TOKEN", value = token, envir = globalenv())
}

#' Check for NEON API token global variable, set empty if it doesn't exist
#'
#' @returns Nothing. Assigns default NA if the token doesn't exist
.checkNeonTokenGlobal <- function(){
  if (!exists("NEON_TOKEN", globalenv())) {
    .setNeonTokenGlobal()
  }
}
