# Code modified from https://github.com/NEONScience/NEON-utilities/blob/main/neonUtilities/R/loadByProduct.R

#' Isolated function for running the checks of loadByProduct
#'
#' @param [dpID, package, useFasttime] as in loadByProduct
#'
#' @returns error call, if applicable
check_lbp_args <- function(dpID, package, useFasttime){
  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(
      package, 
      "is not a valid package name. Package must be basic or expanded", 
      sep = " "
    ))
  }
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}[.][0-9]{5}[.]00[1-2]{1}", dpID)[1] != 1) {
    stop(paste(
      dpID, 
      "is not a properly formatted data product ID.",
      "The correct format is DP#.#####.00#", 
      sep = " "
    ))
  }
  # error message if for AOP data
  if(substring(dpID, 5, 5) == 3 & dpID != "DP1.30012.001") {
    stop(paste(
      dpID, 
      "is a remote sensing data product and cannot be loaded directly to R",
      "with this function. Use the byFileAOP() function to download locally.", 
      sep = " "
    ))
  }
  # error message for phenocam data
  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(
      dpID, 
      "is a phenological image product, data are hosted by Phenocam.", 
      sep = " "
    ))
  }
  
  # error message for SAE data
  if(dpID == "DP4.00200.001"){
    stop(paste(
      "The bundled eddy covariance data product cannot be stacked and",
      "loaded directly from download.\nTo use these data, download with",
      "zipsByProduct() and then stack with stackEddy().", 
      sep = " "
    ))
  }
  
  # check for fasttime package, if used
  if(useFasttime & !requireNamespace("fasttime", quietly = T)) {
    stop(paste(
      "Parameter useFasttime is TRUE but fasttime package is not",
      "installed. Install and re-try.", 
      sep = " "
    ))
  }
}

check_zip_args <- function(dpID, package, startdate, enddate, avg, tabl){
  # error message if dates aren't formatted correctly
  # separate logic for each, to easily allow only one to be NA
  if (!is.na(startdate)) {
    if (regexpr("[0-9]{4}-[0-9]{2}", startdate) != 1) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  if (!is.na(enddate)) {
    if (regexpr("[0-9]{4}-[0-9]{2}", enddate) !=1 ) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  
  # warning message if using deprecated avg= instead of timeIndex=
  if (!is.na(avg)) {
    cat('Input parameter avg is deprecated; use timeIndex to download by time interval.\n')
  } else {
    avg <- timeIndex
  }
  
  # error message if using timeIndex & tabl
  if (avg != "all" & tabl != "all") {
    stop("Either timeIndex or tabl can be specified, but not both.")
  }
  
  # check and warning message if using tabl=
  if (tabl != "all") {
    message(paste("Warning: Downloading only table ", tabl, ". Downloading by table is not recommended unless you are already familiar with the data product and its contents.\n", 
                  sep = ""))
    if (!tabl %in% table_types$tableName) {
      message(paste("Warning: ", tabl, " is not in list of known tables. Download will be attempted, but check name and check neonUtilities for updates.\n", 
                    sep = ""))
    } else {
      if (!dpID %in% table_types$productID[which(table_types$tableName == tabl)]) {
        message(paste(tabl, " is a table in ", 
                      paste(table_types$productID[which(table_types$tableName == tabl)], collapse = " "), 
                      ", not in ", dpID, 
                      ". Download will be attempted, but check for updates.\n", sep = ""))
      }
      if ("lab-current" %in% table_types$tableType[which(table_types$tableName == tabl)] | 
         "lab-all" %in% table_types$tableType[which(table_types$tableName == tabl)]) {
        stop(paste("Download by table is not available for lab metadata tables. To get the complete dataset for table ", 
                   tabl, ", download the most recently published site and month of data for ", 
                   paste(table_types$productID[which(table_types$tableName == tabl)], collapse=" or "), 
                   ".", sep = ""))
      }
    }
  }
  
  # error for Phenocam data
  if (dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep = " "))
  }
  
  # error for Aeronet data
  if (dpID %in% c("DP1.00043.001")) {
    stop(paste("Spectral sun photometer (", dpID, ") data are hosted by Aeronet.", sep = ""))
  }
  
  # error for DHP expanded package
  if (dpID == "DP1.10017.001" & package == "expanded") {
    stop("Digital hemispherical images expanded file packages exceed R download limits. Either download from the data portal, or download the basic package and use the URLs in the data to download the images themselves. Follow instructions in the Data Product User Guide for image file naming.")
  }
  
  # error message for individual SAE products
  if(dpID %in% c('DP1.00007.001','DP1.00010.001','DP1.00034.001','DP1.00035.001',
                 'DP1.00036.001','DP1.00037.001','DP1.00099.001','DP1.00100.001',
                 'DP2.00008.001','DP2.00009.001','DP2.00024.001','DP3.00008.001',
                 'DP3.00009.001','DP3.00010.001','DP4.00002.001','DP4.00007.001',
                 'DP4.00067.001','DP4.00137.001','DP4.00201.001','DP1.00030.001')) {
    stop(paste(dpID, 'is only available in the bundled eddy covariance data product. Download DP4.00200.001 to access these data.', sep=' '))
  }
}

#' get the URLs of available products
#'
#' @param [dpID, release, token, site, avg] as in loadByProduct
get_avail_urls <- function(dpID, release, token = NA, site = "all", avg = NA,
                           release = "current"){
  # query the products endpoint for the product requested
  if (release == "current" | release == "PROVISIONAL") {
    prod.req <- neonUtilities:::getAPI(
      apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                     dpID, sep = ""), 
      token = token
    )
  } else {
    prod.req <- neonUtilities:::getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                      dpID, "?release=", release, sep = ""), token = token)
  }
  if (is.null(prod.req)) {
    stop("no such product available")
  }
  avail <- jsonlite::fromJSON(
    httr::content(prod.req, as = 'text', encoding = 'UTF-8'), 
    simplifyDataFrame = TRUE, flatten = TRUE
  )
  # get the urls for months with data available
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  # subset by sites if requested
  if(!"all" %in% site) {
    month.urls <- month.urls[sort(unlist(sapply(site, grep, month.urls)))]
  } else {
    month.urls <- month.urls
  }
  # error message if product not found
  if (!is.null(avail$error$status)) {
    if (release=="LATEST") {
      stop(paste("No data found for product ", dpID, 
                 ". LATEST data requested; check that token is valid for LATEST access.", sep = ""))
    } else {
      if (any(grepl("Release not found", avail$error$detail))) {
        stop(paste("Release not found. Valid releases for product ", dpID, 
                   " are ", paste0(avail$data$validReleases, collapse=" "), sep = ""))
      } else {
        stop(paste("No data found for product", dpID, sep=" "))
      }
    }
  }
  # check that token was used
  if (!is.na(token) & !is.null(prod.req$headers$`x-ratelimit-limit`)) {
    if (prod.req$headers$`x-ratelimit-limit` == 200) {
      cat('API token was not recognized. Public rate limit applied.\n')
    }
  }
  
  # error message if averaging interval is invalid
  if (avg!="all") {
    # if product is OS, proceed with normal download
    if (avail$data$productScienceTeamAbbr %in% c("TOS","AOS","AOP") |
       dpID %in% c("DP1.20267.001","DP1.00101.001","DP1.00013.001","DP1.00038.001")) {
      cat(paste(dpID, " is not a streaming sensor (IS) data product; cannot subset by averaging interval. Proceeding to download all available data.\n",
                sep = ""))
      avg <- "all"
    } else {
      # exceptions for water quality, SAE, summary weather statistics
      if (dpID %in% c("DP1.20288.001","DP4.00001.001","DP4.00200.001")) {
        cat(paste("Downloading by time interval is not available for ", dpID,
                  ". Proceeding to download all available data.\n", sep = ""))
        avg <- "all"
      } else {
        # check and make sure the averaging interval is valid for the product
        if (!avg %in% table_types$tableTMI[which(table_types$productID == dpID)]) {
          stop(paste(avg, " is not a valid time interval for ", dpID,
                     ". Use function getTimeIndex() to find valid time intervals.", sep = ""))
        }
      }
    }
  }
  
  # get the urls for months with data available
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  
  # error message if nothing is available
  if (length(month.urls) == 0) {
    stop("There are no data matching the search criteria.")
  }
  
  # subset by sites if requested
  if (!"all" %in% site) {
    month.urls <- month.urls[sort(unlist(sapply(site, grep, month.urls)))]
  } else {
    month.urls <- month.urls
  }
  
  # error message if nothing is available
  if (length(month.urls) == 0) {
    stop("There are no data at the selected site(s).")
  }
  
  # subset by dates if requested
  if (!is.na(startdate)) {
    datelist <- regmatches(month.urls, regexpr("20[0-9]{2}-[0-9]{2}", month.urls))
    month.urls <- month.urls[which(datelist >= startdate)]
  }
  
  # error message if nothing is available
  if(length(month.urls) == 0) {
    stop("There are no data at the selected date(s).")
  }
  
  if (!is.na(enddate)) {
    datelist <- regmatches(month.urls, regexpr("20[0-9]{2}-[0-9]{2}", month.urls))
    month.urls <- month.urls[which(datelist <= enddate)]
  }
  
  # error message if nothing is available
  if (length(month.urls) == 0) {
    stop("There are no data at the selected date(s).")
  }
  
  # return
  month.urls
}

new_loadByProduct <- function(){
  # check for errors
  check_lbp_args(dpID, package, useFastTime)
  
  # create a temporary directory to save to
  temppath <- file.path(
    tempdir(), paste("zips", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
  )
  dir.create(temppath)
  
  # do zipByProduct steps
}

#' @description
#' Pull files from the NEON API, by data product, merge data for each table, and read into the current R environment
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param avg Deprecated; use timeIndex
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param tabl Either the string 'all', or the name of a single data table to download. Defaults to 'all'.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param nCores The number of cores to parallelize the stacking procedure. By default it is set to a single core.
#' @param forceParallel If the data volume to be processed does not meet minimum requirements to run in parallel, this overrides. Set to FALSE as default.
#' @param token User specific API token (generated within neon.datascience user accounts)
#' @param useFasttime Should the fasttime package be used to read date-time fields? Defaults to false.
#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.
#' @return A named list of all the data tables in the data product downloaded, plus a validation file and a variables file, as available.
#' @examples
#' \dontrun{
#' # To download plant foliar properties data from all sites, expanded data package:
#' cfc <- loadByProduct(dpID="DP1.10026.001", site="all", package="expanded")
#' }
#' @export
loadByProduct <- function(dpID, site="all", startdate=NA, enddate=NA, package="basic",
                          release="current", timeIndex="all", tabl="all", 
                          check.size=TRUE, include.provisional=FALSE,
                          nCores=1, forceParallel=FALSE, token=NA_character_, 
                          useFasttime=FALSE, avg=NA) {
  
  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}[.][0-9]{5}[.]00[1-2]{1}", dpID)[1]!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
  }
  
  # error message if for AOP data
  if(substring(dpID, 5, 5)==3 & dpID!="DP1.30012.001") {
    stop(paste(dpID, "is a remote sensing data product and cannot be loaded directly to R with this function. Use the byFileAOP() function to download locally.", sep=" "))
  }
  
  # error message for phenocam data
  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }
  
  # error message for SAE data
  if(dpID == "DP4.00200.001"){
    stop("The bundled eddy covariance data product cannot be stacked and loaded directly from download.\nTo use these data, download with zipsByProduct() and then stack with stackEddy().")
  }
  
  # check for fasttime package, if used
  if(useFasttime & !requireNamespace("fasttime", quietly=T)) {
    stop("Parameter useFasttime is TRUE but fasttime package is not installed. Install and re-try.")
  }
  
  # create a temporary directory to save to
  temppath <- file.path(tempdir(), paste("zips", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
  dir.create(temppath)
  
  # pass the request to zipsByProduct() to download
  zipsByProduct(dpID=dpID, site=site, startdate=startdate, enddate=enddate, package=package,
                release=release, avg=avg, timeIndex=timeIndex, tabl=tabl, check.size=check.size, 
                savepath=temppath, include.provisional=include.provisional, load=TRUE, token=token)
  
  # if zipsByProduct() can't download anything, don't pass to stackByTable()
  if(length(list.files(temppath))==0) {
    return(invisible())
  }
  
  # stack and load the downloaded files using stackByTable
  out <- stackByTable(filepath=paste(temppath, "/filesToStack", substr(dpID, 5, 9), sep=""),
                      savepath="envt", folder=TRUE, nCores=nCores, 
                      saveUnzippedFiles=FALSE, useFasttime=useFasttime)
  # Remove temppath directory
  unlink(temppath, recursive=T)
  return(out)
}