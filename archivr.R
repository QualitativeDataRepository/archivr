if(!"jsonlite" %in% rownames(installed.packages())) {
  install.packages("jsonlite", repos="http://cran.us.r-project.org")
}
if(!"xml2" %in% rownames(installed.packages())) {
  install.packages("xml2", repos="http://cran.us.r-project.org")
}
if(!"rvest" %in% rownames(installed.packages())) {
  install.packages("rvest", repos="http://cran.us.r-project.org")
}
if(!"stringr" %in% rownames(installed.packages())) {
  install.packages("stringr", repos="http://cran.us.r-project.org")
}

library(jsonlite)
library(xml2)
library(rvest)
library(stringr)

#' Default url for the Wayback Machine
.wb_available_url <- "http://archive.org/wayback/available?url="
#' Global var for the API key for perma.cc
.perma_cc_key <- ""

#' Get archiving data from a list of Urls
#'
#' @param lst A list of urls to check.
#' @param source "wayback", "perma_cc" or "both".
#' @return A dataframe containing the original urls, their http status,
#'  availability, the archive url if it exists and a timestamp for the last
#'  web crawl.
archiv <- function (lst, source="wayback") {
  if (source == "perma_cc") {
    print ("This feature is not available... yet")
  } else if (source == "both") {
    print ("This feature is not available... yet")
  } else if (source == "wayback") {
    newlst <- lapply(lst, from_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "wayback_url", "timestamp")
    return (df)
  } else {
    print ("Could not confirm source.")
    return(FALSE)
  }
}

archiv.fromUrl <- function (url, source="wayback") {
  return(archiv(get_urls_from_webpage(url), source))
}

#' Check whether a url is available in the Wayback Machine
#'
#' @param url The url to check.
#' @return a jsonlite object where
#'   object$url is the original url.
#'   object$$archived_snapshots$closest$status is the http status
#'   object$archived_snapshots$closest$available is TRUE
#'   object$archived_snapshots$closest$url is the archived url.
#'   object$archived_snapshots$closest$timestamp is the last time the url
#'     was crawled.
from_wayback <- function (url) {
  envelop = paste0(.wb_available_url, url)
  reply <- fromJSON(envelop)
  result <- list(url, "000", FALSE, "url not found", "unknown")
  if (length(reply$archived_snapshots)) {
    result = reply
  }
  return (result)
}

#' Set the api key(s) for Perma.cc apis, if required.
#'
#' @param key The Api Key.
#' @return TRUE
#' @examples
#' add("", 1)
set_api_key <- function (key) {
    .perma_cc_key <<- key
}

get_urls_from_webpage <- function (url) {
  pg <- read_html(url)
  lst <- unique(html_attr(html_nodes(pg, "a"), "href"))
  Filter(function(x)
    startsWith(x, "http"), lst)
}
