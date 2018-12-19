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
.perma_cc_api_url <- "https://api.perma.cc/v1/public/archives/?url="
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
    newlst <- lapply(lst, from_perma_cc)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "perma_cc_url", "timestamp")
    return(df)
  } else if (source == "both") {
    newlst <- lapply(lst, function(x) {
      wb <- from_wayback(x)
      pc <- from_perma_cc(x)
      result <- list(x, "000", FALSE, "url not found", "unknown", "url not found", "unknown")
      if (as.logical((unname(unlist(wb)[3]))) && as.logical(unname(unlist(pc)[3]))) {
        result <- c(wb$url, wb$archived_snapshots$closest$status,
          wb$archived_snapshots$closest$available,
          wb$archived_snapshots$closest$url,
          wb$archived_snapshots$closest$timestamp, pc[[4]], pc[[5]])
      }
      return(result)
    })
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "wayback_url", "wayback_timestamp", "perma_cc_url", "perma_cc_timestamp")
    return(df)
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


#' Collect information on whether links in a url are archived.
#'
#' @param url The url to extract links from.
#' @param source Either "wayback," "perma_cc" or "both".
#' @return a dataframe containing the url, status, availability,
#'   archived url(s) and timestamp(s)
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

#' Check whether a url is available in Perma.cc
#'
#' @param url The url to check.
#' @return a vector containing
#'   the original url.
#'   the http status
#'   TRUE if successful or FALSE
#'   the archived url.
#'   the last time the url was crawled.
from_perma_cc <- function (url) {
  envelop = paste0(.perma_cc_api_url, url)
  reply <- fromJSON(envelop)
  result <- list(url, "000", FALSE, "url not found", "unknown")
  if (length(unlist(reply$objects))) {
    step <- unlist(reply$objects)
    status <- ifelse(step["captures.status"]=="success" || step["captures.status1"] == "success", "200", "000")
    available <- ifelse(step["captures.status"]=="success" || step["captures.status1"] == "success", TRUE, FALSE)
    playback_url <- ifelse(is.na(step["captures.playback_url"]), step["captures.playback_url1"], step["captures.playback_url"])
    timestamp <- ifelse(is.na(step["creation_timestamp"]), "unknown", step["creation_timestamp"])
    print (step["captures.playback_url1"])
    print (playback_url)
    result <- c(unname(step["url"]), unname(status), unname(available), unname(playback_url), unname(timestamp))
    print(result)
  }
  return(result)
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
