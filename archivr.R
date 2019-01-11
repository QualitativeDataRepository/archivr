#' The following may not be necessary once packaging is set up.

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
if(!"readtext" %in% rownames(installed.packages())) {
  install.packages("readtext", repos="http://cran.us.r-project.org")
}
if(!"curl" %in% rownames(installed.packages())) {
  install.packages("curl", repos="http://cran.us.r-project.org")
}

library(readtext)
library(jsonlite)
library(xml2)
library(rvest)
library(stringr)
library(curl)

#' Default url for the Wayback Machine
.wb_available_url <- "http://archive.org/wayback/available?url="
#' Global var for the API key for perma.cc
.perma_cc_key <- ""
.wb_save_url <- "https://web.archive.org/save/"
.perma_cc_api_url <- "https://api.perma.cc/v1/public/archives/?url="
.perma_cc_post_api_url <- "https://api.perma.cc/v1/archives/?api_key="
.perma_cc_post_batch_api_url <- "https://api.perma.cc/v1/archives/batches?api_key="
.folder_id <- 1
.perma_cc_status_url <- function (id, api=.perma_cc_key) {
  url <- "https://api.perma.cc/v1/archives/batches/"
  key <- paste0("?api_key=", api)
  return (paste0(url, id, key))
}

#' Archive a list of urls in perma_cc.
#'
#' @param url_list A list of urls to archive.
#' @param method Either "wayback" or "perma_cc." Defaults to "wayback."
#' @return A dataframe containing the original urls, the urls to the
#'   archived website, the screenshot and a timestamp.
archiv <- function (url_list, method="wayback") {
  if (method == "perma_cc") {
    newlst <- lapply(url_list, save_url)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "GUID", "timestamp", "perma_cc_url", "perma_cc_screenshot", "perma_cc_short_url")
    return(df)
  } else {
    newlst <- lapply(url_list, save_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "wayback_url", "timestamp")
    return (df)
  }
}

#' Save a batch of urls to a folder
#' @param url_list A vector of urls to archive.
#' @param api (Optional api key)
#' @param folder (Mandatory, but defaults to .folder_id)
save_batch <- function (url_list, api=.perma_cc_key, folder=.folder_id) {
  api_url <- paste0(.perma_cc_post_batch_api_url, api)
  print(api_url)
  setting <- new_handle()
  handle_setopt(setting, customrequest = "POST")
  handle_setform(setting, urls=list_string(url_list), target_folder=folder)
  r <- curl_fetch_memory(api_url, setting)
  print(r)
  reply <- fromJSON(rawToChar(r$content))
  print(reply)
  if ((!(is.null(reply$detail))) && reply$detail == "Authentication credentials were not provided.") {
    result <- "Please input your api key:\nUse 'set_api_key(API_KEY)'"
  } else if ((!(is.null(reply$error)))) {
    result <- "Received an error reply, likely because your limit has been exceeded."
  } else {
    result <- reply$id
    return(result)
  }
}

list_string <- function (url_list) {
  quotes <- paste('"', url_list, '"', sep="")
  string <- paste (quotes, sep=", ", collapse=", ")
  return (paste0("'[", string, "]'"))
}

save_url <- function (arc_url, api=.perma_cc_key, method="perma_cc") {
  if (method == "perma_cc") {
    api_url <- paste0(.perma_cc_post_api_url, api)
    setting <- new_handle()
    handle_setopt(setting, customrequest = "POST")
    handle_setform(setting, url = arc_url)
    result <- list(arc_url, "noguid", "unknown", "no url", "no screenshot", "no short url")
    r <- curl_fetch_memory(api_url, setting)
    reply <- fromJSON(rawToChar(r$content))
    if ((!(is.null(reply$detail))) && reply$detail == "Authentication credentials were not provided.") {
      result <- "Please input your api key:\nUse 'set_api_key(API_KEY)'"
    } else if ((!(is.null(reply$error)))) {
      result <- "Received an error reply, likely because your limit has been exceeded."
    } else {
      if (!(reply$url == "Not a valid URL.")) {
        result <- c(reply$url, reply$guid, reply$archive_timestamp,
          reply$captures[1,]$playback_url, reply$captures[2,]$playback_url,
        paste0("https://perma.cc/", reply$guid))
      }
      return(result)
    }
  } else if (method == "wayback") {
    return (save_wayback(arc_url))
  }

}

save_wayback <- function (arc_url) {
  envelop <- paste0(.wb_save_url, arc_url)
  reply <- curl_fetch_memory(envelop)
  if (reply$status_code == 200) {
    result <- from_wayback(arc_url)
  } else {
    print (paste0 ("Discovered an error in saving the url. Received http status ",
   reply$status_code, ". Perhaps try again at another time."))
   result <- from_wayback(arc_url)
  }
  return(result)
}

#' Get archiving data from a list of Urls
#'
#' @param lst A list of urls to check.
#' @param source "wayback", "perma_cc" or "both".
#' @return A dataframe containing the original urls, their http status,
#'  availability, the archive url if it exists and a timestamp for the last
#'  web crawl.
view_archiv <- function (lst, source="wayback") {
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
view_archiv.fromUrl <- function (url, source="wayback") {
  return(view_archiv(get_urls_from_webpage(url), source))
}

view_archiv.fromText <- function (fp, source="wayback") {
  return(view_archiv(extract_urls_from_text(fp), source))
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
    result <- c(unname(step["url"]), unname(status), unname(available), unname(playback_url), unname(timestamp))
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

#' Extracts the urls from a webpage.
#'
#' @param url The url to extract urls.
#' @return a vector of urls.
get_urls_from_webpage <- function (url) {
  pg <- read_html(url)
  lst <- unique(html_attr(html_nodes(pg, "a"), "href"))
  Filter(function(x)
    startsWith(x, "http"), lst)
}


#' Get the urls from a text file or string
#'
#' @param fp A filepath or string.
extract_urls_from_text <- function (fp) {
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  text <- tryCatch({
    readChar(fp, file.info(fp)$size)
  }, warning = function(w) {
    fp
  }, error = function(e) {
  }, finally = {
  })
  ext <- gregexpr(url_pattern, text)
  result1 <- unique(unlist(regmatches(text, ext)))
  result2 <- sapply(result1, function(x) {
    last <- str_sub(x, start=-1)
    if (last == ">" || last == ")") {
      return(str_sub(x, 0, -2))
    } else {
      return(x)
    }
  })
  return (result2)
}

extract_urls_from_folder <- function (fp) {
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  text <- readtext(fp)
  text <- Reduce(paste, readtext(fp)$text)
  ext <- gregexpr(url_pattern, text)
  result1 <- unique(unlist(regmatches(text, ext)))
  result2 <- sapply(result1, function(x) {
    last <- str_sub(x, start=-1)
    if (last == ">" || last == ")") {
      return(str_sub(x, 0, -2))
    } else {
      return(x)
    }
  })
  return (result2)
}
