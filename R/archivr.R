#' Copyright <2019> <Qualitative Data Repository, Syracuse University>

#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:

#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software.

#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#' THE SOFTWARE.


#' Archivr: Save Your Websites in Perma.cc or the Wayback Machine
#'
#' Archivr is a toolkit for the long-run archiving of Qualitative data.
#' It takes a list of urls and uses either the perma.cc or Wayback Machine
#' archives to store the webpages for future reference. It will also parse
#' word or html documents for urls to be archived.
#' @docType package
#' @name archivr

library(readtext)
library(jsonlite)
library(xml2)
library(rvest)
library(stringr)
library(curl)
library(tools)
library(textreadr)


archiv_env <- new.env()
archiv_env$perma_cc_key <- ""
archiv_env$perma_cc_folder_id <- 0

#' Get the folder id and name from all text files in a perma.cc folder
#'
#' @importFrom jsonlite fromJSON
#' @export
#' @return The id and name of the first top folder (usually "Personal Links")
#'    in perma.cc
get_default_folder <- function (default=1) {
  perma_cc_key <- get('perma_cc_key', envir=archiv_env)
  if (perma_cc_key == "") {
    reply <- FALSE
  } else {
    envelop = paste0(.perma_cc_user_url, perma_cc_key)
    data <- fromJSON(envelop)
    id <- data$top_level_folders[default]$id
    folder_name <- data$top_level_folders[default]$name
    reply <- c(id, folder_name)
  }
  return(reply)
}

#' Default url for the Wayback Machine
.wb_available_url <- "http://archive.org/wayback/available?url="
.perma_cc_user_url <- "https://api.perma.cc/v1/user/?api_key="
#' Global var for the API key for perma.cc
.perma_cc_folder_pref <- "https://api.perma.cc/v1/folders/"
.wb_save_url <- "https://web.archive.org/save/"
.perma_cc_api_url <- "https://api.perma.cc/v1/public/archives/?url="
.perma_cc_post_api_url <- "https://api.perma.cc/v1/archives/?api_key="
.perma_cc_post_batch_api_url <- "https://api.perma.cc/v1/archives/batches?api_key="

.perma_cc_status_url <- function (id, api="") {
  if (is.null(api)) {
    api <- get_api_key()
  }
  url <- "https://api.perma.cc/v1/archives/batches/"
  key <- paste0("?api_key=", api)
  return (paste0(url, id, key))
}

#' Archive a list of urls in perma_cc.
#'
#' @param url_list A list of urls to archive.
#' @param method Either "wayback" or "perma_cc." Defaults to "wayback."
#' @export
#' @return A dataframe containing the original urls, the urls to the
#'   archived website, the screenshot and a timestamp.
archiv <- function (url_list, method="wayback") {
  if (method == "perma_cc") {
    newlst <- lapply(url_list, archiv_url)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "GUID", "timestamp", "perma_cc_url", "perma_cc_screenshot", "perma_cc_short_url")
    return(df)
  } else {
    newlst <- lapply(url_list, archiv_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "wayback_url", "timestamp")
    return (df)
  }
}

#' Save a batch of urls to a folder - THIS CURRENTLY DOES NOT WORK.
#' @import curl
#' @param url_list A vector of urls to archive.
#' @param api (Optional api key)
#' @param folder (Mandatory, but defaults to .folder_id)
archiv_batch <- function (url_list, api="", folder="") {
  api_url <- paste0(.perma_cc_post_batch_api_url, api)
  setting <- new_handle()
  handle_setopt(setting, customrequest = "POST")
  handle_setform(setting, urls=list_string(url_list), target_folder=folder)
  r <- curl_fetch_memory(api_url, setting)
  reply <- fromJSON(rawToChar(r$content))
  if ((!(is.null(reply$detail))) && reply$detail == "Authentication credentials were not provided.") {
    result <- "Please input your api key:\nUse 'set_api_key(API_KEY)'"
  } else if ((!(is.null(reply$error)))) {
    result <- "Received an error reply, likely because your limit has been exceeded."
  } else {
    result <- reply$id
    return(result)
  }
}

#' Creates a json string from a list of urls.
#'
#' @param url_list A list of urls.
#' @export
#' @return A json string representing the list.
list_string <- function (url_list) {
  quotes <- paste('"', url_list, '"', sep="")
  string <- paste (quotes, sep=", ", collapse=", ")
  return (paste0("'[", string, "]'"))
}

#' Saves a single url in either perma.cc or the wayback machine.
#'
#' @param arc_url The url to archive.
#' @param method Either "perma_cc" or the default, "wayback."
#' @importFrom jsonlite fromJSON
#' @import curl
#' @export
#' @return A list or object representing the result.
archiv_url <- function (arc_url, fold="", api="", method="perma_cc") {
  if (is.null(api)) {
    api <- get_api_key()
  }
  if (is.null(fold)) {
    fold <- get_folder_id()
  }
  if (method == "perma_cc") {
    folder_url <- paste0()
    api_url <- paste0(.perma_cc_post_api_url, api)
    setting <- new_handle()
    handle_setopt(setting, customrequest = "POST")
    handle_setform(setting, url = arc_url, folder = fold)
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
    return (archiv_wayback(arc_url))
  }

}

#' Save a url on the wayback machine.
#' @param arc_url - the url to archive.
#' @import curl
#' @export
#' @return A list or object representing the result.
archiv_wayback <- function (arc_url) {
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
#' @param method "wayback", "perma_cc" or "both".
#' @export
#' @return A dataframe containing the original urls, their http status,
#'  availability, the archive url if it exists and a timestamp for the last
#'  web crawl.
view_archiv <- function (lst, method="wayback") {
  if (typeof(lst) == "")
  if (method == "perma_cc") {
    newlst <- lapply(lst, from_perma_cc)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "perma_cc_url", "timestamp")
    return(df)
  } else if (method == "both") {
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
  } else if (method == "wayback") {
    newlst <- lapply(lst, from_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "status", "available?", "wayback_url", "timestamp")
    return (df)
  } else {
    print ("Could not confirm method.")
    return(FALSE)
  }
}


#' Collect information on whether links in a url are archived.
#'
#' @param url The url to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, status, availability,
#'   archived url(s) and timestamp(s)
view_archiv.fromUrl <- function (url, method="wayback") {
  return(view_archiv(extract_urls_from_webpage(url), method))
}

#' Collect information on whether links in a file are archived.
#'
#' @param fp The filepath to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, status, availability,
#'   archived url(s) and timestamp(s)
view_archiv.fromText <- function (fp, method="wayback") {
  return(view_archiv(extract_urls_from_text(fp), method))
}

#' Save the links in a url in perma.cc or Wayback.
#'
#' @param url The url to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, status, availability,
#'   archived url(s) and timestamp(s)
archiv.fromUrl <- function (url, method="wayback") {
  return(archiv(extract_urls_from_webpage(url), method))
}

#' Save the links in a text file (docx, pdf, markdown) in perma.cc or Wayback.
#'
#' @param fp The filepath to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, status, availability,
#'   archived url(s) and timestamp(s)
archiv.fromText <- function (fp, method="wayback") {
  return(archiv(extract_urls_from_text(fp), method))
}

#' Check whether a url is available in the Wayback Machine
#'
#' @param url The url to check.
#' @importFrom jsonlite fromJSON
#' @export
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
  } else {
    print("Received a NULL value from archived snapshots from Wayback.")
  }
  return (result)
}

#' Check whether a url is available in Perma.cc
#'
#' @param url The url to check.
#' @importFrom jsonlite fromJSON
#' @export
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
  } else {
    print ("An error occurred when retrieving perma_cc objects.")
  }
  return(result)
}

#' Set the api key(s) for Perma.cc apis, if required.
#'
#' @param key The Api Key.
#' @export
set_api_key <- function (key) {
  old <- archiv_env$perma_cc_key
  assign('perma_cc_key', key, envir=archiv_env)
  invisible(old)
}

#' Set the folder to save items in Perma.cc.
#'
#' @param id The folder id. This will be a string of numbers. If you do not
#'   know your folder id, get_folder_ids() will output a complete list of
#'   folders
#' @export
#' @return TRUE
set_folder_id <- function (id) {
  old <- archiv_env$perma_cc_folder_id
  assign('perma_cc_folder_id', id, envir=archiv_env)
  invisible(old)
}

#' Extracts the urls from a webpage.
#'
#' @param url The url to extract urls.
#' @import rvest xml2
#' @export
#' @return a vector of urls.
extract_urls_from_webpage <- function (url) {
  pg <- read_html(url)
  lst <- unique(html_attr(html_nodes(pg, "a"), "href"))
  Filter(function(x)
    startsWith(x, "http"), lst)
}

#' Get the urls from a text file or string
#'
#' @param fp A filepath or string.
#' @import readtext
#' @import stringr
#' @import tools
#' @export
#' @return a List of Urls.
extract_urls_from_text <- function (fp) {
  url_pattern <- "(http[s]?:?\\/\\/|www)(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  text <- tryCatch({
    if (file_ext(fp) == "docx") {
      gsub("\\s", " ", readtext(fp)$text)
    } else {
      readChar(fp, file.info(fp)$size)
    }
  }, warning = function(w) {
    fp
  }, error = function(e) {
  }, finally = {
  })
  ext <- gregexpr(url_pattern, text)
  result1 <- unique(unlist(regmatches(text, ext)))
  result2 <- sapply(result1, function(x) {
    last <- str_sub(x, start=-1)
    if (last == ">" || last == ")" || last ==",") {
      return(str_sub(x, 0, -2))
    } else {
      return(x)
    }
  })
  return (result2)
}

#' Get the urls from all text files in a folder
#'
#' @param fp A filepath or string.
#' @import readtext
#' @import stringr
#' @export
#' @return A list of urls.
extract_urls_from_folder <- function (fp) {
  url_pattern <- "(http[s]?:)?[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})?(\\/.*)?"
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

#' Works with get_subfolders to flatten the folder ids tree
#' @param folder_list a list of perma.cc folder objects
#' @export
#' @return A list of vectors with the id and name.
check_folder <- function(folder_list) {
  if (is.null(folder_list)) {
    folder_list
  } else if (folder_list['has_children'] == "FALSE") {
    subset(folder_list, select=c("id", "name"))
  } else {
    rbind(unname(c(folder_list['id'], folder_list['name'])), get_subfolders(folder_list['id']))
  }
}

#' Works with check_folder to flatten folder ids tree
#' @param id A folder id
#' @importFrom jsonlite fromJSON
#' @export
#' @return A list of vectors with the id and name.
get_subfolders <- function (id) {
  perma_cc_key <- get('perma_cc_key', envir=archiv_env)
  if (perma_cc_key == "") {
    NULL
  } else if (is.null(id)) {
    NULL
  } else {
    .perma_cc_folder_suff <- paste0("/folders?api_key=", perma_cc_key)
    envelop <- paste0(.perma_cc_folder_pref, id, .perma_cc_folder_suff)
    data <- fromJSON(envelop)$objects
    reply <- NULL
    for (row in 1:nrow(data)) {
      fold <- check_folder(data[row,])
      reply <- rbind(reply, fold)
    }
  return(reply)
  }
}

#' Get the api key if set.
#' @export
#' @return The current api key state.
get_api_key <- function() {
  archiv_env$perma_cc_key
}

#' Get the root folder id for the current api key.
#' @export
#' @return The current folder id state.
get_folder_id <- function () {
  archiv_env$perma_cc_folder_id
}

#' Get the folder ids starting from the default folder.
#' @importFrom jsonlite fromJSON
#' @export
#' @return A list of vectors with the top folder and all its children.
get_folder_ids <- function () {
  perma_cc_key <- get('perma_cc_key', envir=archiv_env)
  reply <- NULL
  if (is.null(perma_cc_key)) {
    reply <- FALSE
  } else {
    envelop = paste0(.perma_cc_user_url, perma_cc_key)
    data <- fromJSON(envelop)$top_level_folders
    if (length(unlist(data))) {
      for (row in 1:nrow(data))
        reply <- rbind(reply, check_folder(data[row,]))
    } else {
      print ("Error in extracting root folders in Perma.cc.")
    }
  }
  return (reply)
}
