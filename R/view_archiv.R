#' Get archiving data from a list of Urls
#'
#' @param lst A list of urls to check.
#' @param method "wayback", "perma_cc" or "both".
#' @export
#' @return A dataframe containing the original urls,
#'  availability, the archive url if it exists and a timestamp for the last
#'  web crawl.
#'
#'  Where method is "both", "availability" is TRUE if the URL is archived by either
#'  service
#' @examples
#' \dontrun{
#' urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://apsa.net")
#' checkArchiveStatus <- view_archiv(urls, method="both")
#' }
#'
view_archiv <- function (lst, method="wayback") {
  if (method == "perma_cc") {
    newlst <- lapply(lst, view_perma_cc)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=TRUE))
    colnames(df) <- c("url", "available", "perma_cc_url", "timestamp")
    return(df)
  } else if (method == "wayback") {
    newlst <- lapply(lst, view_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=TRUE))
    colnames(df) <- c("url","available", "wayback_url", "timestamp")
    return (df)
  } else if (method == "both") {
    newlst <- lapply(lst, function(x) {
      wb <- view_wayback(x)
      pc <- view_perma_cc(x)
      result <- list(x,  FALSE, "url not found", "unknown", "url not found", "unknown")
      if (isTRUE(wb[[2]]) || isTRUE(pc[[2]])) {
        result <- c(wb[[1]], TRUE, wb[[3]], wb[[4]], pc[[3]], pc[[4]])
      }
      return(result)
    })
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=TRUE))
    colnames(df) <- c("url",  "available", "wayback_url", "wayback_timestamp",
                      "perma_cc_url", "perma_cc_timestamp")
    return(df)
  } else {
    warning ("Could not confirm method.")
    return(FALSE)
  }
}


#' Collect information on whether links contained in a webpage are archived.
#'
#' @param url The url of the webpage to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, availability,
#'   archived url(s) and timestamp(s)
#' @examples
#' \dontrun{
#' checkArchiveStatus <- view_archiv.fromUrl(
#'    "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
#'    method="both"
#'  )
#' }
#' 
view_archiv.fromUrl <- function (url, method="wayback") {
  return(view_archiv(extract_urls_from_webpage(url), method))
}

#' Collect information on whether links in a file are archived.
#'
#' @param fp The filepath to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @export
#' @return a dataframe containing the url, availability,
#'   archived url(s) and timestamp(s)
#' @examples
#' \dontrun{
#' checkArchiveStatus <- view_archiv.fromText("testfile.docx", method="both")
#' }

view_archiv.fromText <- function (fp, method="wayback") {
  return(view_archiv(extract_urls_from_text(fp), method))
}


#' Check whether a url is available in the Wayback Machine
#'
#' @param url The url to check.
#' @importFrom jsonlite fromJSON
#' @export
#' @return a list containing
#'   the original url.
#'   TRUE if successful or FALSE
#'   the archived url.
#'   the last time the url was crawled.
#' @examples
#' checkArchiveStatus <- view_wayback(
#'     "https://www-cs-faculty.stanford.edu/~knuth/retd.html"
#'     )
view_wayback <- function (url) {
  envelop = paste0(.wb_available_url, url)
  reply <- fromJSON(envelop)
  result <- list(url, FALSE, "url not found", "unknown")
  if (length(reply$archived_snapshots)) {
    wb <- reply$archived_snapshots$closest
    result = list(url, wb$available, wb$url, wb$timestamp)
  } else {
    message(paste("Received a NULL value from archived snapshots from Wayback for",
                url))
  }
  return (result)
}

#' Check whether a url is available in Perma.cc
#'
#' @param url The url to check.
#' @importFrom jsonlite fromJSON
#' @export
#' @return a list containing
#'   the original url.
#'   TRUE if successful or FALSE
#'   the archived url.
#'   the last time the url was crawled.
#' @examples
#' checkArchiveStatus <- view_perma_cc(
#'     "https://www-cs-faculty.stanford.edu/~knuth/retd.html"
#'     )
view_perma_cc <- function (url) {
  envelop <-  paste0(.perma_cc_api_url, url)
  reply <- fromJSON(envelop)
  result <- list(url, FALSE, "url not found", "unknown")
  if (length(unlist(reply$objects))) {
    # We do have results; grabbing the first one
    step <- unlist(reply$objects[1,])
    available <- ifelse(step["captures.status"]=="success" || step["captures.status1"] == "success",
                        TRUE, FALSE)
    
    perma_url <- ifelse(is.na(step["guid"]), "url not found",
                           paste0("https://perma.cc/", unname(step["guid"])))
    
    
    timestamp <- ifelse (is.na(step["creation_timestamp"]), "unknown",
                         step["creation_timestamp"])


    result <- list(unname(step["url"]), available, unname(perma_url), unname(timestamp))
  } else if (length(unlist(reply$meta))) {
    # perma_cc returns 0 objects but does return meta for a valid call
    message(paste("No perma.cc object found for ", url))
  } else {
    # something has gone actually wrong
    warning("An error occurred when retrieving perma_cc objects.")
  }
  return(result)
}
