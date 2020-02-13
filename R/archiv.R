#' Archive a list of urls in Wayback or perma_cc.
#'
#' @param url_list A list of urls to archive.
#' @param method Either "wayback" or "perma_cc." Defaults to "wayback."
#' @export
#' @return A dataframe containing the original urls, the urls to the
#'   archived website and a timestamp. For Perma.cc also the URL to the
#'   screenshot, and the short URL.
#' @examples
#' \dontrun{
#' urls <- c("https://qdr.syr.edu", "https://cran.r-project.org/", "https://apsa.net")
#'
#' # archive in Wayback machine
#' archivedUrls <- archiv(urls)
#'
#' # archive in perma.cc
#' set_api_key("API KEY")
#' set_folder_id("FOLDER ID")
#' archivedUrls <- archiv(urls, method="perma_cc")
#' }
#'

archiv <- function (url_list, method="wayback") {
  if (method == "perma_cc") {
    fold <- get_folder_id()
    if (is.null(fold) || fold == "") {
      message("Setting folder based on api key.")
      set_folder_id(get_default_folder())
      fold <- toString(get_folder_id())
      if (is.null(fold) || fold == "") {
        stop("Unable to set perma.cc folder. Make sure you API key is set using
             'set_api_key(API_KEY)'")
      }}
    newlst <- lapply(url_list, archiv_perma)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "GUID", "timestamp", "perma_cc_url", "perma_cc_screenshot",
                      "perma_cc_short_url")
    return(df)
  } else {
    newlst <- lapply(url_list, archiv_wayback)
    df <- data.frame(matrix(unlist(newlst), nrow=length(newlst), byrow=T))
    colnames(df) <- c("url", "available", "wayback_url", "timestamp")
    return (df)
  }
}


#' Saves a single url in perma.cc.
#'
#' @param arc_url The url to archive.
#' @importFrom jsonlite fromJSON
#' @import curl
#' @export
#' @return A list or object representing the result.
#' @examples
#' \dontrun{
#' set_api_key("API KEY")
#' set_folder_id("FOLDER ID")
#' archivedUrl <- archiv_perma("https://qdr.syr.edu")
#' }

archiv_perma <- function (arc_url) {
  api <- get_api_key()
  fold <- toString(get_folder_id())
  if (is.null(api) || api == "") {
    stop("API key not set for perma.cc. Use 'set_api_key() to set your key before
         using method='perma_cc'")
  }
  folder_url <- paste0()
  api_url <- paste0(.perma_cc_post_api_url, api)
  setting <- new_handle()
  handle_setopt(setting, customrequest = "POST")
  handle_setform(setting, url = arc_url, folder = fold)
  result <- list(arc_url, "noguid", "unknown", "no url", "no screenshot", "no short url")
  r <- curl_fetch_memory(api_url, setting)
  reply <- fromJSON(rawToChar(r$content))
  if ((!(is.null(reply$detail))) && reply$detail == "Authentication credentials
      were not provided.") {
    stop("Please input your api key:\nUse 'set_api_key(API_KEY)'")
  } else if ((!(is.null(reply$error)))) {
    stop("Received an error reply, likely because your limit has been exceeded.")
  } else {
    if (!is.null(reply$url) && !(reply$url == "Not a valid URL.")) {
      result <- c(reply$url, reply$guid, reply$archive_timestamp,
                  reply$captures[1,]$playback_url, reply$captures[2,]$playback_url,
                  paste0("https://perma.cc/", reply$guid))
    } else {
    }
  }
  return(result)
}

#' Save a url on the wayback machine.
#' @param arc_url - the url to archive.
#' @import curl
#' @export
#' @return A list representing the result.
#' @examples
#' \dontrun{
#' archivedUrl <- archiv_wayback("https://qdr.syr.edu")
#' }
archiv_wayback <- function (arc_url) {
  envelop <- paste0(.wb_save_url, arc_url)
  reply <- curl_fetch_memory(envelop)
  if (reply$status_code == 200) {
    result <- view_wayback(arc_url)
  } else {
    message (paste0 ("Discovered an error in saving the url. Received http status ",
                     reply$status_code, ". Perhaps try again at another time."))
    result <- view_wayback(arc_url)
  }
  return(result)
}



#' Save the links in a url in perma.cc or Wayback.
#'
#' @param url The url to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @param except A regular expression for URLs to exclude from extraction
#' @export
#' @return a dataframe containing the url, availability,
#'   archived url(s) and timestamp(s)
#' @examples
#' \dontrun{
#' # Wayback
#' archivedURLs <- archiv.fromUrl(
#'      "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
#'      except="validator\\.w3\\.org"
#'      )
#'
#' #perma.cc
#' set_api_key("API KEY")
#' set_folder_id("42")
#' archivedURLs <- archiv.fromUrl(
#'      "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
#'      method="perma_cc"
#'      )
#' }
archiv.fromUrl <- function (url, method="wayback", except = NULL) {
  return(archiv(extract_urls_from_webpage(url, except), method))
}

#' Save the links in a text file (docx, pdf, markdown) in perma.cc or Wayback.
#'
#' @param fp The filepath to extract links from.
#' @param method Either "wayback," "perma_cc" or "both".
#' @param except A regular expression for URLs to exclude from extraction
#' @export
#' @return a dataframe containing the url, availability,
#'   archived url(s) and timestamp(s)
#' @examples
#' \dontrun{
#' # Wayback
#' archivedURLs <- archiv.fromText("testdoc.docx", except="doi\\.org\\/")
#'
#' #perma.cc
#' set_api_key("API KEY")
#' set_folder_id("42")
#' archivedURLs <- archiv.fromText("testdoc.docx", method="perma_cc")
#' }
archiv.fromText <- function (fp, method="wayback", except = NULL) {
  return(archiv(extract_urls_from_text(fp, except), method))
}


