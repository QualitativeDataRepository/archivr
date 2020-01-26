#' Extracts the urls from a webpage.
#'
#' The function works simply by extracting the `href`` attribute from all `a` nodes.
#' It is called internally from `archiv.fromUrl` but can be useful as a separate function
#' if you want to filter which links you archive.
#' @param url The url to extract urls.
#' @param except A regular expression for URLs to exclude from extraction
#' @importFrom rvest html_attr html_nodes 
#' @importFrom xml2 read_html
#' @export
#' @return a vector of urls.
#' @examples
#' urlList <- extract_urls_from_webpage(
#'      "https://www-cs-faculty.stanford.edu/~knuth/retd.html",
#'      except="validator\\.w3\\.org"
#'      )
extract_urls_from_webpage <- function (url, except = NULL) {
  pg <- xml2::read_html(url)
  lst <- unique(html_attr(html_nodes(pg, "a"), "href"))
  if (!is.null(except)) {
    lst <- Filter(function(x) !grepl(except, x), lst)
  }
  Filter(function(x)
    startsWith(x, "http"), lst)
}

#' Get the urls from a text file, .pdf file, .docx file, or string
#'
#' `extract_urls_from_text` is called internally from `archiv.fromText` but can be
#' useful as a separate function if you want to filter which links you archive.
#' Text extraction relies on the [readtext::readtext()] function from the package of
#' the same name, so all file formats supported by `readtext` are supported.
#' @param fp A filepath or string.
#' @param except A regular expression for URLs to exclude from extraction
#' @importFrom readtext readtext
#' @importFrom tools file_ext
#' @import xml2
#' @export
#' @return a List of Urls.
#' @examples
#' \dontrun{
#' urlList <- extract_urls_from_text("textdoc.docx", except="doi\\.org\\/")
#' }
extract_urls_from_text <- function (fp, except = NULL) {
  url_pattern <-
    "(http[s]?:?\\/\\/|www)(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  readtext_ext <- c("txt", "json", "csv", "tab", "tsv", "pdf", "odt", "doc", "docx", "rtf")
  text <- tryCatch({
    if (file_ext(fp) %in% readtext_ext) {
      gsub("\\s", " ", readtext(fp)$text)
    }
    else if (file_ext(fp) == "xml") {
      # Get all text and all attributes and concate them to a single string
      xmlText <- read_xml(fp) %>% xml_find_all(xpath= "//text()")  %>%
        sapply(paste, collapse = " ") %>% paste(collapse= " ")
      xmlAttr <- read_xml(fp) %>% xml_find_all(xpath= "//.") %>% xml_attrs() %>%
        sapply(paste, collapse = " ") %>% paste(collapse= " ")
      paste(xmlText, xmlAttr, sep=" ")
    }
    else if (file_ext(fp) == "html" || file_ext(fp) == "htm") {
      # read only the a href attributes starting with http
      # and immediately return (no need for regex here)
      pg <- xml2::read_html(fp)
      lst <- unique(html_attr(html_nodes(pg, "a"), "href"))
      if (!is.null(except)) {
        lst <- Filter(function(x) !grepl(except, x), lst)
      }
      Filter(function(x)
        startsWith(x, "http"), lst)
    }
    else {
      readChar(fp, file.info(fp)$size)
    }
  }, warning = function(w) {
    fp
  }, error = function(e) {
  }, finally = {
  })
  ext <- gregexpr(url_pattern, text)
  result1 <- unique(unlist(regmatches(text, ext)))
  # remove except
  if (!is.null(except)) {
    result1 <- Filter(function(x) !grepl(except, x), result1)
  }
  result2 <- sapply(result1, function(x) {
    last <- sub(".+(.)$", "\\1", x)
    if (last == ">" || last == ")" || last =="," || last =='<') {
      return(sub(".$", "",x))
    } else {
      return(x)
    }
  })
  return (result2)
}


#' #' Get the urls from all text, pdf, or docx files in a folder
#' #' Currently disabled
#' #' @param fp A filepath or string.
#' #' @import readtext
#' #' @import stringr
#' #' @export
#' #' @return A list of urls.
#' extract_urls_from_folder <- function (fp) {
#'   url_pattern <- "(http[s]?:)?[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})?(\\/.*)?"
#'   text <- readtext(fp)
#'   text <- Reduce(paste, readtext(fp)$text)
#'   ext <- gregexpr(url_pattern, text)
#'   result1 <- unique(unlist(regmatches(text, ext)))
#'   result2 <- sapply(result1, function(x) {
#'     last <- str_sub(x, start=-1)
#'     if (last == ">" || last == ")") {
#'       return(str_sub(x, 0, -2))
#'     } else {
#'       return(x)
#'     }
#'   })
#'   return (result2)
#' }
