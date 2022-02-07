# API URLs

#' Default url for the Wayback Machine
.wb_available_url <- "http://archive.org/wayback/available?url="
.perma_cc_user_url <- "https://api.perma.cc/v1/user/?api_key="
#' Global var for the API key for perma.cc
.perma_cc_folder_pref <- "https://api.perma.cc/v1/folders/"
.wb_save_url <- "https://web.archive.org/save/"
.perma_cc_api_url <- "https://api.perma.cc/v1/public/archives/?url="
.perma_cc_post_api_url <- "https://api.perma.cc/v1/archives/?api_key="
.perma_cc_post_batch_api_url <- "https://api.perma.cc/v1/archives/batches?api_key="

.perma_cc_status_url <- function (id) {
  api <- get_api_key()
  url <- "https://api.perma.cc/v1/archives/batches/"
  key <- paste0("?api_key=", api)
  return (paste0(url, id, key))
}

# Perma CC API Keys handling

#' Set the api key(s) for Perma.cc apis, if required.
#'
#' @param key The Api Key.
#' @export
#' @examples
#' set_api_key("API_KEY")
set_api_key <- function (key) {
  old <- archiv_env$perma_cc_key
  assign('perma_cc_key', key, envir=archiv_env)
  invisible(old)
}

#' Get the perma.cc api key if set.
#' @export
#' @return The current api key state.
#' @examples
#' set_api_key("API KEY")
#' get_api_key()
get_api_key <- function() {
  get('perma_cc_key', envir=archiv_env)
}

# Perma CC Folder handling

#' Get the folder id of the perma.cc default folder (usually "Personal Links")
#'
#' @param default The number of the folder to accept by default.
#' @importFrom jsonlite fromJSON
#' @export
#' @return The id and name of the first top folder (usually "Personal Links")
#'    in perma.cc
#' @examples
#' \dontrun{
#' set_api_key("API_KEY")
#' permaFolder <- get_default_folder()
#' }

get_default_folder <- function (default=1) {
  perma_cc_key <- get('perma_cc_key', envir=archiv_env)
  if (perma_cc_key == "") {
    stop("Please input your perma.cc api key: Use 'set_api_key(API_KEY)'")
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

#' Get the folder id set for the current perma.cc api key.
#'
#' Will return `NULL` if no id has been explicitly set using [set_folder_id()]
#' @export
#' @return The current folder id state.
#' @seealso [get_folder_ids()]
#' @examples
#' set_folder_id("42")
#' get_folder_id()
get_folder_id <- function () {
  get('perma_cc_folder_id', envir=archiv_env)
}

#' Set the folder to save items in Perma.cc.
#'
#' @param id The folder id. This will be a string of numbers. If you do not
#'   know your folder id, get_folder_ids() will output a complete list of
#'   folders
#' @export
#' @seealso [get_folder_ids()]
#' @return TRUE
#' @examples
#' set_folder_id("42")
set_folder_id <- function (id) {
  old <- archiv_env$perma_cc_folder_id
  assign('perma_cc_folder_id', id, envir=archiv_env)
  invisible(old)
}

' Get the perma.cc folder ids starting from the default folder.
#'
#' Use this to find the id of the perma.cc folder you want to set via [set_folder_id()].
#' Requires API key to be set via [archivr::set_api_key()]
#' @importFrom jsonlite fromJSON
#' @export
#' @return A list of vectors with the top folder and all its children.
#' @examples
#' \dontrun{
#' set_api_key("API KEY")
#' permaFolderIDs <- get_folder_ids()
#' }

get_folder_ids <- function () {
  perma_cc_key <- get_api_key()
  reply <- NULL
  if (is.null(perma_cc_key) || perma_cc_key == "") {
    stop("Please input your perma.cc api key: Use 'set_api_key(API_KEY)'")
  } else {
    envelop = paste0(.perma_cc_user_url, perma_cc_key)
    data <- fromJSON(envelop)$top_level_folders
    if (length(unlist(data))) {
      for (row in 1:nrow(data))
        reply <- rbind(reply, .check_folder(data[row,]))
    } else {
      message("Error in extracting root folders in perma.cc.")
    }
  }
  return (reply)
}
