

#' Creates a json string from a list of urls.
#' @param url_list a list of URLs
#' @return a JSON string of the URLs
.list_string <- function (url_list) {
  quotes <- paste('"', url_list, '"', sep="")
  string <- paste (quotes, sep=", ", collapse=", ")
  return (paste0("'[", string, "]'"))
}


#' Works with get_subfolders to flatten the folder ids tree
#' @param folder_list a list of perma.cc folder objects
#' @return A list of vectors with the id and name.
.check_folder <- function(folder_list) {
  if (is.null(folder_list)) {
    folder_list
  } else if (folder_list['has_children'] == "FALSE") {
    subset(folder_list, select=c("id", "name"))
  } else {
    rbind(unname(c(folder_list['id'], folder_list['name'])), .get_subfolders(folder_list['id']))
  }
}

#' Works with check_folder to flatten folder ids tree
#' @param id A folder id
#' @importFrom jsonlite fromJSON
#' @return A list of vectors with the id and name.
.get_subfolders <- function (id) {
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
      fold <- .check_folder(data[row,])
      reply <- rbind(reply, fold)
    }
    return(reply)
  }
}
