#' @docType package
#' @name archivr
#' @title Archive URLs in Perma.cc or the Wayback Machine
#' @description Provides functionality to archive URLs contained in lists, files, or websites
#' @details Link rot is by now a well-known problem in academic publishing (e.g. Hennessey and Ge, 2013 <doi:10.1186/1471-2105-14-S14-S5>). A significant share of URLs cited in academic articles are no longer reachable after relatively short periods of time. This is particularly true for research that uses sources beyond the US and Western European mainstream. Especially where those sources are not on high-traffic websites, researchers can also not necessarily rely on the Internet Archive's wayback machine to automatically crawl and save them.   
#' 
#' archivr is a toolkit to help researchers and/or the institutions supporting them (such as data repositories, libraries, or publishers) with large amounts of URLs. It takes a list of URLs and uses either perma.cc (<https://perma.cc/>, see Zittrain and Albert 2013 <doi:10.2139/ssrn.2329161>) or the Internet Archive's Wayback Machine <https://archive.org/web/> archives to store the webpages for future reference. It will also parse documents or webpages for URLs to be archived, and can check the archiving status of a list of URLs in the Internet Archive and the (public) perma.cc archives.
#' 
#' The package provides three main sets of functions along these lines:
#' 
#' \itemize{
#' \item Archiving: the \code{\link{archiv}} function provides the main archiving functionality, with \code{\link{archiv.fromText}} automating it for all URLs in a document (for all document types supported by the readtext package) and \code{\link{archiv.fromUrl}} for all URLs on a webpage.
#' \item Extract: the \code{\link{extract_urls_from_webpage}} and \code{\link{extract_urls_from_text}} function provide the URL extraction functionality as a standalone. This can be useful to check for issues or to avoid archiving URLs unnecessarily.
#' \item View: the \code{\link{view_archiv}} function checks whether a list of URLs can be found in perma.cc or the Internet Archive and returns the timestamp of the last available copy. \code{\link{view_archiv.fromText}} \code{\link{view_archiv.fromUrl}} perform this check for all URLs in a document or webpage as above 
#' }
#' 
#' 
#' 
NULL


archiv_env <- new.env()
archiv_env$perma_cc_key <- ""
archiv_env$perma_cc_folder_id <- NULL


