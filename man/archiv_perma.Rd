% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/archiv.R
\name{archiv_perma}
\alias{archiv_perma}
\title{Saves a single url in perma.cc.}
\usage{
archiv_perma(arc_url)
}
\arguments{
\item{arc_url}{The url to archive.}
}
\value{
A list or object representing the result.
}
\description{
Saves a single url in perma.cc.
}
\examples{
\dontrun{
set_api_key("API KEY")
set_folder_id("FOLDER ID")
archivedUrl <- archiv_perma("https://qdr.syr.edu")
}
}
