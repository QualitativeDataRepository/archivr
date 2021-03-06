% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apitools.R
\name{get_folder_id}
\alias{get_folder_id}
\title{Get the folder id set for the current perma.cc api key.}
\usage{
get_folder_id()
}
\value{
The current folder id state.
}
\description{
Will return `NULL` if no id has been explicitly set using [set_folder_id()]
}
\examples{
set_folder_id("42")
get_folder_id()
}
\seealso{
[get_folder_ids()]
}
