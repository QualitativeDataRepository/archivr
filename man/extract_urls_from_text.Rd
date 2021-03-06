% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_urls.R
\name{extract_urls_from_text}
\alias{extract_urls_from_text}
\title{Get the urls from a text file, .pdf file, .docx file, or string}
\usage{
extract_urls_from_text(fp, except = NULL)
}
\arguments{
\item{fp}{A filepath or string.}

\item{except}{A regular expression for URLs to exclude from extraction}
}
\value{
a List of Urls.
}
\description{
`extract_urls_from_text` is called internally from `archiv.fromText` but can be
useful as a separate function if you want to filter which links you archive.
Text extraction relies on the [readtext::readtext()] function from the package of
the same name, so all file formats supported by `readtext` are supported.
}
\examples{
\dontrun{
urlList <- extract_urls_from_text("textdoc.docx", except="doi\\\\.org\\\\/")
}
}
