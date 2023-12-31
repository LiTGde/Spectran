% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{examplespectra_descriptor}
\alias{examplespectra_descriptor}
\title{Details on the example spectra used by Spectran}
\format{
\code{examplespectra_descriptor} A list with an element per language. The following description covers elements within each language element, which is a tibble with 11 rows and 8 columns.
\describe{
\item{Name}{Category of the spectrum (short)}
\item{Beschreibung}{Short description of the category of the spectrum}
\item{Identifier}{A list of character vectors containing the unique identifiers of each spectrum}
\item{Button_Name}{A list of character vectors containing the text to be displayed on the buttons for each spectrum}
\item{Dateinamen}{A list of character vectors containing the text used for the download-filenames}
\item{URL}{A character vector with URLs to videos giving some context to the measurements or NA, in which case nothing will be displayed}
\item{embargo}{A logical vector that indicates whether a special message should be displayed before downloading this spectrum}
\item{download}{A list of named character vectors containing a short description of the spectrum.}
}
}
\source{
\url{https://cie.co.at/data-tables}
}
\usage{
examplespectra_descriptor
}
\description{
Contains additional details on the measurement and CIE Standard Illuminant Spectra collected in \link{examplespectra}.
}
\keyword{datasets}
