#' `Spec`ifications for visual and nonvisual data wrangling and plotting
#'
#' Contains vectors and tables that are the main basis for the calculations
#' Spectran does and also contain basic naming and coloring conventions.
#'
#' @format `Specs` A list with 7 elements
#' \describe{
#'   \item{AS_wide}{A table of action spectra for the 5 human receptor types and V(lambda) from 1924. The table is in the wide format. The unit of the wavelength (Wellenlaenge) is nm, the unit of the action spectrum is 1 (dimensionless).}
#'   \item{AS_long}{A table of action spectra for the 5 human receptor types and V(lambda) from 1924. The table is in the long format. The unit of the wavelength (Wellenlaenge) is nm, the unit of the action spectrum is 1 (dimensionless).}
#'   \item{Efficacy}{A named vector of inverse values of efficacy of luminous radiation for the 5 human receptor types and V(lambda) from 1924. The unit is lm/W.}
#'   \item{Alpha}{A list. `names` contains the names of the 5 human receptor types. `adjectives` contains these names in their adjective form for every languages included in Spectran. `descriptions` contains the descriptions used for tables and plotting. `abb` contains the abbreviations (if any) used for the equivalent daylight illuminance for that receptor type.}
#'   \item{Vlambda}{A character scalar containing the properly escaped version of V(lambda), used e.g. in Plots}
#'   \item{Alpha.ico}{A character scalar containing the properly escaped version of the sign `alpha`, used e.g. in Plots}
#'   \item{Plot}{A table of Peak Wavelength (nm) values, Names, Abbreviations, and colors for the 5 human receptor types and V(lambda) from 1924. This table is mainly used for plotting.}
#'
#' }
#' @source [CIE
#'   S026](<https://cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0>)
#'
#'   [DIN/TS
#'   5031-100](<https://www.dinmedia.de/de/vornorm/din-ts-5031-100/343737176>)
"Specs"

#' Hex Color pallets for Spectran
#'
#' Contains named vectors of Hex color codes for Spectran's color pallets.
#'
#' @format `ColorP` A list containing 5 elements, each a named vector of colors
#' \describe{
#'   \item{Lang}{401 colors along the spectrum that make a nice, yet nonaccurate representation of the visible spectrum. Created from a picture provided by `Dieter Lang`.}
#'   \item{Lang_bright}{same as `Lang` but with a slightly brightened version of the picture. Strikes a nice balance between "accuracy" and "nice colors". Is the default when starting [Spectran()]}
#'   \item{Dan_Bruton}{401 colors along the spectrum that were made with the `cooltools::wavelength2col()` function and are based on Fortran Code by Dan_Bruton. Probably most accurate, but black at both spectral ends, which does not represent the colors themselves but also human sensitivity to them.}
#'   \item{Rainbow}{401 colors along the spectrum that make a vibrant and pleasing, yet nonaccurate representation of the visible spectrum. Created by the [grDevices::rainbow()] function.}
#'   \item{Color_Rendering}{14 colors that represent the reference colors used to determine the color rendering index.}
#' }
#' @source [grDevices::rainbow()]
#' 
#' [cooltools](https://github.com/obreschkow/cooltools)
#' 
#' <https://github.com/LiTGde/Spectran/issues/14>
"ColorP"

#' Example spectra used by Spectran
#'
#' Contains Measurement and CIE Standard Illuminant Spectra. Additional data to the spectra can be found in the [examplespectra_descriptor] dataset.
#'
#' @format `examplespectra` A list of two elements, each containing a table of spectral data
#' \describe{
#'   \item{Measurement}{Spectral data measured by a spectroradiometer. The unit of the wavelength (Wellenlaenge) is nm, the unit of the spectral data is W/m^2.}
#'   \item{CIE}{Spectral data from the CIE describing Standard Illuminants. The unit of the wavelength (Wellenlaenge) is nm, the unit of the spectral data is W/m^2.}
#' }
#' @source <https://cie.co.at/data-tables>
"examplespectra"

#' Details on the example spectra used by Spectran
#'
#' Contains additional details on the measurement and CIE Standard Illuminant Spectra collected in [examplespectra].
#'
#' @format `examplespectra_descriptor` A list with an element per language. The following description covers elements within each language element, which is a tibble with 11 rows and 8 columns.
#' \describe{
#'   \item{Name}{Category of the spectrum (short)}
#'   \item{Beschreibung}{Short description of the category of the spectrum}
#'   \item{Identifier}{A list of character vectors containing the unique identifiers of each spectrum}
#'   \item{Button_Name}{A list of character vectors containing the text to be displayed on the buttons for each spectrum}
#'   \item{Dateinamen}{A list of character vectors containing the text used for the download-filenames}
#'   \item{URL}{A character vector with URLs to videos giving some context to the measurements or NA, in which case nothing will be displayed}
#'   \item{embargo}{A logical vector that indicates whether a special message should be displayed before downloading this spectrum}
#'   \item{download}{A list of named character vectors containing a short description of the spectrum.}
#' }
#' @source <https://cie.co.at/data-tables>
"examplespectra_descriptor"
