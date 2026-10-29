# Details on the example spectra used by Spectran

Contains additional details on the measurement and CIE Standard
Illuminant Spectra collected in
[examplespectra](https://litgde.github.io/Spectran/reference/examplespectra.md).

## Usage

``` r
examplespectra_descriptor
```

## Format

`examplespectra_descriptor` A list with an element per language. The
following description covers elements within each language element,
which is a tibble with 11 rows and 8 columns.

- Name:

  Category of the spectrum (short)

- Beschreibung:

  Short description of the category of the spectrum

- Identifier:

  A list of character vectors containing the unique identifiers of each
  spectrum

- Button_Name:

  A list of character vectors containing the text to be displayed on the
  buttons for each spectrum

- Dateinamen:

  A list of character vectors containing the text used for the
  download-filenames

- URL:

  A character vector with URLs to videos giving some context to the
  measurements or NA, in which case nothing will be displayed

- embargo:

  A logical vector that indicates whether a special message should be
  displayed before downloading this spectrum

- download:

  A list of named character vectors containing a short description of
  the spectrum.

## Source

<https://cie.co.at/data-tables>
