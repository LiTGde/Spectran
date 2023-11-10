## Resubmission #2 (Version 1.0.3)

This is a resubmission. In this version I have made changes as per your request from 6 November 2023:

* Please use only undirected quotation marks in the description text. e.g. `Spectran` --> 'Spectran'

  I have changed directed to undirected quotations in the description text

* It seems as you have too many blank spaces in your description text, probably because line breaks count as blank spaces too. Please omit these.

  I have removed blank spaces in the description text at the beginning of each line in the description text

* If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
  authors (year) <doi:...>
  authors (year) <arXiv:...>
  authors (year, ISBN:...)
  or if those are not available: <https:...>
  with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
  (If you want to add a title as well please put it in quotes: "Title")

  I have added the references to the description field of the DESCRIPTION file.

* Size of tarball: 5282320 bytes
  -> A CRAN package should not be larger than 5 MB. Please reduce the size.

  I have reduced the file size of the package to below 5 MB.

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions) e.g.: in R/import_eigen.R you have: print(cat("Developer Troubleshootn"))

  All functions that use the print()/cat() method are internal functions used for development and troubleshooting. They are not used in the exported functions, nor do the exported functions call those internal functions. Thus this should not be a problem.

* Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().

  I did not find any functions that write by default or in the examples/vignettes/tests in the user's home filespace.

## Resubmission #1 (Version 1.0.2)
This is a resubmission. In this version I have:

* changed the erroneous `testduplicates-packae` from the documentation of `Spectran-package.R` to the correct `Spectran-package`.

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* Imports includes 35 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
  
  I have moved all the packages that are not used in the main functions to Suggests.
  
## check_win_devel
  
* Possibly misspelled words in DESCRIPTION:
  Spectran (9:51)
  CSV (10:34)
  melanopic (9:5)
  
  These are not misspelled words.