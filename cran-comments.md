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