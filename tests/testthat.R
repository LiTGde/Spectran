library(testthat)
library(Spectran)

# Keep package checks independent of user-level cache permissions.
options(sass.cache = FALSE)

test_check("Spectran")
