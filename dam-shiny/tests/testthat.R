# Standard testthat entry point. dam-shiny is a plain Shiny app (not an R
# package), so we point test_dir at tests/testthat directly; setup.R sources
# the engine from R/ before the test files run.
#
# Run from the app root with:  Rscript tests/testthat.R
library(testthat)

this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
if (is.na(this_dir) || !nzchar(this_dir)) {
  this_dir <- if (dir.exists("tests")) "tests"
              else if (dir.exists("testthat")) "."
              else stop("Run from the dam-shiny app root: Rscript tests/testthat.R")
}

testthat::test_dir(file.path(this_dir, "testthat"), stop_on_failure = TRUE)
