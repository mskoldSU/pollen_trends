# Standalone regression test for read_sheet().
# From the repository root, run: Rscript tests/test-read-sheet.R
# Requires: tidyverse, readxl, writexl, lubridate, assertthat

suppressPackageStartupMessages(library(tidyverse))
source("R/functions.R")

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Install the test dependency with install.packages('writexl')")
}

# Build a minimal workbook with the columns expected by read_sheet().
years <- as.character(2019:1979)
sample_data <- as.data.frame(
  setNames(as.list(rep(0, length(years))), years),
  check.names = FALSE
)
sample_data <- cbind(date = as.Date("2019-04-15"), sample_data)
# The original workbook has a blank first-column header (readxl calls it ...1).
names(sample_data)[1] <- ""
sample_data[["2019"]] <- 7

file <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(Betula = sample_data), file)

# The supplied file must be used for BOTH sheet validation and reading.
result <- read_sheet("Betula", file = file)
stopifnot(
  nrow(result) == length(years),
  all(result$species == "Betula"),
  result$value[result$year == 2019] == 7
)

# A nonexistent sheet must fail validation for this workbook.
failure <- tryCatch(read_sheet("Alnus", file = file), error = identity)
stopifnot(inherits(failure, "error"))
stopifnot(grepl("Alnus is not a sheet in", conditionMessage(failure), fixed = TRUE))

unlink(file)
cat("All read_sheet regression tests passed.\n")
