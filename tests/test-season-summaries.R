# Standalone regression tests for season_summaries().
# From repository root: Rscript tests/test-season-summaries.R
# Requires: tidyverse

suppressPackageStartupMessages(library(tidyverse))
source("R/functions.R")

# Deliberately unordered input: cumulative sums must use chronological days.
observations <- tibble(
  species = rep("Betula", 3),
  year = rep(2020, 3),
  day = c(102, 103, 101),
  value = c(5, 3, 2)
)
extract_summary <- function(data) {
  season_summaries(data, "Betula") %>%
    filter(species == "Betula", year == 2020) %>%
    select(measure, value) %>%
    arrange(measure)
}

expected <- tibble(
  measure = c("first_day", "high_season", "high_start",
              "mean_day", "median_day", "season"),
  value = c(101, 3, 101, 102.1, 102, 2)
)
actual <- extract_summary(observations)
stopifnot(
  identical(actual$measure, sort(expected$measure)),
  isTRUE(all.equal(
    actual$value,
    expected$value[match(actual$measure, expected$measure)]
  ))
)

# Reordering rows must not change the computed seasonal summaries.
set.seed(123)
shuffled <- observations[sample(nrow(observations)), ]
stopifnot(isTRUE(all.equal(actual, extract_summary(shuffled))))

# Exactly 10% of the seasonal maximum does not count as high season,
# because the current scientific definition uses a strict > threshold.
threshold <- tibble(
  species = "Alnus", year = 2021,
  day = 1:3, value = c(1, 10, 0)
)
threshold_summary <- season_summaries(threshold, "Alnus")
stopifnot(
  threshold_summary$value[threshold_summary$measure == "high_season"] == 1,
  threshold_summary$value[threshold_summary$measure == "high_start"] == 2
)

# Current behavior: years with no positive observations have no summaries.
zero <- tibble(species = "Betula", year = 2022, day = 1:3, value = 0)
stopifnot(nrow(season_summaries(zero, "Betula")) == 0)

cat("All season_summaries regression tests passed.\n")
