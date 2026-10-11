# Reproducing the R analyses and tests

Run these commands from the repository root.

## Required software

- R (a recent stable release) and the R packages `tidyverse`, `readxl`, `writexl`, `lubridate`, `assertthat`, and `ggridges`.
- RStudio is optional; it can be used to run the scripts locally.
- The README figures and analyses additionally use `knitr` and `rmarkdown`.

Install the dependencies in R:

```r
install.packages(c(
  "tidyverse", "readxl", "writexl", "lubridate",
  "assertthat", "ggridges", "knitr", "rmarkdown"
))
```

## Run regression tests without original data

In the RStudio Terminal (or another terminal), run:

```sh
Rscript tests/test-read-sheet.R
Rscript tests/test-season-summaries.R
```

The Excel-import regression test generates a temporary workbook. Both tests are standalone and **do not require the research workbook**.

GitHub Actions runs the same tests on pull requests, pushes to `master`, and manual dispatches. See [the workflow](.github/workflows/r-tests.yml) and each PR's Checks tab.

## Reproduce figures and statistical results

`readme.Rmd` is the source for the rendered `readme.md`. Its analysis requires the **external** historical data workbook, expected at:

```text
data/Pollen Stockholm dayly values 1979-2019.xlsx
```

That workbook is not present in this repository. Obtain the appropriate workbook separately and place it at the expected path before rendering the document. The existing import expects a particular sheet and column layout (see [Issue #2](https://github.com/mskoldSU/pollen_trends/issues/2)). With the workbook available, render from the repository root in R:

```r
rmarkdown::render("readme.Rmd")
```

The current setup documents package requirements but **does not pin package versions**. Consider `renv` if exact historical dependency reproduction or collaboration becomes a priority; do not treat passing current-package tests as evidence of bit-for-bit historical reproducibility.
