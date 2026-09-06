# R environment

The thesis code uses the following non-base R packages:

```text
apollo
 dplyr
 forcats
 ggpattern
 ggplot2
 haven
 patchwork
 purrr
 readr
 scales
 stringr
 tibble
 tidyr
```

Known version information recorded directly in the analysis code:

- `apollo`: tested against **0.3.8**
- `ggplot2`: **>= 4.0.2** is required by the harmonisation/plotting script because of `ggpattern`

## Creating the exact renv.lock

The thesis files supplied for repository preparation do not contain a saved `renv.lock` or a complete `sessionInfo()` record. It would therefore be misleading to invent exact package versions.

To capture the real environment, run `create_renv_lock.R` **on the same computer/R library used to run the final thesis analysis, before updating any packages**. The script checks the packages, initialises `renv`, and writes `renv.lock` at the repository root.

From the repository root:

```r
source("environment/create_renv_lock.R")
```

Then commit the newly created `renv.lock` to GitHub.

A collaborator can subsequently restore the recorded environment with:

```r
install.packages("renv")
renv::restore()
```
