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

## Reproducing the R environment

The exact R environment used for the final thesis analysis is recorded in the repository-level `renv.lock`.

The lockfile was generated from the original analysis environment using:

- R 4.5.1
- Apollo 0.3.8
- ggplot2 4.0.3

It also records the versions of all other required packages and dependencies.

To restore the environment after cloning or downloading the repository, open the project in R and run:

```r
install.packages("renv")
renv::restore()
```

`renv` will use the repository-level `renv.lock` to restore the recorded package versions.

Package citation information is provided in [`PACKAGE_CITATIONS.md`](PACKAGE_CITATIONS.md).
