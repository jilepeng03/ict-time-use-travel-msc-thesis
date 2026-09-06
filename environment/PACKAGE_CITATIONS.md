# Package and software citations

## Apollo

The MDCEV estimation is implemented with the `apollo` R package. The thesis cites:

Hess, S. and Palma, D. (2019). “Apollo: A flexible, powerful and customisable freeware package for choice model estimation and application.” *Journal of Choice Modelling*, 32, 100170. https://doi.org/10.1016/j.jocm.2019.100170

The analysis scripts note compatibility with Apollo **0.3.8**.

## Other R packages

The code also relies on `dplyr`, `forcats`, `ggpattern`, `ggplot2`, `haven`, `patchwork`, `purrr`, `readr`, `scales`, `stringr`, `tibble`, and `tidyr`.

Package-maintainer citation recommendations can change between package versions. The exact package versions used for the thesis are recorded in `renv.lock`. Preferred citations for the installed versions can be obtained directly in R, for example:

```r
citation("ggplot2")
citation("dplyr")
citation("haven")
citation("readr")
citation("tidyr")
citation("apollo")
```

This avoids attributing a citation belonging to a package version different from the one actually used for the thesis.
