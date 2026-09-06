# Create an exact renv.lock from the R library used for the final thesis analysis.
# IMPORTANT: Run this on the original analysis machine BEFORE updating packages.
# Run from the repository root:
#   source("environment/create_renv_lock.R")

required_packages <- c(
  "apollo", "dplyr", "forcats", "ggpattern", "ggplot2", "haven",
  "patchwork", "purrr", "readr", "scales", "stringr", "tibble", "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "These packages are missing from the current R library: ",
    paste(missing_packages, collapse = ", "),
    ".\nDo not install new versions if your goal is to capture the original thesis environment. ",
    "Run this script in the R library/environment used for the final analysis."
  )
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialise renv without changing the packages already installed in the user's library.
if (!file.exists("renv/activate.R")) {
  renv::init(bare = TRUE, restart = FALSE)
}

# Record only packages used by the project and their dependencies.
renv::snapshot(
  packages = required_packages,
  prompt = FALSE
)

cat("\nCreated renv.lock using:\n")
cat(R.version.string, "\n\n")
for (pkg in required_packages) {
  cat(sprintf("%-12s %s\n", pkg, as.character(utils::packageVersion(pkg))))
}
cat("\nCommit renv.lock to the repository.\n")
