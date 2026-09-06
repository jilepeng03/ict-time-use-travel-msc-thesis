# Is ICT changing how people spend and organise their day?

**Evidence from two harmonised UK time-use surveys, 2014–15 and 2023, with particular attention to travel**

This repository contains the R code supporting Jile Peng's MSc Transport dissertation at the Department of Civil and Environmental Engineering, Imperial College London (2026), supervised by Professor Aruna Sivakumar.

## Project summary

Information and communication technologies (ICT) may substitute for travel, complement it, or modify how activities and trips are organised. This project studies these mechanisms using two harmonised waves of UK time-use diary data (2014–15 and 2023), with the National Travel Survey (NTS) used as an independent benchmark for diary-based travel measures.

The analysis uses a multiple discrete–continuous extreme value (MDCEV) model of daily time allocation. ICT is represented through two channels: overall ICT exposure in baseline utility (the participation margin) and in-activity ICT in the satiation term (the duration margin). Separate-wave models, a pooled period-interaction specification, model-implied reallocation, and a two-stage cross-fitted robustness check are included.

## Research questions

1. How has the share of daily time involving ICT changed across activity groups, and in travel in particular?
2. Are the diary-based travel patterns consistent with an independent official source, the National Travel Survey?
3. Is ICT exposure associated with substitution for, or enrichment of, daily activities, and does this differ between overall ICT exposure and in-activity ICT, with travel as the key case?
4. Have these associations changed over the period, and does any apparent change survive the difference in how device use was measured between the waves?

## Repository structure

```text
.
├── README.md
├── CITATION.cff
├── LICENSE
├── .gitignore
├── .Rprofile
├── renv.lock
├── ict-time-use-travel-msc-thesis.Rproj
├── code/
│   ├── 01_harmonised_timeuse_analysis.R
│   ├── 02_mdcev_pooled_analysis.R
│   ├── 03_mdcev_2stage_crossfit.R
│   ├── 04_mdcev_prediction_decomposition.R
│   └── 05_travel_timing_validation.R
├── data/
│   └── README.md
├── environment/
│   ├── README.md
│   ├── create_renv_lock.R
│   └── PACKAGE_CITATIONS.md
├── renv/
│   ├── activate.R
│   └── settings.json
└── results/
    └── README.md
```

## Data

The survey microdata are **not redistributed in this repository**. The study uses:

- UK Time Use Survey 2014–15 (UK Data Service Study 8128)
- CTUR UK Time Use Survey, March 2023 (UK Data Service Study 9336)
- National Travel Survey (UK Data Service Study 5340), using 2014–15 and 2023 for validation

See [`data/README.md`](data/README.md) for the expected local filenames and access notes. Data should only be obtained and used under the terms of the original data providers/licences.

## How to run the analysis

Run R with the **repository root as the working directory**. Do not set the working directory to `code/`, because paths are written relative to the repository root.

### 1. Harmonisation and descriptive analysis

```r
source("code/01_harmonised_timeuse_analysis.R")
```

This harmonises the two time-use waves, constructs activity/ICT/location measures, creates descriptive outputs, prepares the episode-level analysis file, and performs the main NTS contextual comparisons.

### 2. MDCEV estimation

```r
source("code/02_mdcev_pooled_analysis.R")
```

This estimates the separate 2014–15 and 2023 models and the pooled period-interaction model. The headline specification uses raw ICT shares; alternative scaling specifications are defined within the script.

### 3. Two-stage cross-fitted robustness check

This script depends on objects created in Sections 1–3 of `02_mdcev_pooled_analysis.R`. Follow the instructions at the top of the script, then run:

```r
source("code/03_mdcev_2stage_crossfit.R")
```

### 4. Prediction and decomposition

```r
source("code/04_mdcev_prediction_decomposition.R")
```

This uses saved MDCEV model objects to validate baseline predictions and decompose the model-implied ICT reallocation into participation and duration channels.

### 5. Travel timing and external validation

```r
source("code/05_travel_timing_validation.R")
```

This produces travel-timing distributions and the detailed time-use-survey-versus-NTS validation outputs. If the required harmonised objects are not present, the script attempts to source the first script automatically.

## Outputs

Generated figures, tables, model objects, and diagnostics are written to `combined_outputs_harmonised/`. This generated directory is excluded from Git because the contents can be recreated from the source data and code. The `results/` folder is reserved for selected shareable outputs if these are later added to the public repository.

## R environment and package citations

The analysis uses R packages including `haven`, `dplyr`, `tidyr`, `readr`, `ggplot2`, `forcats`, `scales`, `stringr`, `ggpattern`, `purrr`, `tibble`, `patchwork`, and `apollo`. The MDCEV scripts were tested against **Apollo 0.3.8**, and the plotting script requires **ggplot2 >= 4.0.2**.

The R environment used for the thesis is recorded in `renv.lock` (R 4.5.1), generated from the original R installation used for the final analysis. To restore the recorded package environment, open the project in R and run:

```r
renv::restore()
```

See [`environment/README.md`](environment/README.md) for environment details and [`environment/PACKAGE_CITATIONS.md`](environment/PACKAGE_CITATIONS.md) for package citation information.

## Thesis

MSc Transport dissertation, Imperial College London, submitted August 2026. A public thesis link can be added here if/when one becomes available.

## Citation

Citation metadata for this repository are provided in [`CITATION.cff`](CITATION.cff). Once the repository is public, GitHub can use this file to display a **Cite this repository** option.

## Licence

The repository is currently being prepared as a private thesis repository. The included `LICENSE` is deliberately restrictive pending confirmation with the supervisor about the appropriate open-source licence and the data/IP position. Replace it with the agreed licence (for example, MIT if approved) before presenting the repository as openly reusable.

## Contact

Repository owner: **Jile Peng**  
GitHub: **@jilepeng03**
