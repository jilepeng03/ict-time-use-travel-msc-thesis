# Data access and local file layout

The raw survey microdata used in this project are not included in the GitHub repository. They should be obtained from the original data provider and used under the applicable licence/terms.

## Data sources

### UK Time Use Survey 2014–15
- UK Data Service Study **8128**
- Used for the 2014–15 time-use diary wave.
- The analysis is restricted to adults aged 18+ for comparability with the 2023 survey.

Expected local files used by the scripts:

```text
uktus15_diary_ep_long.dta
uktus15_individual.dta
```

### CTUR UK Time Use Survey, March 2023
- UK Data Service Study **9336**
- Used for the 2023 time-use diary wave.

Expected local file:

```text
eliddi_episode_long.dta
```

### National Travel Survey
- UK Data Service Study **5340**
- The dissertation uses 2014–15 and 2023 for external validation of diary-based travel measures.

Files used by the validation workflow may include:

```text
nts_trip_2014_5_2023.dta
individual_eul_2002-2024.dta
day_eul_2002-2024.dta
household_eul_2002-2024.dta
```

The validation code can also recognise the full trip-file name `trip_eul_2002-2024.dta` in place of the pre-filtered trip file where applicable.

## Local folder layout

After obtaining the data, place the required files in this `data/` directory:

```text
data/
├── eliddi_episode_long.dta
├── uktus15_diary_ep_long.dta
├── uktus15_individual.dta
├── nts_trip_2014_5_2023.dta
├── individual_eul_2002-2024.dta
├── day_eul_2002-2024.dta
└── household_eul_2002-2024.dta
```

The repository `.gitignore` excludes `.dta` files, helping prevent accidental upload of licensed microdata.

## Redistribution

Do **not** commit the raw microdata to this repository unless the relevant data-provider licence explicitly permits redistribution and this has been agreed with the supervisor. This repository is designed so that code and documentation can be shared without redistributing the survey microdata.
