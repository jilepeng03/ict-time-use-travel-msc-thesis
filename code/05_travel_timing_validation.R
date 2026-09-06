
# Colour palette for TUS vs NTS external validation figures
validation_source_palette <- c(
  "TUS harmonised travel episodes" = "#7B61FF",
  "NTS trip file: England, ages 18+, all days" = "#F39C12"
)
# ============================================================
# Travel timing distributions  (ADD-ON MODULE)
# Adds to: 01_harmonised_timeuse_analysis.R
#
# Supervisor request:
#   For travel / mobility episodes, describe and plot -- for BOTH periods --
#     A. When travel happens across the day     (30-minute resolution)
#     B. Distribution of episode START and END times (10-minute, overlaid)
#     C. Distribution of episode DURATION        (10-minute; native)
#
# Resolution choices:
#   * T1 (time-of-day profile) uses 30-minute slots: a smoother backdrop that
#     stays readable for the smaller 2023 sample.
#   * T2 (start/end) and T3 (duration) use the native 10-minute grid, which is
#     lossless (all diary times/durations are exact 10-minute multiples).
#   * Both periods always share the SAME resolution so the lines stay comparable.
#
# Design notes (kept consistent with the main analysis):
#   * One row = one TRAVEL TRIP. We use the merged primary-activity spells
#     (df2014_primary_merged / df2023_primary_merged). Adjacent travel episodes
#     (codes 110-117 -> 110) are already merged in the main script, and these
#     are PRIMARY rows only, so there is no double counting.
#   * All metrics are weighted and expressed per diary-day using the SAME
#     period_day_denominator, so unequal sample sizes do not distort the plots.
#   * x-axis is clock time (diary day runs 04:00 -> 04:00, so 0-4h is the
#     late-night / early-morning tail). Trips crossing midnight wrap via %% 1440.
#
# HOW TO RUN:
#   Run the main harmonisation script first, THEN:
#     source("code/05_travel_timing_validation.R")
#   If the required objects are missing, this script will try to source the
#   main script automatically (set MAIN_SCRIPT below).
# ============================================================

# ---- 0. Guard: make sure the main script objects exist ----
MAIN_SCRIPT <- "code/01_harmonised_timeuse_analysis.R"

needed <- c("df2014_primary_merged", "df2023_primary_merged",
            "period_day_denominator", "presentation_theme", "activity_levels",
            "df2014_raw", "df2023_raw")

if (!all(vapply(needed, exists, logical(1)))) {
  if (file.exists(MAIN_SCRIPT)) {
    message("Required objects not found - sourcing main script: ", MAIN_SCRIPT)
    source(MAIN_SCRIPT)
  } else {
    stop(
      "Run the main harmonisation script first. It builds the objects this ",
      "module needs (df2014_primary_merged, df2023_primary_merged, ",
      "period_day_denominator, presentation_theme). Then source this file."
    )
  }
}

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tidyr)
  library(readr); library(stringr); library(forcats)
})

# ---- Config ----
# Change FOCUS_GROUP to reuse this module for another activity group,
# e.g. "work" or "leisure". Default "travel" answers the supervisor request.
FOCUS_GROUP  <- "travel"
T1_SLOT_MIN  <- 30    # time-of-day profile resolution
BE_SLOT_MIN  <- 30    # start/end resolution (30-min slots to reduce jitter)

out_dir <- "combined_outputs_harmonised/figures_travel_timing"
tab_dir <- "combined_outputs_harmonised/tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Helpers ----
# Weighted quantile (for the weighted median).
wtd_quantile <- function(x, w, probs = 0.5) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  stats::approx(cw, x, xout = probs, ties = "ordered", rule = 2)$y
}

# Format minutes-since-midnight as HH:MM (clock time).
min_to_hhmm <- function(m) {
  m <- m %% 1440
  sprintf("%02d:%02d", floor(m / 60), round(m %% 60))
}

# Allocate each episode's minutes into fixed time-of-day slots of `slot_min`
# minutes (analogous to the main script's expand_episode_hours, but general).
expand_episode_slots <- function(data, slot_min = 30) {
  n_slots <- 1440 / slot_min
  data %>%
    mutate(
      .eid   = row_number(),
      s      = as.numeric(start),
      e      = s + as.numeric(time),
      s_slot = floor(s / slot_min),
      e_slot = floor((e - 1) / slot_min)
    ) %>%
    filter(!is.na(s), !is.na(e), !is.na(wt)) %>%
    rowwise() %>%
    mutate(slot = list(s_slot:e_slot)) %>%
    unnest(slot) %>%
    ungroup() %>%
    mutate(
      slot_start  = slot * slot_min,
      slot_end    = (slot + 1) * slot_min,
      overlap_min = pmax(0, pmin(e, slot_end) - pmax(s, slot_start)),
      weighted_overlap_time = overlap_min * wt
    ) %>%
    filter(slot >= 0, slot < n_slots, overlap_min > 0)
}

# ============================================================
# 1. Travel-trip dataset: merged primary spells, one row per trip
# ============================================================
travel_trips <- bind_rows(df2014_primary_merged, df2023_primary_merged) %>%
  mutate(period = factor(period, levels = c("2014-2015", "2023"))) %>%
  filter(pri_group == FOCUS_GROUP, !is.na(start), !is.na(time), time > 0) %>%
  mutate(
    start_min = as.numeric(start),                 # minutes on 24h clock (0 = 00:00)
    dur_min   = as.numeric(time),
    end_min   = (start_min + dur_min) %% 1440       # wrap trips crossing midnight
  )

message(sprintf("Travel trips: 2014-2015 = %s, 2023 = %s",
                sum(travel_trips$period == "2014-2015"),
                sum(travel_trips$period == "2023")))

write_csv(
  travel_trips %>% select(period, person_day, start, start_min, end_min, dur_min, wt),
  file.path(tab_dir, "travel_timing_trip_level.csv")
)

# ============================================================
# 1b. Optional weekday diagnostic lookup.
#     This all-week version DOES NOT use the weekday-only subset in the
#     external validation plots. The lookup is kept only so the script still
#     prints weekday sample sizes for reference.
#       2014-15: ddayw  (1 = Mon-Fri, 2 = Saturday, 3 = Sunday)
#       2023   : dday   (1-5 = Mon-Fri, 6 = Saturday, 7 = Sunday)
# ============================================================
weekday_lookup <- bind_rows(
  df2014_raw %>%
    transmute(
      person_day = paste(serial, pnum, daynum, sep = "_"),
      is_weekday = as.numeric(ddayw) == 1
    ),
  df2023_raw %>%
    transmute(
      person_day = paste(as.character(mainid), as.character(diaryord), sep = "_"),
      is_weekday = as.numeric(dday) %in% 1:5
    )
) %>%
  distinct(person_day, is_weekday)

travel_trips_weekday <- travel_trips %>%
  inner_join(weekday_lookup, by = "person_day") %>%
  filter(is_weekday)

message(sprintf("Weekday-only travel trips: 2014-2015 = %s, 2023 = %s",
                sum(travel_trips_weekday$period == "2014-2015"),
                sum(travel_trips_weekday$period == "2023")))

# External validation subsets:
#   all-week = Monday-Sunday
#   weekday  = Monday-Friday
#   weekend  = Saturday-Sunday
travel_trips_allweek <- travel_trips
travel_trips_weekend <- travel_trips %>%
  inner_join(weekday_lookup, by = "person_day") %>%
  filter(!is_weekday)

# Legacy object name retained for the all-week validation.
travel_trips_validation <- travel_trips_allweek

message(sprintf("All-week travel trips used for validation: 2014-2015 = %s, 2023 = %s",
                sum(travel_trips_allweek$period == "2014-2015"),
                sum(travel_trips_allweek$period == "2023")))
message(sprintf("Weekend-only travel trips: 2014-2015 = %s, 2023 = %s",
                sum(travel_trips_weekend$period == "2014-2015"),
                sum(travel_trips_weekend$period == "2023")))

# ============================================================
# 2A / T1. When travel happens across the day  (30-minute slots)
#          Weighted travel minutes per diary-day, by 30-min slot.
# ============================================================
travel_slots <- expand_episode_slots(travel_trips, T1_SLOT_MIN) %>%
  group_by(period, slot) %>%
  summarise(travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    travel_minutes_per_diary_day = travel_minutes / weighted_diary_days,
    time_hours = slot * T1_SLOT_MIN / 60          # 0 .. 23.5
  )

write_csv(travel_slots, file.path(tab_dir, "travel_timing_minutes_by_30min.csv"))

p_travel_slots <- ggplot(
  travel_slots,
  aes(x = time_hours, y = travel_minutes_per_diary_day, colour = period)
) +
  geom_line(linewidth = 1.3) +
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
  labs(
    title = "When travel happens across the day",
    subtitle = "Weighted travel minutes per diary-day, 30-minute slots (diary day 04:00-04:00)",
    x = "Hour of day",
    y = "Travel minutes per diary-day",
    colour = "Period"
  ) +
  presentation_theme

ggsave(file.path(out_dir, "T1_travel_minutes_by_30min.png"),
       p_travel_slots, width = 10, height = 6, dpi = 300)

# ============================================================
# 2B / T2. Start-time and end-time distribution  (30-minute, FACETED)
#          Start/end times aggregated into BE_SLOT_MIN slots to reduce jitter
#          in the smaller 2023 sample. Two panels; two period lines per panel.
# ============================================================
start_end_dist <- travel_trips %>%
  select(period, wt, start_min, end_min) %>%
  pivot_longer(c(start_min, end_min),
               names_to = "boundary", values_to = "tmin") %>%
  mutate(
    boundary = recode(boundary, start_min = "Trip start", end_min = "Trip end"),
    boundary = factor(boundary, levels = c("Trip start", "Trip end")),
    slot_min = floor(tmin / BE_SLOT_MIN) * BE_SLOT_MIN     # slot start, minutes
  ) %>%
  group_by(period, boundary, slot_min) %>%
  summarise(weighted_trips = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    trips_per_diary_day = weighted_trips / weighted_diary_days,
    time_hours = slot_min / 60
  )

write_csv(start_end_dist,
          file.path(tab_dir, "travel_timing_start_end_30min.csv"))

p_start_end <- ggplot(
  start_end_dist,
  aes(x = time_hours, y = trips_per_diary_day, colour = period)
) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ boundary) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(
    title = "Distribution of travel start and end times",
    subtitle = "Weighted travel trips per diary-day, 30-minute slots",
    x = "Hour of day",
    y = "Trips per diary-day",
    colour = "Period"
  ) +
  presentation_theme

ggsave(file.path(out_dir, "T2_travel_start_end_time_distribution.png"),
       p_start_end, width = 12, height = 5.5, dpi = 300)

# ============================================================
# 2C / T3. Duration distribution  (10-minute; native grid)
#          Durations are exact 10-minute multiples -> plot per exact value.
# ============================================================
DUR_MAX <- 120

travel_dur <- travel_trips %>%
  filter(dur_min > 0, dur_min <= DUR_MAX) %>%
  group_by(period, dur_min) %>%
  summarise(weighted_trips = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(trips_per_diary_day = weighted_trips / weighted_diary_days)

write_csv(travel_dur, file.path(tab_dir, "travel_timing_duration_10min.csv"))

long_share <- travel_trips %>%
  group_by(period) %>%
  summarise(share_over_max = sum(wt[dur_min > DUR_MAX], na.rm = TRUE) /
                             sum(wt, na.rm = TRUE),
            .groups = "drop")

p_travel_dur <- ggplot(
  travel_dur,
  aes(x = dur_min, y = trips_per_diary_day, colour = period)
) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, DUR_MAX, 20)) +
  labs(
    title = "Distribution of trip duration",
    # Line break added: the single-line version overflowed the canvas and the
    # end of the sentence was clipped. "Trip" is used throughout for the
    # merged spell, so the title now matches the axis label.
    subtitle = paste0("Weighted merged trips per diary-day, 10-minute grid\n",
                      "trips longer than ", DUR_MAX, " min excluded from the curve"),
    x = "Trip duration (minutes)",
    y = "Trips per diary-day",
    colour = "Period"
  ) +
  presentation_theme

ggsave(file.path(out_dir, "T3_travel_duration_distribution.png"),
       p_travel_dur, width = 10, height = 6, dpi = 300)

# ============================================================
# 2D. Summary statistics table (weighted), both periods
#     NOTE: start/end means use clock minutes. Travel is concentrated in
#     daytime, so the tiny 00:00-04:00 share barely affects the linear mean.
# ============================================================
travel_timing_summary <- travel_trips %>%
  group_by(period) %>%
  summarise(
    raw_trips           = n(),
    weighted_trips      = sum(wt, na.rm = TRUE),
    mean_start_min      = weighted.mean(start_min, wt, na.rm = TRUE),
    median_start_min    = wtd_quantile(start_min, wt, 0.5),
    mean_end_min        = weighted.mean(end_min, wt, na.rm = TRUE),
    median_end_min      = wtd_quantile(end_min, wt, 0.5),
    mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
    median_duration_min = wtd_quantile(dur_min, wt, 0.5),
    p90_duration_min    = wtd_quantile(dur_min, wt, 0.9),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  left_join(long_share, by = "period") %>%
  mutate(
    trips_per_diary_day = weighted_trips / weighted_diary_days,
    mean_start_hhmm     = min_to_hhmm(mean_start_min),
    median_start_hhmm   = min_to_hhmm(median_start_min),
    mean_end_hhmm       = min_to_hhmm(mean_end_min),
    median_end_hhmm     = min_to_hhmm(median_end_min)
  ) %>%
  select(period, raw_trips, trips_per_diary_day,
         mean_start_hhmm, median_start_hhmm,
         mean_end_hhmm, median_end_hhmm,
         mean_duration_min, median_duration_min, p90_duration_min,
         share_over_max)

write_csv(travel_timing_summary,
          file.path(tab_dir, "travel_timing_summary_by_period.csv"))

message("\n=== Travel timing summary by period ===")
print(as.data.frame(travel_timing_summary))

# ============================================================
# 2E / T4. Change plot: 2023 minus 2014-2015, 30-min slots (matches T1)
# ============================================================
travel_slots_diff <- travel_slots %>%
  select(period, time_hours, travel_minutes_per_diary_day) %>%
  pivot_wider(names_from = period, values_from = travel_minutes_per_diary_day,
              values_fill = 0) %>%
  mutate(diff = `2023` - `2014-2015`)

write_csv(travel_slots_diff,
          file.path(tab_dir, "travel_timing_minutes_by_30min_change.csv"))

p_travel_slots_diff <- ggplot(travel_slots_diff, aes(x = time_hours, y = diff)) +
  geom_col(width = T1_SLOT_MIN / 60 * 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
  labs(
    title = "Change in travel minutes per diary-day by time of day (2023 - 2014-15)",
    subtitle = "30-minute slots",
    x = "Hour of day",
    y = "Change in travel minutes per diary-day"
  ) +
  presentation_theme

ggsave(file.path(out_dir, "T4_travel_minutes_by_30min_change.png"),
       p_travel_slots_diff, width = 10, height = 6, dpi = 300)

# ============================================================
message("\nDONE: travel timing distributions.")
message("Figures: ", out_dir)
message("  T1_travel_minutes_by_30min.png        (when travel happens, 30-min)")
message("  T2_travel_start_end_time_distribution.png (start & end, 30-min, faceted)")
message("  T3_travel_duration_distribution.png   (duration, 10-min)")
message("  T4_travel_minutes_by_30min_change.png (2023 - 2014-15, 30-min)")
message("Tables: ", tab_dir, "/travel_timing_*.csv")


# ============================================================

# ============================================================
# 3. External validation against National Travel Survey (NTS)
#    UPDATED: use the new NTS trip-level Stata file instead of ODS tables
# ============================================================
# User request:
#   * Replace NTS0501 / NTS0502 / NTS0303 ODS inputs with raw NTS Stata files:
#       trip_eul_2002-2024.dta + individual_eul_2002-2024.dta
#   * Merge the trip and individual files and filter NTS to adults 18+.
#   * Keep all other TUS-side analysis unchanged.
#
# Important notes:
#   * This file is trip-level. It can reproduce trip timing distributions
#     directly (T1 trips in progress, T2 trip starts, T3 mean duration).
#   * The script reads age from the individual file, merges it onto trips,
#     and filters to exact adults 18+ before running the same external validation.
#   * T4/T5 daily anchors are now based on NTS trip-days present in the trip
#     file. Because zero-trip days are not present in a trip-level file, these
#     are useful as a reconciliation check, but should not be described as a
#     full NTS person-day estimate unless you also merge in the NTS day file.
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(forcats); library(scales); library(haven)
})

# NTS raw files are expected in the repository data/ folder.
# The trip file can be either the full file (trip_eul_2002-2024.dta)
# or the pre-filtered file you made (nts_trip_2014_5_2023.dta).
nts_data_dir <- "data"  # Repository data folder; change this if the NTS files are stored elsewhere.
nts_trip_file <- file.path(nts_data_dir, "nts_trip_2014_5_2023.dta")
nts_ind_file  <- file.path(nts_data_dir, "individual_eul_2002-2024.dta")
nts_day_file  <- file.path(nts_data_dir, "day_eul_2002-2024.dta")
nts_hh_file   <- file.path(nts_data_dir, "household_eul_2002-2024.dta")

# Small helper for optional values
`%||%` <- function(x, y) if (is.null(x)) y else x

# If the script is run from another working directory, also try the current script folder
# and the local UKDA folder under the current working directory.
find_file <- function(path, alt_basenames = character()) {
  path <- path.expand(path)
  if (file.exists(path)) return(path)

  base_names <- unique(c(basename(path), alt_basenames))
  script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile %||% getwd(), mustWork = FALSE)),
                         error = function(e) getwd())

  search_dirs <- unique(c(
    nts_data_dir,
    getwd(),
    script_dir,
    file.path(getwd(), "UKDA-5340-stata-NTS", "stata", "stata13"),
    file.path(nts_data_dir, "UKDA-5340-stata-NTS", "stata", "stata13")
  ))

  candidates <- as.vector(outer(search_dirs, base_names, file.path))
  hit <- candidates[file.exists(candidates)][1]
  if (!is.na(hit)) return(hit)

  stop(
    "File not found: ", path,
    "
I searched for: ", paste(base_names, collapse = ", "),
    "
Search folders included: ", paste(search_dirs, collapse = " | ")
  )
}

nts_trip_file <- find_file(
  nts_trip_file,
  alt_basenames = c("nts_trip_2014_5_2023(1).dta", "trip_eul_2002-2024.dta")
)
nts_ind_file  <- find_file(nts_ind_file, alt_basenames = c("individual_eul_2002-2024.dta"))
nts_day_file  <- find_file(nts_day_file, alt_basenames = c("day_eul_2002-2024.dta"))
nts_hh_file   <- find_file(nts_hh_file, alt_basenames = c("household_eul_2002-2024.dta"))

# ---------- helpers ----------
as_num <- function(x) {
  if (inherits(x, "haven_labelled")) return(as.numeric(x))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  readr::parse_number(as.character(x), na = c("", "NA", "[low]", "low", "-9", "-8"))
}

first_existing <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms][1]
  if (length(hit) == 0 || is.na(hit)) NA_character_ else hit
}

standardise_nts_names <- function(df) {
  rename_map <- list(
    SurveyYear       = c("SurveyYear", "surveyyear", "Year", "year"),
    HouseholdID      = c("HouseholdID", "householdid", "HHID", "hhid", "HouseholdId"),
    IndividualID     = c("IndividualID", "individualid", "IndID", "indid", "PersonID", "personid", "IndividualId"),
    DayID            = c("DayID", "dayid", "DiaryDayID", "diarydayid"),
    TripID           = c("TripID", "tripid", "TripId"),
    TravDay          = c("TravDay", "travday", "DayOfWeek", "dayofweek"),
    TripStart        = c("TripStart", "tripstart"),
    TripStartHours   = c("TripStartHours", "tripstarthours", "TripStartHour", "tripstarthour"),
    TripStartMinutes = c("TripStartMinutes", "tripstartminutes", "TripStartMinute", "tripstartminute"),
    JOTXSC           = c("JOTXSC", "jotxsc", "TripTotalTime", "triptotaltime", "TripTotal", "triptotal"),
    JTTXSC           = c("JTTXSC", "jttxsc", "TripTravTime", "triptravtime", "TripTime", "triptime"),
    TripTotalTime    = c("TripTotalTime", "triptotaltime", "TripTotal", "triptotal"),
    TripTravTime     = c("TripTravTime", "triptravtime", "TripTime", "triptime"),
    TripOrigGOR_B02ID = c("TripOrigGOR_B02ID", "triporiggor_b02id", "TripOrigGOR", "triporiggor"),
    TripDestGOR_B02ID = c("TripDestGOR_B02ID", "tripdestgor_b02id", "TripDestGOR", "tripdestgor"),
    W2               = c("W2", "w2"),
    W5               = c("W5", "w5"),
    W5xHH            = c("W5xHH", "w5xhh", "W5XHH")
  )

  for (canon in names(rename_map)) {
    src <- first_existing(names(df), rename_map[[canon]])
    if (!is.na(src) && src != canon && !(canon %in% names(df))) {
      names(df)[names(df) == src] <- canon
    }
  }
  df
}

parse_age_to_years <- function(x) {
  # Works for exact numeric age variables. If the variable is an age-band with
  # labels such as "16-19", "21-29", or "70+", this uses the lower bound.
  if (inherits(x, "haven_labelled")) {
    vals <- as.numeric(x)
    labs <- attr(x, "labels")
    if (!is.null(labs)) {
      lab_lookup <- stats::setNames(names(labs), as.character(as.numeric(labs)))
      lab_txt <- unname(lab_lookup[as.character(vals)])
      lower <- readr::parse_number(lab_txt)
      return(ifelse(!is.na(lower), lower, vals))
    }
    return(vals)
  }
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  readr::parse_number(as.character(x))
}

parse_hour_label <- function(hour) sprintf("%02d:00", as.integer(hour))

make_hourly_index <- function(df, value_col, group_cols) {
  # index = hourly value / average hourly value * 100 within each group
  df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(index_avg_hour_100 = .data[[value_col]] / mean(.data[[value_col]], na.rm = TRUE) * 100) %>%
    ungroup()
}

# ---------- read and prepare NTS files ----------
nts_trip_raw <- haven::read_dta(nts_trip_file) %>% standardise_nts_names()
nts_ind_raw  <- haven::read_dta(nts_ind_file)  %>% standardise_nts_names()
nts_day_raw  <- haven::read_dta(nts_day_file)  %>% standardise_nts_names()
nts_hh_raw   <- haven::read_dta(nts_hh_file)   %>% standardise_nts_names()

message("NTS trip file: ", nts_trip_file)
message("NTS individual file: ", nts_ind_file)
message("NTS day file: ", nts_day_file)
message("NTS household file: ", nts_hh_file)

# W2 is stored in the household file. Merge it into the individual,
# trip and day files using SurveyYear + HouseholdID.
required_hh_vars <- c("SurveyYear", "HouseholdID", "W2")
missing_hh_vars <- setdiff(required_hh_vars, names(nts_hh_raw))

if (length(missing_hh_vars) > 0) {
  stop(
    "The household file is missing required variables after name ",
    "standardisation: ",
    paste(missing_hh_vars, collapse = ", ")
  )
}

# Retain only the years used in this analysis before constructing the lookup.
nts_hh_w2 <- nts_hh_raw %>%
  mutate(
    SurveyYear = as.integer(as_num(SurveyYear)),
    HouseholdID = as.character(HouseholdID),
    W2 = as_num(W2)
  ) %>%
  filter(SurveyYear %in% c(2014L, 2015L, 2023L))

# Check that each household-year has only one non-missing W2 value.
w2_conflicts <- nts_hh_w2 %>%
  filter(is.finite(W2)) %>%
  group_by(SurveyYear, HouseholdID) %>%
  summarise(
    n_distinct_w2 = n_distinct(W2),
    .groups = "drop"
  ) %>%
  filter(n_distinct_w2 > 1)

if (nrow(w2_conflicts) > 0) {
  print(w2_conflicts)
  stop(
    "Conflicting W2 values were found for the same SurveyYear + ",
    "HouseholdID. Inspect the printed household keys before continuing."
  )
}

nts_hh_w2 <- nts_hh_w2 %>%
  group_by(SurveyYear, HouseholdID) %>%
  summarise(
    W2 = {
      valid_w2 <- W2[is.finite(W2)]
      if (length(valid_w2) == 0) NA_real_ else valid_w2[1]
    },
    .groups = "drop"
  )

attach_household_w2 <- function(df, data_name) {
  required_keys <- c("SurveyYear", "HouseholdID")
  missing_keys <- setdiff(required_keys, names(df))

  if (length(missing_keys) > 0) {
    stop(
      data_name,
      " is missing household merge key(s): ",
      paste(missing_keys, collapse = ", ")
    )
  }

  # Remove any pre-existing W2 so that the household-file value is definitive.
  if ("W2" %in% names(df)) {
    df <- df %>% select(-W2)
  }

  out <- df %>%
    mutate(
      SurveyYear = as.integer(as_num(SurveyYear)),
      HouseholdID = as.character(HouseholdID)
    ) %>%
    left_join(
      nts_hh_w2,
      by = c("SurveyYear", "HouseholdID"),
      relationship = "many-to-one"
    )

  target_rows <- out$SurveyYear %in% c(2014L, 2015L, 2023L)
  target_n <- sum(target_rows, na.rm = TRUE)
  matched_n <- sum(target_rows & is.finite(out$W2), na.rm = TRUE)
  unmatched_n <- target_n - matched_n

  message(
    data_name,
    ": household W2 matched for ",
    matched_n,
    " of ",
    target_n,
    " target-year rows (",
    sprintf("%.2f", 100 * matched_n / max(target_n, 1)),
    "%); unmatched = ",
    unmatched_n
  )

  if (matched_n == 0) {
    stop(
      "No household W2 values matched into ",
      data_name,
      ". Check SurveyYear and HouseholdID formats."
    )
  }

  out
}

nts_ind_raw  <- attach_household_w2(nts_ind_raw,  "NTS individual file")
nts_trip_raw <- attach_household_w2(nts_trip_raw, "NTS trip file")
nts_day_raw  <- attach_household_w2(nts_day_raw,  "NTS day file")

# Save a merge diagnostic for transparency.
nts_w2_merge_diagnostic <- bind_rows(
  tibble(
    dataset = "individual",
    target_rows = sum(nts_ind_raw$SurveyYear %in% c(2014L, 2015L, 2023L), na.rm = TRUE),
    rows_with_w2 = sum(
      nts_ind_raw$SurveyYear %in% c(2014L, 2015L, 2023L) &
        is.finite(nts_ind_raw$W2),
      na.rm = TRUE
    )
  ),
  tibble(
    dataset = "trip",
    target_rows = sum(nts_trip_raw$SurveyYear %in% c(2014L, 2015L, 2023L), na.rm = TRUE),
    rows_with_w2 = sum(
      nts_trip_raw$SurveyYear %in% c(2014L, 2015L, 2023L) &
        is.finite(nts_trip_raw$W2),
      na.rm = TRUE
    )
  ),
  tibble(
    dataset = "day",
    target_rows = sum(nts_day_raw$SurveyYear %in% c(2014L, 2015L, 2023L), na.rm = TRUE),
    rows_with_w2 = sum(
      nts_day_raw$SurveyYear %in% c(2014L, 2015L, 2023L) &
        is.finite(nts_day_raw$W2),
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    match_rate = rows_with_w2 / pmax(target_rows, 1)
  )

write_csv(
  nts_w2_merge_diagnostic,
  file.path(
    tab_dir,
    "external_validation_NTS_household_W2_merge_diagnostic.csv"
  )
)

# Age is stored in the individual file, not necessarily in the trip file.
# Age_B01ID has 21 labelled categories. We use those category labels to make
# an exact 18+ filter rather than treating the variable codes as ages.
#
# The parser extracts the youngest age represented by each labelled category.
# Examples that are handled include "18", "18-20", "18 to 20",
# "21-24", and "85 and over". A category is retained only when its lower
# bound is at least 18. This excludes any category containing age 17.
parse_age_band_lower <- function(x) {
  txt <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  txt <- stringr::str_replace_all(txt, "[–—]", "-")

  # Take the first number in the category label as its lower bound.
  lower <- suppressWarnings(
    as.numeric(stringr::str_extract(txt, "[0-9]+"))
  )

  # Explicitly treat under-1 / less-than categories as beginning at zero.
  lower[stringr::str_detect(txt, "under\\s*1|less than\\s*1")] <- 0
  lower
}

exact_age_candidates <- c("Age", "age", "AGE", "AgeExact", "ExactAge", "age_exact")
# ------------------------------------------------------------
# Exact NTS age filter: ages 18+
# ------------------------------------------------------------
# Verified against the official UK Data Service response-level lookup:
#
# Age_B01ID:
#   1  = under 1
#   2  = 1-2
#   3  = 3-4
#   4  = 5-10
#   5  = 11-15
#   6  = 16
#   7  = 17
#   8  = 18
#   9  = 19
#   10 = 20
#   11 = 21-25
#   12 = 26-29
#   13 = 30-39
#   14 = 40-49
#   15 = 50-59
#   16 = 60-64
#   17 = 65-69
#   18 = 70-74
#   19 = 75-79
#   20 = 80-84
#   21 = 85+
#
# Therefore exact age 18+ corresponds to Age_B01ID codes 8-21.
# Do not interpret the category code itself as the respondent's age.

if ("Age_B01ID" %in% names(nts_ind_raw)) {
  age_var <- "Age_B01ID"

  nts_ind_age <- nts_ind_raw %>%
    mutate(
      .nts_age_band_code = as_num(Age_B01ID),
      # Compatibility variable used later in the script.
      # This is the Age_B01ID category code, not age in years.
      .nts_age_value = .nts_age_band_code,
      .nts_age_filter =
        !is.na(.nts_age_band_code) &
        .nts_age_band_code >= 8 &
        .nts_age_band_code <= 21
    )

  nts_age_rule <- paste0(
    "Age_B01ID categories 8-21, corresponding exactly to ages 18+"
  )

  age_b01_lookup <- tibble::tribble(
    ~.nts_age_band_code, ~.nts_age_band_label,
    1,  "Under 1",
    2,  "1-2",
    3,  "3-4",
    4,  "5-10",
    5,  "11-15",
    6,  "16",
    7,  "17",
    8,  "18",
    9,  "19",
    10, "20",
    11, "21-25",
    12, "26-29",
    13, "30-39",
    14, "40-49",
    15, "50-59",
    16, "60-64",
    17, "65-69",
    18, "70-74",
    19, "75-79",
    20, "80-84",
    21, "85+"
  )

  message("Age_B01ID categories retained for exact NTS 18+ filter:")
  print(
    age_b01_lookup %>%
      filter(.nts_age_band_code >= 8)
  )

  # Save the verified response-level mapping for reproducibility.
  write_csv(
    age_b01_lookup,
    file.path(
      tab_dir,
      "external_validation_NTS_Age_B01ID_verified_lookup.csv"
    )
  )

} else if ("Age_B04ID" %in% names(nts_ind_raw)) {
  stop(
    "Age_B01ID was not found. This final exact-18+ version does not ",
    "substitute Age_B04ID because its broader categories may not identify ",
    "age 18 exactly."
  )

} else {
  stop(
    "Age_B01ID was not found in individual_eul_2002-2024.dta. ",
    "The exact age-18+ NTS filter cannot be applied."
  )
}

nts_ind_age <- nts_ind_age %>%
  filter(.nts_age_filter)

# Merge trip and individual files using the strongest available person-level keys.
preferred_keys <- c("SurveyYear", "HouseholdID", "IndividualID")
merge_keys <- preferred_keys[preferred_keys %in% names(nts_trip_raw) & preferred_keys %in% names(nts_ind_age)]

if (!("IndividualID" %in% merge_keys)) {
  stop(
    "Cannot safely merge NTS trip and individual files: IndividualID was not found in both files.\n",
    "Trip names include: ", paste(names(nts_trip_raw), collapse = ", "), "\n",
    "Individual names include: ", paste(names(nts_ind_raw), collapse = ", ")
  )
}

message("Merging NTS trip + individual files by: ", paste(merge_keys, collapse = ", "))
message("NTS individual records before/after 18+ filter: ", nrow(nts_ind_raw), " -> ", nrow(nts_ind_age))

# Prepare NTS day file denominator for stricter reconciliation panels.
# The day file includes person-days with zero trips, so it is the correct
# denominator for trips/person-day and total travel minutes/day.
day_merge_keys <- preferred_keys[preferred_keys %in% names(nts_day_raw) & preferred_keys %in% names(nts_ind_age)]
if (!("IndividualID" %in% day_merge_keys)) {
  stop(
    "Cannot safely merge NTS day and individual files: IndividualID was not found in both files.\n",
    "Day names include: ", paste(names(nts_day_raw), collapse = ", "), "\n",
    "Individual names include: ", paste(names(nts_ind_raw), collapse = ", ")
  )
}

nts_day_weight_var <- if ("W5" %in% names(nts_day_raw)) {
  "W5"
} else if ("W5xHH" %in% names(nts_day_raw)) {
  "W5xHH"
} else {
  NA_character_
}

nts_day_adult <- nts_day_raw %>%
  inner_join(
    nts_ind_age %>%
      select(all_of(day_merge_keys), any_of(c(".nts_age_value", ".nts_age_band_code"))) %>%
      distinct(),
    by = day_merge_keys
  ) %>%
  mutate(
    target_year = as.integer(as_num(SurveyYear)),
    period = ifelse(target_year %in% c(2014L, 2015L), "2014-2015", "2023"),
    period = factor(period, levels = c("2014-2015", "2023")),
    # Assumption used by this script: TravDay 1-5 = Monday-Friday.
    is_weekday = as_num(TravDay) %in% 1:5,
    # The supplied NTS day EUL file has no trip-origin/destination geography fields.
    # NTS is England-resident by design; trip-level England filtering is still applied above.
    is_england = TRUE,
    day_wt = if (!is.na(nts_day_weight_var)) as_num(.data[[nts_day_weight_var]]) else 1
  ) %>%
  filter(target_year %in% c(2014L, 2015L, 2023L),
         is.finite(day_wt), day_wt > 0) %>%
  distinct(period, target_year, DayID, HouseholdID, IndividualID, TravDay,
           .nts_age_value, .nts_age_band_code, is_weekday, is_england, day_wt,
           .keep_all = TRUE)

if (is.na(nts_day_weight_var)) {
  message("NTS day file has no W5/W5xHH weight. Strict reconciliation panels use unweighted NTS day-file denominators and unweighted NTS trip numerators.")
} else {
  message("NTS day file denominator weight: ", nts_day_weight_var)
}
message(sprintf("NTS adult day-file person-days: 2014-2015 = %s, 2023 = %s",
                sum(nts_day_adult$period == "2014-2015"),
                sum(nts_day_adult$period == "2023")))

write_csv(
  nts_day_adult %>%
    select(any_of(c("period", "target_year", "DayID", "HouseholdID", "IndividualID", "TravDay",
                    ".nts_age_value", ".nts_age_band_code", "is_weekday", "is_england", "day_wt"))),
  file.path(tab_dir, "external_validation_NTS_day_file_cleaned_2014_2015_2023_18plus.csv")
)

nts_trip_raw <- nts_trip_raw %>%
  inner_join(
    nts_ind_age %>%
      select(all_of(merge_keys), any_of(c(".nts_age_value", ".nts_age_band_code"))) %>%
      distinct(),
    by = merge_keys
  )

message("NTS trips after adult 18+ merge: ", nrow(nts_trip_raw))
message("NTS age filter rule: ", nts_age_rule)
nts_age_filtered <- TRUE
nts_age_label <- "ages 18+"


# ------------------------------------------------------------
# NTS seven-day diary denominator
# ------------------------------------------------------------
# NTS respondents keep a seven-day travel diary. For daily averages:
#   all week = weighted persons × 7
#   weekdays = weighted persons × 5
#   weekends = weighted persons × 2
#
# This denominator includes zero-trip days and therefore matches the TUS
# denominator based on all valid diary-days. It is preferable to deriving
# the denominator from the trip file, which necessarily excludes zero-trip
# days.
# Preferred benchmark specification: use household-file W2, now merged
# into the NTS individual records.
nts_ind_weight_var <- if ("W2" %in% names(nts_ind_age)) {
  "W2"
} else {
  NA_character_
}

if (is.na(nts_ind_weight_var)) {
  stop(
    "W2 was not available after merging the household file into the ",
    "NTS individual records. Check the W2 merge diagnostic."
  )
}

nts_person_keys <- c(
  "SurveyYear", "HouseholdID", "IndividualID"
)
nts_person_keys <- nts_person_keys[
  nts_person_keys %in% names(nts_ind_age)
]

if (!("IndividualID" %in% nts_person_keys)) {
  stop("IndividualID is required for the NTS seven-day denominator.")
}

nts_adult_persons <- nts_ind_age %>%
  mutate(
    target_year = as.integer(as_num(SurveyYear)),
    period = ifelse(
      target_year %in% c(2014L, 2015L),
      "2014-2015",
      "2023"
    ),
    period = factor(
      period,
      levels = c("2014-2015", "2023")
    ),
    person_wt = as_num(.data[[nts_ind_weight_var]])
  ) %>%
  filter(
    target_year %in% c(2014L, 2015L, 2023L),
    is.finite(person_wt),
    person_wt > 0
  ) %>%
  distinct(
    across(all_of(nts_person_keys)),
    .keep_all = TRUE
  )

nts_weighted_persons_by_period <- nts_adult_persons %>%
  group_by(period) %>%
  summarise(
    nts_weighted_persons = sum(person_wt, na.rm = TRUE),
    nts_unweighted_persons = n(),
    .groups = "drop"
  )

make_nts_seven_day_denominator <- function(day_key) {
  diary_days_per_person <- dplyr::case_when(
    day_key == "allweek" ~ 7,
    day_key == "weekday" ~ 5,
    day_key == "weekend" ~ 2,
    TRUE ~ NA_real_
  )

  if (!is.finite(diary_days_per_person)) {
    stop("Unknown day_key: ", day_key)
  }

  nts_weighted_persons_by_period %>%
    mutate(
      diary_days_per_person = diary_days_per_person,
      nts_weighted_diary_days =
        nts_weighted_persons * diary_days_per_person
    )
}

message(
  "NTS daily denominator uses the weighted 18+ individual sample × ",
  "7 diary days (5 weekdays + 2 weekend days). Weight: ",
  nts_ind_weight_var
)

write_csv(
  nts_weighted_persons_by_period,
  file.path(
    tab_dir,
    "external_validation_NTS_weighted_persons_for_seven_day_denominator.csv"
  )
)

# ============================================================
# NTS TRIP-LEVEL WEIGHTING (revised)
# ------------------------------------------------------------
# The earlier specification used W2 for the trip numerator. The NTS user
# guide defines W2 as the DIARY SAMPLE HOUSEHOLD weight and W5 as the
# STAGE weight, and instructs analysts to gross short walks using the
# JJXSC trip-count variable together with W5. Two consequences followed
# from using W2 and plain row counts:
#
#  (a) Short walks under one mile are recorded on only one day of the
#      seven-day diary. In this extract they appear on TravDay 1 and 7 and
#      are absent on days 2-6. Counting rows therefore misses roughly
#      six-sevenths of them. Measured walk share was 9.9% against a
#      published NTS figure of about 21.7%, and the trip rate was 776 per
#      person per year against a published 921.
#
#  (b) W2 carries no adjustment for the drop-off in trip reporting across
#      the diary week, which W5 does.
#
# JOTXSC is already grossed for short walks, so the two must not both be
# applied to it. The construction below keeps every downstream expression
# unchanged by splitting the weight in two:
#
#      jj      = JJXSC, the short-walk grossing factor (7 for a short
#                walk, 1 otherwise)
#      wt_base = the trip weight itself, used wherever a DAY or a PERSON
#                is being counted rather than a trip
#      wt      = jj * wt_base, so that sum(wt) is a correct trip count and
#                a correct denominator for modal and purpose shares
#      dur_min = JOTXSC / jj, the duration of ONE trip rather than of the
#                grossed group, so that sum(dur_min * wt) recovers the
#                official grossed total exactly
#
# The per-trip duration also repairs the timing and duration
# distributions: previously a short walk was carried at seven times its
# real length, which pushed its end time and its position in the duration
# histogram into the wrong place.
#
# Weight definitions (NTS EUL lookup table, 2002-2024):
#   W2    = "Weighted diary sample"    (diary-sample household/person weight)
#   W5    = "Weighted travel sample"                      (trip/stage weight)
#   W5xHH = "Weighted travel sample - excluding household weight"
#
# WHICH TRIP WEIGHT TO USE HERE, AND WHY (empirical, not definitional):
# Per-person LEVELS (trips/day, minutes/day) are formed as
#     sum(trips * trip_weight) / (weighted adults[W2] * 7).
# The numerator's trip weight and the W2 person denominator must sit on the
# same gross-up basis or the ratio is scaled wrong. In THIS pipeline:
#
#   trip weight   NTS trips/day (2014-15)   NTS min/day   walk share
#   W5xHH             ~2.54                    ~60          ~21%   <- matches
#                                                                     published
#   W5                ~3.37                    ~81          ~21%   <- ~1/3 TOO
#                                                                     HIGH
#
# W5xHH is the household-weight-EXCLUDED trip weight; paired with the W2
# denominator (which carries the household weight) it reproduces the
# published NTS adult trip rate. The full W5 effectively applies the
# household gross-up twice relative to this denominator and inflates the
# per-person levels by about a third, which also breaks the TUS-vs-NTS
# level comparison. min/trip and modal/purpose SHARES are basis-independent
# (the household weight cancels), so they are essentially identical under
# W5 and W5xHH -- only the per-person levels are affected.
#
# NOTE on an earlier revision: switching to W5 on the definitional grounds
# that it is the "standard" trip weight (DfT user-guide Example 2 uses
# W5/W2) was tried and REJECTED, because DfT's Example-2 denominator is not
# the seven-day person denominator built here; empirically W5 overshoots
# published NTS levels in this pipeline. W5xHH is retained.
# Setting NTS_TRIP_WEIGHT to "W2" and NTS_USE_JJXSC to FALSE reproduces the
# earliest (uncorrected) results exactly.
# ============================================================

NTS_TRIP_WEIGHT <- "W5xHH"   # EMPIRICALLY VALIDATED choice: with this
                             # pipeline's seven-day person denominator, W5xHH
                             # reproduces the published NTS adult trip rate
                             # (~2.54 trips/day, ~60 min/day, ~21% walk).
                             # Alternatives: "W5" OVERSHOOTS to ~3.4 trips/day
                             # and ~81 min/day (about a third above published,
                             # see note below) and must NOT be used here;
                             # "W2" reproduces the earliest (uncorrected) run.
NTS_USE_JJXSC   <- TRUE      # was FALSE (plain row counts)

nts_weight_var <- if (NTS_TRIP_WEIGHT %in% names(nts_trip_raw)) {
  NTS_TRIP_WEIGHT
} else if ("W5xHH" %in% names(nts_trip_raw)) {
  # Prefer W5xHH: with this pipeline's W2 seven-day denominator it reproduces
  # published NTS adult levels, whereas W5 overshoots by about a third.
  "W5xHH"
} else if ("W5" %in% names(nts_trip_raw)) {
  "W5"
} else if ("W2" %in% names(nts_trip_raw)) {
  "W2"
} else {
  NA_character_
}

if (is.na(nts_weight_var)) {
  stop(
    "No usable NTS trip weight was found. Expected one of W5xHH, W5 or W2 ",
    "in the trip file."
  )
}

nts_use_jjxsc <- isTRUE(NTS_USE_JJXSC) && ("JJXSC" %in% names(nts_trip_raw))

if (isTRUE(NTS_USE_JJXSC) && !nts_use_jjxsc) {
  warning(
    "JJXSC was requested but is not present in the NTS trip file. ",
    "Trip counts will NOT be grossed for short walks and will be ",
    "understated; modal shares will understate walking."
  )
}

message(
  "NTS trip-level weighting: weight = ", nts_weight_var,
  "; short-walk grossing via JJXSC = ", nts_use_jjxsc
)

if (!("JOTXSC" %in% names(nts_trip_raw))) {
  stop(
    "JOTXSC was not found after NTS name standardisation. ",
    "This benchmark version requires the official overall trip-duration ",
    "variable including waiting time."
  )
}

required_nts_vars <- c("SurveyYear", "TravDay")
missing_required <- setdiff(required_nts_vars, names(nts_trip_raw))
if (length(missing_required) > 0) {
  stop("Missing required NTS variables after standardisation: ", paste(missing_required, collapse = ", "))
}

# Create start time and duration robustly. Existing analysis below is unchanged.
nts_trips <- nts_trip_raw %>%
  mutate(
    target_year = as.integer(as_num(SurveyYear)),
    period = ifelse(target_year %in% c(2014L, 2015L), "2014-2015", "2023"),
    period = factor(period, levels = c("2014-2015", "2023")),
    # Short-walk grossing factor: 7 for a short walk, 1 otherwise.
    jj = if (nts_use_jjxsc) {
      j <- as_num(.data[["JJXSC"]])
      ifelse(is.finite(j) & j > 0, j, 1)
    } else {
      1
    },
    # wt_base counts a DAY or a PERSON; wt counts a TRIP.
    wt_base = if (!is.na(nts_weight_var)) as_num(.data[[nts_weight_var]]) else 1,
    wt = wt_base * jj,
    start_min = dplyr::coalesce(
      if ("TripStart" %in% names(.)) as_num(.data[["TripStart"]]) else NA_real_,
      if (all(c("TripStartHours", "TripStartMinutes") %in% names(.))) {
        as_num(.data[["TripStartHours"]]) * 60 + as_num(.data[["TripStartMinutes"]])
      } else NA_real_
    ),
    # Official NTS overall trip duration:
    # includes travelling plus waiting time between stages, is grossed
    # for short walks, and excludes Series-of-Calls trips by construction.
    # JOTXSC is already grossed for short walks, so it is divided by the
    # same factor to recover the duration of a single trip. Multiplying
    # back up by wt = wt_base * jj restores the official grossed total.
    dur_min = as_num(.data[["JOTXSC"]]) / jj,
    end_min_raw = start_min + dur_min,
    end_min = end_min_raw %% 1440,
    # Assumption used by this script: TravDay 1-5 = Monday-Friday.
    # If your data dictionary says TravDay is coded differently, change this line.
    is_weekday = as_num(TravDay) %in% 1:5,
    # England filter: where trip origin/destination GOR variables exist, keep trips with both
    # origin and destination in English Government Office Regions (codes 1-9).
    # The NTS is already an England-resident survey; this additionally removes trips located
    # outside England when those location variables are available.
    is_england = case_when(
      all(c("TripOrigGOR_B02ID", "TripDestGOR_B02ID") %in% names(.)) ~
        as_num(.data[["TripOrigGOR_B02ID"]]) %in% 1:9 &
        as_num(.data[["TripDestGOR_B02ID"]]) %in% 1:9,
      TRUE ~ TRUE
    )
  ) %>%
  filter(target_year %in% c(2014L, 2015L, 2023L),
         is_england,
         is.finite(start_min), is.finite(dur_min), dur_min > 0,
         is.finite(wt), wt > 0)

message(sprintf("NTS trip file loaded and filtered to 18+, all days and England: 2014-2015 = %s trips, 2023 = %s trips; age variable = %s",
                sum(nts_trips$period == "2014-2015"),
                sum(nts_trips$period == "2023"),
                age_var))

write_csv(
  nts_trips %>%
    select(any_of(c("period", "target_year", "TripID", "DayID", "HouseholdID", "IndividualID", "TravDay",
                    ".nts_age_value", ".nts_age_band_code", "start_min", "end_min", "dur_min", "wt", "is_weekday", "is_england"))),
  file.path(tab_dir, "external_validation_NTS_trip_file_cleaned_2014_2015_2023_18plus.csv")
)

# Helper for NTS trips-in-progress using the same logic as TUS expand_episode_slots.
nts_expand_trip_slots <- function(data, slot_min = 60) {
  n_slots <- 1440 / slot_min
  data %>%
    mutate(
      .eid   = row_number(),
      start  = start_min,
      time   = dur_min,
      s      = as.numeric(start),
      e      = s + as.numeric(time),
      s_slot = floor(s / slot_min),
      e_slot = floor((e - 1) / slot_min)
    ) %>%
    filter(!is.na(s), !is.na(e), !is.na(wt)) %>%
    rowwise() %>%
    mutate(slot = list(s_slot:e_slot)) %>%
    unnest(slot) %>%
    ungroup() %>%
    mutate(
      slot_start  = slot * slot_min,
      slot_end    = (slot + 1) * slot_min,
      overlap_min = pmax(0, pmin(e, slot_end) - pmax(s, slot_start)),
      weighted_overlap_time = overlap_min * wt
    ) %>%
    filter(slot >= 0, slot < n_slots, overlap_min > 0)
}

# ============================================================
# 3A / 3B. External validation T1/T2: three day-type versions
#          all-week, weekday and weekend
# ============================================================
# The main data logic is unchanged. This section only repeats the same T1/T2
# validation for three day-type subsets and uses shorter plot subtitles/legends.

# Long labels retained for T3/T4/T5 and legacy tables below.
nts_trip_source_allweek <- paste0("NTS trip file: England, ", nts_age_label, ", all days")
nts_trip_source_alldays <- nts_trip_source_allweek

validation_source_palette <- stats::setNames(
  c("#7B61FF", "#F39C12"),
  c("TUS harmonised travel episodes", nts_trip_source_allweek)
)

# Short labels used only in T1/T2 plots so the legend fits.
tus_source_short <- "TUS travel episodes"
nts_source_short <- "NTS trip file\nEngland, ages 18+"
validation_source_palette_short <- stats::setNames(
  c("#7B61FF", "#F39C12"),
  c(tus_source_short, nts_source_short)
)
validation_linetype_short <- stats::setNames(
  c("solid", "dashed"),
  c(tus_source_short, nts_source_short)
)

nts_trips_allweek <- nts_trips
nts_trips_weekday <- nts_trips %>% filter(is_weekday %in% TRUE)
nts_trips_weekend <- nts_trips %>% filter(is_weekday %in% FALSE)

validation_day_sets <- list(
  allweek = list(
    label = "All-week",
    file_label = "allweek",
    tus = travel_trips_allweek,
    nts = nts_trips_allweek
  ),
  weekday = list(
    label = "Weekday",
    file_label = "weekday",
    tus = travel_trips_weekday,
    nts = nts_trips_weekday
  ),
  weekend = list(
    label = "Weekend",
    file_label = "weekend",
    tus = travel_trips_weekend,
    nts = nts_trips_weekend
  )
)

make_external_validation_t1_t2 <- function(day_key, cfg) {
  day_label <- cfg$label
  file_label <- cfg$file_label
  tus_data <- cfg$tus
  nts_data <- cfg$nts

  message(sprintf("Creating external validation T1/T2 (%s): TUS trips = %s; NTS trips = %s",
                  day_label, nrow(tus_data), nrow(nts_data)))

  # ---- T1: trips in progress across the day ----
  nts_t1_hourly <- nts_expand_trip_slots(nts_data, 60) %>%
    group_by(period, slot) %>%
    summarise(
      nts_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hour = slot,
      hour_label = parse_hour_label(hour)
    ) %>%
    make_hourly_index("nts_travel_minutes", c("period")) %>%
    transmute(
      period,
      hour,
      hour_label,
      source = nts_source_short,
      index_avg_hour_100 = index_avg_hour_100
    )

  tus_t1_hourly <- expand_episode_slots(tus_data, 60) %>%
    group_by(period, slot) %>%
    summarise(
      tus_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      hour = slot,
      hour_label = parse_hour_label(hour),
      period = factor(period, levels = c("2014-2015", "2023"))
    ) %>%
    make_hourly_index("tus_travel_minutes", c("period")) %>%
    transmute(
      period,
      hour,
      hour_label,
      source = tus_source_short,
      index_avg_hour_100 = index_avg_hour_100
    )

  t1_compare_local <- bind_rows(tus_t1_hourly, nts_t1_hourly) %>%
    mutate(
      source = factor(source, levels = c(tus_source_short, nts_source_short)),
      day_type = day_label
    )

  write_csv(
    t1_compare_local,
    file.path(tab_dir, paste0("external_validation_T1_TUS_vs_NTS_tripfile_hourly_index_", file_label, ".csv"))
  )

  p_t1_compare_local <- ggplot(
    t1_compare_local,
    aes(x = hour, y = index_avg_hour_100, colour = source, linetype = source)
  ) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~ period, ncol = 1) +
    scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
    scale_colour_manual(values = validation_source_palette_short, na.value = "grey50") +
    scale_linetype_manual(values = validation_linetype_short, na.value = "dashed") +
    labs(
      title = paste0("External validation T1: trips in progress across the day (", day_label, ")"),
      subtitle = paste0(
        "Weighted TUS travel episodes vs weighted NTS trips; matched survey years\n",
        "Traveller records only; both indexed to average hour = 100"
      ),
      x = "Hour of day",
      y = "Index (average hour = 100)",
      colour = "Source",
      linetype = "Source"
    ) +
    presentation_theme +
    theme(legend.text = element_text(lineheight = 0.95))

  ggsave(
    file.path(out_dir, paste0("EV_T1_TUS_vs_NTS_tripfile_trips_in_progress_hourly_index_", file_label, ".png")),
    p_t1_compare_local,
    width = 12,
    height = 7,
    dpi = 300
  )

  # Keep legacy all-week filename for existing slides/workflows.
  if (day_key == "allweek") {
    ggsave(
      file.path(out_dir, "EV_T1_TUS_vs_NTS_tripfile_trips_in_progress_hourly_index.png"),
      p_t1_compare_local,
      width = 12,
      height = 7,
      dpi = 300
    )
  }

  # ---- T2: trip start-time distribution ----
  nts_t2_start_hourly <- nts_data %>%
    mutate(
      hour = floor(start_min / 60),
      hour_label = parse_hour_label(hour)
    ) %>%
    group_by(period, hour, hour_label) %>%
    summarise(weighted_starts = sum(wt, na.rm = TRUE), .groups = "drop") %>%
    group_by(period) %>%
    mutate(start_share_percent = weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100) %>%
    ungroup() %>%
    transmute(
      period,
      hour,
      hour_label,
      source = nts_source_short,
      start_share_percent
    )

  tus_t2_start_hourly <- tus_data %>%
    mutate(
      hour = floor(start_min / 60),
      hour_label = parse_hour_label(hour)
    ) %>%
    group_by(period, hour, hour_label) %>%
    summarise(weighted_starts = sum(wt, na.rm = TRUE), .groups = "drop") %>%
    group_by(period) %>%
    mutate(start_share_percent = weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100) %>%
    ungroup() %>%
    transmute(
      period = factor(period, levels = c("2014-2015", "2023")),
      hour,
      hour_label,
      source = tus_source_short,
      start_share_percent
    )

  t2_compare_local <- bind_rows(tus_t2_start_hourly, nts_t2_start_hourly) %>%
    mutate(
      source = factor(source, levels = c(tus_source_short, nts_source_short)),
      day_type = day_label
    )

  write_csv(
    t2_compare_local,
    file.path(tab_dir, paste0("external_validation_T2_TUS_vs_NTS_tripfile_start_time_share_", file_label, ".csv"))
  )

  p_t2_compare_local <- ggplot(
    t2_compare_local,
    aes(x = hour, y = start_share_percent, colour = source, linetype = source)
  ) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~ period, ncol = 1) +
    scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_colour_manual(values = validation_source_palette_short, na.value = "grey50") +
    scale_linetype_manual(values = validation_linetype_short, na.value = "dashed") +
    labs(
      title = paste0("External validation T2: trip start-time distribution (", day_label, ")"),
      subtitle = paste0(
        "Weighted TUS travel episodes vs weighted NTS trips; matched survey years\n",
        "Traveller records only; zero-travel days do not enter this distribution"
      ),
      x = "Trip start hour",
      y = "Share of trip starts",
      colour = "Source",
      linetype = "Source"
    ) +
    presentation_theme +
    theme(legend.text = element_text(lineheight = 0.95))

  ggsave(
    file.path(out_dir, paste0("EV_T2_TUS_vs_NTS_tripfile_start_time_distribution_", file_label, ".png")),
    p_t2_compare_local,
    width = 12,
    height = 7,
    dpi = 300
  )

  # Keep legacy all-week filename for existing slides/workflows.
  if (day_key == "allweek") {
    ggsave(
      file.path(out_dir, "EV_T2_TUS_vs_NTS_tripfile_start_time_distribution.png"),
      p_t2_compare_local,
      width = 12,
      height = 7,
      dpi = 300
    )
  }

  list(t1 = t1_compare_local, t2 = t2_compare_local)
}

validation_results <- list()
for (day_key in names(validation_day_sets)) {
  validation_results[[day_key]] <- make_external_validation_t1_t2(day_key, validation_day_sets[[day_key]])
}

# Legacy objects retained for the later peak-diagnostic block.
# They use the all-week comparison, while combined files include all three subsets.
t1_compare <- validation_results$allweek$t1
t2_compare <- validation_results$allweek$t2

t1_compare_all_daytypes <- bind_rows(
  validation_results$allweek$t1,
  validation_results$weekday$t1,
  validation_results$weekend$t1
)
t2_compare_all_daytypes <- bind_rows(
  validation_results$allweek$t2,
  validation_results$weekday$t2,
  validation_results$weekend$t2
)

write_csv(
  t1_compare_all_daytypes,
  file.path(tab_dir, "external_validation_T1_TUS_vs_NTS_tripfile_hourly_index_all_daytypes.csv")
)
write_csv(
  t2_compare_all_daytypes,
  file.path(tab_dir, "external_validation_T2_TUS_vs_NTS_tripfile_start_time_share_all_daytypes.csv")
)

# ============================================================
# 3C. T3: average trip duration anchor from NTS trip file
# ============================================================

nts_duration_for_compare <- nts_trips %>%
  group_by(period) %>%
  summarise(
    nts_avg_trip_duration_min = weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(period = factor(period, levels = c("2014-2015", "2023")))

write_csv(
  nts_duration_for_compare,
  file.path(tab_dir, "external_validation_NTS_tripfile_average_trip_duration_anchor.csv")
)

# TUS mean duration from travel_timing_summary created above.
tus_duration_for_compare <- travel_timing_summary %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    tus_mean_duration_min = mean_duration_min
  )

t3_duration_anchor_compare <- tus_duration_for_compare %>%
  left_join(nts_duration_for_compare, by = "period") %>%
  pivot_longer(
    cols = c(tus_mean_duration_min, nts_avg_trip_duration_min),
    names_to = "source",
    values_to = "mean_duration_min"
  ) %>%
  mutate(
    source = recode(
      source,
      tus_mean_duration_min = "TUS harmonised travel episodes",
      nts_avg_trip_duration_min = nts_trip_source_alldays
    )
  )

write_csv(
  t3_duration_anchor_compare,
  file.path(tab_dir, "external_validation_T3_TUS_vs_NTS_tripfile_duration_anchor.csv")
)

p_t3_duration_anchor <- ggplot(
  t3_duration_anchor_compare,
  aes(x = period, y = mean_duration_min, fill = source)
) +
  geom_col(position = "dodge") +
  labs(
    title = "External validation T3: average trip duration anchor",
    subtitle = paste0("TUS adults 18+ vs NTS trip file, ", nts_age_label,
                      ", all available days"),
    x = NULL,
    y = "Mean trip duration (minutes)",
    fill = "Source"
  ) +
  scale_fill_manual(values = validation_source_palette, na.value = "grey50") +
  presentation_theme

ggsave(
  file.path(out_dir, "EV_T3_TUS_vs_NTS_tripfile_average_duration_anchor.png"),
  p_t3_duration_anchor,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ============================================================
# 3C-ii. T4/T5 daily-style anchors from NTS trip file
# ============================================================
# NOTE: Since this is a trip-level file, zero-trip diary days are not observed.
# The NTS side below therefore uses unique DayID values present in the trip file
# as the denominator. This is NOT equivalent to official NTS person-day averages
# unless zero-trip days are merged from the NTS day file.

nts_tripday_denominator <- nts_trips %>%
  distinct(period, DayID, .keep_all = TRUE) %>%
  group_by(period) %>%
  # wt_base, not wt: this counts diary DAYS, and the short-walk grossing
  # factor of the day's first trip must not multiply the day itself.
  summarise(nts_weighted_trip_days = sum(wt_base, na.rm = TRUE), .groups = "drop")

nts_daily_anchor <- nts_trips %>%
  group_by(period) %>%
  summarise(
    nts_weighted_trips = sum(wt, na.rm = TRUE),
    nts_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nts_tripday_denominator, by = "period") %>%
  mutate(
    nts_trips_per_day      = nts_weighted_trips / nts_weighted_trip_days,
    nts_travel_min_per_day = nts_weighted_travel_min / nts_weighted_trip_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

write_csv(nts_daily_anchor,
          file.path(tab_dir, "external_validation_NTS_tripfile_tripday_anchors.csv"))

# TUS per-diary-day travel minutes and trips (all diary-days in denominator).
tus_daily <- travel_trips %>%
  group_by(period) %>%
  summarise(
    tus_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    tus_weighted_trips      = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    tus_travel_min_per_day = tus_weighted_travel_min / weighted_diary_days,
    tus_trips_per_day      = tus_weighted_trips / weighted_diary_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

write_csv(tus_daily,
          file.path(tab_dir, "external_validation_TUS_daily_travel_metrics.csv"))

# ---- T4: total travel minutes per day-style denominator, TUS vs NTS ----
t4_daily_minutes <- tus_daily %>%
  transmute(period, `TUS harmonised travel episodes` = tus_travel_min_per_day) %>%
  left_join(
    nts_daily_anchor %>%
      transmute(period, !!nts_trip_source_alldays := nts_travel_min_per_day),
    by = "period"
  ) %>%
  pivot_longer(-period, names_to = "source", values_to = "travel_min_per_day") %>%
  mutate(source = factor(source,
    levels = c("TUS harmonised travel episodes", nts_trip_source_alldays)))

write_csv(t4_daily_minutes,
          file.path(tab_dir, "external_validation_T4_daily_travel_minutes_tripfile.csv"))

p_t4_daily_minutes <- ggplot(
  t4_daily_minutes,
  aes(x = period, y = travel_min_per_day, fill = source)
) +
  geom_col(position = "dodge") +
  labs(
    title = "External validation T4: total travel time",
    subtitle = "TUS adults 18+ per diary-day vs NTS all-week England trip file per observed trip-day; use cautiously",
    x = NULL,
    y = "Travel minutes",
    fill = "Source"
  ) +
  scale_fill_manual(values = validation_source_palette, na.value = "grey50") +
  presentation_theme

ggsave(
  file.path(out_dir, "EV_T4_TUS_vs_NTS_tripfile_daily_travel_minutes.png"),
  p_t4_daily_minutes, width = 10, height = 5.5, dpi = 300
)

# ---- T5: number of trips per day-style denominator, TUS vs NTS ----
t5_daily_trips <- tus_daily %>%
  transmute(period, `TUS harmonised travel episodes` = tus_trips_per_day) %>%
  left_join(
    nts_daily_anchor %>%
      transmute(period, !!nts_trip_source_alldays := nts_trips_per_day),
    by = "period"
  ) %>%
  pivot_longer(-period, names_to = "source", values_to = "trips_per_day") %>%
  mutate(source = factor(source,
    levels = c("TUS harmonised travel episodes", nts_trip_source_alldays)))

write_csv(t5_daily_trips,
          file.path(tab_dir, "external_validation_T5_daily_trip_count_tripfile.csv"))

p_t5_daily_trips <- ggplot(
  t5_daily_trips,
  aes(x = period, y = trips_per_day, fill = source)
) +
  geom_col(position = "dodge") +
  labs(
    title = "External validation T5: number of trips",
    subtitle = "TUS adults 18+ per diary-day vs NTS all-week England trip file per observed trip-day; use cautiously",
    x = NULL,
    y = "Trips",
    fill = "Source"
  ) +
  scale_fill_manual(values = validation_source_palette, na.value = "grey50") +
  presentation_theme

ggsave(
  file.path(out_dir, "EV_T5_TUS_vs_NTS_tripfile_daily_trip_count.png"),
  p_t5_daily_trips, width = 10, height = 5.5, dpi = 300
)

# ---- Reconciliation: trips-per-day x minutes-per-trip = total minutes-per-day ----
reconciliation <- tus_daily %>%
  select(period, tus_trips_per_day, tus_travel_min_per_day) %>%
  left_join(
    nts_daily_anchor %>% select(period, nts_trips_per_day, nts_travel_min_per_day),
    by = "period"
  ) %>%
  mutate(
    tus_implied_min_per_trip = tus_travel_min_per_day / tus_trips_per_day,
    nts_implied_min_per_trip = nts_travel_min_per_day / nts_trips_per_day
  )

write_csv(reconciliation,
          file.path(tab_dir, "external_validation_T4T5_reconciliation_tripfile.csv"))

message("\n=== T4/T5 travel reconciliation (TUS vs NTS trip file; NTS excludes zero-trip days) ===")
print(as.data.frame(reconciliation))
# 3D. Compact peak timing diagnostics for reporting
# ============================================================
# These tables make it easy to state whether the morning/evening peak locations
# match between TUS and NTS.

peak_window <- function(df, value_col) {
  df %>%
    filter(hour >= 5, hour <= 20) %>%
    group_by(period, source) %>%
    slice_max(order_by = .data[[value_col]], n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(period, source, desc(.data[[value_col]])) %>%
    mutate(hour_label = sprintf("%02d:00", as.integer(hour)))
}

t1_peak_diagnostics <- peak_window(t1_compare, "index_avg_hour_100")
write_csv(
  t1_peak_diagnostics,
  file.path(tab_dir, "external_validation_T1_peak_diagnostics.csv")
)

t2_peak_diagnostics <- peak_window(t2_compare, "start_share_percent")
write_csv(
  t2_peak_diagnostics,
  file.path(tab_dir, "external_validation_T2_peak_diagnostics.csv")
)

message("\nDONE: NTS external validation add-on.")
message("NTS extracted tables written to: ", tab_dir)
message("External validation figures written to: ", out_dir)
message("  EV_T1_TUS_vs_NTS_tripfile_trips_in_progress_hourly_index.png  (all-week)")
message("  EV_T2_TUS_vs_NTS_tripfile_start_time_distribution.png        (all-week)")
message("  EV_T3_TUS_vs_NTS_tripfile_average_duration_anchor.png")
message("  EV_T4_TUS_vs_NTS_tripfile_daily_travel_minutes.png           (total time/day)")
message("  EV_T5_TUS_vs_NTS_tripfile_daily_trip_count.png               (trips/day)")
message("Key diagnostic table: ", tab_dir, "/external_validation_T4T5_reconciliation_tripfile.csv")


library(patchwork)

# ============================================================
# 3E. MAIN RECONCILIATION: ALL RECORDED DAYS
#     TUS diary-day denominator vs NTS seven-day denominator
# ============================================================
# Numerators:
#   TUS = weighted harmonised travel spells.
#   NTS = weighted trip-file trips and minutes.
#
# Denominators:
#   TUS = all valid diary-days, including zero-travel days.
#   NTS = weighted adult persons × 7 days
#         (or ×5 weekdays / ×2 weekend days).
#
# Thus both sides estimate unconditional averages per recorded day.
# T1 and T2 remain trip/spell distributions and are unaffected by
# zero-travel days.
# ============================================================

tus_day_denominator_by_daytype <- bind_rows(
  df2014_primary_merged,
  df2023_primary_merged
) %>%
  mutate(
    period = factor(
      period,
      levels = c("2014-2015", "2023")
    )
  ) %>%
  select(period, person_day, wt) %>%
  distinct() %>%
  left_join(weekday_lookup, by = "person_day")

make_tus_denominator <- function(day_key) {
  if (day_key == "allweek") {
    period_day_denominator %>%
      transmute(
        period = factor(
          period,
          levels = c("2014-2015", "2023")
        ),
        tus_weighted_diary_days = weighted_diary_days
      )
  } else if (day_key == "weekday") {
    tus_day_denominator_by_daytype %>%
      filter(is_weekday %in% TRUE) %>%
      group_by(period) %>%
      summarise(
        tus_weighted_diary_days = sum(wt, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (day_key == "weekend") {
    tus_day_denominator_by_daytype %>%
      filter(is_weekday %in% FALSE) %>%
      group_by(period) %>%
      summarise(
        tus_weighted_diary_days = sum(wt, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    stop("Unknown day_key: ", day_key)
  }
}

all_days_source_levels <- c(
  "TUS all diary-days (18+)",
  "NTS seven-day diary average (18+)"
)

all_days_source_palette <- c(
  "TUS all diary-days (18+)" = "#7B61FF",
  "NTS seven-day diary average (18+)" = "#F39C12"
)

make_reconciliation_panel_seven_day <- function(day_key, cfg) {
  day_label <- cfg$label
  file_label <- cfg$file_label
  tus_data <- cfg$tus
  nts_data <- cfg$nts

  day_title <- dplyr::case_when(
    day_key == "allweek" ~ "all week",
    day_key == "weekday" ~ "weekdays",
    day_key == "weekend" ~ "weekends",
    TRUE ~ day_label
  )

  tus_denominator <- make_tus_denominator(day_key)
  nts_denominator <- make_nts_seven_day_denominator(day_key)

  tus_daily_local <- tus_data %>%
    group_by(period) %>%
    summarise(
      tus_weighted_travel_min =
        sum(dur_min * wt, na.rm = TRUE),
      tus_weighted_spells =
        sum(wt, na.rm = TRUE),
      tus_mean_duration_min =
        weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(tus_denominator, by = "period") %>%
    mutate(
      tus_spells_per_day =
        tus_weighted_spells / tus_weighted_diary_days,
      tus_travel_min_per_day =
        tus_weighted_travel_min / tus_weighted_diary_days,
      period = factor(
        period,
        levels = c("2014-2015", "2023")
      )
    )

  nts_daily_local <- nts_data %>%
    group_by(period) %>%
    summarise(
      nts_weighted_trips =
        sum(wt, na.rm = TRUE),
      nts_weighted_travel_min =
        sum(dur_min * wt, na.rm = TRUE),
      nts_mean_duration_min =
        weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(nts_denominator, by = "period") %>%
    mutate(
      nts_trips_per_day =
        nts_weighted_trips / nts_weighted_diary_days,
      nts_travel_min_per_day =
        nts_weighted_travel_min / nts_weighted_diary_days,
      period = factor(
        period,
        levels = c("2014-2015", "2023")
      )
    )

  reconciliation_local <- tus_daily_local %>%
    select(
      period,
      tus_spells_per_day,
      tus_mean_duration_min,
      tus_travel_min_per_day,
      tus_weighted_diary_days
    ) %>%
    left_join(
      nts_daily_local %>%
        select(
          period,
          nts_trips_per_day,
          nts_mean_duration_min,
          nts_travel_min_per_day,
          nts_weighted_persons,
          diary_days_per_person,
          nts_weighted_diary_days
        ),
      by = "period"
    ) %>%
    mutate(
      day_type = day_label,
      denominator_definition = paste0(
        "TUS: all valid diary-days; NTS: weighted persons × ",
        diary_days_per_person,
        " diary days; zero-travel days included"
      ),
      tus_identity_error =
        tus_travel_min_per_day -
        tus_spells_per_day * tus_mean_duration_min,
      nts_identity_error =
        nts_travel_min_per_day -
        nts_trips_per_day * nts_mean_duration_min
    )

  write_csv(
    reconciliation_local,
    file.path(
      tab_dir,
      paste0(
        "external_validation_reconciliation_SEVENDAY_ALL_DAYS_",
        file_label,
        ".csv"
      )
    )
  )

  spells_plot_data <- bind_rows(
    tus_daily_local %>%
      transmute(
        period,
        source = "TUS all diary-days (18+)",
        value = tus_spells_per_day
      ),
    nts_daily_local %>%
      transmute(
        period,
        source = "NTS seven-day diary average (18+)",
        value = nts_trips_per_day
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = all_days_source_levels
      )
    )

  duration_plot_data <- bind_rows(
    tus_daily_local %>%
      transmute(
        period,
        source = "TUS all diary-days (18+)",
        value = tus_mean_duration_min
      ),
    nts_daily_local %>%
      transmute(
        period,
        source = "NTS seven-day diary average (18+)",
        value = nts_mean_duration_min
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = all_days_source_levels
      )
    )

  minutes_plot_data <- bind_rows(
    tus_daily_local %>%
      transmute(
        period,
        source = "TUS all diary-days (18+)",
        value = tus_travel_min_per_day
      ),
    nts_daily_local %>%
      transmute(
        period,
        source = "NTS seven-day diary average (18+)",
        value = nts_travel_min_per_day
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = all_days_source_levels
      )
    )

  make_all_days_bar <- function(
      data,
      panel_title,
      y_label
  ) {
    ggplot(
      data,
      aes(x = period, y = value, fill = source)
    ) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = all_days_source_palette,
        na.value = "grey50"
      ) +
      labs(
        title = panel_title,
        x = NULL,
        y = y_label,
        fill = "Source"
      ) +
      presentation_theme
  }

  p_reconcile <- (
    make_all_days_bar(
      spells_plot_data,
      "Travel spells / trips per recorded day",
      "Travel spells / trips"
    ) |
      make_all_days_bar(
        duration_plot_data,
        "Minutes per travel spell / trip",
        "Mean duration (minutes)"
      ) |
      make_all_days_bar(
        minutes_plot_data,
        "Total travel min / recorded day",
        "Travel minutes"
      )
  ) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(
        "All-day reconciliation: TUS vs NTS seven-day diary (",
        day_title,
        ")"
      ),
      subtitle = paste0(
        "Both sides include zero-travel days. TUS uses all valid diary-days; ",
        "NTS uses weighted persons × ",
        ifelse(day_key == "allweek", 7,
               ifelse(day_key == "weekday", 5, 2)),
        " recorded days."
      ),
      theme = theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 12)
      )
    )

  ggsave(
    file.path(
      out_dir,
      paste0(
        "EV_reconciliation_panel_SEVENDAY_ALL_DAYS_",
        file_label,
        ".png"
      )
    ),
    p_reconcile,
    width = 15,
    height = 5.8,
    dpi = 300
  )

  reconciliation_local
}

reconciliation_seven_day_by_daytype <- bind_rows(
  lapply(
    names(validation_day_sets),
    function(day_key) {
      make_reconciliation_panel_seven_day(
        day_key,
        validation_day_sets[[day_key]]
      )
    }
  )
)

write_csv(
  reconciliation_seven_day_by_daytype,
  file.path(
    tab_dir,
    paste0(
      "external_validation_reconciliation_",
      "SEVENDAY_ALL_DAYS_all_daytypes.csv"
    )
  )
)


# ------------------------------------------------------------
# Preferred official NTS benchmark values
# ------------------------------------------------------------
# These are the external-validation values supplied and checked using
# JOTXSC and the official NTS diary weighting conventions.
nts_official_daily_benchmark <- tibble::tribble(
  ~period,      ~nts_official_travel_min_per_person_day,
  "2014-2015",  64.15,
  "2023",       61.48
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023"))
  )

write_csv(
  nts_official_daily_benchmark,
  file.path(
    tab_dir,
    "external_validation_NTS_official_JOTXSC_daily_benchmark_18plus.csv"
  )
)

# Diagnostic only: compare the reconstructed microdata value with the
# accepted official benchmark. The official value is used in the main figure.
nts_reconstructed_vs_official <- reconciliation_seven_day_by_daytype %>%
  filter(day_type == "All-week") %>%
  select(
    period,
    nts_reconstructed_travel_min_per_day = nts_travel_min_per_day
  ) %>%
  left_join(
    nts_official_daily_benchmark,
    by = "period"
  ) %>%
  mutate(
    difference_minutes =
      nts_reconstructed_travel_min_per_day -
      nts_official_travel_min_per_person_day
  )

write_csv(
  nts_reconstructed_vs_official,
  file.path(
    tab_dir,
    "external_validation_NTS_reconstructed_vs_official_JOTXSC_check.csv"
  )
)

tus_primary_daily_for_official_compare <-
  reconciliation_seven_day_by_daytype %>%
  filter(day_type == "All-week") %>%
  transmute(
    period,
    `TUS primary travel, all diary-days (18+)` =
      tus_travel_min_per_day
  )

official_compare_primary <- tus_primary_daily_for_official_compare %>%
  left_join(
    nts_official_daily_benchmark %>%
      transmute(
        period,
        `NTS official JOTXSC (18+)` =
          nts_official_travel_min_per_person_day
      ),
    by = "period"
  ) %>%
  pivot_longer(
    cols = -period,
    names_to = "source",
    values_to = "travel_min_per_day"
  )

official_source_levels <- c(
  "TUS primary travel, all diary-days (18+)",
  "NTS official JOTXSC (18+)"
)

official_source_palette <- c(
  "TUS primary travel, all diary-days (18+)" = "#7B61FF",
  "NTS official JOTXSC (18+)" = "#F39C12"
)

p_official_primary_compare <- official_compare_primary %>%
  mutate(
    source = factor(source, levels = official_source_levels)
  ) %>%
  ggplot(
    aes(
      x = period,
      y = travel_min_per_day,
      fill = source
    )
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", travel_min_per_day)),
    position = position_dodge(width = 0.9),
    vjust = -0.35,
    size = 4
  ) +
  scale_fill_manual(values = official_source_palette) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Average travel time per person per recorded day",
    subtitle = paste0(
      "NTS uses the preferred official JOTXSC benchmark; ",
      "TUS includes all diary-days, including zero-travel days"
    ),
    x = NULL,
    y = "Travel minutes per person per day",
    fill = "Source"
  ) +
  presentation_theme +
  theme(legend.position = "bottom")

ggsave(
  file.path(
    out_dir,
    "EV_OFFICIAL_NTS_JOTXSC_vs_TUS_primary_travel_minutes_per_day.png"
  ),
  p_official_primary_compare,
  width = 10,
  height = 6,
  dpi = 300
)

message("Main all-day seven-day reconciliation panels saved:")
message("  EV_reconciliation_panel_SEVENDAY_ALL_DAYS_allweek.png")
message("  EV_reconciliation_panel_SEVENDAY_ALL_DAYS_weekday.png")
message("  EV_reconciliation_panel_SEVENDAY_ALL_DAYS_weekend.png")
message(
  "Interpretation: unconditional averages per recorded day, ",
  "including zero-travel days on both sides."
)


# 4. SENSITIVITY CHECK: TUS travel episodes WITHOUT adjacent merging
#    Creates three additional ALL-WEEK figures only:
#      1) T1 trips in progress across the day
#      2) T2 trip start-time distribution
#      3) Trip-file reconciliation panel
#
#    IMPORTANT:
#    * These outputs use df2014_premerge / df2023_premerge, i.e. each original
#      primary travel episode is retained as a separate unit.
#    * Existing merged-trip figures are NOT overwritten.
#    * NTS data and filters are unchanged.
# ============================================================

if (!exists("df2014_premerge") || !exists("df2023_premerge")) {
  stop(
    "The unmerged sensitivity check requires df2014_premerge and df2023_premerge. ",
    "Run/source the main harmonisation script first."
  )
}

# Original harmonised PRIMARY episodes before formal adjacent-episode merging.
travel_episodes_unmerged_allweek <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(period = factor(period, levels = c("2014-2015", "2023"))) %>%
  filter(
    pri_group == FOCUS_GROUP,
    !is.na(start),
    !is.na(time),
    time > 0
  ) %>%
  mutate(
    start_min = as.numeric(start),
    dur_min   = as.numeric(time),
    end_min   = (start_min + dur_min) %% 1440
  )

message(sprintf(
  "UNMERGED TUS travel episodes (all-week): 2014-2015 = %s, 2023 = %s",
  sum(travel_episodes_unmerged_allweek$period == "2014-2015"),
  sum(travel_episodes_unmerged_allweek$period == "2023")
))

write_csv(
  travel_episodes_unmerged_allweek %>%
    select(period, person_day, start, start_min, end_min, dur_min, wt),
  file.path(tab_dir, "travel_timing_episode_level_UNMERGED_TUS_allweek.csv")
)

unmerged_tus_source <- "TUS unmerged travel episodes"
unmerged_source_levels <- c(unmerged_tus_source, nts_source_short)
unmerged_validation_palette <- c(
  "TUS unmerged travel episodes" = "#7B61FF",
  "NTS trip file\nEngland, ages 18+" = "#F39C12"
)
unmerged_validation_linetype <- c(
  "TUS unmerged travel episodes" = "solid",
  "NTS trip file\nEngland, ages 18+" = "dashed"
)

# ------------------------------------------------------------
# 4A. T1: trips/episodes in progress across the day — all week
# ------------------------------------------------------------
tus_t1_unmerged <- expand_episode_slots(
  travel_episodes_unmerged_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    tus_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour),
    period = factor(period, levels = c("2014-2015", "2023"))
  ) %>%
  make_hourly_index("tus_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = unmerged_tus_source,
    index_avg_hour_100
  )

nts_t1_unmerged_comparison <- nts_expand_trip_slots(
  nts_trips_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    nts_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour)
  ) %>%
  make_hourly_index("nts_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    index_avg_hour_100
  )

t1_compare_unmerged <- bind_rows(
  tus_t1_unmerged,
  nts_t1_unmerged_comparison
) %>%
  mutate(source = factor(source, levels = unmerged_source_levels))

write_csv(
  t1_compare_unmerged,
  file.path(
    tab_dir,
    "external_validation_T1_TUS_UNMERGED_vs_NTS_tripfile_hourly_index_allweek.csv"
  )
)

p_t1_unmerged <- ggplot(
  t1_compare_unmerged,
  aes(
    x = hour,
    y = index_avg_hour_100,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_colour_manual(values = unmerged_validation_palette, na.value = "grey50") +
  scale_linetype_manual(values = unmerged_validation_linetype, na.value = "dashed") +
  labs(
    title = "External validation T1: trips in progress across the day (All-week, TUS unmerged)",
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS retains original primary travel episodes; both indexed to average hour = 100"
    ),
    x = "Hour of day",
    y = "Index (average hour = 100)",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    "EV_T1_TUS_UNMERGED_vs_NTS_tripfile_trips_in_progress_hourly_index_allweek.png"
  ),
  p_t1_unmerged,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 4B. T2: trip/episode start-time distribution — all week
# ------------------------------------------------------------
tus_t2_unmerged <- travel_episodes_unmerged_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    hour,
    hour_label,
    source = unmerged_tus_source,
    start_share_percent
  )

nts_t2_unmerged_comparison <- nts_trips_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    start_share_percent
  )

t2_compare_unmerged <- bind_rows(
  tus_t2_unmerged,
  nts_t2_unmerged_comparison
) %>%
  mutate(source = factor(source, levels = unmerged_source_levels))

write_csv(
  t2_compare_unmerged,
  file.path(
    tab_dir,
    "external_validation_T2_TUS_UNMERGED_vs_NTS_tripfile_start_time_share_allweek.csv"
  )
)

p_t2_unmerged <- ggplot(
  t2_compare_unmerged,
  aes(
    x = hour,
    y = start_share_percent,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_colour_manual(values = unmerged_validation_palette, na.value = "grey50") +
  scale_linetype_manual(values = unmerged_validation_linetype, na.value = "dashed") +
  labs(
    title = "External validation T2: trip start-time distribution (All-week, TUS unmerged)",
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS retains original primary travel episodes"
    ),
    x = "Trip/episode start hour",
    y = "Share of trip/episode starts",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    "EV_T2_TUS_UNMERGED_vs_NTS_tripfile_start_time_distribution_allweek.png"
  ),
  p_t2_unmerged,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 4C. Reconciliation panel — all week, trip-file denominator
# ------------------------------------------------------------
tus_unmerged_daily <- travel_episodes_unmerged_allweek %>%
  group_by(period) %>%
  summarise(
    tus_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    tus_weighted_trips      = sum(wt, na.rm = TRUE),
    tus_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    period_day_denominator %>%
      transmute(
        period = factor(period, levels = c("2014-2015", "2023")),
        tus_weighted_diary_days = weighted_diary_days
      ),
    by = "period"
  ) %>%
  mutate(
    tus_travel_min_per_day =
      tus_weighted_travel_min / tus_weighted_diary_days,
    tus_trips_per_day =
      tus_weighted_trips / tus_weighted_diary_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

nts_unmerged_tripday_denominator <- nts_trips_allweek %>%
  distinct(period, DayID, .keep_all = TRUE) %>%
  group_by(period) %>%
  summarise(
    # wt_base: counting days, not trips. See the weighting note above.
    nts_weighted_trip_days = sum(wt_base, na.rm = TRUE),
    .groups = "drop"
  )

nts_daily_for_unmerged_check <- nts_trips_allweek %>%
  group_by(period) %>%
  summarise(
    nts_weighted_trips      = sum(wt, na.rm = TRUE),
    nts_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    nts_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nts_unmerged_tripday_denominator, by = "period") %>%
  mutate(
    nts_trips_per_day =
      nts_weighted_trips / nts_weighted_trip_days,
    nts_travel_min_per_day =
      nts_weighted_travel_min / nts_weighted_trip_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

reconciliation_unmerged <- tus_unmerged_daily %>%
  select(
    period,
    tus_trips_per_day,
    tus_travel_min_per_day,
    tus_mean_duration_min
  ) %>%
  left_join(
    nts_daily_for_unmerged_check %>%
      select(
        period,
        nts_trips_per_day,
        nts_travel_min_per_day,
        nts_mean_duration_min
      ),
    by = "period"
  ) %>%
  mutate(
    tus_implied_min_per_trip =
      tus_travel_min_per_day / tus_trips_per_day,
    nts_implied_min_per_trip =
      nts_travel_min_per_day / nts_trips_per_day
  )

write_csv(
  reconciliation_unmerged,
  file.path(
    tab_dir,
    "external_validation_T4T5_reconciliation_tripfile_TUS_UNMERGED_allweek.csv"
  )
)

unmerged_reconcile_levels <- c(
  "TUS unmerged (18+)",
  "NTS trip file (18+)"
)
unmerged_reconcile_palette <- c(
  "TUS unmerged (18+)" = "#7B61FF",
  "NTS trip file (18+)" = "#F39C12"
)

trips_plot_data_unmerged <- bind_rows(
  tus_unmerged_daily %>%
    transmute(
      period,
      source = "TUS unmerged (18+)",
      value = tus_trips_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_trips_per_day
    )
) %>%
  mutate(source = factor(source, levels = unmerged_reconcile_levels))

duration_plot_data_unmerged <- bind_rows(
  tus_unmerged_daily %>%
    transmute(
      period,
      source = "TUS unmerged (18+)",
      value = tus_mean_duration_min
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_mean_duration_min
    )
) %>%
  mutate(source = factor(source, levels = unmerged_reconcile_levels))

minutes_plot_data_unmerged <- bind_rows(
  tus_unmerged_daily %>%
    transmute(
      period,
      source = "TUS unmerged (18+)",
      value = tus_travel_min_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_travel_min_per_day
    )
) %>%
  mutate(source = factor(source, levels = unmerged_reconcile_levels))

make_unmerged_reconcile_bar <- function(data, panel_title, y_label) {
  ggplot(data, aes(x = period, y = value, fill = source)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = unmerged_reconcile_palette,
      na.value = "grey50"
    ) +
    labs(
      title = panel_title,
      x = NULL,
      y = y_label,
      fill = "Source"
    ) +
    presentation_theme
}

p_reconciliation_unmerged <- (
  make_unmerged_reconcile_bar(
    trips_plot_data_unmerged,
    "Episodes per person-day",
    "Travel episodes"
  ) |
    make_unmerged_reconcile_bar(
      duration_plot_data_unmerged,
      "Minutes per episode",
      "Mean episode duration (minutes)"
    ) |
    make_unmerged_reconcile_bar(
      minutes_plot_data_unmerged,
      "Total travel min / day",
      "Travel minutes"
    )
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = paste0(
      "Do the totals reconcile? TUS unmerged episodes (18+) vs ",
      "NTS trip file (18+, all week)"
    ),
    subtitle = paste0(
      "Sensitivity check: adjacent TUS travel episodes are not merged; ",
      "NTS trip construction is unchanged"
    ),
    theme = theme(legend.position = "bottom")
  )

ggsave(
  file.path(
    out_dir,
    "EV_reconciliation_panel_TUS_UNMERGED_tripfile_allweek.png"
  ),
  p_reconciliation_unmerged,
  width = 15,
  height = 5.7,
  dpi = 300
)

message("UNMERGED TUS sensitivity figures saved without overwriting merged results:")
message("  EV_T1_TUS_UNMERGED_vs_NTS_tripfile_trips_in_progress_hourly_index_allweek.png")
message("  EV_T2_TUS_UNMERGED_vs_NTS_tripfile_start_time_distribution_allweek.png")
message("  EV_reconciliation_panel_TUS_UNMERGED_tripfile_allweek.png")


# ============================================================
# 5. SENSITIVITY CHECK: TUS UNMERGED primary + secondary travel
#    Adds three ALL-WEEK figures without overwriting any earlier output:
#      1) T1 travel episodes in progress across the day
#      2) T2 travel-episode start-time distribution
#      3) Trip-file reconciliation panel
#
#    Inclusion rule:
#      * retain every unmerged episode whose PRIMARY activity is travel; OR
#      * retain an episode whose primary activity is NOT travel but whose
#        SECONDARY activity is travel.
#      * an episode is included only once, so episodes where both primary and
#        secondary are travel are not double-counted.
#
#    Interpretation:
#      This is a sensitivity analysis of recorded travel involvement, not a
#      strict door-to-door trip measure. NTS construction is unchanged.
# ============================================================

if (!exists("df2014_premerge") || !exists("df2023_premerge")) {
  stop(
    "The primary-plus-secondary unmerged sensitivity check requires ",
    "df2014_premerge and df2023_premerge. Run/source the main ",
    "harmonisation script first."
  )
}

travel_episodes_unmerged_primary_secondary_allweek <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    travel_role = dplyr::case_when(
      pri_group == FOCUS_GROUP ~ "Primary travel",
      pri_group != FOCUS_GROUP & sec_group == FOCUS_GROUP ~ "Secondary travel only",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(travel_role),
    !is.na(start),
    !is.na(time),
    time > 0
  ) %>%
  mutate(
    start_min = as.numeric(start),
    dur_min   = as.numeric(time),
    end_min   = (start_min + dur_min) %% 1440
  )

message(sprintf(
  paste0(
    "UNMERGED TUS primary + secondary travel episodes (all-week): ",
    "2014-2015 = %s, 2023 = %s"
  ),
  sum(
    travel_episodes_unmerged_primary_secondary_allweek$period == "2014-2015"
  ),
  sum(
    travel_episodes_unmerged_primary_secondary_allweek$period == "2023"
  )
))

message(sprintf(
  paste0(
    "Secondary-travel-only additions: 2014-2015 = %s, 2023 = %s"
  ),
  sum(
    travel_episodes_unmerged_primary_secondary_allweek$period == "2014-2015" &
      travel_episodes_unmerged_primary_secondary_allweek$travel_role ==
      "Secondary travel only"
  ),
  sum(
    travel_episodes_unmerged_primary_secondary_allweek$period == "2023" &
      travel_episodes_unmerged_primary_secondary_allweek$travel_role ==
      "Secondary travel only"
  )
))

write_csv(
  travel_episodes_unmerged_primary_secondary_allweek %>%
    select(
      period, person_day, travel_role,
      pri_group, sec_group,
      start, start_min, end_min, dur_min, wt
    ),
  file.path(
    tab_dir,
    "travel_timing_episode_level_UNMERGED_PRIMARY_PLUS_SECONDARY_TUS_allweek.csv"
  )
)

unmerged_ps_tus_source <-
  "TUS unmerged primary + secondary travel episodes"

unmerged_ps_source_levels <- c(
  unmerged_ps_tus_source,
  nts_source_short
)

unmerged_ps_validation_palette <- c(
  "TUS unmerged primary + secondary travel episodes" = "#7B61FF",
  "NTS trip file\nEngland, ages 18+" = "#F39C12"
)

unmerged_ps_validation_linetype <- c(
  "TUS unmerged primary + secondary travel episodes" = "solid",
  "NTS trip file\nEngland, ages 18+" = "dashed"
)

# ------------------------------------------------------------
# 5A. T1: travel episodes in progress across the day — all week
# ------------------------------------------------------------
tus_t1_unmerged_ps <- expand_episode_slots(
  travel_episodes_unmerged_primary_secondary_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    tus_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour),
    period = factor(period, levels = c("2014-2015", "2023"))
  ) %>%
  make_hourly_index("tus_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = unmerged_ps_tus_source,
    index_avg_hour_100
  )

nts_t1_unmerged_ps_comparison <- nts_expand_trip_slots(
  nts_trips_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    nts_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour)
  ) %>%
  make_hourly_index("nts_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    index_avg_hour_100
  )

t1_compare_unmerged_ps <- bind_rows(
  tus_t1_unmerged_ps,
  nts_t1_unmerged_ps_comparison
) %>%
  mutate(source = factor(source, levels = unmerged_ps_source_levels))

write_csv(
  t1_compare_unmerged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T1_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_",
      "vs_NTS_tripfile_hourly_index_allweek.csv"
    )
  )
)

p_t1_unmerged_ps <- ggplot(
  t1_compare_unmerged_ps,
  aes(
    x = hour,
    y = index_avg_hour_100,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_colour_manual(
    values = unmerged_ps_validation_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = unmerged_ps_validation_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T1: travel in progress across the day ",
      "(All-week, TUS unmerged + secondary travel)"
    ),
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS includes primary travel plus episodes with secondary-only travel; ",
      "both indexed to average hour = 100"
    ),
    x = "Hour of day",
    y = "Index (average hour = 100)",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T1_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
      "trips_in_progress_hourly_index_allweek.png"
    )
  ),
  p_t1_unmerged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 5B. T2: travel-episode start-time distribution — all week
# ------------------------------------------------------------
tus_t2_unmerged_ps <-
  travel_episodes_unmerged_primary_secondary_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    hour,
    hour_label,
    source = unmerged_ps_tus_source,
    start_share_percent
  )

nts_t2_unmerged_ps_comparison <- nts_trips_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    start_share_percent
  )

t2_compare_unmerged_ps <- bind_rows(
  tus_t2_unmerged_ps,
  nts_t2_unmerged_ps_comparison
) %>%
  mutate(source = factor(source, levels = unmerged_ps_source_levels))

write_csv(
  t2_compare_unmerged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T2_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_",
      "vs_NTS_tripfile_start_time_share_allweek.csv"
    )
  )
)

p_t2_unmerged_ps <- ggplot(
  t2_compare_unmerged_ps,
  aes(
    x = hour,
    y = start_share_percent,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_colour_manual(
    values = unmerged_ps_validation_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = unmerged_ps_validation_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T2: travel start-time distribution ",
      "(All-week, TUS unmerged + secondary travel)"
    ),
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS includes primary travel plus episodes with secondary-only travel"
    ),
    x = "Travel-episode start hour",
    y = "Share of travel-episode starts",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T2_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
      "start_time_distribution_allweek.png"
    )
  ),
  p_t2_unmerged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 5C. Reconciliation panel — all week, trip-file denominator
# ------------------------------------------------------------
tus_unmerged_ps_daily <-
  travel_episodes_unmerged_primary_secondary_allweek %>%
  group_by(period) %>%
  summarise(
    tus_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    tus_weighted_episodes   = sum(wt, na.rm = TRUE),
    tus_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    period_day_denominator %>%
      transmute(
        period = factor(period, levels = c("2014-2015", "2023")),
        tus_weighted_diary_days = weighted_diary_days
      ),
    by = "period"
  ) %>%
  mutate(
    tus_travel_min_per_day =
      tus_weighted_travel_min / tus_weighted_diary_days,
    tus_episodes_per_day =
      tus_weighted_episodes / tus_weighted_diary_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

# Reuse the unchanged NTS trip-file aggregates created in Section 4.
# Recreate them only if that object is unavailable for any reason.
if (!exists("nts_daily_for_unmerged_check")) {
  nts_unmerged_ps_tripday_denominator <- nts_trips_allweek %>%
    distinct(period, DayID, .keep_all = TRUE) %>%
    group_by(period) %>%
    summarise(
      # wt_base: counting days, not trips. See the weighting note above.
      nts_weighted_trip_days = sum(wt_base, na.rm = TRUE),
      .groups = "drop"
    )

  nts_daily_for_unmerged_check <- nts_trips_allweek %>%
    group_by(period) %>%
    summarise(
      nts_weighted_trips      = sum(wt, na.rm = TRUE),
      nts_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
      nts_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(nts_unmerged_ps_tripday_denominator, by = "period") %>%
    mutate(
      nts_trips_per_day =
        nts_weighted_trips / nts_weighted_trip_days,
      nts_travel_min_per_day =
        nts_weighted_travel_min / nts_weighted_trip_days,
      period = factor(period, levels = c("2014-2015", "2023"))
    )
}

reconciliation_unmerged_ps <- tus_unmerged_ps_daily %>%
  select(
    period,
    tus_episodes_per_day,
    tus_travel_min_per_day,
    tus_mean_duration_min
  ) %>%
  left_join(
    nts_daily_for_unmerged_check %>%
      select(
        period,
        nts_trips_per_day,
        nts_travel_min_per_day,
        nts_mean_duration_min
      ),
    by = "period"
  )

write_csv(
  reconciliation_unmerged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T4T5_reconciliation_tripfile_",
      "TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_allweek.csv"
    )
  )
)

unmerged_ps_reconcile_levels <- c(
  "TUS unmerged + secondary travel (18+)",
  "NTS trip file (18+)"
)

unmerged_ps_reconcile_palette <- c(
  "TUS unmerged + secondary travel (18+)" = "#7B61FF",
  "NTS trip file (18+)" = "#F39C12"
)

episodes_plot_data_unmerged_ps <- bind_rows(
  tus_unmerged_ps_daily %>%
    transmute(
      period,
      source = "TUS unmerged + secondary travel (18+)",
      value = tus_episodes_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_trips_per_day
    )
) %>%
  mutate(
    source = factor(source, levels = unmerged_ps_reconcile_levels)
  )

duration_plot_data_unmerged_ps <- bind_rows(
  tus_unmerged_ps_daily %>%
    transmute(
      period,
      source = "TUS unmerged + secondary travel (18+)",
      value = tus_mean_duration_min
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_mean_duration_min
    )
) %>%
  mutate(
    source = factor(source, levels = unmerged_ps_reconcile_levels)
  )

minutes_plot_data_unmerged_ps <- bind_rows(
  tus_unmerged_ps_daily %>%
    transmute(
      period,
      source = "TUS unmerged + secondary travel (18+)",
      value = tus_travel_min_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_travel_min_per_day
    )
) %>%
  mutate(
    source = factor(source, levels = unmerged_ps_reconcile_levels)
  )

make_unmerged_ps_reconcile_bar <- function(data, panel_title, y_label) {
  ggplot(data, aes(x = period, y = value, fill = source)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = unmerged_ps_reconcile_palette,
      na.value = "grey50"
    ) +
    labs(
      title = panel_title,
      x = NULL,
      y = y_label,
      fill = "Source"
    ) +
    presentation_theme
}

p_reconciliation_unmerged_ps <- (
  make_unmerged_ps_reconcile_bar(
    episodes_plot_data_unmerged_ps,
    "Travel episodes per person-day",
    "Travel episodes"
  ) |
    make_unmerged_ps_reconcile_bar(
      duration_plot_data_unmerged_ps,
      "Minutes per travel episode",
      "Mean episode duration (minutes)"
    ) |
    make_unmerged_ps_reconcile_bar(
      minutes_plot_data_unmerged_ps,
      "Total recorded travel min / day",
      "Travel minutes"
    )
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = paste0(
      "Sensitivity check: TUS unmerged primary + secondary travel (18+) ",
      "vs NTS trip file (18+, all week)"
    ),
    subtitle = paste0(
      "TUS adds episodes where primary activity is not travel but secondary ",
      "activity is travel; each original episode is counted once"
    ),
    theme = theme(legend.position = "bottom")
  )

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_reconciliation_panel_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_",
      "tripfile_allweek.png"
    )
  ),
  p_reconciliation_unmerged_ps,
  width = 15,
  height = 5.7,
  dpi = 300
)

message(
  paste0(
    "UNMERGED primary + secondary travel sensitivity figures saved ",
    "without overwriting earlier results:"
  )
)
message(
  paste0(
    "  EV_T1_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
    "trips_in_progress_hourly_index_allweek.png"
  )
)
message(
  paste0(
    "  EV_T2_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
    "start_time_distribution_allweek.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_TUS_UNMERGED_PRIMARY_PLUS_SECONDARY_",
    "tripfile_allweek.png"
  )
)


# ============================================================
# 6. SENSITIVITY CHECK: TUS MERGED primary + secondary travel
#    Adds three ALL-WEEK figures without overwriting any earlier output:
#      1) T1 merged travel spells in progress across the day
#      2) T2 merged travel-spell start-time distribution
#      3) Trip-file reconciliation panel
#
#    Inclusion rule before merging:
#      * primary activity is travel; OR
#      * primary activity is not travel but secondary activity is travel.
#
#    Merge rule:
#      * selected travel-involved episodes are sorted within person-day;
#      * consecutive/overlapping selected episodes are merged when there is
#        no positive gap between the end of the preceding selected spell and
#        the start of the next selected episode;
#      * an original episode is included only once.
#
#    Interpretation:
#      These are merged "travel-involvement spells", not necessarily identical
#      to conventional door-to-door trips, because secondary travel is included.
# ============================================================

if (!exists("df2014_premerge") || !exists("df2023_premerge")) {
  stop(
    "The merged primary-plus-secondary sensitivity check requires ",
    "df2014_premerge and df2023_premerge. Run/source the main ",
    "harmonisation script first."
  )
}

travel_selected_ps_premerge <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    travel_role = dplyr::case_when(
      pri_group == FOCUS_GROUP ~ "Primary travel",
      pri_group != FOCUS_GROUP & sec_group == FOCUS_GROUP ~
        "Secondary travel only",
      TRUE ~ NA_character_
    ),
    start_num = as.numeric(start),
    end_unwrapped = start_num + as.numeric(time)
  ) %>%
  filter(
    !is.na(travel_role),
    !is.na(start_num),
    !is.na(time),
    time > 0
  )

# Merge adjacent or overlapping selected travel-involved episodes.
# cummax() makes the grouping robust to chains of overlapping episodes.
travel_spells_merged_primary_secondary_allweek <-
  travel_selected_ps_premerge %>%
  arrange(period, person_day, start_num, end_unwrapped) %>%
  group_by(period, person_day) %>%
  mutate(
    previous_selected_end =
      dplyr::lag(cummax(end_unwrapped), default = -Inf),
    begins_new_spell =
      dplyr::row_number() == 1L |
      start_num > previous_selected_end + 1e-8,
    merged_spell_id = cumsum(begins_new_spell)
  ) %>%
  group_by(period, person_day, merged_spell_id) %>%
  summarise(
    start_min = min(start_num, na.rm = TRUE),
    merged_end_unwrapped = max(end_unwrapped, na.rm = TRUE),
    dur_min = merged_end_unwrapped - start_min,
    end_min = merged_end_unwrapped %% 1440,
    wt = dplyr::first(wt),
    n_component_episodes = dplyr::n(),
    includes_primary_travel = any(travel_role == "Primary travel"),
    includes_secondary_only_travel =
      any(travel_role == "Secondary travel only"),
    spell_composition = dplyr::case_when(
      includes_primary_travel & includes_secondary_only_travel ~
        "Primary + secondary-only travel",
      includes_primary_travel ~ "Primary travel only",
      TRUE ~ "Secondary travel only"
    ),
    .groups = "drop"
  ) %>%
  filter(is.finite(dur_min), dur_min > 0) %>%
  mutate(
    start = start_min,
    time = dur_min,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

message(sprintf(
  paste0(
    "MERGED TUS primary + secondary travel spells (all-week): ",
    "2014-2015 = %s, 2023 = %s"
  ),
  sum(
    travel_spells_merged_primary_secondary_allweek$period == "2014-2015"
  ),
  sum(
    travel_spells_merged_primary_secondary_allweek$period == "2023"
  )
))

message(sprintf(
  paste0(
    "Merged spells containing secondary-only travel: ",
    "2014-2015 = %s, 2023 = %s"
  ),
  sum(
    travel_spells_merged_primary_secondary_allweek$period == "2014-2015" &
      travel_spells_merged_primary_secondary_allweek$
      includes_secondary_only_travel
  ),
  sum(
    travel_spells_merged_primary_secondary_allweek$period == "2023" &
      travel_spells_merged_primary_secondary_allweek$
      includes_secondary_only_travel
  )
))

write_csv(
  travel_spells_merged_primary_secondary_allweek,
  file.path(
    tab_dir,
    "travel_timing_spell_level_MERGED_PRIMARY_PLUS_SECONDARY_TUS_allweek.csv"
  )
)

merged_ps_tus_source <-
  "TUS merged primary + secondary travel spells"

merged_ps_source_levels <- c(
  merged_ps_tus_source,
  nts_source_short
)

merged_ps_validation_palette <- c(
  "TUS merged primary + secondary travel spells" = "#7B61FF",
  "NTS trip file\nEngland, ages 18+" = "#F39C12"
)

merged_ps_validation_linetype <- c(
  "TUS merged primary + secondary travel spells" = "solid",
  "NTS trip file\nEngland, ages 18+" = "dashed"
)

# ------------------------------------------------------------
# 6A. T1: merged travel spells in progress across the day
# ------------------------------------------------------------
tus_t1_merged_ps <- expand_episode_slots(
  travel_spells_merged_primary_secondary_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    tus_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour),
    period = factor(period, levels = c("2014-2015", "2023"))
  ) %>%
  make_hourly_index("tus_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = merged_ps_tus_source,
    index_avg_hour_100
  )

nts_t1_merged_ps_comparison <- nts_expand_trip_slots(
  nts_trips_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    nts_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour)
  ) %>%
  make_hourly_index("nts_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    index_avg_hour_100
  )

t1_compare_merged_ps <- bind_rows(
  tus_t1_merged_ps,
  nts_t1_merged_ps_comparison
) %>%
  mutate(source = factor(source, levels = merged_ps_source_levels))

write_csv(
  t1_compare_merged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T1_TUS_MERGED_PRIMARY_PLUS_SECONDARY_",
      "vs_NTS_tripfile_hourly_index_allweek.csv"
    )
  )
)

p_t1_merged_ps <- ggplot(
  t1_compare_merged_ps,
  aes(
    x = hour,
    y = index_avg_hour_100,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_colour_manual(
    values = merged_ps_validation_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = merged_ps_validation_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T1: travel in progress across the day ",
      "(All-week, TUS merged + secondary travel)"
    ),
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS merges adjacent selected episodes after adding secondary-only travel; ",
      "both indexed to average hour = 100"
    ),
    x = "Hour of day",
    y = "Index (average hour = 100)",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T1_TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
      "trips_in_progress_hourly_index_allweek.png"
    )
  ),
  p_t1_merged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 6B. T2: merged travel-spell start-time distribution
# ------------------------------------------------------------
tus_t2_merged_ps <-
  travel_spells_merged_primary_secondary_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    hour,
    hour_label,
    source = merged_ps_tus_source,
    start_share_percent
  )

nts_t2_merged_ps_comparison <- nts_trips_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period,
    hour,
    hour_label,
    source = nts_source_short,
    start_share_percent
  )

t2_compare_merged_ps <- bind_rows(
  tus_t2_merged_ps,
  nts_t2_merged_ps_comparison
) %>%
  mutate(source = factor(source, levels = merged_ps_source_levels))

write_csv(
  t2_compare_merged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T2_TUS_MERGED_PRIMARY_PLUS_SECONDARY_",
      "vs_NTS_tripfile_start_time_share_allweek.csv"
    )
  )
)

p_t2_merged_ps <- ggplot(
  t2_compare_merged_ps,
  aes(
    x = hour,
    y = start_share_percent,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ period, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 23)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_colour_manual(
    values = merged_ps_validation_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = merged_ps_validation_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T2: travel start-time distribution ",
      "(All-week, TUS merged + secondary travel)"
    ),
    subtitle = paste0(
      "TUS adults 18+ vs NTS England ages 18+; matched survey years\n",
      "TUS merges adjacent selected episodes after adding secondary-only travel"
    ),
    x = "Merged travel-spell start hour",
    y = "Share of merged travel-spell starts",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(legend.text = element_text(lineheight = 0.95))

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T2_TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
      "start_time_distribution_allweek.png"
    )
  ),
  p_t2_merged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 6C. Reconciliation panel — merged primary + secondary travel
# ------------------------------------------------------------
tus_merged_ps_daily <-
  travel_spells_merged_primary_secondary_allweek %>%
  group_by(period) %>%
  summarise(
    tus_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
    tus_weighted_spells     = sum(wt, na.rm = TRUE),
    tus_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    period_day_denominator %>%
      transmute(
        period = factor(period, levels = c("2014-2015", "2023")),
        tus_weighted_diary_days = weighted_diary_days
      ),
    by = "period"
  ) %>%
  mutate(
    tus_travel_min_per_day =
      tus_weighted_travel_min / tus_weighted_diary_days,
    tus_spells_per_day =
      tus_weighted_spells / tus_weighted_diary_days,
    period = factor(period, levels = c("2014-2015", "2023"))
  )

if (!exists("nts_daily_for_unmerged_check")) {
  nts_merged_ps_tripday_denominator <- nts_trips_allweek %>%
    distinct(period, DayID, .keep_all = TRUE) %>%
    group_by(period) %>%
    summarise(
      # wt_base: counting days, not trips. See the weighting note above.
      nts_weighted_trip_days = sum(wt_base, na.rm = TRUE),
      .groups = "drop"
    )

  nts_daily_for_unmerged_check <- nts_trips_allweek %>%
    group_by(period) %>%
    summarise(
      nts_weighted_trips      = sum(wt, na.rm = TRUE),
      nts_weighted_travel_min = sum(dur_min * wt, na.rm = TRUE),
      nts_mean_duration_min   = weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(nts_merged_ps_tripday_denominator, by = "period") %>%
    mutate(
      nts_trips_per_day =
        nts_weighted_trips / nts_weighted_trip_days,
      nts_travel_min_per_day =
        nts_weighted_travel_min / nts_weighted_trip_days,
      period = factor(period, levels = c("2014-2015", "2023"))
    )
}

reconciliation_merged_ps <- tus_merged_ps_daily %>%
  select(
    period,
    tus_spells_per_day,
    tus_travel_min_per_day,
    tus_mean_duration_min
  ) %>%
  left_join(
    nts_daily_for_unmerged_check %>%
      select(
        period,
        nts_trips_per_day,
        nts_travel_min_per_day,
        nts_mean_duration_min
      ),
    by = "period"
  )

write_csv(
  reconciliation_merged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T4T5_reconciliation_tripfile_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_allweek.csv"
    )
  )
)

merged_ps_reconcile_levels <- c(
  "TUS merged + secondary travel (18+)",
  "NTS trip file (18+)"
)

merged_ps_reconcile_palette <- c(
  "TUS merged + secondary travel (18+)" = "#7B61FF",
  "NTS trip file (18+)" = "#F39C12"
)

spells_plot_data_merged_ps <- bind_rows(
  tus_merged_ps_daily %>%
    transmute(
      period,
      source = "TUS merged + secondary travel (18+)",
      value = tus_spells_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_trips_per_day
    )
) %>%
  mutate(source = factor(source, levels = merged_ps_reconcile_levels))

duration_plot_data_merged_ps <- bind_rows(
  tus_merged_ps_daily %>%
    transmute(
      period,
      source = "TUS merged + secondary travel (18+)",
      value = tus_mean_duration_min
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_mean_duration_min
    )
) %>%
  mutate(source = factor(source, levels = merged_ps_reconcile_levels))

minutes_plot_data_merged_ps <- bind_rows(
  tus_merged_ps_daily %>%
    transmute(
      period,
      source = "TUS merged + secondary travel (18+)",
      value = tus_travel_min_per_day
    ),
  nts_daily_for_unmerged_check %>%
    transmute(
      period,
      source = "NTS trip file (18+)",
      value = nts_travel_min_per_day
    )
) %>%
  mutate(source = factor(source, levels = merged_ps_reconcile_levels))

make_merged_ps_reconcile_bar <- function(data, panel_title, y_label) {
  ggplot(data, aes(x = period, y = value, fill = source)) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = merged_ps_reconcile_palette,
      na.value = "grey50"
    ) +
    labs(
      title = panel_title,
      x = NULL,
      y = y_label,
      fill = "Source"
    ) +
    presentation_theme
}

p_reconciliation_merged_ps <- (
  make_merged_ps_reconcile_bar(
    spells_plot_data_merged_ps,
    "Merged travel spells per person-day",
    "Merged travel spells"
  ) |
    make_merged_ps_reconcile_bar(
      duration_plot_data_merged_ps,
      "Minutes per merged travel spell",
      "Mean spell duration (minutes)"
    ) |
    make_merged_ps_reconcile_bar(
      minutes_plot_data_merged_ps,
      "Total recorded travel min / day",
      "Travel minutes"
    )
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = paste0(
      "Sensitivity check: TUS merged primary + secondary travel (18+) ",
      "vs NTS trip file (18+, all week)"
    ),
    subtitle = paste0(
      "Secondary-only travel episodes are added before adjacent selected ",
      "episodes are merged; each original episode is counted once"
    ),
    theme = theme(legend.position = "bottom")
  )

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_reconciliation_panel_TUS_MERGED_PRIMARY_PLUS_SECONDARY_",
      "tripfile_allweek.png"
    )
  ),
  p_reconciliation_merged_ps,
  width = 15,
  height = 5.7,
  dpi = 300
)

message(
  "MERGED primary + secondary travel sensitivity figures saved ",
  "without overwriting earlier results:"
)
message(
  paste0(
    "  EV_T1_TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
    "trips_in_progress_hourly_index_allweek.png"
  )
)
message(
  paste0(
    "  EV_T2_TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_tripfile_",
    "start_time_distribution_allweek.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_TUS_MERGED_PRIMARY_PLUS_SECONDARY_",
    "tripfile_allweek.png"
  )
)


# ============================================================
# 7. WEIGHTED TRAVELLER-DAY SENSITIVITY:
#    MERGED PRIMARY + SECONDARY TRAVEL
#
# This section creates an additional all-week version of:
#   * T1 travel in progress
#   * T2 travel-spell start-time distribution
#   * weighted traveller-day reconciliation panel
#
# Inclusion rule before merging:
#   1) primary activity is travel; OR
#   2) primary activity is not travel AND secondary activity is travel.
#
# No duplication:
#   Episodes with both primary and secondary travel are retained once under
#   "Primary travel", because the secondary-only condition explicitly requires
#   pri_group != FOCUS_GROUP.
#
# Merge:
#   Adjacent or overlapping selected episodes are merged within person-day.
#
# Denominator:
#   Both TUS and NTS include only observed traveller-days and both use survey
#   weights. This is not an all-person-day estimate.
#
# All output filenames below are new and do not overwrite the primary-only
# weighted traveller-day results generated earlier.
# ============================================================

if (!exists("travel_spells_merged_primary_secondary_allweek")) {
  stop(
    "Section 7 requires travel_spells_merged_primary_secondary_allweek. ",
    "Run the complete script so Section 6 constructs the merged ",
    "primary-plus-secondary travel spells first."
  )
}

weighted_merged_ps_tus_source <-
  "TUS merged primary + secondary travel spells"

weighted_merged_ps_nts_source <-
  "NTS weighted trips\nEngland, ages 18+"

weighted_merged_ps_source_levels <- c(
  weighted_merged_ps_tus_source,
  weighted_merged_ps_nts_source
)

weighted_merged_ps_palette <- stats::setNames(
  c("#7B61FF", "#F39C12"),
  weighted_merged_ps_source_levels
)

weighted_merged_ps_linetype <- stats::setNames(
  c("solid", "dashed"),
  weighted_merged_ps_source_levels
)

# ------------------------------------------------------------
# 7A. T1 — weighted travel in progress across the day
# ------------------------------------------------------------
tus_t1_weighted_merged_ps <- expand_episode_slots(
  travel_spells_merged_primary_secondary_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    weighted_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour),
    period = factor(period, levels = c("2014-2015", "2023"))
  ) %>%
  make_hourly_index("weighted_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = weighted_merged_ps_tus_source,
    index_avg_hour_100
  )

nts_t1_weighted_merged_ps <- nts_expand_trip_slots(
  nts_trips_allweek,
  60
) %>%
  group_by(period, slot) %>%
  summarise(
    weighted_travel_minutes = sum(weighted_overlap_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hour = slot,
    hour_label = parse_hour_label(hour),
    period = factor(period, levels = c("2014-2015", "2023"))
  ) %>%
  make_hourly_index("weighted_travel_minutes", c("period")) %>%
  transmute(
    period,
    hour,
    hour_label,
    source = weighted_merged_ps_nts_source,
    index_avg_hour_100
  )

t1_compare_weighted_merged_ps <- bind_rows(
  tus_t1_weighted_merged_ps,
  nts_t1_weighted_merged_ps
) %>%
  mutate(
    source = factor(
      source,
      levels = weighted_merged_ps_source_levels
    )
  )

write_csv(
  t1_compare_weighted_merged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T1_WEIGHTED_TRAVELLERDAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_allweek.csv"
    )
  )
)

p_t1_weighted_merged_ps <- ggplot(
  t1_compare_weighted_merged_ps,
  aes(
    x = hour,
    y = index_avg_hour_100,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~period, ncol = 1) +
  scale_x_continuous(
    breaks = seq(0, 24, 3),
    limits = c(0, 23)
  ) +
  scale_colour_manual(
    values = weighted_merged_ps_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = weighted_merged_ps_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T1: travel in progress across the day ",
      "(All-week, merged primary + secondary travel)"
    ),
    subtitle = paste0(
      "Weighted TUS travel-involvement spells vs weighted NTS trips; ",
      "matched survey years\n",
      "Secondary-only travel is added once, then adjacent selected ",
      "episodes are merged; average hour = 100"
    ),
    x = "Hour of day",
    y = "Index (average hour = 100)",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(
    legend.text = element_text(lineheight = 0.95)
  )

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T1_WEIGHTED_TRAVELLERDAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_",
      "trips_in_progress_hourly_index_allweek.png"
    )
  ),
  p_t1_weighted_merged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 7B. T2 — weighted merged travel-spell start distribution
# ------------------------------------------------------------
tus_t2_weighted_merged_ps <-
  travel_spells_merged_primary_secondary_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    hour,
    hour_label,
    source = weighted_merged_ps_tus_source,
    start_share_percent
  )

nts_t2_weighted_merged_ps <- nts_trips_allweek %>%
  mutate(
    hour = floor(start_min / 60),
    hour_label = parse_hour_label(hour)
  ) %>%
  group_by(period, hour, hour_label) %>%
  summarise(
    weighted_starts = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    start_share_percent =
      weighted_starts / sum(weighted_starts, na.rm = TRUE) * 100
  ) %>%
  ungroup() %>%
  transmute(
    period = factor(period, levels = c("2014-2015", "2023")),
    hour,
    hour_label,
    source = weighted_merged_ps_nts_source,
    start_share_percent
  )

t2_compare_weighted_merged_ps <- bind_rows(
  tus_t2_weighted_merged_ps,
  nts_t2_weighted_merged_ps
) %>%
  mutate(
    source = factor(
      source,
      levels = weighted_merged_ps_source_levels
    )
  )

write_csv(
  t2_compare_weighted_merged_ps,
  file.path(
    tab_dir,
    paste0(
      "external_validation_T2_WEIGHTED_TRAVELLERDAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_allweek.csv"
    )
  )
)

p_t2_weighted_merged_ps <- ggplot(
  t2_compare_weighted_merged_ps,
  aes(
    x = hour,
    y = start_share_percent,
    colour = source,
    linetype = source
  )
) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~period, ncol = 1) +
  scale_x_continuous(
    breaks = seq(0, 24, 3),
    limits = c(0, 23)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1)
  ) +
  scale_colour_manual(
    values = weighted_merged_ps_palette,
    na.value = "grey50"
  ) +
  scale_linetype_manual(
    values = weighted_merged_ps_linetype,
    na.value = "dashed"
  ) +
  labs(
    title = paste0(
      "External validation T2: travel start-time distribution ",
      "(All-week, merged primary + secondary travel)"
    ),
    subtitle = paste0(
      "Weighted TUS travel-involvement spells vs weighted NTS trips; ",
      "matched survey years\n",
      "Secondary-only travel is added once, then adjacent selected ",
      "episodes are merged"
    ),
    x = "Merged travel-spell start hour",
    y = "Share of merged travel-spell starts",
    colour = "Source",
    linetype = "Source"
  ) +
  presentation_theme +
  theme(
    legend.text = element_text(lineheight = 0.95)
  )

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_T2_WEIGHTED_TRAVELLERDAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_vs_NTS_",
      "start_time_distribution_allweek.png"
    )
  ),
  p_t2_weighted_merged_ps,
  width = 12,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 7C. MAIN all-day seven-day reconciliation:
#     merged primary + secondary travel
# ------------------------------------------------------------
# TUS numerator includes:
#   primary travel, plus secondary-only travel once, merged across adjacent
#   selected episodes.
#
# Denominators:
#   TUS = all valid diary-days, including zero-travel days.
#   NTS = weighted adult persons × 7 recorded days.
# ------------------------------------------------------------

tus_all_days_denominator_allweek <- make_tus_denominator("allweek")
nts_seven_day_denominator_allweek <-
  make_nts_seven_day_denominator("allweek")

tus_merged_ps_all_days <- 
  travel_spells_merged_primary_secondary_allweek %>%
  group_by(period) %>%
  summarise(
    tus_weighted_spells = sum(wt, na.rm = TRUE),
    tus_weighted_travel_min =
      sum(dur_min * wt, na.rm = TRUE),
    tus_mean_duration_min =
      weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    tus_all_days_denominator_allweek,
    by = "period"
  ) %>%
  mutate(
    tus_spells_per_day =
      tus_weighted_spells / tus_weighted_diary_days,
    tus_travel_min_per_day =
      tus_weighted_travel_min / tus_weighted_diary_days,
    period = factor(
      period,
      levels = c("2014-2015", "2023")
    )
  )

nts_seven_day_all_days <- nts_trips_allweek %>%
  group_by(period) %>%
  summarise(
    nts_weighted_trips = sum(wt, na.rm = TRUE),
    nts_weighted_travel_min =
      sum(dur_min * wt, na.rm = TRUE),
    nts_mean_duration_min =
      weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    nts_seven_day_denominator_allweek,
    by = "period"
  ) %>%
  mutate(
    nts_trips_per_day =
      nts_weighted_trips / nts_weighted_diary_days,
    nts_travel_min_per_day =
      nts_weighted_travel_min / nts_weighted_diary_days,
    period = factor(
      period,
      levels = c("2014-2015", "2023")
    )
  )

reconciliation_merged_ps_seven_day <-
  tus_merged_ps_all_days %>%
  select(
    period,
    tus_spells_per_day,
    tus_mean_duration_min,
    tus_travel_min_per_day,
    tus_weighted_diary_days
  ) %>%
  left_join(
    nts_seven_day_all_days %>%
      select(
        period,
        nts_trips_per_day,
        nts_mean_duration_min,
        nts_travel_min_per_day,
        nts_weighted_persons,
        nts_weighted_diary_days
      ),
    by = "period"
  ) %>%
  mutate(
    denominator_definition = paste0(
      "TUS all diary-days; NTS weighted persons × 7; ",
      "zero-travel days included on both sides"
    ),
    tus_identity_error =
      tus_travel_min_per_day -
      tus_spells_per_day * tus_mean_duration_min,
    nts_identity_error =
      nts_travel_min_per_day -
      nts_trips_per_day * nts_mean_duration_min
  )

write_csv(
  reconciliation_merged_ps_seven_day,
  file.path(
    tab_dir,
    paste0(
      "external_validation_reconciliation_SEVENDAY_ALL_DAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_allweek.csv"
    )
  )
)

merged_ps_all_days_levels <- c(
  "TUS merged primary + secondary, all diary-days (18+)",
  "NTS seven-day diary average (18+)"
)

merged_ps_all_days_palette <- c(
  "TUS merged primary + secondary, all diary-days (18+)" =
    "#7B61FF",
  "NTS seven-day diary average (18+)" =
    "#F39C12"
)

spells_plot_merged_ps_all_days <- bind_rows(
  tus_merged_ps_all_days %>%
    transmute(
      period,
      source =
        "TUS merged primary + secondary, all diary-days (18+)",
      value = tus_spells_per_day
    ),
  nts_seven_day_all_days %>%
    transmute(
      period,
      source = "NTS seven-day diary average (18+)",
      value = nts_trips_per_day
    )
) %>%
  mutate(
    source = factor(
      source,
      levels = merged_ps_all_days_levels
    )
  )

duration_plot_merged_ps_all_days <- bind_rows(
  tus_merged_ps_all_days %>%
    transmute(
      period,
      source =
        "TUS merged primary + secondary, all diary-days (18+)",
      value = tus_mean_duration_min
    ),
  nts_seven_day_all_days %>%
    transmute(
      period,
      source = "NTS seven-day diary average (18+)",
      value = nts_mean_duration_min
    )
) %>%
  mutate(
    source = factor(
      source,
      levels = merged_ps_all_days_levels
    )
  )

minutes_plot_merged_ps_all_days <- bind_rows(
  tus_merged_ps_all_days %>%
    transmute(
      period,
      source =
        "TUS merged primary + secondary, all diary-days (18+)",
      value = tus_travel_min_per_day
    ),
  nts_seven_day_all_days %>%
    transmute(
      period,
      source = "NTS seven-day diary average (18+)",
      value = nts_travel_min_per_day
    )
) %>%
  mutate(
    source = factor(
      source,
      levels = merged_ps_all_days_levels
    )
  )

make_merged_ps_all_days_bar <- function(
    data,
    panel_title,
    y_label
) {
  ggplot(
    data,
    aes(x = period, y = value, fill = source)
  ) +
    geom_col(position = "dodge") +
    scale_fill_manual(
      values = merged_ps_all_days_palette,
      na.value = "grey50"
    ) +
    labs(
      title = panel_title,
      x = NULL,
      y = y_label,
      fill = "Source"
    ) +
    presentation_theme
}

p_reconciliation_merged_ps_seven_day <- (
  make_merged_ps_all_days_bar(
    spells_plot_merged_ps_all_days,
    "Merged travel spells / trips per recorded day",
    "Travel spells / trips"
  ) |
    make_merged_ps_all_days_bar(
      duration_plot_merged_ps_all_days,
      "Minutes per merged travel spell / trip",
      "Mean duration (minutes)"
    ) |
    make_merged_ps_all_days_bar(
      minutes_plot_merged_ps_all_days,
      "Total travel min / recorded day",
      "Travel minutes"
    )
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = paste0(
      "All-day reconciliation: TUS merged primary + secondary travel ",
      "vs NTS seven-day diary"
    ),
    subtitle = paste0(
      "Both sides include zero-travel days. TUS adds secondary-only ",
      "travel once and merges adjacent selected episodes; ",
      "NTS denominator = weighted persons × 7."
    ),
    theme = theme(
      legend.position = "bottom",
      plot.subtitle = element_text(size = 12)
    )
  )

ggsave(
  file.path(
    out_dir,
    paste0(
      "EV_reconciliation_panel_SEVENDAY_ALL_DAYS_",
      "TUS_MERGED_PRIMARY_PLUS_SECONDARY_allweek.png"
    )
  ),
  p_reconciliation_merged_ps_seven_day,
  width = 15,
  height = 5.8,
  dpi = 300
)

message(
  "\nMerged primary + secondary all-day seven-day output saved:"
)
message(
  paste0(
    "  EV_reconciliation_panel_SEVENDAY_ALL_DAYS_",
    "TUS_MERGED_PRIMARY_PLUS_SECONDARY_allweek.png"
  )
)
message(
  "T1/T2 remain weighted trip/spell timing distributions and ",
  "do not require zero-travel-day denominators."
)


# ============================================================
# FINAL OFFICIAL BENCHMARK FIGURE:
# NTS JOTXSC vs both TUS travel definitions
# ============================================================
tus_merged_daily_for_official_compare <-
  reconciliation_merged_ps_seven_day %>%
  transmute(
    period,
    `TUS primary + secondary merged (18+)` =
      tus_travel_min_per_day
  )

official_compare_all_tus_definitions <-
  tus_primary_daily_for_official_compare %>%
  left_join(
    tus_merged_daily_for_official_compare,
    by = "period"
  ) %>%
  left_join(
    nts_official_daily_benchmark %>%
      transmute(
        period,
        `NTS official JOTXSC (18+)` =
          nts_official_travel_min_per_person_day
      ),
    by = "period"
  ) %>%
  pivot_longer(
    cols = -period,
    names_to = "source",
    values_to = "travel_min_per_day"
  )

official_all_source_levels <- c(
  "TUS primary travel, all diary-days (18+)",
  "TUS primary + secondary merged (18+)",
  "NTS official JOTXSC (18+)"
)

official_all_source_palette <- c(
  "TUS primary travel, all diary-days (18+)" = "#7B61FF",
  "TUS primary + secondary merged (18+)" = "#4DB6AC",
  "NTS official JOTXSC (18+)" = "#F39C12"
)

p_official_all_compare <- official_compare_all_tus_definitions %>%
  mutate(
    source = factor(source, levels = official_all_source_levels)
  ) %>%
  ggplot(
    aes(
      x = period,
      y = travel_min_per_day,
      fill = source
    )
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = sprintf("%.2f", travel_min_per_day)),
    position = position_dodge(width = 0.9),
    vjust = -0.35,
    size = 3.7
  ) +
  scale_fill_manual(values = official_all_source_palette) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "NTS official benchmark versus alternative TUS travel definitions",
    subtitle = paste0(
      "Average minutes per person per recorded day, age 18+; ",
      "zero-travel days included"
    ),
    x = NULL,
    y = "Travel minutes per person per day",
    fill = "Source"
  ) +
  presentation_theme +
  theme(legend.position = "bottom")

ggsave(
  file.path(
    out_dir,
    "EV_OFFICIAL_NTS_JOTXSC_vs_TUS_primary_and_secondary_travel_minutes_per_day.png"
  ),
  p_official_all_compare,
  width = 11.5,
  height = 6,
  dpi = 300
)

write_csv(
  official_compare_all_tus_definitions,
  file.path(
    tab_dir,
    "external_validation_OFFICIAL_NTS_JOTXSC_vs_TUS_definitions.csv"
  )
)

message(
  "Preferred official NTS benchmark used in the main comparison: ",
  "64.15 min/day (2014-15) and 61.48 min/day (2023)."
)



# ============================================================
# 8. CONDITIONAL TRAVELLER-DAY RECONCILIATION
#    ALL-WEEK / WEEKDAY / WEEKEND
#
# Additional sensitivity figures retained alongside the preferred
# official all-day JOTXSC benchmark.
#
# These figures condition on having at least one recorded travel
# spell/trip during the relevant day type:
#
#   TUS denominator = weighted person-days with travel
#   NTS denominator = weighted DayID values appearing in the trip file
#
# Therefore these are traveller-day statistics, not population-average
# statistics. They are useful for comparing trip/spell construction once
# zero-travel days have been removed from both surveys.
#
# Two TUS definitions are produced:
#   A) merged primary travel only
#   B) merged primary + secondary-only travel
#
# Output names are new and do not overwrite the preferred official
# seven-day all-days figures.
# ============================================================

message(
  "\nSection 8: generating conditional traveller-day reconciliation ",
  "for all-week, weekday and weekend..."
)

TRAVELLER_DAY_SUFFIX <- "OFFICIAL_JOTXSC_W2_CONDITIONAL_TRAVELLERDAYS"

conditional_source_levels_primary <- c(
  "TUS traveller-days (18+)",
  "NTS trip-file traveller-days (18+)"
)

conditional_source_palette_primary <- c(
  "TUS traveller-days (18+)" = "#7B61FF",
  "NTS trip-file traveller-days (18+)" = "#F39C12"
)

conditional_source_levels_ps <- c(
  "TUS primary + secondary traveller-days (18+)",
  "NTS trip-file traveller-days (18+)"
)

conditional_source_palette_ps <- c(
  "TUS primary + secondary traveller-days (18+)" = "#7B61FF",
  "NTS trip-file traveller-days (18+)" = "#F39C12"
)

# Attach weekday/weekend status to merged primary + secondary spells.
travel_spells_merged_primary_secondary_daytyped <-
  travel_spells_merged_primary_secondary_allweek %>%
  left_join(
    weekday_lookup %>%
      select(person_day, is_weekday) %>%
      distinct(),
    by = "person_day"
  )

conditional_day_sets <- list(
  allweek = list(
    label = "all week",
    file_label = "allweek",
    tus_primary = validation_day_sets$allweek$tus,
    tus_primary_secondary =
      travel_spells_merged_primary_secondary_daytyped,
    nts = validation_day_sets$allweek$nts
  ),
  weekday = list(
    label = "weekdays",
    file_label = "weekday",
    tus_primary = validation_day_sets$weekday$tus,
    tus_primary_secondary =
      travel_spells_merged_primary_secondary_daytyped %>%
      filter(is_weekday %in% TRUE),
    nts = validation_day_sets$weekday$nts
  ),
  weekend = list(
    label = "weekends",
    file_label = "weekend",
    tus_primary = validation_day_sets$weekend$tus,
    tus_primary_secondary =
      travel_spells_merged_primary_secondary_daytyped %>%
      filter(is_weekday %in% FALSE),
    nts = validation_day_sets$weekend$nts
  )
)

make_conditional_traveller_day_summary <- function(
    tus_data,
    nts_data,
    tus_definition = c("primary", "primary_secondary")
) {
  tus_definition <- match.arg(tus_definition)

  tus_day_denominator <- tus_data %>%
    distinct(period, person_day, .keep_all = TRUE) %>%
    group_by(period) %>%
    summarise(
      tus_weighted_traveller_days = sum(wt, na.rm = TRUE),
      tus_unweighted_traveller_days = n(),
      .groups = "drop"
    )

  nts_day_denominator <- nts_data %>%
    distinct(period, DayID, .keep_all = TRUE) %>%
    group_by(period) %>%
    summarise(
      # wt_base: counting traveller days, not trips.
      nts_weighted_traveller_days = sum(wt_base, na.rm = TRUE),
      nts_unweighted_traveller_days = n(),
      .groups = "drop"
    )

  tus_summary <- tus_data %>%
    group_by(period) %>%
    summarise(
      tus_weighted_spells = sum(wt, na.rm = TRUE),
      tus_weighted_travel_min =
        sum(dur_min * wt, na.rm = TRUE),
      tus_mean_duration_min =
        weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      tus_day_denominator,
      by = "period"
    ) %>%
    mutate(
      tus_spells_per_traveller_day =
        tus_weighted_spells /
        tus_weighted_traveller_days,
      tus_travel_min_per_traveller_day =
        tus_weighted_travel_min /
        tus_weighted_traveller_days,
      period = factor(
        period,
        levels = c("2014-2015", "2023")
      )
    )

  nts_summary <- nts_data %>%
    group_by(period) %>%
    summarise(
      nts_weighted_trips = sum(wt, na.rm = TRUE),
      nts_weighted_travel_min =
        sum(dur_min * wt, na.rm = TRUE),
      nts_mean_duration_min =
        weighted.mean(dur_min, wt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      nts_day_denominator,
      by = "period"
    ) %>%
    mutate(
      nts_trips_per_traveller_day =
        nts_weighted_trips /
        nts_weighted_traveller_days,
      nts_travel_min_per_traveller_day =
        nts_weighted_travel_min /
        nts_weighted_traveller_days,
      period = factor(
        period,
        levels = c("2014-2015", "2023")
      )
    )

  list(
    tus = tus_summary,
    nts = nts_summary
  )
}

make_conditional_traveller_day_panel <- function(
    day_key,
    cfg,
    tus_definition = c("primary", "primary_secondary")
) {
  tus_definition <- match.arg(tus_definition)

  tus_data <- if (tus_definition == "primary") {
    cfg$tus_primary
  } else {
    cfg$tus_primary_secondary
  }

  nts_data <- cfg$nts

  summaries <- make_conditional_traveller_day_summary(
    tus_data = tus_data,
    nts_data = nts_data,
    tus_definition = tus_definition
  )

  tus_summary <- summaries$tus
  nts_summary <- summaries$nts

  if (tus_definition == "primary") {
    tus_source <- "TUS traveller-days (18+)"
    source_levels <- conditional_source_levels_primary
    source_palette <- conditional_source_palette_primary
    definition_label <- "PRIMARY_ONLY"
    title_tus <- "TUS primary travel"
    first_panel_title <- "Trips per traveller-day"
    second_panel_title <- "Minutes per trip"
  } else {
    tus_source <-
      "TUS primary + secondary traveller-days (18+)"
    source_levels <- conditional_source_levels_ps
    source_palette <- conditional_source_palette_ps
    definition_label <- "MERGED_PRIMARY_PLUS_SECONDARY"
    title_tus <- "TUS merged primary + secondary travel"
    first_panel_title <- "Merged travel spells per traveller-day"
    second_panel_title <- "Minutes per merged travel spell"
  }

  nts_source <- "NTS trip-file traveller-days (18+)"

  reconciliation <- tus_summary %>%
    select(
      period,
      tus_spells_per_traveller_day,
      tus_mean_duration_min,
      tus_travel_min_per_traveller_day,
      tus_weighted_traveller_days,
      tus_unweighted_traveller_days
    ) %>%
    left_join(
      nts_summary %>%
        select(
          period,
          nts_trips_per_traveller_day,
          nts_mean_duration_min,
          nts_travel_min_per_traveller_day,
          nts_weighted_traveller_days,
          nts_unweighted_traveller_days
        ),
      by = "period"
    ) %>%
    mutate(
      day_type = cfg$label,
      tus_definition = tus_definition,
      denominator_definition = paste0(
        "Conditional on at least one recorded travel spell/trip; ",
        "zero-travel days excluded on both sides"
      ),
      tus_identity_error =
        tus_travel_min_per_traveller_day -
        tus_spells_per_traveller_day *
        tus_mean_duration_min,
      nts_identity_error =
        nts_travel_min_per_traveller_day -
        nts_trips_per_traveller_day *
        nts_mean_duration_min
    )

  write_csv(
    reconciliation,
    file.path(
      tab_dir,
      paste0(
        "external_validation_reconciliation_",
        TRAVELLER_DAY_SUFFIX,
        "_",
        definition_label,
        "_",
        cfg$file_label,
        ".csv"
      )
    )
  )

  trips_plot_data <- bind_rows(
    tus_summary %>%
      transmute(
        period,
        source = tus_source,
        value = tus_spells_per_traveller_day
      ),
    nts_summary %>%
      transmute(
        period,
        source = nts_source,
        value = nts_trips_per_traveller_day
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = source_levels
      )
    )

  duration_plot_data <- bind_rows(
    tus_summary %>%
      transmute(
        period,
        source = tus_source,
        value = tus_mean_duration_min
      ),
    nts_summary %>%
      transmute(
        period,
        source = nts_source,
        value = nts_mean_duration_min
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = source_levels
      )
    )

  minutes_plot_data <- bind_rows(
    tus_summary %>%
      transmute(
        period,
        source = tus_source,
        value = tus_travel_min_per_traveller_day
      ),
    nts_summary %>%
      transmute(
        period,
        source = nts_source,
        value = nts_travel_min_per_traveller_day
      )
  ) %>%
    mutate(
      source = factor(
        source,
        levels = source_levels
      )
    )

  make_conditional_bar <- function(
      data,
      panel_title,
      y_label
  ) {
    ggplot(
      data,
      aes(
        x = period,
        y = value,
        fill = source
      )
    ) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = source_palette,
        na.value = "grey50"
      ) +
      labs(
        title = panel_title,
        x = NULL,
        y = y_label,
        fill = "Source"
      ) +
      presentation_theme
  }

  panel <- (
    make_conditional_bar(
      trips_plot_data,
      first_panel_title,
      ifelse(
        tus_definition == "primary",
        "Trips",
        "Merged travel spells"
      )
    ) |
      make_conditional_bar(
        duration_plot_data,
        second_panel_title,
        "Mean duration (minutes)"
      ) |
      make_conditional_bar(
        minutes_plot_data,
        "Total travel min / traveller-day",
        "Travel minutes"
      )
  ) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(
        "Weighted reconciliation: ",
        title_tus,
        " vs NTS traveller-days (",
        cfg$label,
        ")"
      ),
      subtitle = paste0(
        "Both sides use W2/diary weights and include only days ",
        "with at least one recorded travel spell/trip; ",
        "zero-travel days are excluded."
      ),
      theme = theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 12)
      )
    )

  ggsave(
    file.path(
      out_dir,
      paste0(
        "EV_reconciliation_panel_",
        TRAVELLER_DAY_SUFFIX,
        "_",
        definition_label,
        "_",
        cfg$file_label,
        ".png"
      )
    ),
    panel,
    width = 15,
    height = 5.8,
    dpi = 300
  )

  reconciliation
}

conditional_primary_results <- bind_rows(
  lapply(
    names(conditional_day_sets),
    function(day_key) {
      make_conditional_traveller_day_panel(
        day_key = day_key,
        cfg = conditional_day_sets[[day_key]],
        tus_definition = "primary"
      )
    }
  )
)

conditional_primary_secondary_results <- bind_rows(
  lapply(
    names(conditional_day_sets),
    function(day_key) {
      make_conditional_traveller_day_panel(
        day_key = day_key,
        cfg = conditional_day_sets[[day_key]],
        tus_definition = "primary_secondary"
      )
    }
  )
)

write_csv(
  conditional_primary_results,
  file.path(
    tab_dir,
    paste0(
      "external_validation_reconciliation_",
      TRAVELLER_DAY_SUFFIX,
      "_PRIMARY_ONLY_all_daytypes.csv"
    )
  )
)

write_csv(
  conditional_primary_secondary_results,
  file.path(
    tab_dir,
    paste0(
      "external_validation_reconciliation_",
      TRAVELLER_DAY_SUFFIX,
      "_MERGED_PRIMARY_PLUS_SECONDARY_all_daytypes.csv"
    )
  )
)

message(
  "\nConditional traveller-day figures saved for all-week, weekday ",
  "and weekend."
)
message("Primary-only figures:")
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_PRIMARY_ONLY_allweek.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_PRIMARY_ONLY_weekday.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_PRIMARY_ONLY_weekend.png"
  )
)
message("Merged primary + secondary figures:")
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_MERGED_PRIMARY_PLUS_SECONDARY_allweek.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_MERGED_PRIMARY_PLUS_SECONDARY_weekday.png"
  )
)
message(
  paste0(
    "  EV_reconciliation_panel_",
    TRAVELLER_DAY_SUFFIX,
    "_MERGED_PRIMARY_PLUS_SECONDARY_weekend.png"
  )
)
message(
  "Interpretation: conditional traveller-day sensitivity only; ",
  "the preferred headline benchmark remains the all-days official ",
  "JOTXSC comparison."
)



# ============================================================
# 9. TRAVEL PURPOSE / DESTINATION-ACTIVITY ANALYSIS
# ============================================================
# UKTUS/CTUR travel episodes do not directly record a conventional NTS-style
# trip-purpose variable. Therefore, trip purpose is inferred from the next
# non-travel primary activity following each merged travel spell.
#
# Interpretation:
# The six destination-activity groups exactly follow the harmonised TUS
# activity classification used elsewhere in the dissertation, with travel
# removed because travel is the episode being classified:
#
#   Work
#   Personal care
#   Maintenance
#   Shopping
#   Social & leisure
#   Other
#
# This is best described in the dissertation/PPT as:
# "destination activity after travel (a time-use proxy for trip purpose)".
# ============================================================

message("\nSection 9: analysing travel by inferred destination activity...")

purpose_levels <- c(
  "Work",
  "Personal care",
  "Maintenance",
  "Shopping",
  "Social & leisure",
  "Other"
)

recode_destination_purpose <- function(x) {
  # pri_group labels in the harmonised objects may use either spaces or
  # underscores and may label leisure as "social & leisure". Normalise first.
  x_norm <- x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish()

  dplyr::case_when(
    x_norm == "work" ~ "Work",
    x_norm %in% c("personal care", "personal") ~ "Personal care",
    x_norm == "maintenance" ~ "Maintenance",
    x_norm == "shopping" ~ "Shopping",
    x_norm %in% c("leisure", "social & leisure", "social and leisure") ~
      "Social & leisure",
    x_norm == "other" ~ "Other",
    # COMPARABILITY FIX: a following spell that is itself "travel" (chained
    # travel) or a missing following activity (last spell of the diary day,
    # next_primary_group = NA) has NO counterpart in the NTS recorded trip
    # purpose. Previously these were swept into "Other" (TRUE ~ "Other"),
    # which inflated TUS "Other" to ~8,600 vs NTS ~110. Return NA so they are
    # dropped from the six-group comparison and both surveys' "Other" mean the
    # same genuine residual category.
    x_norm == "travel" | is.na(x_norm) ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# Reconstruct the ordered sequence of merged primary-activity spells.
# The next primary non-travel spell is used as the destination-activity proxy.
primary_sequence <- bind_rows(
  df2014_primary_merged,
  df2023_primary_merged
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    start_num = as.numeric(start),
    time_num = as.numeric(time)
  ) %>%
  filter(
    !is.na(person_day),
    !is.na(start_num),
    !is.na(time_num),
    time_num > 0
  ) %>%
  arrange(period, person_day, start_num) %>%
  group_by(period, person_day) %>%
  mutate(
    next_primary_group = lead(pri_group),
    next_primary_start = lead(start_num)
  ) %>%
  ungroup()

travel_by_purpose <- primary_sequence %>%
  filter(pri_group == "travel") %>%
  mutate(
    destination_purpose =
      recode_destination_purpose(next_primary_group),
    destination_purpose = factor(
      destination_purpose,
      levels = purpose_levels
    ),
    dur_min = time_num
  )

# COMPARABILITY FIX: keep the raw next-activity distribution for the diagnostic
# print below, but drop travel-chain / no-following-activity rows (now NA) so
# the six-group TUS vs NTS comparison uses a like-for-like destination purpose.
travel_by_purpose_raw <- travel_by_purpose
travel_by_purpose <- travel_by_purpose %>%
  filter(!is.na(destination_purpose))

message(
  "Travel spells assigned a destination-activity purpose: ",
  sum(!is.na(travel_by_purpose$destination_purpose)),
  " of ",
  nrow(travel_by_purpose)
)


message("Observed next-primary-activity labels before recoding (full, incl. travel-chain / NA):")
print(
  travel_by_purpose_raw %>%
    count(period, next_primary_group, sort = TRUE)
)
message(
  "Dropped as non-comparable (next activity = travel, or no following activity): ",
  "2014-2015 = ",
  sum(is.na(travel_by_purpose_raw$destination_purpose) &
        travel_by_purpose_raw$period == "2014-2015"),
  ", 2023 = ",
  sum(is.na(travel_by_purpose_raw$destination_purpose) &
        travel_by_purpose_raw$period == "2023")
)

message("Six-group destination-activity counts after recoding:")
print(
  travel_by_purpose %>%
    count(period, destination_purpose, .drop = FALSE)
)

# Weighted total travel minutes and weighted spell counts.
purpose_summary <- travel_by_purpose %>%
  group_by(period, destination_purpose) %>%
  summarise(
    weighted_spells = sum(wt, na.rm = TRUE),
    weighted_travel_minutes = sum(dur_min * wt, na.rm = TRUE),
    mean_spell_duration_min =
      weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    period_day_denominator,
    by = "period"
  ) %>%
  group_by(period) %>%
  mutate(
    spells_per_diary_day =
      weighted_spells / weighted_diary_days,
    travel_minutes_per_diary_day =
      weighted_travel_minutes / weighted_diary_days,
    share_of_weighted_spells =
      weighted_spells / sum(weighted_spells, na.rm = TRUE),
    share_of_travel_minutes =
      weighted_travel_minutes /
      sum(weighted_travel_minutes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  tidyr::complete(
    period = factor(c("2014-2015", "2023"),
                    levels = c("2014-2015", "2023")),
    destination_purpose = factor(
      purpose_levels,
      levels = purpose_levels
    ),
    fill = list(
      weighted_spells = 0,
      weighted_travel_minutes = 0,
      mean_spell_duration_min = NA_real_,
      weighted_diary_days = NA_real_,
      spells_per_diary_day = 0,
      travel_minutes_per_diary_day = 0,
      share_of_weighted_spells = 0,
      share_of_travel_minutes = 0
    )
  )

write_csv(
  purpose_summary,
  file.path(
    tab_dir,
    "travel_by_subsequent_TUS_activity_summary.csv"
  )
)

write_csv(
  travel_by_purpose %>%
    select(
      period,
      person_day,
      start,
      dur_min,
      wt,
      next_primary_group,
      destination_purpose
    ),
  file.path(
    tab_dir,
    "travel_by_subsequent_TUS_activity_spell_level.csv"
  )
)

# ------------------------------------------------------------
# Figure A: travel minutes per diary-day by destination activity
# ------------------------------------------------------------
p_purpose_minutes <- purpose_summary %>%
  ggplot(
    aes(
      x = destination_purpose,
      y = travel_minutes_per_diary_day,
      fill = period
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", travel_minutes_per_diary_day)
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.25,
    size = 3.8
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Travel time by subsequent TUS activity group",
    subtitle = paste0(
      "Weighted minutes per diary-day, including zero-travel days; ",
      "classified by the next primary activity"
    ),
    x = NULL,
    y = "Travel minutes per diary-day",
    fill = "Period"
  ) +
  presentation_theme +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "T5_travel_minutes_by_subsequent_TUS_activity.png"
  ),
  p_purpose_minutes,
  width = 11,
  height = 6.3,
  dpi = 300
)

# ------------------------------------------------------------
# Figure B: composition of travel spells by destination activity
# ------------------------------------------------------------
p_purpose_share <- purpose_summary %>%
  ggplot(
    aes(
      x = period,
      y = share_of_weighted_spells,
      fill = destination_purpose
    )
  ) +
  geom_col(width = 0.68) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Composition of travel spells by subsequent TUS activity group",
    subtitle = "Weighted share of merged primary-travel spells",
    x = NULL,
    y = "Share of travel spells",
    fill = "Destination activity"
  ) +
  presentation_theme +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "T6_travel_spell_composition_by_subsequent_TUS_activity.png"
  ),
  p_purpose_share,
  width = 9.5,
  height = 6.3,
  dpi = 300
)

# ------------------------------------------------------------
# Figure C: one-slide combined figure
# ------------------------------------------------------------
p_purpose_combined <-
  p_purpose_minutes +
  p_purpose_share +
  patchwork::plot_layout(
    widths = c(1.35, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Travel by subsequent TUS activity group, 2014–15 and 2023",
    subtitle = paste0(
      "The next primary activity after each merged travel spell is classified ",
      "using the same six non-travel activity groups as the main TUS analysis"
    )
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "T5T6_travel_by_subsequent_TUS_activity_PPT.png"
  ),
  p_purpose_combined,
  width = 15,
  height = 6.5,
  dpi = 300
)

# ------------------------------------------------------------
# Compact change table for PPT narration
# ------------------------------------------------------------
purpose_change <- purpose_summary %>%
  select(
    period,
    destination_purpose,
    travel_minutes_per_diary_day,
    share_of_weighted_spells
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(
      travel_minutes_per_diary_day,
      share_of_weighted_spells
    )
  ) %>%
  mutate(
    change_minutes_per_day =
      `travel_minutes_per_diary_day_2023` -
      `travel_minutes_per_diary_day_2014-2015`,
    change_spell_share_pp =
      100 * (
        `share_of_weighted_spells_2023` -
        `share_of_weighted_spells_2014-2015`
      )
  ) %>%
  arrange(desc(abs(change_minutes_per_day)))

write_csv(
  purpose_change,
  file.path(
    tab_dir,
    "travel_by_subsequent_TUS_activity_change_2014_2023.csv"
  )
)

message("Travel-purpose outputs saved:")
message("  T5_travel_minutes_by_subsequent_TUS_activity.png")
message("  T6_travel_spell_composition_by_subsequent_TUS_activity.png")
message("  T5T6_travel_by_subsequent_TUS_activity_PPT.png")
message(
  "Interpret as the subsequent TUS activity group after travel. ",
  "The six groups match the main harmonised TUS classification exactly, ",
  "excluding travel itself."
)



# ============================================================
# 10. TUS–NTS COMPARISON BY SIX HARMONISED TRAVEL-PURPOSE GROUPS
# ============================================================
# TUS purpose definition:
#   subsequent primary TUS activity after each merged travel spell.
#
# NTS purpose definition:
#   directly recorded TripPurpose_B01ID (23-category full purpose list).
#
# Both are mapped to the same six non-travel TUS activity groups:
#   Work; Personal care; Maintenance; Shopping;
#   Social & leisure; Other.
#
# Important: the two surveys retain different diary instruments.
# This is a harmonised broad-group comparison, not a claim that the
# underlying purpose measures are identical.
# ============================================================

message(
  "\nSection 10: comparing TUS and NTS by six harmonised ",
  "travel-purpose groups..."
)

# Detect the preferred full NTS purpose variable.
nts_purpose_candidates <- c(
  "TripPurpose_B01ID",
  "trippurpose_b01id",
  "TripPurpose_B02ID",
  "trippurpose_b02id",
  "TripPurpose_B04ID",
  "trippurpose_b04id"
)

nts_purpose_var <- nts_purpose_candidates[
  nts_purpose_candidates %in% names(nts_trips_allweek)
][1]

if (length(nts_purpose_var) == 0 || is.na(nts_purpose_var)) {
  possible_purpose_vars <- names(nts_trips_allweek)[
    stringr::str_detect(
      names(nts_trips_allweek),
      stringr::regex("purp|purpose", ignore_case = TRUE)
    )
  ]

  print(possible_purpose_vars)

  stop(
    "No recognised NTS trip-purpose variable was found. ",
    "Possible purpose-related variables are printed above."
  )
}

message("NTS purpose variable used: ", nts_purpose_var)

# Full 23-category mapping is preferred.
map_nts_purpose_b01_to_six <- function(code) {
  code <- as_num(code)

  dplyr::case_when(
    # Commuting, business, other work
    code %in% c(1, 2, 3) ~ "Work",

    # Personal business: medical, eating/drinking, other
    code %in% c(7, 8, 9) ~ "Personal care",

    # Education and escort activities are treated as obligation/care-related
    # activities in the harmonised broad classification.
    code %in% c(4, 19, 20, 21, 22, 23) ~ "Maintenance",

    # Food and non-food shopping
    code %in% c(5, 6) ~ "Shopping",

    # Visiting, socialising, entertainment, sport, holidays,
    # day trips and just walking
    code %in% 10:17 ~ "Social & leisure",

    # Other non-escort
    code == 18 ~ "Other",

    # Symmetry with the TUS side: unrecognised / missing codes are NOT forced
    # into "Other"; they are dropped so both surveys' "Other" is the same
    # genuine residual (NTS code 18 only).
    TRUE ~ NA_character_
  )
}

# Fallback mappings if only the published 14- or 8-category band exists.
map_nts_purpose_b02_to_six <- function(code) {
  code <- as_num(code)

  dplyr::case_when(
    code %in% c(1, 2) ~ "Work",
    code == 7 ~ "Personal care",
    code %in% c(3, 4, 6) ~ "Maintenance",
    code == 5 ~ "Shopping",
    code %in% 8:13 ~ "Social & leisure",
    code == 14 ~ "Other",
    TRUE ~ NA_character_
  )
}

map_nts_purpose_b04_to_six <- function(code) {
  code <- as_num(code)

  dplyr::case_when(
    code %in% c(1, 2) ~ "Work",
    code == 6 ~ "Personal care",
    code %in% c(3, 5) ~ "Maintenance",
    code == 4 ~ "Shopping",
    code == 7 ~ "Social & leisure",
    code == 8 ~ "Other",
    TRUE ~ NA_character_
  )
}

nts_purpose_mapper <- if (
  stringr::str_detect(
    nts_purpose_var,
    stringr::regex("B01ID$", ignore_case = TRUE)
  )
) {
  map_nts_purpose_b01_to_six
} else if (
  stringr::str_detect(
    nts_purpose_var,
    stringr::regex("B02ID$", ignore_case = TRUE)
  )
) {
  map_nts_purpose_b02_to_six
} else {
  map_nts_purpose_b04_to_six
}

nts_travel_by_purpose <- nts_trips_allweek %>%
  mutate(
    harmonised_purpose = nts_purpose_mapper(
      .data[[nts_purpose_var]]
    ),
    harmonised_purpose = factor(
      harmonised_purpose,
      levels = purpose_levels
    )
  ) %>%
  # COMPARABILITY FIX: drop unrecognised/missing NTS purpose codes so the
  # residual "Other" is symmetric with the TUS side.
  filter(!is.na(harmonised_purpose))

message("NTS six-group purpose counts:")
print(
  nts_travel_by_purpose %>%
    count(period, harmonised_purpose, .drop = FALSE)
)

# TUS all-valid-diary-day denominator.
tus_purpose_for_comparison <- purpose_summary %>%
  transmute(
    period,
    source = "TUS",
    harmonised_purpose = factor(
      destination_purpose,
      levels = purpose_levels
    ),
    events_per_recorded_day = spells_per_diary_day,
    minutes_per_recorded_day = travel_minutes_per_diary_day,
    mean_minutes_per_event = mean_spell_duration_min,
    event_share = share_of_weighted_spells,
    minute_share = share_of_travel_minutes
  )

# NTS all-seven-day denominator, including zero-trip days.
# BUG FIX: make_nts_seven_day_denominator() returns the weighted diary-day
# total in the column `nts_weighted_diary_days`, NOT `nts_weighted_recorded_days`.
# Selecting the non-existent name aborted Section 10 right after the NTS count
# print (before any figure was saved). Rename on select so the downstream
# per-recorded-day divisions keep working unchanged.
nts_allweek_denominator <- make_nts_seven_day_denominator(
  "allweek"
) %>%
  select(
    period,
    nts_weighted_recorded_days = nts_weighted_diary_days
  )

nts_purpose_for_comparison <- nts_travel_by_purpose %>%
  group_by(period, harmonised_purpose) %>%
  summarise(
    weighted_events = sum(wt, na.rm = TRUE),
    weighted_minutes = sum(dur_min * wt, na.rm = TRUE),
    mean_minutes_per_event =
      weighted.mean(dur_min, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    nts_allweek_denominator,
    by = "period"
  ) %>%
  group_by(period) %>%
  mutate(
    source = "NTS",
    events_per_recorded_day =
      weighted_events / nts_weighted_recorded_days,
    minutes_per_recorded_day =
      weighted_minutes / nts_weighted_recorded_days,
    event_share =
      weighted_events / sum(weighted_events, na.rm = TRUE),
    minute_share =
      weighted_minutes / sum(weighted_minutes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    period,
    source,
    harmonised_purpose = factor(
      harmonised_purpose,
      levels = purpose_levels
    ),
    events_per_recorded_day,
    minutes_per_recorded_day,
    mean_minutes_per_event,
    event_share,
    minute_share
  ) %>%
  tidyr::complete(
    period = factor(
      c("2014-2015", "2023"),
      levels = c("2014-2015", "2023")
    ),
    source = "NTS",
    harmonised_purpose = factor(
      purpose_levels,
      levels = purpose_levels
    ),
    fill = list(
      events_per_recorded_day = 0,
      minutes_per_recorded_day = 0,
      mean_minutes_per_event = NA_real_,
      event_share = 0,
      minute_share = 0
    )
  )

tus_nts_purpose_comparison <- bind_rows(
  tus_purpose_for_comparison,
  nts_purpose_for_comparison
) %>%
  mutate(
    source = factor(source, levels = c("TUS", "NTS")),
    period = factor(
      period,
      levels = c("2014-2015", "2023")
    ),
    harmonised_purpose = factor(
      harmonised_purpose,
      levels = purpose_levels
    )
  )

write_csv(
  tus_nts_purpose_comparison,
  file.path(
    tab_dir,
    "external_validation_TUS_NTS_six_group_travel_purpose_comparison.csv"
  )
)


# Diagnostic: identify any non-finite values before plotting.
purpose_plot_diagnostic <- tus_nts_purpose_comparison %>%
  filter(
    !is.finite(minutes_per_recorded_day) |
      !is.finite(event_share)
  )

if (nrow(purpose_plot_diagnostic) > 0) {
  message("Rows with non-finite purpose-plot values:")
  print(purpose_plot_diagnostic)
} else {
  message("Purpose-plot diagnostic passed: all plotted values are finite.")
}

# ------------------------------------------------------------
# Figure D: TUS versus NTS minutes/day by purpose
# ------------------------------------------------------------
p_tus_nts_purpose_minutes <- tus_nts_purpose_comparison %>%
  filter(is.finite(minutes_per_recorded_day), !is.na(harmonised_purpose)) %>%
  ggplot(
    aes(
      x = harmonised_purpose,
      y = minutes_per_recorded_day,
      fill = source
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.78),
    na.rm = TRUE,
    width = 0.7
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", minutes_per_recorded_day)
    ),
    position = position_dodge(width = 0.78),
    vjust = -0.25,
    size = 3.2
  ) +
  facet_wrap(~period, nrow = 1) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Travel time by harmonised purpose: TUS versus NTS",
    subtitle = paste0(
      "Weighted minutes per recorded day, including zero-travel days; ",
      "TUS uses subsequent activity and NTS uses recorded trip purpose"
    ),
    x = NULL,
    y = "Travel minutes per recorded day",
    fill = "Survey"
  ) +
  presentation_theme +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "EV_T5_TUS_vs_NTS_travel_minutes_by_six_harmonised_purposes.png"
  ),
  p_tus_nts_purpose_minutes,
  width = 14,
  height = 6.4,
  dpi = 300
)

# ------------------------------------------------------------
# Figure E: TUS versus NTS purpose composition
# ------------------------------------------------------------
p_tus_nts_purpose_composition <- tus_nts_purpose_comparison %>%
  filter(is.finite(event_share), !is.na(harmonised_purpose)) %>%
  ggplot(
    aes(
      x = source,
      y = event_share,
      fill = harmonised_purpose
    )
  ) +
  geom_col(width = 0.68, na.rm = TRUE) +
  facet_wrap(~period, nrow = 1) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Composition of travel by harmonised purpose: TUS versus NTS",
    subtitle = paste0(
      "Weighted share of TUS travel spells and NTS trips; ",
      "six broad activity groups"
    ),
    x = NULL,
    y = "Share of travel spells / trips",
    fill = "Harmonised purpose"
  ) +
  presentation_theme +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "EV_T6_TUS_vs_NTS_travel_composition_by_six_harmonised_purposes.png"
  ),
  p_tus_nts_purpose_composition,
  width = 11,
  height = 6.4,
  dpi = 300
)

# ------------------------------------------------------------
# Figure F: one-slide PPT figure
# ------------------------------------------------------------
p_tus_nts_purpose_ppt <-
  p_tus_nts_purpose_minutes +
  p_tus_nts_purpose_composition +
  patchwork::plot_layout(
    widths = c(1.45, 1),
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Travel by harmonised purpose: TUS and NTS",
    subtitle = paste0(
      "Both surveys are mapped to the same six broad activity groups; ",
      "all recorded days are included"
    )
  ) &
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "EV_T5T6_TUS_vs_NTS_six_harmonised_purposes_PPT.png"
  ),
  p_tus_nts_purpose_ppt,
  width = 17,
  height = 7,
  dpi = 300
)

message("TUS–NTS six-purpose comparison figures saved:")
message(
  "  EV_T5_TUS_vs_NTS_travel_minutes_by_six_harmonised_purposes.png"
)
message(
  "  EV_T6_TUS_vs_NTS_travel_composition_by_six_harmonised_purposes.png"
)
message(
  "  EV_T5T6_TUS_vs_NTS_six_harmonised_purposes_PPT.png"
)
message(
  "NTS uses directly recorded trip purpose; TUS uses the next primary ",
  "activity after each merged travel spell. Both are harmonised into ",
  "the same six broad groups."
)

# ============================================================
# Section 10b (SENSITIVITY): home-bound reattribution of TUS travel
# ------------------------------------------------------------
# Rationale: NTS codes a "return home" trip with the purpose of the OUTBOUND
# leg (the away-from-home activity). The main TUS proxy above instead uses the
# activity that FOLLOWS each trip; for trips ending at home the following
# activity is whatever the person does at home, which drains travel time out of
# Work and into personal care / maintenance / leisure relative to NTS. This
# variant re-aligns the CONVENTION (not the six-group mapping): for home-bound
# TUS trips only, the purpose is taken from the activity that PRECEDED the trip
# (the origin activity); every non-home trip is unchanged. It is reported
# ALONGSIDE the main result and does NOT replace the headline figures.
#
# CAVEAT: loc6 == "home" collapses own home, other people's home and second
# homes into one category, so "home-bound" here also captures visits to another
# residence. This variant is therefore an UPPER BOUND on the reattribution and
# is indicative only.
# ============================================================
message(
  "\nSection 10b: home-bound reattribution sensitivity ",
  "(TUS return-home trips coded by ORIGIN activity, mirroring the NTS convention)..."
)

homebound_sequence <- bind_rows(
  df2014_primary_merged,
  df2023_primary_merged
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    start_num = as.numeric(start),
    time_num  = as.numeric(time)
  ) %>%
  filter(
    !is.na(person_day), !is.na(start_num), !is.na(time_num), time_num > 0
  ) %>%
  arrange(period, person_day, start_num) %>%
  group_by(period, person_day) %>%
  mutate(
    next_group = lead(pri_group),   # activity AFTER the trip (destination activity)
    next_loc6  = lead(loc6),        # location AFTER the trip (destination location)
    prev_group = lag(pri_group)     # activity BEFORE the trip (origin activity)
  ) %>%
  ungroup()

travel_by_purpose_homebound <- homebound_sequence %>%
  filter(pri_group == "travel") %>%
  mutate(
    is_homebound = !is.na(next_loc6) & next_loc6 == "home",
    # Home-bound trips take the ORIGIN activity; all others keep the destination
    # (next) activity exactly as in the main proxy.
    purpose_source_group = dplyr::if_else(is_homebound, prev_group, next_group),
    destination_purpose  = recode_destination_purpose(purpose_source_group),
    destination_purpose  = factor(destination_purpose, levels = purpose_levels),
    dur_min = time_num
  ) %>%
  filter(!is.na(destination_purpose))

message(sprintf(
  paste0(
    "Home-bound TUS trips reattributed to origin activity: ",
    "2014-2015 = %s, 2023 = %s (of %s / %s travel spells)"
  ),
  sum(travel_by_purpose_homebound$is_homebound &
        travel_by_purpose_homebound$period == "2014-2015"),
  sum(travel_by_purpose_homebound$is_homebound &
        travel_by_purpose_homebound$period == "2023"),
  sum(homebound_sequence$pri_group == "travel" &
        homebound_sequence$period == "2014-2015"),
  sum(homebound_sequence$pri_group == "travel" &
        homebound_sequence$period == "2023")
))

purpose_summary_homebound <- travel_by_purpose_homebound %>%
  group_by(period, destination_purpose) %>%
  summarise(
    weighted_travel_minutes = sum(dur_min * wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    minutes_per_recorded_day = weighted_travel_minutes / weighted_diary_days
  ) %>%
  tidyr::complete(
    period = factor(c("2014-2015", "2023"), levels = c("2014-2015", "2023")),
    destination_purpose = factor(purpose_levels, levels = purpose_levels),
    fill = list(weighted_travel_minutes = 0, minutes_per_recorded_day = 0)
  ) %>%
  transmute(
    period,
    source = "TUS (home-bound reattributed)",
    harmonised_purpose = factor(destination_purpose, levels = purpose_levels),
    minutes_per_recorded_day
  )

# Three-way minutes/day comparison: main proxy, home-bound variant, NTS.
homebound_minutes_comparison <- bind_rows(
  tus_purpose_for_comparison %>%
    transmute(
      period,
      source = "TUS (next activity)",
      harmonised_purpose,
      minutes_per_recorded_day
    ),
  purpose_summary_homebound,
  nts_purpose_for_comparison %>%
    transmute(
      period,
      source = "NTS",
      harmonised_purpose,
      minutes_per_recorded_day
    )
) %>%
  mutate(
    source = factor(
      source,
      levels = c(
        "TUS (next activity)",
        "TUS (home-bound reattributed)",
        "NTS"
      )
    ),
    period = factor(period, levels = c("2014-2015", "2023")),
    harmonised_purpose = factor(harmonised_purpose, levels = purpose_levels)
  )

write_csv(
  homebound_minutes_comparison,
  file.path(
    tab_dir,
    "external_validation_TUS_homebound_sensitivity_minutes_by_purpose.csv"
  )
)

# Wide gap-to-NTS table: shows how far each TUS variant sits from NTS.
homebound_gap_table <- homebound_minutes_comparison %>%
  tidyr::pivot_wider(
    names_from = source,
    values_from = minutes_per_recorded_day
  ) %>%
  mutate(
    gap_next_vs_nts      = `TUS (next activity)` - NTS,
    gap_homebound_vs_nts = `TUS (home-bound reattributed)` - NTS
  ) %>%
  arrange(period, harmonised_purpose)

write_csv(
  homebound_gap_table,
  file.path(
    tab_dir,
    "external_validation_TUS_homebound_gap_to_NTS.csv"
  )
)

message("Home-bound sensitivity gap-to-NTS (minutes per recorded day):")
print(as.data.frame(homebound_gap_table))

# Figure: main proxy vs home-bound variant vs NTS.
p_homebound_sensitivity <- homebound_minutes_comparison %>%
  filter(is.finite(minutes_per_recorded_day), !is.na(harmonised_purpose)) %>%
  ggplot(
    aes(x = harmonised_purpose, y = minutes_per_recorded_day, fill = source)
  ) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72, na.rm = TRUE) +
  geom_text(
    aes(label = sprintf("%.1f", minutes_per_recorded_day)),
    position = position_dodge(width = 0.8),
    vjust = -0.25,
    size = 2.7
  ) +
  facet_wrap(~period, ncol = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Travel time by purpose: home-bound reattribution sensitivity",
    subtitle = paste0(
      "TUS (next activity) is the main proxy; TUS (home-bound reattributed) ",
      "codes return-home trips by the ORIGIN activity\nto mirror the NTS convention"
    ),
    x = NULL,
    y = "Travel minutes per recorded day",
    fill = "Series"
  ) +
  presentation_theme +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  file.path(
    out_dir,
    "EV_T5_TUS_vs_NTS_travel_minutes_by_six_harmonised_purposes_HOMEBOUND_SENSITIVITY.png"
  ),
  p_homebound_sensitivity,
  width = 10.5,
  height = 10.5,
  dpi = 300
)

message("Home-bound reattribution sensitivity outputs saved:")
message(
  "  EV_T5_TUS_vs_NTS_travel_minutes_by_six_harmonised_purposes_HOMEBOUND_SENSITIVITY.png"
)
message(
  "  external_validation_TUS_homebound_sensitivity_minutes_by_purpose.csv"
)
message(
  "  external_validation_TUS_homebound_gap_to_NTS.csv"
)
message(
  "Interpretation: sensitivity only. loc6 == 'home' includes other residences, ",
  "so this is an UPPER BOUND on the reattribution; the headline result remains ",
  "the main next-activity proxy."
)


# ============================================================
# 11. TUS-NTS MODE SPLIT, AND DESIGN-BASED SIGNIFICANCE TESTS
#     FOR BOTH THE PURPOSE SPLIT AND THE MODE SPLIT
# ------------------------------------------------------------
# WHY THIS SECTION EXISTS
#
# Section 10 compares the TUS and NTS purpose composition but reports the
# gaps descriptively, with no indication of whether they are larger than
# sampling error. It also does not use transport mode at all, even though
# both waves record it. Harms et al. (2018) validated the 2014-15 UK TUS
# against the NTS on exactly this dimension (their Figure 2), so adding it
# gives a second, independent validation axis and a direct comparison point
# with the published literature.
#
# WHERE MODE COMES FROM
#
#   2014-15  the WhereWhen field, retained in the harmonised data as `loc`.
#            It combines location and transport mode, with separate codes for
#            car driver, car passenger, van, bus, train, tram/underground,
#            coach, taxi, aeroplane and boat.
#   2023     the activity code itself (110-117), which is mode-based rather
#            than purpose-based: walking, cycle, own car, public transport,
#            other, taxi, motorbike.
#   NTS      MainMode_B04ID.
#
# The 2023 coding is the coarsest of the three (all public transport is a
# single code, and there is no driver/passenger split), so the harmonised
# scheme can only be as fine as 2023 allows:
#
#   Walk | Cycle | Car & taxi | Public transport | Unspecified/other
#
# TREATMENT OF UNSPECIFIED MODE
#
# 15.6% of 2014-15 travel episodes carry "Unspecified transport mode", and
# about 6% of 2023 episodes are 110/115; the NTS has essentially none,
# because an interviewer-coded trip record always has a main mode. Including
# the unspecified category would therefore guarantee a significant TUS-NTS
# difference on that category and mechanically depress every other share in
# the TUS. The headline comparison is consequently computed over CLASSIFIED
# trips only, with the unspecified share reported separately as a data
# quality panel. The version that keeps the category is written to CSV as a
# sensitivity.
#
# MAIN MODE OF A MERGED TRIP
#
# A merged travel spell can contain several modes. The main mode is defined
# as the mode accounting for the most minutes within the spell, which is the
# closest available analogue to the NTS main-mode convention. Taking the
# first leg instead would systematically over-count walking, because most
# multi-modal journeys begin on foot.
#
# INFERENCE
#
# On the NTS side the weight is the grossed trip weight (JJXSC * W5xHH), so
# a short-walk record enters as one sampled unit carrying the weight of
# seven trips. That is the standard treatment of grossed survey data and is
# what a design-based package would also do, but it means the effective
# sample size for walking is smaller than the weighted trip count suggests
# and the walk interval is correspondingly wider.
#
# Shares are ratio estimators from two independent complex samples. Standard
# errors use the ultimate-cluster (linearisation) estimator with the
# individual as the cluster, so that repeated diary days and multiple trips
# from the same person are not treated as independent observations.
# Stratification and finite-population corrections are ignored, which makes
# the intervals mildly conservative. Two tests are reported:
#
#   (1) a per-category two-sided z-test of the TUS-NTS difference in share,
#       with Benjamini-Hochberg correction across categories within each
#       period and split; and
#   (2) an overall Wald test of whether the whole composition differs,
#       obtained by dropping one category and using the full linearised
#       covariance matrix.
#
# Composition, not level, is what is being tested here: the per-diary-day
# rates in Section 10 rest on two different denominators (TUS diary-days
# against the NTS seven-day construction) and are reported descriptively.
# ============================================================

message("\nSection 11: TUS-NTS mode split and significance testing...")

# The travel-timing module calls its figure directory `out_dir`; other modules
# use `fig_dir`. Resolve whichever exists so this section is drop-in either way.
fig_out_dir <- if (exists("out_dir")) {
  out_dir
} else if (exists("fig_dir")) {
  fig_dir
} else {
  "combined_outputs_harmonised/figures_travel_timing"
}
dir.create(fig_out_dir, showWarnings = FALSE, recursive = TRUE)
message("Section 11 figures will be written to: ", fig_out_dir)

# The script-level `validation_source_palette` is keyed on long labels
# ("TUS harmonised travel episodes", "NTS trip file: England, ages 18+, all
# days"). Section 11 codes its source column as "TUS"/"NTS", so reusing that
# palette leaves ggplot with no shared levels: it falls back to na.value and
# both series render in the same grey. Define a palette keyed on the labels
# actually used here, keeping the same two colours as the rest of the module.
section11_source_palette <- c(TUS = "#7B61FF", NTS = "#F39C12")
section11_source_labels <- c(
  TUS = "TUS (time-use diaries)",
  NTS = "NTS (National Travel Survey)"
)

mode_levels <- c(
  "Walk",
  "Cycle",
  "Car & taxi",
  "Public transport",
  "Unspecified/other"
)

mode_levels_classified <- setdiff(mode_levels, "Unspecified/other")


# ------------------------------------------------------------
# 11A. Design-based inference helpers
# ------------------------------------------------------------

# Weighted category shares with a cluster-linearised covariance matrix.
#   group   : category of each observation
#   wt      : survey weight
#   cluster : clustering unit (individual)
# Returns the share vector, its covariance matrix, and the number of clusters.
svy_share_cov <- function(group, wt, cluster, levs) {
  keep <- !is.na(group) & is.finite(wt) & wt > 0 & !is.na(cluster)
  group <- factor(as.character(group)[keep], levels = levs)
  wt <- as.numeric(wt)[keep]
  cluster <- as.character(cluster)[keep]

  K <- length(levs)
  if (length(wt) == 0) {
    return(list(p = stats::setNames(rep(NA_real_, K), levs),
                V = matrix(NA_real_, K, K, dimnames = list(levs, levs)),
                m = 0, n = 0))
  }

  # Build the indicator matrix directly. model.matrix() silently drops levels
  # with no observations, which would misalign the columns against `levs`
  # whenever a category is empty in one period or one survey.
  Y <- outer(as.character(group), levs, "==") * 1
  colnames(Y) <- levs
  Y[is.na(Y)] <- 0

  B <- sum(wt)
  A <- colSums(Y * wt)
  p <- A / B

  # per-cluster weighted totals
  A_c <- rowsum(Y * wt, group = cluster, reorder = TRUE)
  B_c <- as.numeric(rowsum(wt, group = cluster, reorder = TRUE))

  # linearised residuals of the ratio estimator
  U <- (A_c - outer(B_c, p)) / B
  m <- nrow(U)

  V <- if (m > 1) (m / (m - 1)) * crossprod(U) else matrix(NA_real_, K, K)
  dimnames(V) <- list(levs, levs)

  list(p = p, V = V, m = m, n = length(wt))
}


# Compare the same composition between two independent surveys.
compare_share_cov <- function(fit1, fit2, levs, label1 = "TUS", label2 = "NTS") {
  d <- fit1$p - fit2$p
  Vd <- fit1$V + fit2$V
  se <- sqrt(pmax(0, diag(Vd)))

  z <- d / se
  pv <- 2 * stats::pnorm(-abs(z))

  per_cat <- tibble::tibble(
    category = factor(levs, levels = levs),
    share_1 = as.numeric(fit1$p),
    share_2 = as.numeric(fit2$p),
    diff = as.numeric(d),
    se_diff = as.numeric(se),
    ci_low = as.numeric(d - 1.96 * se),
    ci_high = as.numeric(d + 1.96 * se),
    z = as.numeric(z),
    p_value = as.numeric(pv)
  ) %>%
    mutate(
      p_adj = stats::p.adjust(p_value, method = "BH"),
      sig = dplyr::case_when(
        is.na(p_adj) ~ "",
        p_adj < 0.001 ~ "***",
        p_adj < 0.01 ~ "**",
        p_adj < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )
  names(per_cat)[names(per_cat) == "share_1"] <- paste0("share_", label1)
  names(per_cat)[names(per_cat) == "share_2"] <- paste0("share_", label2)

  # Overall Wald test: drop the last category to avoid the singularity
  # created by the shares summing to one.
  K <- length(levs)
  overall <- tibble::tibble(
    wald_stat = NA_real_, wald_df = K - 1L, wald_p = NA_real_
  )
  if (K > 1 && all(is.finite(Vd))) {
    idx <- seq_len(K - 1)
    W <- tryCatch({
      as.numeric(t(d[idx]) %*% solve(Vd[idx, idx, drop = FALSE]) %*% d[idx])
    }, error = function(e) NA_real_)
    overall$wald_stat <- W
    overall$wald_p <- if (is.finite(W)) {
      stats::pchisq(W, df = K - 1, lower.tail = FALSE)
    } else NA_real_
  }

  list(per_category = per_cat, overall = overall)
}


# ------------------------------------------------------------
# 11B. Harmonised mode: TUS
# ------------------------------------------------------------
# Episode-level modes, taken BEFORE the merge so that a multi-modal journey
# is not reduced to its first leg.

classify_tus_mode_2014 <- function(loc_text) {
  a <- stringr::str_to_lower(as.character(loc_text))
  dplyr::case_when(
    stringr::str_detect(a, "on foot") ~ "Walk",
    stringr::str_detect(a, "bicycle") ~ "Cycle",
    stringr::str_detect(a,
      "passenger car|van|lorry|tractor|moped|motorcycle|motorboat|taxi|private travelling mode|private transport mode"
    ) ~ "Car & taxi",
    stringr::str_detect(a,
      "bus|tram|underground|train|aeroplane|boat or ship|coach|public transport"
    ) ~ "Public transport",
    TRUE ~ "Unspecified/other"
  )
}

classify_tus_mode_2023 <- function(code) {
  code <- as_num(code)
  dplyr::case_when(
    code == 111 ~ "Walk",
    code == 112 ~ "Cycle",
    code %in% c(113, 116, 117) ~ "Car & taxi",
    code == 114 ~ "Public transport",
    TRUE ~ "Unspecified/other"
  )
}

tus_mode_episodes <- bind_rows(
  df2014_tagged_for_merge,
  df2023_tagged_for_merge
) %>%
  filter(pri_group == "travel", is.finite(time), time > 0) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    mode5 = dplyr::if_else(
      period == "2014-2015",
      classify_tus_mode_2014(loc),
      classify_tus_mode_2023(pri_code_harmonised)
    ),
    mode5 = factor(mode5, levels = mode_levels)
  )

# Diagnostic: how much of each wave is unclassifiable.
tus_mode_episode_diagnostic <- tus_mode_episodes %>%
  group_by(period, mode5) %>%
  summarise(
    weighted_episodes = sum(wt, na.rm = TRUE),
    weighted_minutes = sum(time * wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(period) %>%
  mutate(
    episode_share = weighted_episodes / sum(weighted_episodes, na.rm = TRUE),
    minute_share = weighted_minutes / sum(weighted_minutes, na.rm = TRUE)
  ) %>%
  ungroup()

write_csv(
  tus_mode_episode_diagnostic,
  file.path(tab_dir, "external_validation_TUS_mode_episode_diagnostic.csv")
)

message("TUS unspecified-mode share of travel episodes:")
print(
  tus_mode_episode_diagnostic %>%
    filter(mode5 == "Unspecified/other") %>%
    select(period, episode_share, minute_share)
)

# Main mode of each merged spell: the mode with the most minutes.
tus_trip_mode <- tus_mode_episodes %>%
  group_by(period, mainid, diaryord, person_day, harmonised_spell_id, mode5) %>%
  summarise(mode_minutes = sum(time, na.rm = TRUE), .groups = "drop") %>%
  group_by(period, mainid, diaryord, person_day, harmonised_spell_id) %>%
  slice_max(order_by = mode_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(period, mainid, diaryord, person_day, harmonised_spell_id,
         main_mode = mode5)

# Attach the main mode to the merged travel spells used everywhere else.
tus_trips_with_mode <- travel_by_purpose %>%
  left_join(
    tus_trip_mode,
    by = c("period", "mainid", "diaryord", "person_day", "harmonised_spell_id")
  ) %>%
  mutate(
    main_mode = as.character(main_mode),
    main_mode = dplyr::if_else(is.na(main_mode), "Unspecified/other", main_mode),
    main_mode = factor(main_mode, levels = mode_levels)
  )


# ------------------------------------------------------------
# 11C. Harmonised mode: NTS
# ------------------------------------------------------------

classify_nts_mode <- function(code) {
  code <- as_num(code)
  dplyr::case_when(
    code == 1 ~ "Walk",
    code == 2 ~ "Cycle",
    code %in% c(3, 4, 5, 6, 12) ~ "Car & taxi",
    code %in% c(7, 8, 9, 10, 11, 13) ~ "Public transport",
    TRUE ~ "Unspecified/other"
  )
}

nts_mode_candidates <- c(
  "MainMode_B04ID", "mainmode_b04id",
  "MainMode_B03ID", "mainmode_b03id",
  "MainMode_B11ID", "mainmode_b11id"
)
nts_mode_var <- nts_mode_candidates[
  nts_mode_candidates %in% names(nts_trips_allweek)
][1]

run_mode_comparison <- !is.na(nts_mode_var) && length(nts_mode_var) > 0

if (!run_mode_comparison) {
  message(
    "No recognised NTS main-mode variable was found; the mode split ",
    "comparison is skipped. Mode-like variables present: ",
    paste(
      names(nts_trips_allweek)[
        stringr::str_detect(names(nts_trips_allweek),
                            stringr::regex("mode", ignore_case = TRUE))
      ],
      collapse = ", "
    )
  )
} else {
  message("NTS mode variable used: ", nts_mode_var)

  # Show how every observed NTS mode code is being assigned, so that the
  # mapping can be checked against the data dictionary rather than assumed.
  nts_mode_map_check <- nts_trips_allweek %>%
    mutate(
      .code = as_num(.data[[nts_mode_var]]),
      .label = tryCatch(
        as.character(haven::as_factor(.data[[nts_mode_var]])),
        error = function(e) as.character(as_num(.data[[nts_mode_var]]))
      ),
      .assigned = classify_nts_mode(.data[[nts_mode_var]])
    ) %>%
    group_by(.code, .label, .assigned) %>%
    summarise(weighted_trips = sum(wt, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(weighted_trips))

  write_csv(
    nts_mode_map_check,
    file.path(tab_dir, "external_validation_NTS_mode_code_mapping_check.csv")
  )
  message("NTS mode code assignment (verify against the data dictionary):")
  print(as.data.frame(nts_mode_map_check))

  nts_trips_with_mode <- nts_trips_allweek %>%
    mutate(
      main_mode = factor(
        classify_nts_mode(.data[[nts_mode_var]]),
        levels = mode_levels
      )
    )

  # --------------------------------------------------------
  # 11D. Mode split: shares and significance
  # --------------------------------------------------------
  mode_share_rows <- list()
  mode_test_rows <- list()
  mode_overall_rows <- list()

  for (per in c("2014-2015", "2023")) {
    for (inc in c(FALSE, TRUE)) {
      levs <- if (inc) mode_levels else mode_levels_classified

      tus_p <- tus_trips_with_mode %>%
        filter(period == per, inc | main_mode != "Unspecified/other")
      nts_p <- nts_trips_with_mode %>%
        filter(period == per, inc | main_mode != "Unspecified/other")

      if (nrow(tus_p) == 0 || nrow(nts_p) == 0) next

      f_tus <- svy_share_cov(tus_p$main_mode, tus_p$wt, tus_p$mainid, levs)
      f_nts <- svy_share_cov(nts_p$main_mode, nts_p$wt, nts_p$IndividualID, levs)

      cmp <- compare_share_cov(f_tus, f_nts, levs)

      mode_test_rows[[length(mode_test_rows) + 1]] <-
        cmp$per_category %>%
        mutate(period = per,
               includes_unspecified = inc,
               split = "mode",
               .before = 1)

      mode_overall_rows[[length(mode_overall_rows) + 1]] <-
        cmp$overall %>%
        mutate(period = per,
               includes_unspecified = inc,
               split = "mode",
               n_clusters_TUS = f_tus$m,
               n_clusters_NTS = f_nts$m,
               n_trips_TUS = f_tus$n,
               n_trips_NTS = f_nts$n,
               .before = 1)

      if (!inc) {
        mode_share_rows[[length(mode_share_rows) + 1]] <- bind_rows(
          tibble::tibble(
            period = per, source = "TUS", category = factor(levs, levels = levs),
            share = as.numeric(f_tus$p), se = sqrt(pmax(0, diag(f_tus$V)))
          ),
          tibble::tibble(
            period = per, source = "NTS", category = factor(levs, levels = levs),
            share = as.numeric(f_nts$p), se = sqrt(pmax(0, diag(f_nts$V)))
          )
        )
      }
    }
  }

  mode_share_table <- bind_rows(mode_share_rows) %>%
    filter(is.finite(share)) %>%
    mutate(
      source = factor(source, levels = c("TUS", "NTS")),
      period = factor(period, levels = c("2014-2015", "2023")),
      ci_low = pmax(0, share - 1.96 * se),
      ci_high = pmin(1, share + 1.96 * se)
    )

  mode_tests <- bind_rows(mode_test_rows) %>%
    mutate(period = factor(period, levels = c("2014-2015", "2023")))
  mode_overall <- bind_rows(mode_overall_rows)

  write_csv(mode_share_table,
            file.path(tab_dir, "external_validation_TUS_NTS_mode_split_shares.csv"))
  write_csv(mode_tests,
            file.path(tab_dir, "external_validation_TUS_NTS_mode_split_tests.csv"))
  write_csv(mode_overall,
            file.path(tab_dir, "external_validation_TUS_NTS_mode_split_overall_wald.csv"))

  message("Mode split, classified trips only, overall Wald test:")
  print(mode_overall %>% filter(!includes_unspecified) %>%
          select(period, wald_stat, wald_df, wald_p))

  # ---- Figure: side-by-side mode shares with significance markers ----
  mode_sig_labels <- mode_tests %>%
    filter(!includes_unspecified) %>%
    select(period, category, sig)

  mode_label_y <- mode_share_table %>%
    group_by(period, category) %>%
    summarise(y = max(ci_high, na.rm = TRUE), .groups = "drop") %>%
    left_join(mode_sig_labels, by = c("period", "category")) %>%
    filter(is.finite(y), !is.na(sig))

  p_mode_split <- ggplot(
    mode_share_table,
    aes(x = category, y = share, fill = source)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      position = position_dodge(width = 0.8),
      width = 0.18, linewidth = 0.4, colour = "grey25"
    ) +
    geom_text(
      data = mode_label_y,
      aes(x = category, y = y + 0.03, label = sig),
      inherit.aes = FALSE, size = 5
    ) +
    facet_wrap(~period) +
    scale_fill_manual(
      values = section11_source_palette,
      labels = section11_source_labels
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.14))) +
    labs(
      title = "Mode split: time-use diaries against the National Travel Survey",
      subtitle = paste(
        "Share of trips by main mode, adults 18+, weighted.",
        "Unspecified modes excluded from both sources.",
        "Bars show 95% cluster-robust intervals;",
        "*** p<0.001, ** p<0.01, * p<0.05, ns not significant (BH-adjusted)."
      ),
      x = "Main mode",
      y = "Share of trips",
      fill = "Source"
    ) +
    presentation_theme +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

  ggsave(
    file.path(fig_out_dir, "EV_T7_TUS_vs_NTS_mode_split.png"),
    p_mode_split, width = 13, height = 6.2, dpi = 300
  )

  # ---- Figure: TUS minus NTS gap with confidence intervals ----
  p_mode_gap <- mode_tests %>%
    filter(!includes_unspecified) %>%
    ggplot(aes(x = category, y = 100 * diff)) +
    geom_hline(yintercept = 0, linewidth = 0.7, colour = "black") +
    geom_col(width = 0.62, fill = "grey40") +
    geom_errorbar(
      aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
      width = 0.18, linewidth = 0.45, colour = "grey15"
    ) +
    geom_text(
      aes(
        y = 100 * ci_high + ifelse(diff >= 0, 1.2, -1.2) * 1.0,
        label = sig
      ),
      size = 5
    ) +
    facet_wrap(~period) +
    coord_flip() +
    labs(
      title = "Where the diaries and the NTS disagree on mode",
      subtitle = paste0(
        "TUS share minus NTS share, percentage points\n",
        "95% cluster-robust intervals clustered on the individual"
      ),
      x = "Main mode",
      y = "TUS minus NTS (percentage points)"
    ) +
    presentation_theme

  ggsave(
    file.path(fig_out_dir, "EV_T7b_TUS_vs_NTS_mode_gap.png"),
    p_mode_gap, width = 12, height = 6, dpi = 300
  )

  # ---- Figure: unspecified-mode diagnostic ----
  p_mode_unspec <- tus_mode_episode_diagnostic %>%
    filter(mode5 == "Unspecified/other") %>%
    ggplot(aes(x = period, y = episode_share)) +
    geom_col(width = 0.5, fill = "grey45") +
    geom_text(aes(label = percent(episode_share, accuracy = 0.1)),
              vjust = -0.5, size = 5) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = "Travel episodes with no usable transport mode, time-use diaries",
      subtitle = paste(
        "The NTS has essentially none, because an interviewer-coded trip",
        "always carries a main mode. These episodes are excluded from the",
        "mode comparison."
      ),
      x = NULL, y = "Share of travel episodes"
    ) +
    presentation_theme

  ggsave(
    file.path(fig_out_dir, "EV_T7c_TUS_unspecified_mode_diagnostic.png"),
    p_mode_unspec, width = 9, height = 5.4, dpi = 300
  )

  message("Mode split outputs saved:")
  message("  EV_T7_TUS_vs_NTS_mode_split.png")
  message("  EV_T7b_TUS_vs_NTS_mode_gap.png")
  message("  EV_T7c_TUS_unspecified_mode_diagnostic.png")
}


# ------------------------------------------------------------
# 11E. Significance tests for the SIX-GROUP PURPOSE split
# ------------------------------------------------------------
# Same machinery applied to the Section 10 comparison, which until now
# reported the TUS-NTS purpose gaps without any indication of whether they
# exceed sampling error.

purpose_test_rows <- list()
purpose_overall_rows <- list()
purpose_share_rows <- list()

for (per in c("2014-2015", "2023")) {
  tus_p <- travel_by_purpose %>% filter(period == per)
  nts_p <- nts_travel_by_purpose %>% filter(period == per)

  if (nrow(tus_p) == 0 || nrow(nts_p) == 0) next

  f_tus <- svy_share_cov(
    tus_p$destination_purpose, tus_p$wt, tus_p$mainid, purpose_levels
  )
  f_nts <- svy_share_cov(
    nts_p$harmonised_purpose, nts_p$wt, nts_p$IndividualID, purpose_levels
  )

  cmp <- compare_share_cov(f_tus, f_nts, purpose_levels)

  purpose_test_rows[[length(purpose_test_rows) + 1]] <-
    cmp$per_category %>%
    mutate(period = per, split = "purpose", .before = 1)

  purpose_overall_rows[[length(purpose_overall_rows) + 1]] <-
    cmp$overall %>%
    mutate(period = per, split = "purpose",
           n_clusters_TUS = f_tus$m, n_clusters_NTS = f_nts$m,
           n_trips_TUS = f_tus$n, n_trips_NTS = f_nts$n,
           .before = 1)

  purpose_share_rows[[length(purpose_share_rows) + 1]] <- bind_rows(
    tibble::tibble(
      period = per, source = "TUS",
      category = factor(purpose_levels, levels = purpose_levels),
      share = as.numeric(f_tus$p), se = sqrt(pmax(0, diag(f_tus$V)))
    ),
    tibble::tibble(
      period = per, source = "NTS",
      category = factor(purpose_levels, levels = purpose_levels),
      share = as.numeric(f_nts$p), se = sqrt(pmax(0, diag(f_nts$V)))
    )
  )
}

purpose_tests <- bind_rows(purpose_test_rows) %>%
  mutate(period = factor(period, levels = c("2014-2015", "2023")))
purpose_overall <- bind_rows(purpose_overall_rows)
purpose_share_table <- bind_rows(purpose_share_rows) %>%
  filter(is.finite(share)) %>%
  mutate(
    source = factor(source, levels = c("TUS", "NTS")),
    period = factor(period, levels = c("2014-2015", "2023")),
    ci_low = pmax(0, share - 1.96 * se),
    ci_high = pmin(1, share + 1.96 * se)
  )

write_csv(purpose_tests,
          file.path(tab_dir, "external_validation_TUS_NTS_purpose_split_tests.csv"))
write_csv(purpose_overall,
          file.path(tab_dir, "external_validation_TUS_NTS_purpose_split_overall_wald.csv"))
write_csv(purpose_share_table,
          file.path(tab_dir, "external_validation_TUS_NTS_purpose_split_shares.csv"))

message("Purpose split, overall Wald test:")
print(purpose_overall %>% select(period, wald_stat, wald_df, wald_p))

purpose_sig_labels <- purpose_tests %>% select(period, category, sig)

purpose_label_y <- purpose_share_table %>%
  group_by(period, category) %>%
  summarise(y = max(ci_high, na.rm = TRUE), .groups = "drop") %>%
  left_join(purpose_sig_labels, by = c("period", "category")) %>%
  filter(is.finite(y), !is.na(sig))

p_purpose_split_sig <- ggplot(
  purpose_share_table,
  aes(x = category, y = share, fill = source)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.8),
    width = 0.18, linewidth = 0.4, colour = "grey25"
  ) +
  geom_text(
    data = purpose_label_y,
    aes(x = category, y = y + 0.03, label = sig),
    inherit.aes = FALSE, size = 5
  ) +
  facet_wrap(~period) +
  scale_fill_manual(
    values = section11_source_palette,
    labels = section11_source_labels
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Purpose split: time-use diaries against the National Travel Survey",
    subtitle = paste(
      "Share of trips by harmonised purpose group, adults 18+, weighted.",
      "Bars show 95% cluster-robust intervals;",
      "*** p<0.001, ** p<0.01, * p<0.05, ns not significant (BH-adjusted)."
    ),
    x = "Harmonised purpose group",
    y = "Share of trips",
    fill = "Source"
  ) +
  presentation_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  file.path(fig_out_dir, "EV_T8_TUS_vs_NTS_purpose_split_with_significance.png"),
  p_purpose_split_sig, width = 13, height = 6.2, dpi = 300
)

p_purpose_gap <- purpose_tests %>%
  ggplot(aes(x = category, y = 100 * diff)) +
  geom_hline(yintercept = 0, linewidth = 0.7, colour = "black") +
  geom_col(width = 0.62, fill = "grey40") +
  geom_errorbar(
    aes(ymin = 100 * ci_low, ymax = 100 * ci_high),
    width = 0.18, linewidth = 0.45, colour = "grey15"
  ) +
  geom_text(
    aes(y = 100 * ci_high + ifelse(diff >= 0, 1.2, -1.2) * 1.0, label = sig),
    size = 5
  ) +
  facet_wrap(~period) +
  coord_flip() +
  labs(
    title = "Where the diaries and the NTS disagree on purpose",
    subtitle = paste0(
      "TUS share minus NTS share, percentage points\n",
      "95% cluster-robust intervals clustered on the individual"
    ),
    x = "Harmonised purpose group",
    y = "TUS minus NTS (percentage points)"
  ) +
  presentation_theme

ggsave(
  file.path(fig_out_dir, "EV_T8b_TUS_vs_NTS_purpose_gap.png"),
  p_purpose_gap, width = 12, height = 6, dpi = 300
)


# ------------------------------------------------------------
# 11F. One combined significance table for the write-up
# ------------------------------------------------------------
validation_significance_summary <- bind_rows(
  purpose_tests %>%
    mutate(includes_unspecified = NA) %>%
    select(split, period, category, share_TUS, share_NTS, diff,
           se_diff, ci_low, ci_high, z, p_value, p_adj, sig),
  if (exists("mode_tests")) {
    mode_tests %>%
      filter(!includes_unspecified) %>%
      select(split, period, category, share_TUS, share_NTS, diff,
             se_diff, ci_low, ci_high, z, p_value, p_adj, sig)
  } else NULL
) %>%
  arrange(split, period, category)

write_csv(
  validation_significance_summary,
  file.path(tab_dir, "external_validation_TUS_NTS_significance_summary.csv")
)

message("\nSection 11 complete.")
message("  Tables: external_validation_TUS_NTS_significance_summary.csv and five others")
message(
  "Note: the tests compare COMPOSITION (shares of trips). Per-diary-day ",
  "levels in Section 10 rest on two different denominators and remain ",
  "descriptive."
)


# ============================================================
# 12. SIGNIFICANCE OF THE SEVEN-DAY TUS vs NTS LEVEL DIFFERENCES
#     (the three rows of the "common seven-day basis" table, adults 18+)
# ------------------------------------------------------------
# Until now the three headline level measures --
#     A. trips per day
#     B. minutes per trip   (mean duration, conditional on travelling)
#     C. minutes per day
# -- were reported as point estimates only, because the TUS and NTS
# denominators are constructed differently (TUS: all valid diary-days;
# NTS: weighted persons x 7). This block attaches a DESIGN-BASED standard
# error to each estimate and tests the TUS-NTS gap, clustering on the
# INDIVIDUAL so that repeated diary days and multiple trips from the same
# person are not treated as independent. Each estimate is a ratio of two
# weighted totals, so a cluster (ultimate-cluster) linearisation is used --
# the same inference philosophy as the mode/purpose share tests in Sec. 11.
#
# The point estimates reproduced here match the reconciliation table exactly;
# only the standard errors are new.
#
# IMPORTANT CAVEAT (unchanged from the note above): for trips/day and
# minutes/day the denominators differ by construction (diary-days vs
# persons x 7). The z-test answers "are these two published-style rates
# statistically distinguishable given sampling error under each survey's own
# construction"; it does NOT remove the fact that the day bases differ. The
# minutes-per-trip test is the cleanest like-for-like comparison, since both
# sides are a weighted mean over trips on identical units.
# ============================================================

message("\nSection 12: significance of the seven-day TUS vs NTS level differences...")

# ---- 12A. Ratio-of-totals estimator with a cluster-linearised variance ----
# R = sum(N) / sum(D). N and D are per-observation contributions that ALREADY
# include the survey weight (so N and D may use different weights internally,
# e.g. a trip weight in the numerator and a person weight in the denominator).
# Clustering is on `cluster`; each cluster contributes (sum N, sum D).
svy_ratio_cov <- function(N, D, cluster) {
  ok <- is.finite(N) & is.finite(D) & !is.na(cluster)
  N  <- as.numeric(N)[ok]
  D  <- as.numeric(D)[ok]
  cl <- as.character(cluster)[ok]

  if (length(N) == 0 || sum(D) == 0) {
    return(list(R = NA_real_, var = NA_real_, se = NA_real_, m = 0L, n = 0L))
  }

  TD <- sum(D)
  R  <- sum(N) / TD

  # per-cluster weighted totals
  Nc <- as.numeric(rowsum(N, cl, reorder = TRUE))
  Dc <- as.numeric(rowsum(D, cl, reorder = TRUE))

  # linearised residual of the ratio estimator; sum over all clusters is 0
  u  <- (Nc - R * Dc) / TD
  m  <- length(u)

  v  <- if (m > 1) (m / (m - 1)) * sum(u^2) else NA_real_
  list(R = R, var = v, se = sqrt(v), m = m, n = length(N))
}

# Two independent complex samples: difference, its SE, and a two-sided z-test.
compare_ratio_cov <- function(fit_tus, fit_nts) {
  d  <- fit_tus$R - fit_nts$R
  vd <- fit_tus$var + fit_nts$var
  se <- sqrt(vd)
  z  <- d / se
  p  <- 2 * stats::pnorm(-abs(z))
  tibble::tibble(
    TUS = fit_tus$R, NTS = fit_nts$R,
    se_TUS = fit_tus$se, se_NTS = fit_nts$se,
    diff = d, se_diff = se,
    ci_low = d - 1.96 * se, ci_high = d + 1.96 * se,
    z = z, p_value = p,
    n_clusters_TUS = fit_tus$m, n_clusters_NTS = fit_nts$m,
    n_obs_TUS = fit_tus$n, n_obs_NTS = fit_nts$n
  )
}

star_from_p <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "",
    p < 0.001  ~ "***",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    TRUE       ~ "ns"
  )
}

# ---- 12B. TUS diary-day universe WITH an individual id for clustering ----
# The merged primary files contain every diary-day (>= 1 primary spell per
# day), so a distinct list of person-days reproduces the TUS denominator and
# also carries the individual id (mainid) needed for clustering.
tus_day_universe <- bind_rows(df2014_primary_merged, df2023_primary_merged) %>%
  mutate(period = factor(period, levels = c("2014-2015", "2023"))) %>%
  group_by(period, person_day) %>%
  summarise(
    mainid = dplyr::first(mainid),
    wt     = dplyr::first(wt),
    .groups = "drop"
  ) %>%
  left_join(weekday_lookup, by = "person_day") %>%
  # robust fallback: if an individual id is missing, cluster on the diary-day
  mutate(cluster_id = dplyr::coalesce(as.character(mainid), person_day))

# travel spells per diary-day (count and total minutes)
tus_day_travel_counts <- travel_trips %>%
  group_by(period, person_day) %>%
  summarise(
    n_trips   = dplyr::n(),
    total_min = sum(dur_min, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 12C. Loop over day types (all-week is the headline seven-day table) ----
level_sig_rows <- list()

for (day_key in c("allweek", "weekday", "weekend")) {

  day_label <- dplyr::case_when(
    day_key == "allweek" ~ "All-week",
    day_key == "weekday" ~ "Weekday",
    day_key == "weekend" ~ "Weekend"
  )
  days_per_person <- dplyr::case_when(
    day_key == "allweek" ~ 7,
    day_key == "weekday" ~ 5,
    day_key == "weekend" ~ 2
  )

  # ---- TUS side ----
  tus_day <- switch(
    day_key,
    allweek = tus_day_universe,
    weekday = tus_day_universe %>% filter(is_weekday %in% TRUE),
    weekend = tus_day_universe %>% filter(is_weekday %in% FALSE)
  ) %>%
    left_join(tus_day_travel_counts, by = c("period", "person_day")) %>%
    mutate(
      n_trips   = dplyr::coalesce(n_trips, 0L),
      total_min = dplyr::coalesce(total_min, 0)
    )

  tus_trips_this <- switch(
    day_key,
    allweek = travel_trips_allweek,
    weekday = travel_trips_weekday,
    weekend = travel_trips_weekend
  ) %>%
    mutate(cluster_id = dplyr::coalesce(as.character(mainid), person_day))

  # ---- NTS side ----
  nts_trips_this <- switch(
    day_key,
    allweek = nts_trips_allweek,
    weekday = nts_trips_weekday,
    weekend = nts_trips_weekend
  )

  nts_person_travel <- nts_trips_this %>%
    group_by(period, IndividualID) %>%
    summarise(
      trips_w = sum(wt, na.rm = TRUE),
      min_w   = sum(wt * dur_min, na.rm = TRUE),
      .groups = "drop"
    )

  # every weighted adult contributes days_per_person recorded days, incl.
  # zero-travel persons (left join leaves them at trips_w = min_w = 0)
  nts_person <- nts_adult_persons %>%
    transmute(period, IndividualID, person_wt) %>%
    left_join(nts_person_travel, by = c("period", "IndividualID")) %>%
    mutate(
      trips_w = dplyr::coalesce(trips_w, 0),
      min_w   = dplyr::coalesce(min_w, 0),
      den     = person_wt * days_per_person
    )

  for (per in c("2014-2015", "2023")) {

    tus_d <- tus_day        %>% filter(period == per)
    tus_t <- tus_trips_this %>% filter(period == per)
    nts_p <- nts_person     %>% filter(period == per)
    nts_t <- nts_trips_this %>% filter(period == per)

    if (nrow(tus_d) == 0 || nrow(nts_p) == 0) next

    # A. trips per day  (ratio over recorded days / persons-days)
    fit_tus_trips <- svy_ratio_cov(tus_d$wt * tus_d$n_trips, tus_d$wt,  tus_d$cluster_id)
    fit_nts_trips <- svy_ratio_cov(nts_p$trips_w,            nts_p$den, nts_p$IndividualID)

    # B. minutes per trip  (weighted mean duration over trips -- like-for-like)
    fit_tus_mpt <- svy_ratio_cov(tus_t$wt * tus_t$dur_min, tus_t$wt, tus_t$cluster_id)
    fit_nts_mpt <- svy_ratio_cov(nts_t$wt * nts_t$dur_min, nts_t$wt, nts_t$IndividualID)

    # C. minutes per day
    fit_tus_mpd <- svy_ratio_cov(tus_d$wt * tus_d$total_min, tus_d$wt,  tus_d$cluster_id)
    fit_nts_mpd <- svy_ratio_cov(nts_p$min_w,                nts_p$den, nts_p$IndividualID)

    block <- bind_rows(
      compare_ratio_cov(fit_tus_trips, fit_nts_trips) %>% mutate(measure = "Trips per day"),
      compare_ratio_cov(fit_tus_mpt,   fit_nts_mpt)   %>% mutate(measure = "Minutes per trip"),
      compare_ratio_cov(fit_tus_mpd,   fit_nts_mpd)   %>% mutate(measure = "Minutes per day")
    ) %>%
      mutate(
        day_type = day_label,
        period   = per,
        # BH across the three measures within this period + day type
        p_adj = stats::p.adjust(p_value, method = "BH"),
        sig   = star_from_p(p_adj),
        .before = 1
      )

    level_sig_rows[[length(level_sig_rows) + 1]] <- block
  }
}

seven_day_level_tests <- bind_rows(level_sig_rows) %>%
  mutate(
    measure  = factor(measure,
                      levels = c("Trips per day", "Minutes per trip", "Minutes per day")),
    period   = factor(period, levels = c("2014-2015", "2023")),
    day_type = factor(day_type, levels = c("All-week", "Weekday", "Weekend"))
  ) %>%
  arrange(day_type, period, measure) %>%
  select(day_type, period, measure,
         TUS, NTS, se_TUS, se_NTS, diff, se_diff, ci_low, ci_high,
         z, p_value, p_adj, sig,
         n_clusters_TUS, n_clusters_NTS, n_obs_TUS, n_obs_NTS)

write_csv(
  seven_day_level_tests,
  file.path(tab_dir, "external_validation_TUS_NTS_seven_day_level_tests.csv")
)

message("Seven-day level tests (all-week, the common seven-day basis table):")
print(
  seven_day_level_tests %>%
    filter(day_type == "All-week") %>%
    mutate(
      TUS = round(TUS, 2), NTS = round(NTS, 2),
      diff = round(diff, 2), se_diff = round(se_diff, 3),
      p_value = signif(p_value, 3), p_adj = signif(p_adj, 3)
    ) %>%
    select(period, measure, TUS, NTS, diff, se_diff, z, p_value, p_adj, sig)
)

# ---- 12D. Sanity check: point estimates must match the reconciliation table ----
if (exists("reconciliation_seven_day_by_daytype")) {
  recon_check <- reconciliation_seven_day_by_daytype %>%
    filter(day_type == "All-week") %>%
    transmute(
      period = factor(period, levels = c("2014-2015", "2023")),
      recon_TUS_trips = tus_spells_per_day,
      recon_NTS_trips = nts_trips_per_day,
      recon_TUS_mpt   = tus_mean_duration_min,
      recon_NTS_mpt   = nts_mean_duration_min,
      recon_TUS_mpd   = tus_travel_min_per_day,
      recon_NTS_mpd   = nts_travel_min_per_day
    )
  message("Reconciliation cross-check (should match the estimates above):")
  print(recon_check)
}

# ---- 12E. Figure: all-week levels with 95% CIs and significance stars ----
level_plot_data <- seven_day_level_tests %>%
  filter(day_type == "All-week") %>%
  select(period, measure, TUS, NTS, se_TUS, se_NTS, sig) %>%
  pivot_longer(
    cols = c(TUS, NTS),
    names_to = "source", values_to = "value"
  ) %>%
  mutate(
    se = ifelse(source == "TUS", se_TUS, se_NTS),
    ci_low  = pmax(0, value - 1.96 * se),
    ci_high = value + 1.96 * se,
    source  = factor(source, levels = c("TUS", "NTS"))
  )

level_star_pos <- seven_day_level_tests %>%
  filter(day_type == "All-week") %>%
  transmute(
    period, measure, sig,
    y = pmax(TUS + 1.96 * se_TUS, NTS + 1.96 * se_NTS)
  )

p_level_sig <- ggplot(
  level_plot_data,
  aes(x = period, y = value, fill = source)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.8),
    width = 0.18, linewidth = 0.4, colour = "grey25"
  ) +
  geom_text(
    data = level_star_pos,
    aes(x = period, y = y * 1.04, label = sig),
    inherit.aes = FALSE, size = 5
  ) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_manual(values = c(TUS = "#7B61FF", NTS = "#F39C12")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "Seven-day travel totals: TUS vs NTS, adults 18+",
    subtitle = paste(
      "Bars show 95% cluster-robust intervals (clustered on the individual).",
      "*** p<0.001, ** p<0.01, * p<0.05, ns not significant (BH-adjusted).\n",
      "Trips/day and minutes/day use different day denominators by construction;",
      "minutes/trip is the like-for-like test."
    ),
    x = NULL, y = NULL, fill = "Source"
  ) +
  presentation_theme +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "EV_seven_day_level_TUS_vs_NTS_with_significance.png"),
  p_level_sig, width = 13, height = 5.6, dpi = 300
)

message("Section 12 complete.")
message("  Table : external_validation_TUS_NTS_seven_day_level_tests.csv")
message("  Figure: EV_seven_day_level_TUS_vs_NTS_with_significance.png")
