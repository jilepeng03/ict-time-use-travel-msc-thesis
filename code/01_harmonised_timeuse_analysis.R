# ============================================================
# Combined comparison script: UKTUS 2014-2015, ELIDDI/CTUR 2023, and NTS
# Purpose:
#   1) Compare time-use data step by step: activity, ICT, location x activity, hourly patterns
#   2) Standardise time-use comparisons using weighted average minutes per diary-day
#   3) Use NTS as a parallel travel context, not as direct ICT evidence
# Author: Jile Peng
#   HARMONISED VERSION: 2014-2015 activity codes mapped onto the 2023
#   (ELIDDI/CTUR) coding scheme at the source; see Section 3.0. Outputs go to
#   combined_outputs_harmonised/.
#   Update: 2023 codes 200-203 and 391 are assigned to Social & leisure;
#   the former Leisure group is renamed Social & leisure throughout outputs.
#
# ------------------------------------------------------------
# REVISION (harmonisation review) - what changed and where
# ------------------------------------------------------------
#   [0.15] New ANALYSIS CONFIGURATION block with four switches. Each one
#          defaults to the corrected specification and can be flipped back to
#          reproduce the previous results exactly.
#
#   (1) 2023 weight  oweight -> rweight
#       The recommended weight; it is 0 for the 44 `badcase` diaries, which
#       are now dropped rather than carried at positive weight.
#
#   (2) 2023 `inout` filter  applied -> not applied
#       It had no 2014-15 counterpart and removed 928 episodes (253 travel)
#       from one period only.
#
#   (3) ICT comparability  new
#       Two new episode-level fields, `any_ict_single_prompt` and
#       `ict_reported`, carried through merging and expansion; a new
#       section 4B quantifies the instrument difference between the periods
#       and exports person-day and per-activity ICT shares, both raw and
#       standardised within period, for the MDCEV stage.
#
#   (4) Merge rule  new switch
#       MERGE_REQUIRE_SAME_SECONDARY lets the adjacent-episode merge run on
#       primary activity and location alone, as a robustness check.
#
#   Expected effect on the 2023 headline (verified on the microdata):
#       travel minutes / diary-day   59.67 -> 61.75
#       merged trips / diary-day      1.731 -> 1.837
#   2014-15 is unaffected by (1) and (2) and stays at 82.30 minutes and
#   2.989 trips per diary-day, so the 2014 -> 2023 changes become
#   -38% trips and -25% travel minutes (previously -42% and -27%).
# ============================================================
# TITLE ALIGNMENT UPDATE: the titles and subtitles of the requested figures
# now align with the full plot boundary (plot.title.position = "plot").
# No data, labels, dimensions, colours, or other chart settings were changed.
# ============================================================

# Run this script from the repository root so relative paths resolve correctly.

# =========================
# 0. Packages
# =========================
library(haven)
library(dplyr)

# ggpattern needs ggplot2 >= 4.0.2.
# If ggplot2 is too old, update it before loading ggplot2/ggpattern.
if (!requireNamespace("ggplot2", quietly = TRUE) || packageVersion("ggplot2") < "4.0.2") {
  install.packages("ggplot2")
}
library(ggplot2)
library(forcats)
library(scales)
library(stringr)
library(tidyr)
library(readr)

# For hatched bars indicating non-significant results
# Install once if needed. If your R cannot install automatically, run:
# install.packages("ggpattern")
if (!requireNamespace("ggpattern", quietly = TRUE)) {
  install.packages("ggpattern")
}
library(ggpattern)

# Presentation-friendly theme: larger fonts for PPT
presentation_theme <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 14, hjust = 0, face = "italic", margin = margin(t = 8))
  )

# Hatched bars are used for non-significant results.
# Style is deliberately lighter for PPT: thin and dense stripes.
geom_col_sig <- function(position = "stack") {
  ggpattern::geom_col_pattern(
    position = position,
    colour = NA,
    pattern_fill = NA,
    pattern_colour = "grey25",
    pattern_density = 0.45,
    pattern_spacing = 0.015,
    pattern_size = 0.22,
    pattern_angle = 45,
    pattern_key_scale_factor = 0.55
  )
}

# Special helper for location-decomposition figures:
# significant segments = solid colour; non-significant segments = white background
# with thin coloured stripes matching the location colour.
plot_location_decomposition <- function(plot_data, metric_name) {
  pd <- plot_data %>%
    filter(metric == metric_name) %>%
    mutate(
      sig_flag = ifelse(sig %in% c("***", "**", "*"), "p < 0.05", "not significant"),
      activity = factor(activity, levels = activity_levels),
      location = label_locations(location),
      activity_num = as.numeric(activity)
    ) %>%
    arrange(activity, location) %>%
    group_by(activity) %>%
    mutate(
      pos_value = ifelse(change > 0, change, 0),
      neg_value = ifelse(change < 0, change, 0),
      xmax_pos = cumsum(pos_value),
      xmin_pos = xmax_pos - pos_value,
      xmin_neg = cumsum(neg_value),
      xmax_neg = xmin_neg - neg_value,
      xmin = ifelse(change >= 0, xmin_pos, xmin_neg),
      xmax = ifelse(change >= 0, xmax_pos, xmax_neg),
      ymin = activity_num - 0.38,
      ymax = activity_num + 0.38
    ) %>%
    ungroup()

  p <- ggplot(pd) +
    geom_vline(xintercept = 0, linewidth = 0.8, colour = "black") +
    geom_rect(
      data = pd %>% filter(sig_flag == "p < 0.05"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = location),
      colour = NA
    ) +
    ggpattern::geom_rect_pattern(
      data = pd %>% filter(sig_flag == "not significant"),
      aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        pattern_colour = location
      ),
      fill = "white",
      colour = NA,
      pattern = "stripe",
      pattern_fill = NA,
      pattern_angle = 45,
      pattern_density = 0.55,
      pattern_spacing = 0.012,
      pattern_size = 0.20,
      pattern_key_scale_factor = 0.55
    ) +
    scale_y_continuous(
      breaks = seq_along(activity_levels),
      labels = activity_levels,
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_fill_discrete(name = "Location") +
    scale_pattern_colour_discrete(name = "Not significant", guide = "none") +
    coord_cartesian(clip = "off") +
    labs(
      title = paste0("Location contribution to change in ", metric_name),
      subtitle = "2023 minus 2014-2015",
      x = "Change",
      y = "Activity purpose",
      fill = "Location",
      caption = paste0(
        "Significance coding: solid colour = p < 0.05; coloured stripes = ",
        "not significant.\nWeighted difference in means, cluster-robust on ",
        "the individual, Benjamini-Hochberg adjusted."
      )
    ) +
    presentation_theme +
    theme(
      legend.position = "right",
      plot.title.position = "plot"
    )

  return(p)
}

sig_pattern_scale <- scale_pattern_manual(
  values = c("p < 0.05" = "none", "not significant" = "stripe"),
  name = "Significance"
)

metric_axis_label <- function(metric_name) {
  case_when(
    metric_name == "Average minutes per diary-day" ~ "Change in minutes per diary-day",
    metric_name == "Average duration per episode" ~ "Change in minutes per episode",
    metric_name == "Episodes per active diary-day" ~ "Change in episodes per active diary-day",
    metric_name == "Participation rate" ~ "Change in participation rate (percentage points)",
    metric_name == "Time share" ~ "Change in time share (percentage points)",
    metric_name == "ICT intensity" ~ "Change in ICT share (percentage points)",
    metric_name == "ICT minutes per diary-day" ~ "Change in ICT minutes per diary-day",
    TRUE ~ "Change"
  )
}

# =========================
# 0.1 Output folders
# =========================
dir.create("combined_outputs_harmonised", showWarnings = FALSE)
dir.create("combined_outputs_harmonised/figures_timeuse", showWarnings = FALSE, recursive = TRUE)
dir.create("combined_outputs_harmonised/figures_nts", showWarnings = FALSE, recursive = TRUE)
dir.create("combined_outputs_harmonised/tables", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 0.15 ANALYSIS CONFIGURATION (added in the harmonisation review)
# ------------------------------------------------------------
# All four switches below default to the CORRECTED specification.
# Set any of them back to the old value to reproduce the previous results
# exactly, which makes each change auditable as a sensitivity check.
#
#  (1) WEIGHT_VAR_2023
#      The 2023 ELIDDI file carries two weights:
#        oweight = "Original non-response weight"
#        rweight = "Recommended weight"   <- the data producer's recommendation
#      rweight is exactly 0 for the 44 person-days flagged by `badcase`
#      (low quality diary), i.e. the recommended weight already removes them.
#      Using oweight keeps those 44 diaries in with positive weight, and they
#      are severely under-reported: 12.0 episodes/day vs 24.5, and 25.8
#      travel minutes/day vs 61.0. Effect of the switch on the headline:
#      travel minutes/day 60.5 -> 61.7, travel episodes/day 2.25 -> 2.34.
#      The two weights correlate only 0.67, so subgroup estimates and any
#      downstream MDCEV coefficients may move more than the headline does.
#
#  (2) APPLY_INOUT_FILTER_2023
#      The old code applied `filter(inout %in% c("Indoors","Outdoors"))` to
#      2023 ONLY. `inout` has no 2014-15 counterpart, so this was an
#      asymmetric, non-random deletion applied to just one period: it drops
#      928 episodes, 253 of them travel episodes, and lowers 2023 travel time
#      from 60.46 to 59.67 minutes/day - i.e. it cuts precisely the period the
#      analysis argues has declined. Default is now FALSE (no filter).
#
#  (3) ICT_UNREPORTED_2014_AS_NA
#      In 2014-15 the device question has an explicit "-9 not reported" code
#      covering 6.67% of episodes, which the old code silently recoded to
#      0 = "no ICT". 2023 has no missing device data at all. That asymmetry
#      biases 2014-15 ICT downwards by construction. FALSE keeps the old
#      0-coding for the main results (so `any_ict` never contains NA and all
#      existing downstream code is unaffected); TRUE runs the sensitivity in
#      which unreported episodes are treated as missing.
#      NOTE: `ict_reported` is written out either way, so the sensitivity can
#      also be run downstream without re-running this script.
#
#  (4) MERGE_REQUIRE_SAME_SECONDARY
#      The adjacent-episode merge requires the SECONDARY activity to match.
#      For travel this splits one door-to-door trip into two whenever the
#      traveller starts or stops a secondary activity mid-trip (e.g. begins
#      listening to music after boarding). It blocks 32.6% of adjacent
#      travel-travel pairs in 2014-15 and 28.8% in 2023.
#      Weighted travel units per diary-day:
#                                    2014-15   2023
#        unmerged episodes             4.009   2.249
#        merge on primary + secondary  2.989   1.774   <- current main spec
#        merge on primary only         2.497   1.575
#      For reference, Harms et al. (2018) report 2.61 episodes per trip chain;
#      the current spec yields only ~1.34, so the trip unit here is closer to
#      a stage than to a chain. TRUE keeps the published main specification;
#      set FALSE for the robustness run.
# ============================================================

WEIGHT_VAR_2023 <- "rweight"          # was "oweight"
APPLY_INOUT_FILTER_2023 <- FALSE      # was TRUE
ICT_UNREPORTED_2014_AS_NA <- FALSE    # sensitivity switch; FALSE = main spec
MERGE_REQUIRE_SAME_SECONDARY <- TRUE  # FALSE = primary-only merge robustness

message("\n--- Analysis configuration ---")
message("  2023 weight variable          : ", WEIGHT_VAR_2023)
message("  2023 inout filter applied     : ", APPLY_INOUT_FILTER_2023)
message("  2014-15 unreported ICT as NA  : ", ICT_UNREPORTED_2014_AS_NA)
message("  Merge requires same secondary : ", MERGE_REQUIRE_SAME_SECONDARY)
message("------------------------------\n")

# =========================
# 0.2 Common settings
# =========================
activity_levels <- c("work", "personal care", "maintenance", "shopping", "social & leisure", "travel", "other")
location_levels <- c("home", "work_or_school", "travel", "shopping_services", "restaurant_cafe_pub", "other")

# Display labels for figures. The underlying values keep their machine-readable
# form so that joins and CSV keys are unaffected; only the plotted factor is
# relabelled.
location_labels <- c(
  "Home",
  "Work or school",
  "Travelling",
  "Shops and services",
  "Restaurant, caf\u00e9 or pub",
  "Other"
)
names(location_labels) <- location_levels

label_locations <- function(x) {
  factor(as.character(x), levels = location_levels, labels = location_labels)
}

# Display labels for activity groups, so that figures read as sentence case
# rather than as the lower-case internal keys.
activity_labels <- c(
  "Work", "Personal care", "Maintenance", "Shopping",
  "Social & leisure", "Travel", "Other"
)
names(activity_labels) <- activity_levels

label_activities <- function(x) {
  factor(as.character(x), levels = activity_levels, labels = activity_labels)
}

# ============================================================
# 1. Helper functions
# ============================================================

safe_divide <- function(x, y) {
  ifelse(is.na(y) | y == 0, NA_real_, x / y)
}

# NA-safe max/min. Base max(x, na.rm = TRUE) returns -Inf (with a warning)
# when every value is NA, which can happen once ICT_UNREPORTED_2014_AS_NA is
# switched on. These return NA instead, so a merged spell made entirely of
# unreported episodes stays unreported rather than becoming -Inf.
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

# Expand each diary record to include both primary and secondary activities.
# The original row is retained as the primary activity episode.
# If the secondary activity is not "other", a duplicated episode is added with
# act_group2 set to the secondary activity group. All other fields remain the same.
# This allows the analysis to capture both primary and secondary activities.
add_secondary_activity_episodes <- function(data) {
  primary_rows <- data %>%
    mutate(
      activity_role = "primary",
      activity_name = pri
    )
  
  secondary_rows <- data %>%
    filter(
      !is.na(sec_group),
      sec_group != "other",
      sec_group != pri_group
    ) %>%
    mutate(
      activity_role = "secondary",
      act_group2 = sec_group,
      activity_name = sec
    )

  bind_rows(primary_rows, secondary_rows) %>%
    arrange(period, mainid, diaryord, start, activity_role)
}

# Split episodes across hours, so long episodes are correctly allocated
expand_episode_hours <- function(data) {
  data %>%
    mutate(
      episode_id = row_number(),
      start_min = as.numeric(start),
      end_min = start_min + as.numeric(time),
      start_hour = floor(start_min / 60),
      end_hour = floor((end_min - 1) / 60)
    ) %>%
    filter(!is.na(start_min), !is.na(end_min), !is.na(weighted_time)) %>%
    rowwise() %>%
    mutate(hour = list(start_hour:end_hour)) %>%
    unnest(hour) %>%
    ungroup() %>%
    mutate(
      hour_start = hour * 60,
      hour_end = (hour + 1) * 60,
      overlap_min = pmax(0, pmin(end_min, hour_end) - pmax(start_min, hour_start)),
      weighted_overlap_time = overlap_min * wt
    ) %>%
    filter(hour >= 0, hour <= 23, overlap_min > 0)
}

# ============================================================
# Formal preprocessing: merge adjacent episodes using harmonised 2023 codes
# ============================================================
# This is different from the later robustness check.
# It is applied to BOTH survey periods as part of the formal preprocessing,
# after 2014 codes have been harmonised to the 2023 code system and before
# primary/secondary activities are expanded for descriptive analysis.
#
# Two adjacent original diary episodes are merged only if:
#   1) they are consecutive in time;
#   2) harmonised primary activity code is the same;
#   3) harmonised secondary activity code is the same;
#   4) harmonised location category is the same.
#
# For the merge key only, all travel codes 110-117 are normalised to 110.
# Therefore, adjacent travel episodes are naturally handled by the same rule.

normalise_code_for_merge <- function(code) {
  case_when(
    code %in% 110:117 ~ 110L,
    is.na(code) ~ 999L,
    TRUE ~ as.integer(code)
  )
}

tag_harmonised_merge_spells <- function(data, tolerance_minutes = 1) {
  data %>%
    mutate(
      pri_merge_code = normalise_code_for_merge(pri_code_harmonised),
      sec_merge_code = normalise_code_for_merge(sec_code_harmonised),
      start_order = ifelse(start < 240, start + 1440, start),
      end_order = start_order + time
    ) %>%
    arrange(period, mainid, diaryord, start_order, original_episode_number) %>%
    group_by(period, mainid, diaryord) %>%
    mutate(
      consecutive_time =
        row_number() > 1 &
        !is.na(start_order) &
        !is.na(lag(end_order)) &
        abs(start_order - lag(end_order)) <= tolerance_minutes,

      same_harmonised_primary =
        pri_merge_code == lag(pri_merge_code),

      # Secondary-activity condition. When MERGE_REQUIRE_SAME_SECONDARY is
      # FALSE this is forced to TRUE, so adjacent episodes merge on primary
      # activity and location alone. See the note in section 0.15: for travel
      # this condition alone blocks about a third of adjacent travel pairs.
      same_harmonised_secondary =
        if (isTRUE(MERGE_REQUIRE_SAME_SECONDARY)) {
          sec_merge_code == lag(sec_merge_code)
        } else {
          row_number() > 1
        },

      same_harmonised_location =
        loc6 == lag(loc6),

      merge_with_previous =
        consecutive_time &
        same_harmonised_primary &
        same_harmonised_secondary &
        same_harmonised_location,

      new_spell = ifelse(row_number() == 1 | !merge_with_previous, 1L, 0L),
      harmonised_spell_id = cumsum(new_spell)
    ) %>%
    ungroup()
}

collapse_harmonised_merge_spells <- function(tagged_data) {
  tagged_data %>%
    group_by(period, mainid, diaryord, person_day, harmonised_spell_id) %>%
    summarise(
      start = first(start),
      start_order = first(start_order),
      end_order = max(end_order, na.rm = TRUE),
      time = sum(time, na.rm = TRUE),

      wt = first(wt),
      weighted_time = time * wt,

      pri_code_harmonised = first(pri_code_harmonised),
      sec_code_harmonised = first(sec_code_harmonised),
      pri_merge_code = first(pri_merge_code),
      sec_merge_code = first(sec_merge_code),

      pri = first(pri),
      sec = first(sec),
      loc = first(loc),
      loc6 = first(loc6),

      pri_group = first(pri_group),
      sec_group = first(sec_group),
      act_group2 = first(pri_group),

      any_ict = safe_max(any_ict),

      # ICT comparability fields (see section 0.15 and section 4B).
      # any_ict_single_prompt : 2023 restricted to the phone column so that it
      #                         mimics the single 2014-15 device question.
      # ict_reported          : 0 for 2014-15 episodes where the device
      #                         question was coded -9 "not reported".
      # A spell counts as reported only if every episode in it was reported.
      any_ict_single_prompt = safe_max(any_ict_single_prompt),
      ict_reported = safe_min(ict_reported),

      original_episode_number = paste(original_episode_number, collapse = "; "),
      n_original_episodes = n(),
      merged_any = n_original_episodes > 1,
      .groups = "drop"
    )
}

# Create one row per merged spell showing exactly which original episodes were combined.
build_merge_detail_table <- function(tagged_data) {
  tagged_data %>%
    group_by(period, mainid, diaryord, person_day, harmonised_spell_id) %>%
    summarise(
      merged_any = n() > 1,
      n_original_episodes = n(),

      spell_start = first(start),
      spell_end = max(start_order + time, na.rm = TRUE),
      merged_duration_minutes = sum(time, na.rm = TRUE),

      harmonised_primary_code = first(pri_merge_code),
      harmonised_primary_label = first(pri),
      harmonised_secondary_code = first(sec_merge_code),
      harmonised_secondary_label = first(sec),
      harmonised_location = first(loc6),

      original_episode_numbers = paste(original_episode_number, collapse = " -> "),
      original_start_times = paste(start, collapse = " -> "),
      original_durations = paste(time, collapse = " + "),
      original_primary_codes = paste(pri_code_harmonised, collapse = " -> "),
      original_primary_labels = paste(pri, collapse = " -> "),
      original_secondary_codes = paste(sec_code_harmonised, collapse = " -> "),
      original_secondary_labels = paste(sec, collapse = " -> "),
      original_locations = paste(loc, collapse = " -> "),

      wt = first(wt),
      .groups = "drop"
    ) %>%
    filter(merged_any)
}


# ============================================================
# Significance testing helpers
# ------------------------------------------------------------
# REVISED. The original tests were unweighted Welch two-sample t-tests at
# diary-day level. Three problems followed from that:
#
#   1. The plotted quantity is a WEIGHTED mean but the test was computed on
#      the UNWEIGHTED mean, so the bar and the star referred to different
#      estimands.
#   2. Diary days are clustered within people (2.00 days per respondent in
#      2014-15, 1.73 in 2023) and trips within days. Treating them as
#      independent understates the standard error and overstates
#      significance.
#   3. No adjustment was made for multiple comparisons, although the M1
#      panel alone runs 28 tests and the M1b panel 148.
#
# The replacement compares two weighted means as ratio estimators, using the
# ultimate-cluster (linearisation) variance with the INDIVIDUAL as the
# cluster, and applies a Benjamini-Hochberg correction within each output
# table. Stratification and finite-population corrections are ignored, which
# makes the intervals mildly conservative.
#
# Setting SIG_TEST_METHOD to "welch" restores the previous behaviour exactly,
# so the effect of the change can be audited.
#
# Caveat retained from the original design: `episodes_per_active_day` and
# `avg_duration_per_episode` are conditional on participation, and
# participation itself changed between the waves (travel -14.8 pp, shopping
# -13.4 pp). Those two metrics therefore compare different selected
# populations, which no change of test can repair; they are reported as
# descriptive.
# ============================================================

SIG_TEST_METHOD <- "design"   # "design" (weighted, cluster-robust) or "welch"

# Individual identifier recovered from person_day.
# 2023 person_day is "mainid_diaryord"; 2014-15 is "serial_pnum_daynum".
# Dropping the final component yields the person in both cases.
person_id_from_person_day <- function(person_day) {
  sub("_[^_]+$", "", as.character(person_day))
}

# Weighted mean with a cluster-linearised variance.
svy_wmean_lin <- function(y, w, cluster) {
  keep <- is.finite(y) & is.finite(w) & w > 0 & !is.na(cluster)
  y <- y[keep]; w <- w[keep]; cluster <- as.character(cluster)[keep]
  if (length(y) < 2L || length(unique(cluster)) < 2L) {
    return(c(est = NA_real_, var = NA_real_))
  }
  B <- sum(w)
  R <- sum(w * y) / B
  A_c <- tapply(w * y, cluster, sum)
  B_c <- tapply(w, cluster, sum)
  u <- (A_c - R * B_c) / B
  m <- length(u)
  c(est = R, var = (m / (m - 1)) * sum(u^2))
}

# Two-sided test of the 2023 minus 2014-15 difference in a weighted mean.
svy_diff_p <- function(value, wt, person_day, period) {
  cl <- person_id_from_person_day(person_day)
  i14 <- period == "2014-2015"
  i23 <- period == "2023"
  a <- svy_wmean_lin(value[i14], wt[i14], cl[i14])
  b <- svy_wmean_lin(value[i23], wt[i23], cl[i23])
  v <- a[["var"]] + b[["var"]]
  if (!is.finite(v) || v <= 0) return(NA_real_)
  z <- (b[["est"]] - a[["est"]]) / sqrt(v)
  if (!is.finite(z)) return(NA_real_)
  2 * stats::pnorm(-abs(z))
}

# Router used by every significance block below.
sig_p <- function(value, wt, person_day, period) {
  if (identical(SIG_TEST_METHOD, "welch")) {
    safe_t_test(value[period == "2014-2015"], value[period == "2023"])
  } else {
    svy_diff_p(value, wt, person_day, period)
  }
}

p_to_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ "ns"
  )
}

safe_t_test <- function(x_2014, x_2023) {
  x_2014 <- x_2014[is.finite(x_2014)]
  x_2023 <- x_2023[is.finite(x_2023)]
  if (length(unique(x_2014)) < 2 || length(unique(x_2023)) < 2) {
    return(NA_real_)
  }
  out <- tryCatch(
    t.test(x_2023, x_2014)$p.value,
    error = function(e) NA_real_
  )
  out
}

sig_label_position <- function(change_value) {
  ifelse(
    is.na(change_value),
    NA_real_,
    change_value + ifelse(change_value >= 0, 0.04, -0.04) * max(abs(change_value), 1)
  )
}


# ============================================================
# 2. Read and harmonise ELIDDI / CTUR 2023
# ============================================================

df2023_raw <- read_dta("data/eliddi_episode_long.dta")

# ------------------------------------------------------------------
# AGE ALIGNMENT (read this together with the 2014-15 block below).
# The 2023 CTUR survey sampled ADULTS 18+ ONLY, so no age filter is needed here.
# IMPORTANT: `age` in this file is BANDED (1 = 18-29, 2 = 30-39, ... 6 = 70+),
# NOT age in years. Do NOT write `filter(age >= 18)` on this variable: every
# valid band is already 18+, and `age >= 18` would treat the 1-6 codes as
# under-18 and silently drop the ENTIRE 2023 sample. Age alignment between the
# two periods is achieved solely by the DVAge >= 18 filter on 2014-15 below.
# ------------------------------------------------------------------


# Official CTUR/UKTUS 2023 activity-code based classification.
# This uses the numeric activity codes directly, instead of keyword matching.
# Primary activity is used for the main activity group; travel remains travel.
classify_activity_code_2023 <- function(code) {
  case_when(
    code %in% c(10, 20, 22, 30:32, 40, 90) ~ "personal care",
    code %in% c(50:53, 60:65, 70:73, 80:89,
                210:216, 220, 230:232, 240:245, 263) ~ "maintenance",
    code %in% c(110:117) ~ "travel",
    code %in% c(170:172, 180:183, 190:193, 250:252) ~ "work",
    code %in% c(260:262, 264) ~ "shopping",
    code %in% c(21, 100:102, 130, 200:203,
                270:272, 280:283, 290:299, 300, 310,
                320:325, 330:336, 340:342, 350:357,
                360:364, 370, 390:393) ~ "social & leisure",
    code %in% c(400, 999) | is.na(code) ~ "other",
    TRUE ~ "other"
  )
}

df2023_premerge <- df2023_raw %>%
  mutate(
    # Keep numeric official codes before converting labelled variables to factors
    pri_code = as.numeric(zap_labels(pri)),
    sec_code = as.numeric(zap_labels(sec)),
    
    pri = as_factor(pri),
    sec = as_factor(sec),
    loc = as_factor(loc),
    inout = as_factor(inout),
    computer = as_factor(computer),
    tablet = as_factor(tablet),
    phone = as_factor(phone)
  ) %>%
  filter(!is.na(pri), !is.na(loc)) %>%
  # ------------------------------------------------------------------
  # The `inout` filter is now OPTIONAL and off by default. It has no
  # 2014-15 counterpart, so applying it removed 928 episodes (253 of them
  # travel) from 2023 only. See section 0.15, switch (2).
  # ------------------------------------------------------------------
  { if (isTRUE(APPLY_INOUT_FILTER_2023)) {
      filter(., !is.na(inout), inout %in% c("Indoors", "Outdoors"))
    } else .
  } %>%
  mutate(
    period = "2023",
    mainid = as.character(mainid),
    diaryord = as.character(diaryord),
    person_day = paste(mainid, diaryord, sep = "_"),
    original_episode_number = as.character(epnum),
    pri_text = str_to_lower(as.character(pri)),
    time = as.numeric(time),
    start = (as.numeric(start) + 240) %% 1440,
    # ------------------------------------------------------------------
    # Weight: rweight ("Recommended weight") by default. See section 0.15,
    # switch (1). rweight is 0 for the 44 low-quality (`badcase`) diaries,
    # so selecting it also implements the data producer's exclusion rule.
    # ------------------------------------------------------------------
    wt_raw_2023 = as.numeric(.data[[WEIGHT_VAR_2023]]),
    wt = ifelse(is.na(wt_raw_2023), 1, wt_raw_2023),

    ict_phone = ifelse(phone == "Yes", 1, 0),
    ict_computer = ifelse(computer == "Yes", 1, 0),
    ict_tablet = ifelse(tablet == "Yes", 1, 0),
    any_ict = ifelse(ict_phone == 1 | ict_computer == 1 | ict_tablet == 1, 1, 0),

    # ------------------------------------------------------------------
    # ICT cross-period comparability (see section 0.15, switch (3)).
    # 2014-15 asks ONE device question; 2023 asks THREE separate columns
    # (computer / tablet / phone). Three prompts elicit more "yes" than one,
    # so the 11.5% -> 25.4% rise in the share of travel episodes with ICT
    # mixes real change with an instrument change. `any_ict_single_prompt`
    # keeps only the phone column so that 2023 can be re-estimated on a
    # one-question basis (travel-episode ICT share 25.4% -> 22.6%).
    # 2023 has no missing device data, so ict_reported is always 1 here.
    # ------------------------------------------------------------------
    any_ict_single_prompt = ict_phone,
    ict_reported = 1L,
    weighted_time = time * wt,
    
    # Official classification from primary and secondary activity codes
    pri_group = classify_activity_code_2023(pri_code),
    sec_group = classify_activity_code_2023(sec_code),
    
    # Primary activity remains the final activity for the original episode.
    # Secondary activities are added later as duplicated episodes if sec_group != "other".
    act_group2 = pri_group,

    # Travel context: if either primary or secondary activity is travel,
    # the episode location/context is coded as travel.
    travel_context = pri_group == "travel" | sec_group == "travel",

    loc_text = str_to_lower(as.character(loc)),
    loc6 = case_when(
      travel_context ~ "travel",
      loc_text %in% c("home", "other people's home") ~ "home",
      loc_text %in% c("work", "school/college") ~ "work_or_school",
      loc_text == "shops, etc." ~ "shopping_services",
      loc_text == "restaurant, cafe or pub" ~ "restaurant_cafe_pub",
      TRUE ~ "other"
    )
  ) %>%
  filter(!is.na(time), time > 0, !is.na(start)) %>%
  # ------------------------------------------------------------------
  # A zero weight is the data producer's way of excluding a case: rweight is
  # exactly 0 for the 44 person-days flagged by `badcase` (low quality
  # diary). Those diaries are dropped outright rather than carried at weight
  # zero, so that the UNWEIGHTED diary-day significance tests later in the
  # script use the same sample as the weighted descriptives.
  # With WEIGHT_VAR_2023 = "oweight" no case has weight 0 and nothing is
  # dropped, so the old behaviour is reproduced exactly.
  # ------------------------------------------------------------------
  filter(wt > 0) %>%
  mutate(
    pri_code_harmonised = pri_code,
    sec_code_harmonised = sec_code
  ) %>%
  select(
    period, mainid, diaryord, person_day, original_episode_number,
    start, time, wt, weighted_time,
    pri_code_harmonised, sec_code_harmonised,
    pri, sec, loc, pri_group, sec_group, act_group2, loc6, any_ict,
    any_ict_single_prompt, ict_reported
  )

message(sprintf(
  "2023 prepared with weight '%s': %s episodes, %s person-days retained%s.",
  WEIGHT_VAR_2023,
  nrow(df2023_premerge),
  dplyr::n_distinct(df2023_premerge$person_day),
  if (identical(WEIGHT_VAR_2023, "rweight")) {
    " (zero-weight `badcase` diaries removed)"
  } else {
    ""
  }
))

# ============================================================
# 3. Read and harmonise UKTUS 2014-2015
# ============================================================

df2014_raw <- read_dta("data/uktus15_diary_ep_long.dta") %>%
  # ------------------------------------------------------------------
  # AGE ALIGNMENT: restrict 2014-15 to adults 18+ to match the 2023 CTUR
  # sample (which is 18+ by design). Applied at the raw-read stage so EVERY
  # downstream object (premerge, merge spells, timeuse_all, per-diary-day
  # denominators, all figures, and the crosswalk audit) inherits the same
  # 18+ restriction.
  # DVAge is ACTUAL AGE IN YEARS (SCALE); -7 = "interview not achieved" and
  # any missing value are both dropped by the >= comparison. Without this
  # filter, 2014-15 includes children/teens (the survey covers 8+, and the
  # file contains child diary versions), while 2023 does not -- this inflates
  # 2014-15 trip counts / travel time and biases every cross-period comparison.
  # The code is intentionally asymmetric (filter here, none for 2023) BECAUSE
  # the two surveys code age differently; the RESULT is symmetric (both 18+).
  # ------------------------------------------------------------------
  filter(as.numeric(DVAge) >= 18)

message(sprintf(
  "2014-15 restricted to adults 18+: %s episodes, %s person-days retained.",
  nrow(df2014_raw),
  dplyr::n_distinct(paste(df2014_raw$serial, df2014_raw$pnum, df2014_raw$daynum))
))

# Official UKTUS 2014-2015 activity-code based classification.
# This follows the numeric hierarchy in the official whatdoing activity codes.
# Travel-related activities in the 9000s remain "travel" in the main official version.
classify_activity_code_2014 <- function(code) {
  case_when(
    code >= 0    & code < 1000 ~ "personal care",
    code >= 1000 & code < 2000 ~ "work",
    code >= 2000 & code < 3000 ~ "work",
    code %in% c(3430, 3440) ~ "social & leisure",
    code >= 3000 & code < 3600 ~ "maintenance",
    code >= 3000 & code < 3600 ~ "maintenance",
    code >= 3600 & code < 3700 ~ "shopping",
    code >= 3700 & code < 4000 ~ "maintenance",
    # volunteer / organisational work
    code >= 4000 & code < 4200 ~ "work",
    
    # informal help to other households
    code >= 4200 & code < 4300 ~ "maintenance",
    code >= 4300 & code < 4400 ~ "social & leisure",
    code >= 4400 & code < 5000 ~ "work",
    code >= 5000 & code < 9000 ~ "social & leisure",
    code >= 9000 & code < 9900 ~ "travel",
    code >= 9900 | is.na(code) ~ "other",
    TRUE ~ "other"
  )
}

# ============================================================
# 3.0 HARMONISE UKTUS 2014-2015 ONTO THE 2023 (ELIDDI/CTUR) CODING SCHEME
# ------------------------------------------------------------
# Supervisor request: harmonise the two surveys "at the source".
# Both surveys follow the HETUS time-use activity classification, but
# 2014-2015 uses finer 4-digit codes (278 categories) while 2023 uses a
# condensed scheme (139 categories). Each 2014-2015 activity code is mapped
# onto the closest 2023 code below; the 2023 code system is then used for
# BOTH periods (activity label AND 7-group classification via
# classify_activity_code_2023). This removes coding-granularity differences
# between the two periods.
#
# IMPORTANT NOTES (please review):
#  * 2014-2015 travel is coded by PURPOSE (e.g. travel to work) while 2023
#    travel is coded by MODE (walk, car, public transport). Travel detail
#    cannot be recovered, so all 2014-2015 travel is mapped to 110
#    "Travelling (unspecified)". Travel remains the "travel" group.
#  * Adopting the 2023 groupings changes the GROUP of 14 codes relative to
#    the earlier manual 2014 classification (see activity_crosswalk where
#    group_changed == TRUE), e.g. internet shopping moves maintenance ->
#    shopping; meetings move leisure -> work; religious activities stay in
#    leisure; handicraft moves maintenance -> leisure; caring for pets moves
#    leisure -> maintenance. This supersedes the earlier manual 4000-4999
#    split with the 2023 official groupings.
#  * This crosswalk is a transparent, reviewable draft. Check it against the
#    UKTUS and ELIDDI codebooks and edit any row you disagree with.
# ============================================================

activity_crosswalk <- tibble::tribble(
  ~code_2014, ~label_2014, ~code_2023, ~label_2023, ~group_2014_old, ~group_2023_new,
  -1L, "Not applicable", 999L, "Missing", "other", "other",
  0L, "Unspecified personal care", 30L, "Washing, dressing, personal care (unspecified)", "personal care", "personal care",
  110L, "Sleep", 10L, "Sleeping", "personal care", "personal care",
  111L, "Sleep: In bed not asleep", 20L, "Resting (unspecified)", "personal care", "personal care",
  120L, "Sleep: Sick in bed", 22L, "Sick in bed", "personal care", "personal care",
  210L, "Eating", 40L, "Eating, drinking, meal, at home or work", "personal care", "personal care",
  300L, "Other personal care: Unspecified other personal care", 32L, "Other personal care", "personal care", "personal care",
  310L, "Other personal care: Wash and dress", 31L, "Washing and dressing", "personal care", "personal care",
  390L, "Other personal care: Other specified personal care", 32L, "Other personal care", "personal care", "personal care",
  1000L, "Unspecified employment", 170L, "Paid work including at home (unspecified)", "work", "work",
  1100L, "Main job: unspecified main job", 170L, "Paid work including at home (unspecified)", "work", "work",
  1110L, "Main job: Working time in main job", 171L, "Paid job (incl short breaks, travel at or for work)", "work", "work",
  1120L, "Main job: Coffee and other breaks in main job", 250L, "Work/study break (unspecified)", "work", "work",
  1210L, "Second job: Working time in second job", 171L, "Paid job (incl short breaks, travel at or for work)", "work", "work",
  1220L, "Second job: Coffee and other breaks in second job", 250L, "Work/study break (unspecified)", "work", "work",
  1300L, "Activities related to employment: Unspecified activities related to employment", 172L, "Other activities related to employment", "work", "work",
  1310L, "Activities related to employment: Lunch break", 251L, "Lunch break in main and second jobs", "work", "work",
  1390L, "Activities related to employment: Other specified activities related to employment", 172L, "Other activities related to employment", "work", "work",
  1391L, "Activities related to employment: Activities related to job seeking", 172L, "Other activities related to employment", "work", "work",
  1399L, "Activities related to employment: Other unspecified activities related to employment", 172L, "Other activities related to employment", "work", "work",
  2000L, "Study: Unspecified study school or university", 180L, "Formal education (unspecified)", "work", "work",
  2100L, "Study: Unspecified activities related to school or university", 180L, "Formal education (unspecified)", "work", "work",
  2110L, "Study: Classes and lectures", 181L, "Classes and lectures", "work", "work",
  2120L, "Study: Homework", 182L, "Homework", "work", "work",
  2190L, "Study: other specified activities related to school or university", 193L, "Other activities related to study", "work", "work",
  2210L, "Free time study", 192L, "Free time study", "work", "work",
  3000L, "Unspecified household and family care", 263L, "Household management", "maintenance", "maintenance",
  3100L, "Unspecified food management", 50L, "Preparing food, cooking, washing up (unspecified)", "maintenance", "maintenance",
  3110L, "Food preparation and baking", 51L, "Food preparation and baking", "maintenance", "maintenance",
  3130L, "Dish washing", 52L, "Dish clearing, washing", "maintenance", "maintenance",
  3140L, "Preserving", 53L, "Storing, arranging, preserving food stocks", "maintenance", "maintenance",
  3190L, "Other specified food management", 50L, "Preparing food, cooking, washing up (unspecified)", "maintenance", "maintenance",
  3200L, "Unspecified household upkeep", 60L, "Cleaning and tidying the house (unspecified)", "maintenance", "maintenance",
  3210L, "Cleaning dwelling", 61L, "Cleaning dwelling", "maintenance", "maintenance",
  3220L, "Cleaning yard", 62L, "Cleaning garden, yard", "maintenance", "maintenance",
  3230L, "Heating and water", 84L, "Heating dwelling and water", "maintenance", "maintenance",
  3240L, "Arranging household goods and materials", 63L, "Arranging household goods and materials", "maintenance", "maintenance",
  3250L, "Disposal of waste", 64L, "Recycling and disposal of waste", "maintenance", "maintenance",
  3290L, "Other or unspecified household upkeep", 65L, "Other household upkeep", "maintenance", "maintenance",
  3300L, "Unspecified making and care for textiles", 70L, "Clothes washing, mending, sewing (unspecified)", "maintenance", "maintenance",
  3310L, "Laundry", 71L, "Laundry", "maintenance", "maintenance",
  3320L, "Ironing", 72L, "Ironing", "maintenance", "maintenance",
  3330L, "Handicraft and producing textiles", 363L, "Making handicraft products", "maintenance", "social & leisure",
  3390L, "Other specified making and care for textiles", 73L, "Other textile care, eg mending", "maintenance", "maintenance",
  3410L, "Gardening", 81L, "Gardening", "maintenance", "maintenance",
  3420L, "Tending domestic animals", 82L, "Tending domestic animals", "maintenance", "maintenance",
  3430L, "Caring for pets", 83L, "Caring for pets", "social & leisure", "maintenance",
  3440L, "Walking the dog", 310L, "Walking/dog walking", "social & leisure", "social & leisure",
  3490L, "Other specified gardening and pet care", 80L, "Maintenance of house, diy, gardening, pet care (unspecified)", "maintenance", "maintenance",
  3500L, "Unspecified construction and repairs", 89L, "Other construction and repairs", "maintenance", "maintenance",
  3510L, "House construction and renovation", 85L, "House construction and renovation", "maintenance", "maintenance",
  3520L, "Repairs of dwelling", 86L, "Repairs to dwelling", "maintenance", "maintenance",
  3530L, "Making repairing and maintaining equipment", 87L, "Making, repairing and maintaining equipment", "maintenance", "maintenance",
  3531L, "Woodcraft metalcraft sculpture and pottery", 87L, "Making, repairing and maintaining equipment", "maintenance", "maintenance",
  3539L, "Other specified making repairing and maintaining equipment", 87L, "Making, repairing and maintaining equipment", "maintenance", "maintenance",
  3540L, "Vehicle maintenance", 88L, "Vehicle maintenance", "maintenance", "maintenance",
  3590L, "Other specified construction and repairs", 89L, "Other construction and repairs", "maintenance", "maintenance",
  3600L, "Unspecified shopping and services", 260L, "Shopping, bank etc including online (unspecified)", "shopping", "shopping",
  3610L, "Unspecified shopping", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3611L, "Shopping mainly for food", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3612L, "Shopping mainly for clothing", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3613L, "Shopping mainly related to accommodation", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3614L, "Shopping or browsing at car boot sales or antique fairs", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3615L, "Window shopping or other shopping as leisure", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3619L, "Other specified shopping", 261L, "Shopping (including online/ e-shopping)", "shopping", "shopping",
  3620L, "Commercial and administrative services", 262L, "Commercial and administrative services", "shopping", "shopping",
  3630L, "Personal services", 90L, "Services eg. doctor/dentist/hairdresser", "shopping", "personal care",
  3690L, "Other specified shopping and services", 264L, "Other shopping and services", "shopping", "shopping",
  3710L, "Household management not using the internet", 263L, "Household management", "maintenance", "maintenance",
  3713L, "Shopping for and ordering clothing via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3720L, "Unspecified household management using the internet", 263L, "Household management", "maintenance", "maintenance",
  3721L, "Shopping for and ordering unspecified goods and services via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3722L, "Shopping for and ordering food via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3724L, "Shopping for and ordering goods and services related to accommodation via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3725L, "Shopping for and ordering mass media via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3726L, "Shopping for and ordering entertainment via the internet", 261L, "Shopping (including online/ e-shopping)", "maintenance", "shopping",
  3727L, "Banking and bill paying via the internet", 262L, "Commercial and administrative services", "maintenance", "maintenance",
  3729L, "Other specified household management using the internet", 263L, "Household management", "maintenance", "maintenance",
  3800L, "Unspecified childcare", 210L, "Caring for own children (unspecified)", "maintenance", "maintenance",
  3810L, "Unspecified physical care & supervision of a child", 211L, "Physical care and supervision of child", "maintenance", "maintenance",
  3811L, "Feeding the child", 211L, "Physical care and supervision of child", "maintenance", "maintenance",
  3819L, "Other and unspecified physical care & supervision of a child", 211L, "Physical care and supervision of child", "maintenance", "maintenance",
  3820L, "Teaching the child", 212L, "Teaching child, homework", "maintenance", "maintenance",
  3830L, "Reading playing and talking with child", 213L, "Reading, playing and talking with child", "maintenance", "maintenance",
  3840L, "Accompanying child", 214L, "Accompanying child", "maintenance", "maintenance",
  3890L, "Other or unspecified childcare", 215L, "Other childcare", "maintenance", "maintenance",
  3910L, "Unspecified help to a non-dependent eg injured adult household member", 230L, "Help, caring for adult household members (unspecified)", "maintenance", "maintenance",
  3911L, "Physical care of a non-dependent e.g. injured adult household member", 231L, "Physical care of an adult household member", "maintenance", "maintenance",
  3914L, "Accompanying a non-dependent adult household member e.g. to hospital", 232L, "Other support to an adult household member", "maintenance", "maintenance",
  3919L, "Other specified help to a non-dependent adult household member", 232L, "Other support to an adult household member", "maintenance", "maintenance",
  3920L, "Unspecified help to a dependent adult household member", 230L, "Help, caring for adult household members (unspecified)", "maintenance", "maintenance",
  3921L, "Physical care of a dependent adult household member e.g. Alzheimic parent", 231L, "Physical care of an adult household member", "maintenance", "maintenance",
  3924L, "Accompanying a dependent adult household member e.g. Alzheimic", 232L, "Other support to an adult household member", "maintenance", "maintenance",
  3929L, "Other specified help to a dependent adult household member", 232L, "Other support to an adult household member", "maintenance", "maintenance",
  4000L, "Unspecified volunteer work and meetings", 200L, "Voluntary work for club, organisation (unspecified)", "work", "work",
  4100L, "Unspecified organisational work", 201L, "Organisational work (work for or through an organisation)", "work", "work",
  4110L, "Work for an organisation", 201L, "Organisational work (work for or through an organisation)", "work", "work",
  4120L, "Volunteer work through an organisation", 201L, "Organisational work (work for or through an organisation)", "work", "work",
  4190L, "Other specified organisational work", 201L, "Organisational work (work for or through an organisation)", "work", "work",
  4200L, "Unspecified informal help to other households", 240L, "Unpaid help, caring, for adults not in your household (unspecified)", "maintenance", "maintenance",
  4210L, "Food management as help to other households", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4220L, "Household upkeep as help to other households", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4230L, "Gardening and pet care as help to other households", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4240L, "Construction and repairs as help to other households", 243L, "Construction and repairs as help", "maintenance", "maintenance",
  4250L, "Shopping and services as help to other households", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4260L, "Help to other households in employment and farming", 244L, "Help in employment and farming", "maintenance", "maintenance",
  4270L, "Unspecified childcare as help to other households", 220L, "Caring for other children", "maintenance", "maintenance",
  4271L, "Physical care and supervision of child as help to other household", 220L, "Caring for other children", "maintenance", "maintenance",
  4272L, "Teaching non-coresident child", 220L, "Caring for other children", "maintenance", "maintenance",
  4273L, "Reading playing & talking to non-coresident child", 220L, "Caring for other children", "maintenance", "maintenance",
  4274L, "Accompanying non-coresident child", 220L, "Caring for other children", "maintenance", "maintenance",
  4275L, "Physical care and supervision of own child as help to other household", 216L, "Care of own children living in another household", "maintenance", "maintenance",
  4277L, "Reading playing & talking to own non-coresident child", 216L, "Care of own children living in another household", "maintenance", "maintenance",
  4278L, "Accompanying own non-coresident child", 216L, "Care of own children living in another household", "maintenance", "maintenance",
  4279L, "Other specified childcare as help to other household", 220L, "Caring for other children", "maintenance", "maintenance",
  4280L, "Unspecified help to an adult of another household", 240L, "Unpaid help, caring, for adults not in your household (unspecified)", "maintenance", "maintenance",
  4281L, "Physical care and supervision of an adult as help to another household", 241L, "Help to an adult person of another household", "maintenance", "maintenance",
  4282L, "Accompanying an adult as help to another household", 241L, "Help to an adult person of another household", "maintenance", "maintenance",
  4283L, "Other specified help to an adult member of another household", 241L, "Help to an adult person of another household", "maintenance", "maintenance",
  4289L, "Other specified informal help to another household", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4290L, "Other specified informal help", 245L, "Other informal help to another household", "maintenance", "maintenance",
  4300L, "Unspecified participatory activities", 203L, "Other participatory activities", "social & leisure", "social & leisure",
  4310L, "Meetings", 202L, "Meetings, clubs", "social & leisure", "social & leisure",
  4320L, "Religious activities", 101L, "Religious activities", "social & leisure", "social & leisure",
  4390L, "Other specified participatory activities", 203L, "Other participatory activities", "social & leisure", "social & leisure",
  5000L, "Unspecified social life and entertainment", 330L, "Spending time with friends, family (unspecified)", "social & leisure", "social & leisure",
  5100L, "Unspecified social life", 330L, "Spending time with friends, family (unspecified)", "social & leisure", "social & leisure",
  5110L, "Socialising with family", 331L, "Socialising with family", "social & leisure", "social & leisure",
  5120L, "Visiting and receiving visitors", 332L, "Visiting and receiving visitors", "social & leisure", "social & leisure",
  5130L, "Celebrations", 333L, "Celebrations", "social & leisure", "social & leisure",
  5140L, "Telephone conversation", 335L, "Phone and video conversations", "social & leisure", "social & leisure",
  5190L, "Other specified social life", 336L, "Other social life", "social & leisure", "social & leisure",
  5200L, "Unspecified entertainment and culture", 350L, "Cinema, theatre, sports, cultural event (unspecified)", "social & leisure", "social & leisure",
  5210L, "Cinema", 351L, "Cinema", "social & leisure", "social & leisure",
  5220L, "Unspecified theatre or concerts", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5221L, "Plays musicals or pantomimes", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5222L, "Opera operetta or light opera", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5223L, "Concerts or other performances of classical music", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5224L, "Live music other than classical concerts opera and musicals", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5225L, "Dance performances", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5229L, "Other specified theatre or concerts", 352L, "Theatre and concerts", "social & leisure", "social & leisure",
  5230L, "Art exhibitions and museums", 353L, "Art exhibitions, cultural sites, museums", "social & leisure", "social & leisure",
  5240L, "Unspecified library", 354L, "Library", "social & leisure", "social & leisure",
  5241L, "Borrowing books records audiotapes videotapes CDs VDs etc. from a library", 354L, "Library", "social & leisure", "social & leisure",
  5242L, "Reference to books and other library materials within a library", 354L, "Library", "social & leisure", "social & leisure",
  5243L, "Using internet in the library", 354L, "Library", "social & leisure", "social & leisure",
  5244L, "Using computers in the library other than internet use", 354L, "Library", "social & leisure", "social & leisure",
  5245L, "Reading newspapers in a library", 354L, "Library", "social & leisure", "social & leisure",
  5249L, "Other specified library activities", 354L, "Library", "social & leisure", "social & leisure",
  5250L, "Sports events", 355L, "Attending live sports events", "social & leisure", "social & leisure",
  5290L, "Other unspecified entertainment and culture", 357L, "Other entertainment and culture", "social & leisure", "social & leisure",
  5291L, "Visiting a historical site", 353L, "Art exhibitions, cultural sites, museums", "social & leisure", "social & leisure",
  5292L, "Visiting a wildlife site", 356L, "Zoos, botanical gardens, natural reserves, etc", "social & leisure", "social & leisure",
  5293L, "Visiting a botanical site", 356L, "Zoos, botanical gardens, natural reserves, etc", "social & leisure", "social & leisure",
  5294L, "Visiting a leisure park", 357L, "Other entertainment and culture", "social & leisure", "social & leisure",
  5295L, "Visiting an urban park playground designated play area", 357L, "Other entertainment and culture", "social & leisure", "social & leisure",
  5299L, "Other or unspecified entertainment or culture", 357L, "Other entertainment and culture", "social & leisure", "social & leisure",
  5310L, "Resting - Time out", 21L, "Resting - Time out", "social & leisure", "social & leisure",
  6000L, "Unspecified sports and outdoor activities", 290L, "Playing sports, exercise (unspecified)", "social & leisure", "social & leisure",
  6100L, "Unspecified physical exercise", 290L, "Playing sports, exercise (unspecified)", "social & leisure", "social & leisure",
  6110L, "Walking and hiking", 291L, "Walking and hiking", "social & leisure", "social & leisure",
  6111L, "Taking a walk or hike that lasts at least miles or 1 hour", 291L, "Walking and hiking", "social & leisure", "social & leisure",
  6119L, "Other walk or hike", 291L, "Walking and hiking", "social & leisure", "social & leisure",
  6120L, "Jogging and running", 292L, "Jogging and running", "social & leisure", "social & leisure",
  6130L, "Biking skiing and skating", 293L, "Cycling, skiing and skating", "social & leisure", "social & leisure",
  6131L, "Biking", 293L, "Cycling, skiing and skating", "social & leisure", "social & leisure",
  6132L, "Skiing or skating", 293L, "Cycling, skiing and skating", "social & leisure", "social & leisure",
  6140L, "Unspecified ball games", 294L, "Ball games", "social & leisure", "social & leisure",
  6141L, "Indoor pairs or doubles games", 294L, "Ball games", "social & leisure", "social & leisure",
  6142L, "Indoor team games", 294L, "Ball games", "social & leisure", "social & leisure",
  6143L, "Outdoor pairs or doubles games", 294L, "Ball games", "social & leisure", "social & leisure",
  6144L, "Outdoor team games", 294L, "Ball games", "social & leisure", "social & leisure",
  6149L, "Other specified ball games", 294L, "Ball games", "social & leisure", "social & leisure",
  6150L, "Gymnastics", 295L, "Gymnastics and fitness", "social & leisure", "social & leisure",
  6160L, "Fitness", 295L, "Gymnastics and fitness", "social & leisure", "social & leisure",
  6170L, "Unspecified water sports", 296L, "Water sports", "social & leisure", "social & leisure",
  6171L, "Swimming", 296L, "Water sports", "social & leisure", "social & leisure",
  6179L, "Other specified water sports", 296L, "Water sports", "social & leisure", "social & leisure",
  6190L, "Other specified physical exercise", 299L, "Other sports or outdoor activities", "social & leisure", "social & leisure",
  6200L, "Unspecified productive exercise", 297L, "Productive exercise: hunt, fish, pick berries, mushrooms, herbs", "social & leisure", "social & leisure",
  6210L, "Hunting and fishing", 297L, "Productive exercise: hunt, fish, pick berries, mushrooms, herbs", "social & leisure", "social & leisure",
  6220L, "Picking berries mushroom and herbs", 297L, "Productive exercise: hunt, fish, pick berries, mushrooms, herbs", "social & leisure", "social & leisure",
  6290L, "Other specified productive exercise", 297L, "Productive exercise: hunt, fish, pick berries, mushrooms, herbs", "social & leisure", "social & leisure",
  6310L, "Unspecified sports related activities", 298L, "Sports related activities", "social & leisure", "social & leisure",
  6311L, "Activities related to sports", 298L, "Sports related activities", "social & leisure", "social & leisure",
  6312L, "Activities related to productive exercise", 298L, "Sports related activities", "social & leisure", "social & leisure",
  7000L, "Unspecified hobbies games and computing", 360L, "Hobbies (unspecified)", "social & leisure", "social & leisure",
  7100L, "Unspecified arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7110L, "Unspecified visual arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7111L, "Painting drawing or other graphic arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7112L, "Making videos taking photographs or related photographic activities", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7119L, "Other specified visual arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7120L, "Unspecified performing arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7121L, "Singing or other musical activities", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7129L, "Other specified performing arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7130L, "Literary arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7140L, "Other specified arts", 361L, "Arts (visual, performing, literary)", "social & leisure", "social & leisure",
  7150L, "Unspecified hobbies", 360L, "Hobbies (unspecified)", "social & leisure", "social & leisure",
  7160L, "Collecting", 362L, "Collecting", "social & leisure", "social & leisure",
  7170L, "Correspondence", 341L, "Communication by text (SMS, instant messages, email, etc.)", "social & leisure", "social & leisure",
  7190L, "Other specified or unspecified arts and hobbies", 364L, "Other hobbies", "social & leisure", "social & leisure",
  7220L, "Computing - programming", 391L, "Computer programming, learning software", "social & leisure", "social & leisure",
  7230L, "Unspecified information by computing", 392L, "Information search using internet", "social & leisure", "social & leisure",
  7231L, "Information searching on the internet", 392L, "Information search using internet", "social & leisure", "social & leisure",
  7239L, "Other specified information by computing", 393L, "Other computing", "social & leisure", "social & leisure",
  7240L, "Unspecified communication by computer", 341L, "Communication by text (SMS, instant messages, email, etc.)", "social & leisure", "social & leisure",
  7241L, "Communication on the internet", 341L, "Communication by text (SMS, instant messages, email, etc.)", "social & leisure", "social & leisure",
  7249L, "Other specified communication by computing", 341L, "Communication by text (SMS, instant messages, email, etc.)", "social & leisure", "social & leisure",
  7250L, "Unspecified other computing", 393L, "Other computing", "social & leisure", "social & leisure",
  7251L, "Skype or other video call", 335L, "Phone and video conversations", "social & leisure", "social & leisure",
  7259L, "Other specified computing", 393L, "Other computing", "social & leisure", "social & leisure",
  7300L, "Unspecified games", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  7310L, "Solo games and play", 324L, "Solo games and play, gambling", "social & leisure", "social & leisure",
  7320L, "Unspecified games and play with others", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  7321L, "Billiards pool snooker or petanque", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  7322L, "Chess and bridge", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  7329L, "Other specified parlour games and play", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  7330L, "Computer games", 321L, "Computer games", "social & leisure", "social & leisure",
  7340L, "Gambling", 324L, "Solo games and play, gambling", "social & leisure", "social & leisure",
  7390L, "Other specified games", 334L, "Parlour games and play", "social & leisure", "social & leisure",
  8000L, "Unspecified mass media", 270L, "Watching, streaming, TV/DVD, listening to music (unspecified)", "social & leisure", "social & leisure",
  8100L, "Unspecified reading", 280L, "Reading including e-books (unspecified)", "social & leisure", "social & leisure",
  8110L, "Reading periodicals", 281L, "Reading periodicals", "social & leisure", "social & leisure",
  8120L, "Reading books", 282L, "Reading books", "social & leisure", "social & leisure",
  8190L, "Other specified reading", 283L, "Other reading", "social & leisure", "social & leisure",
  8210L, "Unspecified TV video or DVD watching", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8211L, "Watching a film on TV", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8212L, "Watching sport on TV", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8219L, "Other specified TV watching", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8220L, "Unspecified video watching", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8221L, "Watching a film on video", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8222L, "Watching sport on video", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8229L, "Other specified video watching", 271L, "Watching TV, video or DVD", "social & leisure", "social & leisure",
  8300L, "Unspecified listening to radio and music", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  8310L, "Unspecified radio listening", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  8311L, "Listening to music on the radio", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  8312L, "Listening to sport on the radio", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  8319L, "Other specified radio listening", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  8320L, "Listening to recordings", 272L, "Listening to radio or recordings", "social & leisure", "social & leisure",
  9000L, "Travel related to unspecified time use", 110L, "Travelling (unspecified)", "travel", "travel",
  9010L, "Travel related to personal business", 110L, "Travelling (unspecified)", "travel", "travel",
  9100L, "Travel to/from work", 110L, "Travelling (unspecified)", "travel", "travel",
  9110L, "Travel in the course of work", 110L, "Travelling (unspecified)", "travel", "travel",
  9120L, "Travel to work from home and back only", 110L, "Travelling (unspecified)", "travel", "travel",
  9130L, "Travel to work from a place other than home", 110L, "Travelling (unspecified)", "travel", "travel",
  9210L, "Travel related to education", 110L, "Travelling (unspecified)", "travel", "travel",
  9230L, "Travel escorting to/ from education", 110L, "Travelling (unspecified)", "travel", "travel",
  9310L, "Travel related to household care", 110L, "Travelling (unspecified)", "travel", "travel",
  9360L, "Travel related to shopping", 110L, "Travelling (unspecified)", "travel", "travel",
  9370L, "Travel related to services", 110L, "Travelling (unspecified)", "travel", "travel",
  9380L, "Travel escorting a child other than education", 110L, "Travelling (unspecified)", "travel", "travel",
  9390L, "Travel escorting an adult other than education", 110L, "Travelling (unspecified)", "travel", "travel",
  9400L, "Travel related to organisational work", 110L, "Travelling (unspecified)", "travel", "travel",
  9410L, "Travel related to voluntary work and meetings", 110L, "Travelling (unspecified)", "travel", "travel",
  9420L, "Travel related to informal help to other households", 110L, "Travelling (unspecified)", "travel", "travel",
  9430L, "Travel related to religious activities", 110L, "Travelling (unspecified)", "travel", "travel",
  9440L, "Travel related to participatory activities other than religious activities", 110L, "Travelling (unspecified)", "travel", "travel",
  9500L, "Travel to visit friends/relatives in their homes not respondents household", 110L, "Travelling (unspecified)", "travel", "travel",
  9510L, "Travel related to other social activities", 110L, "Travelling (unspecified)", "travel", "travel",
  9520L, "Travel related to entertainment and culture", 110L, "Travelling (unspecified)", "travel", "travel",
  9600L, "Travel related to other leisure", 110L, "Travelling (unspecified)", "travel", "travel",
  9610L, "Travel related to physical exercise", 110L, "Travelling (unspecified)", "travel", "travel",
  9620L, "Travel related to hunting & fishing", 110L, "Travelling (unspecified)", "travel", "travel",
  9630L, "Travel related to productive exercise other than hunting & fishing", 110L, "Travelling (unspecified)", "travel", "travel",
  9710L, "Travel related to gambling", 110L, "Travelling (unspecified)", "travel", "travel",
  9720L, "Travel related to hobbies other than gambling", 110L, "Travelling (unspecified)", "travel", "travel",
  9800L, "Travel related to changing locality", 110L, "Travelling (unspecified)", "travel", "travel",
  9810L, "Travel to holiday base", 110L, "Travelling (unspecified)", "travel", "travel",
  9820L, "Travel for day trip/just walk", 110L, "Travelling (unspecified)", "travel", "travel",
  9890L, "Other specified travel", 110L, "Travelling (unspecified)", "travel", "travel",
  9940L, "Punctuating activity", 999L, "Missing", "other", "other",
  9941L, "Unknown: at home", 999L, "Missing", "other", "other",
  9950L, "Filling in the time use diary", 400L, "Filling in the time use diary", "other", "other",
  9960L, "No main activity no idea what it might be", 999L, "Missing", "other", "other",
  9970L, "No main activity some idea what it might be", 999L, "Missing", "other", "other",
  9980L, "Illegible activity", 999L, "Missing", "other", "other",
  9990L, "Unspecified time use", 999L, "Missing", "other", "other",
  9999L, "Queryable", 999L, "Missing", "other", "other"
)

# Explicit judgement override for this analysis:
# - The 2023 parent family 200 (codes 200-203: voluntary/club/participatory activities)
#   is treated as Social & leisure rather than paid/study work.
# - Code 391 (computer programming / learning software as a standalone hobby)
#   is treated as Social & leisure.
activity_crosswalk <- activity_crosswalk %>%
  dplyr::mutate(
    group_2023_new = dplyr::if_else(
      code_2023 %in% c(200L, 201L, 202L, 203L, 391L),
      "social & leisure",
      group_2023_new
    )
  )

# Flag rows whose activity GROUP changes vs the earlier 2014 classification
activity_crosswalk <- activity_crosswalk %>%
  dplyr::mutate(group_changed = group_2014_old != group_2023_new)

# ------------------------------------------------------------
# Helper functions derived from the crosswalk
# map_2014_code_to_2023(): UKTUS 2014-2015 numeric code -> 2023 numeric code
# label_for_2023(): 2023 numeric code -> 2023 activity label
# ------------------------------------------------------------
map_2014_code_to_2023 <- function(code) {
  idx <- match(code, activity_crosswalk$code_2014)
  out <- activity_crosswalk$code_2023[idx]
  out[is.na(out)] <- 999L
  out
}

label_for_2023 <- function(code) {
  lab_tbl <- dplyr::distinct(activity_crosswalk, code_2023, label_2023)
  idx <- match(code, lab_tbl$code_2023)
  out <- lab_tbl$label_2023[idx]
  out[is.na(out)] <- "Missing"
  out
}

df2014_premerge <- df2014_raw %>%
  mutate(
    # Keep numeric official codes before converting labelled variables to factors
    whatdoing_code = as.numeric(zap_labels(whatdoing)),
    sec_code_2014 = as.numeric(zap_labels(What_Oth1)),
    
    whatdoing = as_factor(whatdoing),
    WhereWhen = as_factor(WhereWhen),
    Device = as_factor(Device),
    What_Oth1 = as_factor(What_Oth1)
  ) %>%
  filter(!is.na(whatdoing), !is.na(WhereWhen), !is.na(Device)) %>%
  mutate(
    period = "2014-2015",
    # ---- HARMONISED to the 2023 coding scheme ----
    pri_code_harmonised = map_2014_code_to_2023(whatdoing_code),
    sec_code_harmonised = map_2014_code_to_2023(sec_code_2014),
    pri_label_original  = as.character(whatdoing),
    sec_label_original  = as.character(What_Oth1),
    pri = label_for_2023(pri_code_harmonised),   # 2023-scheme activity label
    sec = label_for_2023(sec_code_harmonised),
    loc = as.character(WhereWhen),
    sec_text = str_to_lower(as.character(What_Oth1)),
    pri_text = str_to_lower(pri),
    time = as.numeric(eptime),
    wt = ifelse(is.na(dia_wt_a), 1, as.numeric(dia_wt_a)),
    weighted_time = time * wt,
    # ------------------------------------------------------------------
    # ICT, 2014-15. `Device` has three states: 1 "using device",
    # 0 "not using device", -9 "not reported". The -9 state covers 6.67% of
    # weighted episodes and used to be folded silently into 0, which biases
    # 2014-15 ICT downwards relative to 2023 (which has no missing device
    # data at all). `ict_reported` now records the distinction explicitly.
    # See section 0.15, switch (3).
    # 2014-15 asks a single device question, so the "single prompt" version
    # is identical to the main measure; only 2023 is restricted.
    # ------------------------------------------------------------------
    device_text = str_to_lower(as.character(Device)),
    ict_reported = ifelse(str_detect(device_text, "not reported"), 0L, 1L),
    any_ict_raw = ifelse(device_text == "using device", 1, 0),
    any_ict = if (isTRUE(ICT_UNREPORTED_2014_AS_NA)) {
      ifelse(ict_reported == 0L, NA_real_, any_ict_raw)
    } else {
      any_ict_raw
    },
    any_ict_single_prompt = any_ict,
    mainid = paste(serial, pnum, sep = "_"),
    diaryord = as.character(daynum),
    person_day = paste(serial, pnum, daynum, sep = "_"),
    original_episode_number = as.character(epnum),
    # UKTUS tid: 1 = 04:00-04:10, so wrap after midnight.
    start_raw = (as.numeric(tid) - 1) * 10 + 240,
    start = ifelse(start_raw >= 1440, start_raw - 1440, start_raw),
    
    # Harmonised classification: 2014-2015 codes were mapped to the 2023
    # scheme above, then classified with the SAME classifier as 2023.
    pri_group = classify_activity_code_2023(pri_code_harmonised),
    sec_group = classify_activity_code_2023(sec_code_harmonised),

    # Primary activity remains the final activity for the original episode.
    # Secondary activities are added later as duplicated episodes if sec_group != "other".
    act_group2 = pri_group,

    # Travel context: if either primary or secondary activity is travel,
    # the episode location/context is coded as travel. For 2014-2015, the original
    # location labels also contain transport modes, so these are retained as travel too.
    travel_context = pri_group == "travel" | sec_group == "travel",

    loc_text = str_to_lower(loc),
    loc6 = case_when(

      travel_context | str_detect(
        loc_text,
        "transport mode|public transport|passenger car|on foot|bus|train|van|bicycle|taxi|tram|underground|boat|ship|lorry|tractor|aeroplane|coach|moped|motorcycle|travelling mode"
      ) ~ "travel",

      loc_text %in% c(
        "home",
        "other peoples home",
        "other people's home",
        "second home or weekend house"
      ) ~ "home",
      
      str_detect(
        loc_text,
        "work|school|college|university|working place"
      ) ~ "work_or_school",
      
      str_detect(
        loc_text,
        "shop|shopping|market"
      ) ~ "shopping_services",
      
      str_detect(
        loc_text,
        "restaurant|cafe|pub"
      ) ~ "restaurant_cafe_pub",
      
      TRUE ~ "other"
    )
  ) %>%
  filter(!is.na(time), time > 0, !is.na(start)) %>%
  select(
    period, mainid, diaryord, person_day, original_episode_number,
    start, time, wt, weighted_time,
    pri_code_harmonised, sec_code_harmonised,
    pri, sec, loc, pri_group, sec_group, act_group2, loc6, any_ict,
    any_ict_single_prompt, ict_reported
  )


# ============================================================
# 3A. Formal adjacent-episode merging after harmonisation
# ============================================================
# Apply exactly the same merge rule to 2014-2015 and 2023.

df2014_tagged_for_merge <- tag_harmonised_merge_spells(df2014_premerge)
df2023_tagged_for_merge <- tag_harmonised_merge_spells(df2023_premerge)

# Detailed audit tables: exactly which original episodes were merged
merge_detail_2014 <- build_merge_detail_table(df2014_tagged_for_merge)
merge_detail_2023 <- build_merge_detail_table(df2023_tagged_for_merge)

write_csv(
  merge_detail_2014,
  "combined_outputs_harmonised/tables/formal_merge_detail_2014_2015.csv"
)

write_csv(
  merge_detail_2023,
  "combined_outputs_harmonised/tables/formal_merge_detail_2023.csv"
)

write_csv(
  bind_rows(merge_detail_2014, merge_detail_2023),
  "combined_outputs_harmonised/tables/formal_merge_detail_both_periods.csv"
)

# Compact summary: which harmonised primary-secondary-location combinations
# were merged most often in each period
formal_merge_summary <- bind_rows(
  merge_detail_2014,
  merge_detail_2023
) %>%
  group_by(
    period,
    harmonised_primary_code,
    harmonised_primary_label,
    harmonised_secondary_code,
    harmonised_secondary_label,
    harmonised_location
  ) %>%
  summarise(
    merged_spells_raw = n(),
    original_episodes_absorbed_raw = sum(n_original_episodes),
    episodes_removed_raw = sum(n_original_episodes - 1),
    weighted_merged_spells = sum(wt, na.rm = TRUE),
    weighted_episodes_removed = sum((n_original_episodes - 1) * wt, na.rm = TRUE),
    total_merged_duration_minutes = sum(merged_duration_minutes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period, desc(weighted_episodes_removed))

write_csv(
  formal_merge_summary,
  "combined_outputs_harmonised/tables/formal_merge_summary_by_activity_location.csv"
)

# Period-level diagnostics
formal_merge_diagnostics <- bind_rows(
  df2014_tagged_for_merge,
  df2023_tagged_for_merge
) %>%
  group_by(period) %>%
  summarise(
    original_episode_rows = n(),
    merged_spell_rows = n_distinct(paste(mainid, diaryord, harmonised_spell_id)),
    episodes_removed = original_episode_rows - merged_spell_rows,
    reduction_percent = episodes_removed / original_episode_rows,
    .groups = "drop"
  )

write_csv(
  formal_merge_diagnostics,
  "combined_outputs_harmonised/tables/formal_merge_diagnostics_by_period.csv"
)

# Collapse the tagged original episodes, then expand primary/secondary activities.
# All descriptive analysis below uses these formally preprocessed data.
df2014_primary_merged <- collapse_harmonised_merge_spells(df2014_tagged_for_merge)
df2023_primary_merged <- collapse_harmonised_merge_spells(df2023_tagged_for_merge)

df2014 <- df2014_primary_merged %>%
  add_secondary_activity_episodes()

df2023 <- df2023_primary_merged %>%
  add_secondary_activity_episodes()


# =========================
# Check activity classification
# =========================

# 2023 activity mapping
df2023 %>%
  count(act_group2, activity_name, sort = TRUE) %>%
  write_csv("combined_outputs_harmonised/tables/2023_activity_mapping.csv")

df2014 %>%
  count(act_group2, activity_name, sort = TRUE) %>%
  write_csv("combined_outputs_harmonised/tables/2014_2015_activity_mapping.csv")

# ------------------------------------------------------------
# Output the activity crosswalk (the answer to "which 2014-2015 activity
# maps to which 2023 activity").
#  * static crosswalk: every 2014-2015 code -> 2023 code, with labels and
#    old vs new 7-group classification, and a group_changed flag.
#  * data-driven crosswalk: weighted 2014-2015 episode counts feeding each
#    mapping, so you can see which mappings carry the most data.
# ------------------------------------------------------------
write_csv(
  activity_crosswalk,
  "combined_outputs_harmonised/tables/activity_crosswalk_2014_to_2023.csv"
)

write_csv(
  activity_crosswalk %>% filter(group_changed),
  "combined_outputs_harmonised/tables/activity_crosswalk_group_changes.csv"
)

crosswalk_counts_2014 <- df2014_raw %>%
  mutate(
    code_2014 = as.numeric(zap_labels(whatdoing)),
    wt = ifelse(is.na(dia_wt_a), 1, as.numeric(dia_wt_a)),
    code_2023 = map_2014_code_to_2023(code_2014)
  ) %>%
  filter(!is.na(code_2014)) %>%
  count(code_2014, code_2023, wt = wt, name = "weighted_episodes_2014") %>%
  left_join(dplyr::distinct(activity_crosswalk, code_2014, label_2014), by = "code_2014") %>%
  left_join(dplyr::distinct(activity_crosswalk, code_2023, label_2023), by = "code_2023") %>%
  select(code_2014, label_2014, code_2023, label_2023, weighted_episodes_2014) %>%
  arrange(desc(weighted_episodes_2014))

write_csv(
  crosswalk_counts_2014,
  "combined_outputs_harmonised/tables/activity_crosswalk_2014_to_2023_with_counts.csv"
)


# ------------------------------------------------------------
# Full activity harmonisation check table: EVERY 2014-2015 fine
# activity code and how it is mapped/classified in the 2023 scheme.
# Use this table to review the full classification, not only the few
# examples shown in the PPT.
# ------------------------------------------------------------
activity_harmonisation_check_all <- activity_crosswalk %>%
  left_join(
    crosswalk_counts_2014 %>%
      select(code_2014, weighted_episodes_2014),
    by = "code_2014"
  ) %>%
  mutate(
    weighted_episodes_2014 = tidyr::replace_na(weighted_episodes_2014, 0),
    group_transition = paste0(group_2014_old, " -> ", group_2023_new),
    mapping_flag = case_when(
      group_changed ~ "GROUP CHANGED - review",
      TRUE ~ "same broad group"
    )
  ) %>%
  transmute(
    code_2014,
    activity_2014_fine = label_2014,
    group_2014_native_scheme = group_2014_old,
    mapped_code_2023 = code_2023,
    mapped_activity_2023 = label_2023,
    group_2023_final_scheme = group_2023_new,
    group_transition,
    group_changed,
    mapping_flag,
    weighted_episodes_2014
  ) %>%
  arrange(group_changed, group_2014_native_scheme, code_2014)

write_csv(
  activity_harmonisation_check_all,
  "combined_outputs_harmonised/tables/activity_harmonisation_check_ALL_activities.csv"
)

# Same table, but only rows where the broad activity group changes.
activity_harmonisation_check_changed <- activity_harmonisation_check_all %>%
  filter(group_changed) %>%
  arrange(desc(weighted_episodes_2014), group_2014_native_scheme, code_2014)

write_csv(
  activity_harmonisation_check_changed,
  "combined_outputs_harmonised/tables/activity_harmonisation_check_GROUP_CHANGED_only.csv"
)

# Compact summary of how much weighted 2014-2015 data is affected by each
# group transition. This is useful for deciding which mappings matter most.
activity_harmonisation_transition_summary <- activity_harmonisation_check_all %>%
  group_by(group_transition, group_2014_native_scheme, group_2023_final_scheme, group_changed) %>%
  summarise(
    n_2014_fine_codes = n(),
    weighted_episodes_2014 = sum(weighted_episodes_2014, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(group_changed), desc(weighted_episodes_2014))

write_csv(
  activity_harmonisation_transition_summary,
  "combined_outputs_harmonised/tables/activity_harmonisation_transition_summary.csv"
)

# Optional: print the changed rows to Console so they are easy to inspect.
print(activity_harmonisation_check_changed, n = Inf)




# =========================
# Check location classification
# =========================

# 2023 location mapping
df2023 %>%
  count(loc6, loc, sort = TRUE) %>%
  write_csv("combined_outputs_harmonised/tables/2023_location_mapping.csv")

# 2014-2015 location mapping
df2014 %>%
  count(loc6, loc, sort = TRUE) %>%
  write_csv("combined_outputs_harmonised/tables/2014_2015_location_mapping.csv")


# ============================================================
# 4. Combine formally preprocessed time-use data
# ============================================================


timeuse_all <- bind_rows(df2014, df2023) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    activity = factor(act_group2, levels = activity_levels),
    location = factor(loc6, levels = location_levels),
    loc_act = paste(location, activity, sep = " × ")
  )

write_csv(timeuse_all, "combined_outputs_harmonised/tables/timeuse_formally_merged_and_expanded_episode_level.csv")

# Weighted diary-day denominator by period
period_day_denominator <- timeuse_all %>%
  distinct(period, person_day, wt) %>%
  group_by(period) %>%
  summarise(
    weighted_diary_days = sum(wt, na.rm = TRUE),
    raw_diary_days = n(),
    .groups = "drop"
  )

write_csv(period_day_denominator, "combined_outputs_harmonised/tables/timeuse_weighted_diary_day_denominator.csv")


# ============================================================
# 4B. ICT CROSS-PERIOD COMPARABILITY
# ------------------------------------------------------------
# WHY THIS SECTION EXISTS
#
# The two surveys do not measure ICT the same way:
#
#                       2014-15                    2023
#   instrument          ONE device question        THREE separate columns
#                       (smartphone/tablet/        (computer, tablet, phone)
#                        computer, combined)
#   missing data        6.67% coded -9             none
#                       "not reported"
#   ICT share of        11.5%                      25.4%
#   travel episodes     (matches the 11.3%
#                        reported by Harms
#                        et al. 2018)
#
# Three prompts elicit more affirmative answers than one, so the 2.2x rise
# in measured ICT mixes genuine behavioural change with a change in the
# survey instrument. Because the MDCEV specification enters ICT as a SHARE
# (share of the day on a device -> psi; share of activity time on a device
# -> gamma), a level shift in the measure moves the coefficients
# mechanically. Any "how did the ICT association change by 2023" claim is
# therefore exposed to this, and it is the core contribution of the model.
#
# WHAT THIS SECTION PRODUCES
#
#  1. ict_comparability_diagnostic.csv
#     The raw evidence above, recomputed from the data, so the size of the
#     problem can be quoted directly in the write-up.
#
#  2. ict_person_day_shares.csv
#     Person-day ICT shares for the MDCEV stage, in four versions:
#       ict_share_day            raw, all three 2023 columns  (old measure)
#       ict_share_day_single     2023 restricted to phone     (instrument-matched)
#       ict_share_day_z          raw, standardised WITHIN period
#       ict_share_day_single_z   single-prompt, standardised WITHIN period
#     Within-period standardisation is the recommended fix: it rescales each
#     period to mean 0 / sd 1, so the coefficients compare RELATIVE ICT
#     intensity and absorb the instrument shift. Report the raw version as
#     the headline and the standardised version as the robustness check - if
#     the 2014 -> 2023 changes survive standardisation, the instrument
#     objection is answered.
#
#  3. ict_activity_shares.csv
#     The same, per person-day x activity, for the gamma (in-activity) channel.
# ============================================================

message("\nSection 4B: ICT cross-period comparability diagnostics...")

# ---- 1. Diagnostic: how different are the two instruments? ----
ict_comparability_diagnostic <- timeuse_all %>%
  filter(activity_role == "primary") %>%
  group_by(period) %>%
  summarise(
    weighted_episodes = sum(wt, na.rm = TRUE),
    share_episodes_unreported = sum(wt * (ict_reported == 0), na.rm = TRUE) /
      sum(wt, na.rm = TRUE),
    ict_share_of_all_time = sum(weighted_time * any_ict, na.rm = TRUE) /
      sum(weighted_time, na.rm = TRUE),
    ict_share_of_all_time_single_prompt =
      sum(weighted_time * any_ict_single_prompt, na.rm = TRUE) /
      sum(weighted_time, na.rm = TRUE),
    ict_share_of_travel_episodes =
      sum(wt * any_ict * (pri_group == "travel"), na.rm = TRUE) /
      sum(wt * (pri_group == "travel"), na.rm = TRUE),
    ict_share_of_travel_episodes_single_prompt =
      sum(wt * any_ict_single_prompt * (pri_group == "travel"), na.rm = TRUE) /
      sum(wt * (pri_group == "travel"), na.rm = TRUE),
    total_ict_weighted_minutes = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    ict_minutes_per_diary_day = safe_divide(
      total_ict_weighted_minutes, weighted_diary_days
    )
  )

write_csv(
  ict_comparability_diagnostic,
  "combined_outputs_harmonised/tables/ict_comparability_diagnostic.csv"
)

print(ict_comparability_diagnostic)

# ---- 2. Person-day ICT shares for the MDCEV stage ----
# Primary rows only: secondary rows are duplicated episodes and would
# double-count minutes in the denominator.
ict_person_day_shares <- timeuse_all %>%
  filter(activity_role == "primary") %>%
  group_by(period, person_day, mainid, diaryord) %>%
  summarise(
    wt = first(wt),
    total_minutes = sum(time, na.rm = TRUE),
    ict_minutes = sum(time * any_ict, na.rm = TRUE),
    ict_minutes_single = sum(time * any_ict_single_prompt, na.rm = TRUE),
    minutes_unreported = sum(time * (ict_reported == 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ict_share_day = safe_divide(ict_minutes, total_minutes),
    ict_share_day_single = safe_divide(ict_minutes_single, total_minutes),
    share_day_unreported = safe_divide(minutes_unreported, total_minutes)
  ) %>%
  group_by(period) %>%
  mutate(
    # Within-period standardisation: mean 0, sd 1 inside each survey year.
    # This is what makes the psi and gamma coefficients comparable across
    # periods despite the instrument change.
    ict_share_day_z =
      (ict_share_day - mean(ict_share_day, na.rm = TRUE)) /
      sd(ict_share_day, na.rm = TRUE),
    ict_share_day_single_z =
      (ict_share_day_single - mean(ict_share_day_single, na.rm = TRUE)) /
      sd(ict_share_day_single, na.rm = TRUE)
  ) %>%
  ungroup()

write_csv(
  ict_person_day_shares,
  "combined_outputs_harmonised/tables/ict_person_day_shares.csv"
)

# ---- 3. In-activity ICT shares (the gamma channel) ----
ict_activity_shares <- timeuse_all %>%
  filter(activity_role == "primary") %>%
  group_by(period, person_day, activity) %>%
  summarise(
    wt = first(wt),
    activity_minutes = sum(time, na.rm = TRUE),
    ict_minutes = sum(time * any_ict, na.rm = TRUE),
    ict_minutes_single = sum(time * any_ict_single_prompt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ict_share_activity = safe_divide(ict_minutes, activity_minutes),
    ict_share_activity_single = safe_divide(ict_minutes_single, activity_minutes)
  ) %>%
  group_by(period, activity) %>%
  mutate(
    # Standardised within period AND within activity, because the baseline
    # level of device use differs enormously between, say, work and travel.
    ict_share_activity_z =
      (ict_share_activity - mean(ict_share_activity, na.rm = TRUE)) /
      sd(ict_share_activity, na.rm = TRUE),
    ict_share_activity_single_z =
      (ict_share_activity_single - mean(ict_share_activity_single, na.rm = TRUE)) /
      sd(ict_share_activity_single, na.rm = TRUE)
  ) %>%
  ungroup()

write_csv(
  ict_activity_shares,
  "combined_outputs_harmonised/tables/ict_activity_shares.csv"
)

message("  wrote ict_comparability_diagnostic.csv, ict_person_day_shares.csv, ict_activity_shares.csv")



# ============================================================
# 5. Time-use comparison: activity level
# ============================================================

activity_summary <- timeuse_all %>%
  group_by(period, activity) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    total_weighted_time = sum(weighted_time, na.rm = TRUE),
    avg_duration = weighted.mean(time, wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_weighted_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  group_by(period) %>%
  mutate(
    time_share = total_weighted_time / sum(total_weighted_time, na.rm = TRUE),
    episode_share = weighted_episodes / sum(weighted_episodes, na.rm = TRUE),
    avg_minutes_per_diary_day = total_weighted_time / weighted_diary_days,
    ict_intensity_time = ict_weighted_time / total_weighted_time,
    ict_share_of_ict_time = ict_weighted_time / sum(ict_weighted_time, na.rm = TRUE),
    ict_minutes_per_diary_day = ict_weighted_time / weighted_diary_days
  ) %>%
  ungroup()

write_csv(activity_summary, "combined_outputs_harmonised/tables/timeuse_activity_summary_by_period.csv")

activity_change <- activity_summary %>%
  select(period, activity, time_share, avg_minutes_per_diary_day, avg_duration,
         ict_intensity_time, ict_minutes_per_diary_day) %>%
  pivot_wider(
    names_from = period,
    values_from = c(time_share, avg_minutes_per_diary_day, avg_duration,
                    ict_intensity_time, ict_minutes_per_diary_day)
  ) %>%
  mutate(
    diff_time_share = `time_share_2023` - `time_share_2014-2015`,
    diff_avg_minutes_per_day = `avg_minutes_per_diary_day_2023` - `avg_minutes_per_diary_day_2014-2015`,
    diff_avg_duration = `avg_duration_2023` - `avg_duration_2014-2015`,
    diff_ict_intensity = `ict_intensity_time_2023` - `ict_intensity_time_2014-2015`,
    diff_ict_minutes_per_day = `ict_minutes_per_diary_day_2023` - `ict_minutes_per_diary_day_2014-2015`
  )

write_csv(activity_change, "combined_outputs_harmonised/tables/timeuse_activity_change_2023_minus_2014_2015.csv")

# Figure 1: activity time share
p_activity_share <- ggplot(activity_summary, aes(x = activity, y = time_share, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Time-use activity composition: 2014-2015 vs 2023",
    x = "Activity purpose",
    y = "Share of total weighted time",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/01_activity_time_share_compare.png", p_activity_share, width = 9, height = 5, dpi = 300)

# Figure 2: average minutes per diary day
p_activity_minutes <- ggplot(activity_summary, aes(x = activity, y = avg_minutes_per_diary_day, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average minutes per diary-day by activity",
    x = "Activity purpose",
    y = "Weighted average minutes per diary-day",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/02_activity_avg_minutes_per_day_compare.png", p_activity_minutes, width = 9, height = 5, dpi = 300)

# Figure 3: ICT intensity within activity
p_activity_ict_intensity <- ggplot(activity_summary, aes(x = activity, y = ict_intensity_time, fill = period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "ICT intensity within each activity: 2014-2015 vs 2023",
    x = "Activity purpose",
    y = "Share of episode time involving a device",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/03_activity_ict_intensity_compare.png", p_activity_ict_intensity, width = 9, height = 5, dpi = 300)

# Figure 4: ICT minutes per diary day
p_activity_ict_minutes <- ggplot(activity_summary, aes(x = activity, y = ict_minutes_per_diary_day, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "ICT-involved minutes per diary-day by activity",
    x = "Activity purpose",
    y = "Weighted average ICT-involved minutes per diary-day",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/04_activity_ict_minutes_per_day_compare.png", p_activity_ict_minutes, width = 9, height = 5, dpi = 300)



# ============================================================
# ICT time-based indicators: harmonised pre-merge recorded segments
# ============================================================
# Use this dataset for ICT minutes and ICT time-share calculations.
# It keeps harmonised original recorded episode segments BEFORE adjacent-episode
# merging. This avoids assigning ICT use to the full duration of a merged spell
# when ICT status differs across adjacent segments.
#
# Interpretation:
#   "Recorded segment minutes involving ICT" means the duration of diary
#   segments in which ICT was reported. It is not actual device-use minutes.

timeuse_premerge_primary <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    activity = factor(act_group2, levels = activity_levels),
    location = factor(loc6, levels = location_levels),
    weighted_time = time * wt,
    activity_role = "primary",
    activity_name = pri
  )

period_day_denominator_premerge <- timeuse_premerge_primary %>%
  distinct(period, person_day, wt) %>%
  group_by(period) %>%
  summarise(
    weighted_diary_days = sum(wt, na.rm = TRUE),
    raw_diary_days = n(),
    .groups = "drop"
  )


# ============================================================
# M4b. ICT involvement across activities
# ============================================================

activity_ict_summary <- timeuse_premerge_primary %>%
  group_by(period, activity) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator_premerge, by = "period") %>%
  mutate(
    ict_segment_minutes_per_diary_day =
      ict_recorded_segment_time / weighted_diary_days,
    ict_segment_time_share =
      ict_recorded_segment_time / total_recorded_segment_time
  )

write_csv(
  activity_ict_summary,
  "combined_outputs_harmonised/tables/M4b_ict_involvement_across_activities_by_period.csv"
)

# ------------------------------------------------------------
# Figure: ICT involvement across activities
# Episode minutes involving a device per diary-day
# ------------------------------------------------------------

p_activity_ict_minutes <- ggplot(
  activity_ict_summary %>% mutate(activity = label_activities(activity)),
  aes(
    y = activity,
    x = ict_segment_minutes_per_diary_day,
    fill = period
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "ICT involvement across activities",
    subtitle = "Weighted average recorded segment minutes involving ICT per diary-day",
    x = "Episode minutes involving a device per diary-day",
    y = "Activity purpose",
    fill = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4b_ict_recorded_segment_minutes_across_activities.png",
  p_activity_ict_minutes,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# Figure: ICT intensity across activities
# Share of episode time involving a device
# ------------------------------------------------------------

p_activity_ict_intensity <- ggplot(
  activity_ict_summary %>% mutate(activity = label_activities(activity)),
  aes(
    y = activity,
    x = ict_segment_time_share,
    fill = period
  )
) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "ICT intensity across activities",
    subtitle = NULL,
    x = "Share of episode time involving a device",
    y = "Activity purpose",
    fill = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4b_ict_recorded_segment_intensity_across_activities.png",
  p_activity_ict_intensity,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 5: change in activity share
p_activity_change <- ggplot(activity_change, aes(x = fct_reorder(activity, diff_time_share), y = diff_time_share)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in activity time share: 2023 minus 2014-2015",
    x = "Activity purpose",
    y = "Percentage-point change"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/05_activity_time_share_change.png", p_activity_change, width = 8, height = 5, dpi = 300)

# ============================================================
# 6. Time-use comparison: all vs ICT absolute metrics within activity
# ============================================================

# 2014-2015 contains two survey years, while 2023 contains one year.
# For absolute totals, convert 2014-2015 into annual average.
activity_compare_abs <- bind_rows(
  activity_summary %>%
    transmute(
      period,
      activity,
      type = "All activities",
      weighted_episodes,
      total_time = total_weighted_time,
      avg_duration,
      avg_minutes_per_diary_day
    ),
  activity_summary %>%
    transmute(
      period,
      activity,
      type = "ICT-involved",
      weighted_episodes = ict_weighted_episodes,
      total_time = ict_weighted_time,
      avg_duration = NA_real_,
      avg_minutes_per_diary_day = ict_minutes_per_diary_day
    )
) %>%
  mutate(
    period_years = ifelse(period == "2014-2015", 2, 1),
    weighted_episodes_annual = weighted_episodes / period_years,
    total_time_annual = total_time / period_years
  )

write_csv(activity_compare_abs, "combined_outputs_harmonised/tables/timeuse_activity_all_vs_ict_absolute_metrics.csv")

p_abs_count <- ggplot(activity_compare_abs, aes(x = activity, y = weighted_episodes_annual, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Annual average weighted episode count: all vs ICT-involved",
    x = "Activity purpose",
    y = "Annual average weighted episode count",
    fill = ""
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/06_all_vs_ict_weighted_episode_count_by_period.png", p_abs_count, width = 11, height = 5.5, dpi = 300)

p_abs_time <- ggplot(activity_compare_abs, aes(x = activity, y = total_time_annual, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Annual average total weighted time: all vs ICT-involved",
    x = "Activity purpose",
    y = "Annual average total weighted time (minutes)",
    fill = ""
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/07_all_vs_ict_total_time_by_period.png", p_abs_time, width = 11, height = 5.5, dpi = 300)

p_abs_minutes_day <- ggplot(activity_compare_abs, aes(x = activity, y = avg_minutes_per_diary_day, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Average minutes per diary-day: all vs ICT-involved",
    x = "Activity purpose",
    y = "Weighted average minutes per diary-day",
    fill = ""
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/08_all_vs_ict_avg_minutes_per_day_by_period.png", p_abs_minutes_day, width = 11, height = 5.5, dpi = 300)

# ============================================================
# 7. Time-use comparison: location x activity
# ============================================================

locact_summary <- timeuse_all %>%
  group_by(period, location, activity, loc_act) %>%
  summarise(
    weighted_episodes = sum(wt, na.rm = TRUE),
    total_weighted_time = sum(weighted_time, na.rm = TRUE),
    avg_duration = weighted.mean(time, wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_weighted_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  group_by(period) %>%
  mutate(
    time_share = total_weighted_time / sum(total_weighted_time, na.rm = TRUE),
    avg_minutes_per_diary_day = total_weighted_time / weighted_diary_days,
    ict_intensity_time = ict_weighted_time / total_weighted_time,
    ict_minutes_per_diary_day = ict_weighted_time / weighted_diary_days,
    ict_share_of_ict_time = ict_weighted_time / sum(ict_weighted_time, na.rm = TRUE)
  ) %>%
  ungroup()

write_csv(locact_summary, "combined_outputs_harmonised/tables/timeuse_location_activity_summary_by_period.csv")

locact_change <- locact_summary %>%
  select(period, location, activity, loc_act, time_share, avg_minutes_per_diary_day,
         ict_intensity_time, ict_minutes_per_diary_day) %>%
  pivot_wider(
    names_from = period,
    values_from = c(time_share, avg_minutes_per_diary_day, ict_intensity_time, ict_minutes_per_diary_day)
  ) %>%
  mutate(
    diff_time_share = `time_share_2023` - `time_share_2014-2015`,
    diff_avg_minutes_per_day = `avg_minutes_per_diary_day_2023` - `avg_minutes_per_diary_day_2014-2015`,
    diff_ict_intensity = `ict_intensity_time_2023` - `ict_intensity_time_2014-2015`,
    diff_ict_minutes_per_day = `ict_minutes_per_diary_day_2023` - `ict_minutes_per_diary_day_2014-2015`
  )

write_csv(locact_change, "combined_outputs_harmonised/tables/timeuse_location_activity_change_2023_minus_2014_2015.csv")

# Heatmap: all time share by location x activity, faceted by period
p_locact_share_heatmap <- ggplot(locact_summary, aes(x = activity, y = location, fill = time_share)) +
  geom_tile(color = "white") +
  facet_wrap(~period) +
  scale_fill_gradient(labels = percent_format()) +
  labs(
    title = "Location x activity: share of total weighted time",
    x = "Activity purpose",
    y = "Location",
    fill = "Time share"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/09_location_activity_time_share_heatmap_by_period.png", p_locact_share_heatmap, width = 11, height = 5.5, dpi = 300)

# Bar: average minutes per diary day
p_locact_minutes <- ggplot(locact_summary, aes(x = location, y = avg_minutes_per_diary_day, fill = activity)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Location x activity: average minutes per diary-day",
    x = "Location",
    y = "Weighted average minutes per diary-day",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/10_location_activity_avg_minutes_per_day_by_period.png", p_locact_minutes, width = 12, height = 6, dpi = 300)

# Bar: average minutes per diary day
# Exclude home × personal care to improve visibility
locact_summary_no_sleep <- locact_summary %>%
  filter(!(location == "home" & activity == "personal care"))

p_locact_minutes_no_sleep <- ggplot(
  locact_summary_no_sleep,
  aes(x = location, y = avg_minutes_per_diary_day, fill = activity)
) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  labs(
    title = "Location x activity: average minutes per diary-day (excluding home personal care)",
    x = "Location",
    y = "Weighted average minutes per diary-day",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_timeuse/10b_location_activity_avg_minutes_per_day_no_home_personal_care.png",
  p_locact_minutes_no_sleep,
  width = 12,
  height = 6,
  dpi = 300
)


# ICT intensity heatmap
p_locact_ict_intensity <- ggplot(locact_summary, aes(x = activity, y = location, fill = ict_intensity_time)) +
  geom_tile(color = "white") +
  facet_wrap(~period) +
  scale_fill_gradient(labels = percent_format()) +
  labs(
    title = "ICT intensity by location and activity",
    x = "Activity purpose",
    y = "Location",
    fill = "ICT intensity"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/11_location_activity_ict_intensity_heatmap_by_period.png", p_locact_ict_intensity, width = 11, height = 5.5, dpi = 300)

# Change plot: top location x activity changes
p_locact_change <- locact_change %>%
  slice_max(order_by = abs(diff_time_share), n = 15) %>%
  ggplot(aes(x = fct_reorder(loc_act, diff_time_share), y = diff_time_share)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Largest changes in location x activity time share",
    x = "Location x activity",
    y = "Percentage-point change, 2023 minus 2014-2015"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/12_top_location_activity_time_share_changes.png", p_locact_change, width = 9, height = 6, dpi = 300)

# ============================================================
# 8. Time-use comparison: hourly distributions
# ============================================================

timeuse_hour <- expand_episode_hours(timeuse_all)

hour_activity <- timeuse_hour %>%
  group_by(period, hour, activity) %>%
  summarise(total_time = sum(weighted_overlap_time, na.rm = TRUE), .groups = "drop") %>%
  group_by(period, hour) %>%
  mutate(share_within_hour = total_time / sum(total_time, na.rm = TRUE)) %>%
  ungroup()

write_csv(hour_activity, "combined_outputs_harmonised/tables/timeuse_hourly_activity_distribution.csv")

p_hour_activity <- ggplot(hour_activity, aes(x = hour, y = share_within_hour, color = activity)) +
  geom_line(linewidth = 1) +
  facet_wrap(~period) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "24-hour activity distribution by period",
    x = "Hour of day",
    y = "Share of weighted time within hour",
    color = "Activity purpose"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/13_hourly_activity_distribution_by_period.png", p_hour_activity, width = 12, height = 5.5, dpi = 300)

hour_ict <- timeuse_hour %>%
  group_by(period, hour) %>%
  summarise(
    total_time = sum(weighted_overlap_time, na.rm = TRUE),
    ict_time = sum(weighted_overlap_time * any_ict, na.rm = TRUE),
    ict_share = ict_time / total_time,
    .groups = "drop"
  )

write_csv(hour_ict, "combined_outputs_harmonised/tables/timeuse_hourly_ict_share.csv")

p_hour_ict <- ggplot(hour_ict, aes(x = hour, y = ict_share, color = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "ICT share over time of day: 2014-2015 vs 2023",
    x = "Hour of day",
    y = "Share of weighted time involving ICT",
    color = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/14_hourly_ict_share_compare.png", p_hour_ict, width = 9, height = 5, dpi = 300)

# Top location x activity hourly comparison
selected_locact <- locact_summary %>%
  group_by(loc_act) %>%
  summarise(total_time = sum(total_weighted_time, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_time, n = 12) %>%
  pull(loc_act)

hour_locact <- timeuse_hour %>%
  filter(loc_act %in% selected_locact) %>%
  group_by(period, hour, loc_act) %>%
  summarise(total_time = sum(weighted_overlap_time, na.rm = TRUE), .groups = "drop") %>%
  group_by(period, hour) %>%
  mutate(share_within_hour = total_time / sum(total_time, na.rm = TRUE)) %>%
  ungroup()

write_csv(hour_locact, "combined_outputs_harmonised/tables/timeuse_hourly_top_location_activity_distribution.csv")

p_hour_locact <- ggplot(hour_locact, aes(x = hour, y = share_within_hour, color = period)) +
  geom_line(linewidth = 1) +
  facet_wrap(~loc_act, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "24-hour distribution of top location x activity combinations",
    x = "Hour of day",
    y = "Share within hour",
    color = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/15_hourly_top_location_activity_compare.png", p_hour_locact, width = 12, height = 8, dpi = 300)

# ============================================================
# 9. Fragmentation and switching comparison
# ============================================================

fragmentation_day <- timeuse_all %>%
  group_by(period, person_day) %>%
  arrange(start, .by_group = TRUE) %>%
  mutate(
    prev_activity = lag(activity),
    prev_location = lag(location),
    activity_switch = ifelse(is.na(prev_activity), 0, ifelse(activity != prev_activity, 1, 0)),
    location_switch = ifelse(is.na(prev_location), 0, ifelse(location != prev_location, 1, 0))
  ) %>%
  summarise(
    wt = first(wt),
    n_episodes = n(),
    total_minutes = sum(time, na.rm = TRUE),
    n_activity_switches = sum(activity_switch, na.rm = TRUE),
    n_location_switches = sum(location_switch, na.rm = TRUE),
    ict_time_share = sum(weighted_time * any_ict, na.rm = TRUE) / sum(weighted_time, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(fragmentation_day, "combined_outputs_harmonised/tables/timeuse_fragmentation_day_level.csv")

fragmentation_summary <- fragmentation_day %>%
  group_by(period) %>%
  summarise(
    avg_episodes_per_day = weighted.mean(n_episodes, wt, na.rm = TRUE),
    avg_activity_switches_per_day = weighted.mean(n_activity_switches, wt, na.rm = TRUE),
    avg_location_switches_per_day = weighted.mean(n_location_switches, wt, na.rm = TRUE),
    avg_ict_time_share = weighted.mean(ict_time_share, wt, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(fragmentation_summary, "combined_outputs_harmonised/tables/timeuse_fragmentation_summary_by_period.csv")

fragmentation_long <- fragmentation_summary %>%
  pivot_longer(-period, names_to = "metric", values_to = "value")

p_fragmentation <- ggplot(fragmentation_long, aes(x = metric, y = value, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Fragmentation indicators by period",
    x = "Metric",
    y = "Weighted mean",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_timeuse/16_fragmentation_summary_compare.png", p_fragmentation, width = 9, height = 5.5, dpi = 300)

# ============================================================
# 10. NTS travel context
# Important: NTS is trip-level and has trip purpose, not ICT.
# It is analysed separately as travel context only.
# ============================================================

nts <- read_dta("data/nts_trip_2014_5_2023.dta")

nts1 <- nts %>%
  mutate(
    SurveyYear = as.numeric(SurveyYear),
    TripPurpose_B04ID = as.numeric(TripPurpose_B04ID),
    MainMode_B04ID = as.numeric(MainMode_B04ID),
    trip_distance = as.numeric(TripDisIncSW),
    trip_time = as.numeric(TripTotalTime),
    wt = ifelse(is.na(W5), 1, as.numeric(W5)),
    nts_period = case_when(
      SurveyYear %in% c(2014, 2015) ~ "2014-2015",
      SurveyYear == 2023 ~ "2023",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(nts_period)) %>%
  mutate(
    nts_period = factor(nts_period, levels = c("2014-2015", "2023")),
    purpose_label = case_when(
      TripPurpose_B04ID == 1 ~ "Commuting",
      TripPurpose_B04ID == 2 ~ "Business",
      TripPurpose_B04ID == 3 ~ "Education / escort education",
      TripPurpose_B04ID == 4 ~ "Shopping",
      TripPurpose_B04ID == 5 ~ "Other escort",
      TripPurpose_B04ID == 6 ~ "Personal business",
      TripPurpose_B04ID == 7 ~ "Leisure",
      TripPurpose_B04ID == 8 ~ "Other including just walk",
      TRUE ~ NA_character_
    ),
    purpose_group = case_when(
      TripPurpose_B04ID %in% c(1, 2) ~ "work/business",
      TripPurpose_B04ID == 3 ~ "education",
      TripPurpose_B04ID == 4 ~ "shopping",
      TripPurpose_B04ID == 5 ~ "escort",
      TripPurpose_B04ID == 6 ~ "personal_business",
      TripPurpose_B04ID == 7 ~ "social & leisure",
      TripPurpose_B04ID == 8 ~ "other_walk",
      TRUE ~ NA_character_
    ),
    mode_label = case_when(
      MainMode_B04ID == 1 ~ "Walk",
      MainMode_B04ID == 2 ~ "Pedal cycle",
      MainMode_B04ID == 3 ~ "Car / van driver",
      MainMode_B04ID == 4 ~ "Car / van passenger",
      MainMode_B04ID == 5 ~ "Motorcycle",
      MainMode_B04ID == 6 ~ "Other private transport",
      MainMode_B04ID == 7 ~ "Bus in London",
      MainMode_B04ID == 8 ~ "Other local bus",
      MainMode_B04ID == 9 ~ "Non-local bus",
      MainMode_B04ID == 10 ~ "London Underground",
      MainMode_B04ID == 11 ~ "Surface Rail",
      MainMode_B04ID == 12 ~ "Taxi / minicab",
      MainMode_B04ID == 13 ~ "Other public transport",
      TRUE ~ NA_character_
    ),
    mode_group = case_when(
      MainMode_B04ID == 1 ~ "walking",
      MainMode_B04ID == 2 ~ "cycling",
      MainMode_B04ID %in% c(3, 4, 5, 6, 12) ~ "private_motorised",
      MainMode_B04ID %in% c(7, 8, 9) ~ "bus",
      MainMode_B04ID %in% c(10, 11) ~ "rail",
      MainMode_B04ID == 13 ~ "other_public_transport",
      TRUE ~ NA_character_
    ),
    nts_person_day = paste(IndividualID, DayID, sep = "_")
  )

write_csv(nts1, "combined_outputs_harmonised/tables/nts_clean_trip_level.csv")

# NTS purpose composition
nts_purpose <- nts1 %>%
  filter(!is.na(purpose_group)) %>%
  group_by(nts_period, purpose_group) %>%
  summarise(trips_w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(nts_period) %>%
  mutate(trip_share = trips_w / sum(trips_w, na.rm = TRUE)) %>%
  ungroup()

write_csv(nts_purpose, "combined_outputs_harmonised/tables/nts_trip_purpose_composition.csv")

p_nts_purpose <- ggplot(nts_purpose, aes(x = purpose_group, y = trip_share, fill = nts_period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "NTS travel context: trip purpose composition",
    x = "Trip purpose group",
    y = "Share of trips",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_nts/01_nts_trip_purpose_composition.png", p_nts_purpose, width = 10, height = 5, dpi = 300)

# NTS mode composition
nts_mode <- nts1 %>%
  filter(!is.na(mode_group)) %>%
  group_by(nts_period, mode_group) %>%
  summarise(trips_w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(nts_period) %>%
  mutate(mode_share = trips_w / sum(trips_w, na.rm = TRUE)) %>%
  ungroup()

write_csv(nts_mode, "combined_outputs_harmonised/tables/nts_mode_composition.csv")

p_nts_mode <- ggplot(nts_mode, aes(x = mode_group, y = mode_share, fill = nts_period)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "NTS travel context: mode composition",
    x = "Mode group",
    y = "Share of trips",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_nts/02_nts_mode_composition.png", p_nts_mode, width = 10, height = 5, dpi = 300)

# NTS person-day metrics
# Note: NTS is a 7-day travel diary. These are average per survey day / person-day, not directly comparable to TUS totals.
nts_person_day <- nts1 %>%
  group_by(nts_period, IndividualID, DayID) %>%
  summarise(
    wt = first(wt),
    n_trips = n(),
    total_trip_distance = sum(trip_distance, na.rm = TRUE),
    total_trip_time = sum(trip_time, na.rm = TRUE),
    .groups = "drop"
  )

nts_person_day_summary <- nts_person_day %>%
  group_by(nts_period) %>%
  summarise(
    avg_trips_per_person_day = weighted.mean(n_trips, wt, na.rm = TRUE),
    avg_distance_per_person_day = weighted.mean(total_trip_distance, wt, na.rm = TRUE),
    avg_travel_time_per_person_day = weighted.mean(total_trip_time, wt, na.rm = TRUE),
    raw_person_days = n(),
    .groups = "drop"
  )

write_csv(nts_person_day_summary, "combined_outputs_harmonised/tables/nts_person_day_summary.csv")

nts_person_day_long <- nts_person_day_summary %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "metric", values_to = "value")

p_nts_day_summary <- ggplot(nts_person_day_long, aes(x = metric, y = value, fill = nts_period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "NTS travel context: average person-day metrics",
    x = "Metric",
    y = "Weighted mean per person-day",
    fill = "Period"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_nts/03_nts_person_day_summary.png", p_nts_day_summary, width = 9, height = 5, dpi = 300)

# NTS purpose x mode
nts_purpose_mode <- nts1 %>%
  filter(!is.na(purpose_group), !is.na(mode_group)) %>%
  group_by(nts_period, purpose_group, mode_group) %>%
  summarise(trips_w = sum(wt, na.rm = TRUE), .groups = "drop") %>%
  group_by(nts_period, purpose_group) %>%
  mutate(mode_share_within_purpose = trips_w / sum(trips_w, na.rm = TRUE)) %>%
  ungroup()

write_csv(nts_purpose_mode, "combined_outputs_harmonised/tables/nts_purpose_mode_composition.csv")

p_nts_purpose_mode <- ggplot(nts_purpose_mode, aes(x = purpose_group, y = mode_share_within_purpose, fill = mode_group)) +
  geom_col(position = "fill") +
  facet_wrap(~nts_period) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "NTS travel context: mode composition within trip purpose",
    x = "Trip purpose group",
    y = "Mode share within purpose",
    fill = "Mode group"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_nts/04_nts_purpose_mode_composition.png", p_nts_purpose_mode, width = 12, height = 6, dpi = 300)

# ============================================================
# 11. Carefully aligned time-use and NTS context table
# Do NOT directly equate time-use activity purpose with NTS trip purpose.
# This table places them side-by-side as context only.
# ============================================================

timeuse_context <- activity_summary %>%
  filter(activity %in% c("work", "shopping", "social & leisure", "travel")) %>%
  transmute(
    period = as.character(period),
    domain = as.character(activity),
    source = "Time-use activity time share",
    value = time_share
  )

nts_context <- nts_purpose %>%
  filter(purpose_group %in% c("work/business", "shopping", "social & leisure")) %>%
  mutate(
    domain = case_when(
      purpose_group == "work/business" ~ "work",
      TRUE ~ purpose_group
    )
  ) %>%
  transmute(
    period = as.character(nts_period),
    domain = domain,
    source = "NTS trip purpose share",
    value = trip_share
  )

context_compare <- bind_rows(timeuse_context, nts_context)
write_csv(context_compare, "combined_outputs_harmonised/tables/context_timeuse_vs_nts_not_direct_causal.csv")

p_context <- ggplot(context_compare, aes(x = domain, y = value, fill = source)) +
  geom_col(position = "dodge") +
  facet_wrap(~period) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Time-use and NTS side-by-side context only",
    subtitle = "Time-use activity time share and NTS trip purpose share have different meanings",
    x = "Domain",
    y = "Share",
    fill = "Source"
  ) +
  presentation_theme

ggsave("combined_outputs_harmonised/figures_nts/05_timeuse_nts_context_side_by_side.png", p_context, width = 11, height = 5.5, dpi = 300)


# ============================================================
# 12. Difference plots: 2023 minus 2014-2015
# Purpose: create explicit "change" figures for each key comparison step
# ============================================================

dir.create("combined_outputs_harmonised/figures_differences", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Significance tests for difference plots
# Tests are based on diary-day level values, not raw totals.
# Stars:
#   *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, ns = not significant
# ------------------------------------------------------------

# Activity-level daily metrics for D01-D03 and D06
activity_daily_for_sig <- timeuse_all %>%
  group_by(period, person_day, wt, activity) %>%
  summarise(
    total_minutes = sum(time, na.rm = TRUE),
    ict_minutes = sum(time * any_ict, na.rm = TRUE),
    n_episodes = n(),
    .groups = "drop"
  ) %>%
  right_join(
    timeuse_all %>%
      distinct(period, person_day, wt) %>%
      tidyr::crossing(activity = factor(activity_levels, levels = activity_levels)),
    by = c("period", "person_day", "wt", "activity")
  ) %>%
  group_by(period, person_day, wt) %>%
  mutate(
    total_day_minutes_all_activities = sum(replace_na(total_minutes, 0), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    total_minutes = replace_na(total_minutes, 0),
    ict_minutes = replace_na(ict_minutes, 0),
    n_episodes = replace_na(n_episodes, 0),
    daily_time_share = total_minutes / total_day_minutes_all_activities,
    daily_ict_intensity = ifelse(total_minutes > 0, ict_minutes / total_minutes, NA_real_),
    daily_ict_minutes = ict_minutes,
    daily_avg_minutes = total_minutes
  )

activity_diff_sig <- activity_daily_for_sig %>%
  pivot_longer(
    cols = c(daily_time_share, daily_avg_minutes, daily_ict_intensity, daily_ict_minutes),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  group_by(activity, metric_raw) %>%
  summarise(
    p_value = sig_p(value, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  ) %>%
  mutate(
    metric = recode(
      metric_raw,
      daily_time_share = "Time share",
      daily_avg_minutes = "Average minutes per diary-day",
      daily_ict_intensity = "ICT intensity",
      daily_ict_minutes = "ICT minutes per diary-day"
    )
  )

write_csv(
  activity_diff_sig,
  "combined_outputs_harmonised/tables/diff_activity_metrics_significance_tests.csv"
)

# Activity all-vs-ICT daily metrics for D04-D06
activity_all_ict_daily_sig <- bind_rows(
  activity_daily_for_sig %>%
    transmute(
      period, person_day, wt, activity,
      type = "All activities",
      episode_count = n_episodes,
      minutes = total_minutes
    ),
  activity_daily_for_sig %>%
    transmute(
      period, person_day, wt, activity,
      type = "ICT-involved",
      episode_count = ifelse(ict_minutes > 0, n_episodes, 0),
      minutes = ict_minutes
    )
)

activity_all_ict_sig <- activity_all_ict_daily_sig %>%
  pivot_longer(
    cols = c(episode_count, minutes),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  group_by(activity, type, metric_raw) %>%
  summarise(
    p_value = sig_p(value, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  )

write_csv(
  activity_all_ict_sig,
  "combined_outputs_harmonised/tables/diff_activity_all_vs_ict_significance_tests.csv"
)

# Location x activity daily metrics for D07-D11
locact_daily_for_sig <- timeuse_all %>%
  group_by(period, person_day, wt, location, activity) %>%
  summarise(
    total_minutes = sum(time, na.rm = TRUE),
    ict_minutes = sum(time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(
    timeuse_all %>%
      distinct(period, person_day, wt) %>%
      tidyr::crossing(
        location = factor(location_levels, levels = location_levels),
        activity = factor(activity_levels, levels = activity_levels)
      ),
    by = c("period", "person_day", "wt", "location", "activity")
  ) %>%
  group_by(period, person_day, wt) %>%
  mutate(
    total_day_minutes_all_activities = sum(replace_na(total_minutes, 0), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    total_minutes = replace_na(total_minutes, 0),
    ict_minutes = replace_na(ict_minutes, 0),
    daily_time_share = total_minutes / total_day_minutes_all_activities,
    daily_avg_minutes = total_minutes,
    daily_ict_minutes = ict_minutes,
    daily_ict_intensity = ifelse(total_minutes > 0, ict_minutes / total_minutes, NA_real_)
  )

locact_diff_sig <- locact_daily_for_sig %>%
  pivot_longer(
    cols = c(daily_time_share, daily_avg_minutes, daily_ict_minutes, daily_ict_intensity),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  group_by(location, activity, metric_raw) %>%
  summarise(
    p_value = sig_p(value, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  ) %>%
  mutate(
    metric = recode(
      metric_raw,
      daily_time_share = "Time share",
      daily_avg_minutes = "Average minutes per diary-day",
      daily_ict_minutes = "ICT minutes per diary-day",
      daily_ict_intensity = "ICT intensity"
    )
  )

write_csv(
  locact_diff_sig,
  "combined_outputs_harmonised/tables/diff_location_activity_significance_tests.csv"
)

# -------------------------
# 12.1 Activity-level differences
# -------------------------
activity_diff_long <- activity_change %>%
  select(
    activity,
    diff_time_share,
    diff_avg_minutes_per_day,
    diff_avg_duration,
    diff_ict_intensity,
    diff_ict_minutes_per_day
  ) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "metric",
    values_to = "change"
  ) %>%
  mutate(
    metric = recode(
      metric,
      diff_time_share = "Time share",
      diff_avg_minutes_per_day = "Average minutes per diary-day",
      diff_avg_duration = "Average episode duration",
      diff_ict_intensity = "ICT intensity",
      diff_ict_minutes_per_day = "ICT minutes per diary-day"
    )
  )

write_csv(activity_diff_long, "combined_outputs_harmonised/tables/diff_activity_metrics_2023_minus_2014_2015.csv")

activity_change_sig <- activity_change %>%
  left_join(
    activity_diff_sig %>%
      filter(metric == "Time share") %>%
      select(activity, sig_time_share = sig),
    by = "activity"
  ) %>%
  left_join(
    activity_diff_sig %>%
      filter(metric == "Average minutes per diary-day") %>%
      select(activity, sig_avg_minutes = sig),
    by = "activity"
  ) %>%
  left_join(
    activity_diff_sig %>%
      filter(metric == "ICT intensity") %>%
      select(activity, sig_ict_intensity = sig),
    by = "activity"
  ) %>%
  mutate(
    label_y_time_share = diff_time_share + ifelse(diff_time_share >= 0, 1, -1) *
      0.04 * max(abs(diff_time_share), na.rm = TRUE),
    label_y_avg_minutes = diff_avg_minutes_per_day + ifelse(diff_avg_minutes_per_day >= 0, 1, -1) *
      0.04 * max(abs(diff_avg_minutes_per_day), na.rm = TRUE),
    label_y_ict_intensity = diff_ict_intensity + ifelse(diff_ict_intensity >= 0, 1, -1) *
      0.04 * max(abs(diff_ict_intensity), na.rm = TRUE)
  )

# Figure D1: activity time share change
p_diff_activity_share <- ggplot(
  activity_change_sig,
  aes(x = activity, y = diff_time_share)
) +
  geom_col() +
  geom_text(aes(y = label_y_time_share, label = sig_time_share), size = 5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in activity time share (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in time share (percentage points)"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D01_change_activity_time_share.png",
  p_diff_activity_share,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure D2: activity average minutes per diary-day change
p_diff_activity_minutes <- ggplot(
  activity_change_sig,
  aes(x = activity, y = diff_avg_minutes_per_day)
) +
  geom_col() +
  geom_text(aes(y = label_y_avg_minutes, label = sig_avg_minutes), size = 5) +
  labs(
    title = "Change in average minutes per diary-day (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in minutes per diary-day"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D02_change_activity_avg_minutes_per_day.png",
  p_diff_activity_minutes,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure D3: activity ICT intensity change
p_diff_activity_ict_intensity <- ggplot(
  activity_change_sig,
  aes(x = activity, y = diff_ict_intensity)
) +
  geom_col() +
  geom_text(aes(y = label_y_ict_intensity, label = sig_ict_intensity), size = 5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in ICT intensity by activity (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in ICT share (percentage points)"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D03_change_activity_ict_intensity.png",
  p_diff_activity_ict_intensity,
  width = 10,
  height = 6,
  dpi = 300
)

# -------------------------
# 12.2 All vs ICT absolute metric differences
# Annualised totals are used because 2014-2015 covers two years.
# -------------------------
activity_compare_abs_diff <- activity_compare_abs %>%
  select(period, activity, type, weighted_episodes_annual, total_time_annual, avg_minutes_per_diary_day) %>%
  pivot_wider(
    names_from = period,
    values_from = c(weighted_episodes_annual, total_time_annual, avg_minutes_per_diary_day)
  ) %>%
  mutate(
    diff_weighted_episodes_annual = `weighted_episodes_annual_2023` - `weighted_episodes_annual_2014-2015`,
    diff_total_time_annual = `total_time_annual_2023` - `total_time_annual_2014-2015`,
    diff_avg_minutes_per_diary_day = `avg_minutes_per_diary_day_2023` - `avg_minutes_per_diary_day_2014-2015`
  )

write_csv(activity_compare_abs_diff, "combined_outputs_harmonised/tables/diff_activity_all_vs_ict_absolute_metrics.csv")

activity_compare_abs_diff <- activity_compare_abs_diff %>%
  left_join(
    activity_all_ict_sig %>%
      filter(metric_raw == "episode_count") %>%
      select(activity, type, sig_count = sig),
    by = c("activity", "type")
  ) %>%
  left_join(
    activity_all_ict_sig %>%
      filter(metric_raw == "minutes") %>%
      select(activity, type, sig_minutes = sig),
    by = c("activity", "type")
  ) %>%
  mutate(
    sig_flag_count = ifelse(sig_count %in% c("***", "**", "*"), "p < 0.05", "not significant"),
    sig_flag_minutes = ifelse(sig_minutes %in% c("***", "**", "*"), "p < 0.05", "not significant")
  )

p_diff_abs_count <- ggplot(
  activity_compare_abs_diff,
  aes(x = activity, y = diff_weighted_episodes_annual, fill = type, pattern = sig_flag_count)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  labs(
    title = "Change in annual weighted episode count (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in annual weighted episode count",
    fill = ""
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D04_change_annual_weighted_episode_count_all_vs_ict.png",
  p_diff_abs_count,
  width = 10,
  height = 5,
  dpi = 300
)

p_diff_abs_time <- ggplot(
  activity_compare_abs_diff,
  aes(x = activity, y = diff_total_time_annual, fill = type, pattern = sig_flag_minutes)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  labs(
    title = "Change in annual total weighted time (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in annual total weighted time (minutes)",
    fill = ""
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D05_change_annual_total_weighted_time_all_vs_ict.png",
  p_diff_abs_time,
  width = 10,
  height = 5,
  dpi = 300
)

p_diff_abs_minutes_day <- ggplot(
  activity_compare_abs_diff,
  aes(x = type, y = diff_avg_minutes_per_diary_day, fill = activity, pattern = sig_flag_minutes)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  labs(
    title = "Change in average minutes per diary-day: all vs ICT (2023 - 2014-15)",
    x = "Activity purpose",
    y = "Change in minutes per diary-day",
    fill = ""
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D06_change_avg_minutes_per_day_all_vs_ict.png",
  p_diff_abs_minutes_day,
  width = 10,
  height = 5,
  dpi = 300
)

# -------------------------
# 12.3 Location x activity differences
# Revised axis:
#   x = location
#   fill = activity purpose
# This makes travel location show all activity purposes that occur while travelling.
# -------------------------
locact_diff_plot <- locact_change %>%
  mutate(
    location_simple = recode(as.character(location),
                             home = "home",
                             work_or_school = "work/school",
                             travel = "travel",
                             shopping_services = "shopping/services",
                             restaurant_cafe_pub = "restaurant/cafe/pub",
                             other = "other"),
    location_simple = factor(
      location_simple,
      levels = c("home", "work/school", "travel",
                 "shopping/services", "restaurant/cafe/pub", "other")
    ),
    activity = factor(activity, levels = activity_levels)
  )

write_csv(locact_diff_plot, "combined_outputs_harmonised/tables/diff_location_activity_metrics_2023_minus_2014_2015.csv")

locact_diff_plot <- locact_diff_plot %>%
  left_join(
    locact_diff_sig %>%
      filter(metric == "ICT intensity") %>%
      select(location, activity, sig_ict_intensity = sig),
    by = c("location", "activity")
  ) %>%
  left_join(
    locact_diff_sig %>%
      filter(metric == "Average minutes per diary-day") %>%
      select(location, activity, sig_avg_minutes = sig),
    by = c("location", "activity")
  ) %>%
  left_join(
    locact_diff_sig %>%
      filter(metric == "ICT minutes per diary-day") %>%
      select(location, activity, sig_ict_minutes = sig),
    by = c("location", "activity")
  ) %>%
  left_join(
    locact_diff_sig %>%
      filter(metric == "Time share") %>%
      select(location, activity, sig_time_share = sig),
    by = c("location", "activity")
  ) %>%
  mutate(
    sig_flag_ict_intensity = ifelse(sig_ict_intensity %in% c("***", "**", "*"), "p < 0.05", "not significant"),
    sig_flag_avg_minutes = ifelse(sig_avg_minutes %in% c("***", "**", "*"), "p < 0.05", "not significant"),
    sig_flag_ict_minutes = ifelse(sig_ict_minutes %in% c("***", "**", "*"), "p < 0.05", "not significant"),
    sig_flag_time_share = ifelse(sig_time_share %in% c("***", "**", "*"), "p < 0.05", "not significant")
  )

# Figure D7: change in ICT use / intensity by location x activity
p_diff_locact_ict_intensity <- ggplot(
  locact_diff_plot,
  aes(x = location_simple, y = diff_ict_intensity, fill = activity, pattern = sig_flag_ict_intensity)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in ICT use by location and activity (2023 - 2014-15)",
    x = "Location",
    y = "Change in ICT time share",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D07_change_ict_use_location_activity_bar.png",
  p_diff_locact_ict_intensity,
  width = 12,
  height = 6,
  dpi = 300
)

# Figure D8: same as D7, but excluding home x personal care for clearer PPT display
p_diff_locact_ict_intensity_no_home_pc <- ggplot(
  locact_diff_plot %>% filter(!(location == "home" & activity == "personal care")),
  aes(x = location_simple, y = diff_ict_intensity, fill = activity, pattern = sig_flag_ict_intensity)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in ICT use by location and activity (excluding home personal care)",
    x = "Location",
    y = "Change in ICT time share",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D08_change_ict_use_location_activity_no_home_personal_care.png",
  p_diff_locact_ict_intensity_no_home_pc,
  width = 12,
  height = 6,
  dpi = 300
)

# Figure D9: change in average minutes per diary-day by location x activity
p_diff_locact_minutes <- ggplot(
  locact_diff_plot,
  aes(x = location_simple, y = diff_avg_minutes_per_day, fill = activity, pattern = sig_flag_avg_minutes)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  labs(
    title = "Change in average minutes per diary-day by location and activity",
    x = "Location",
    y = "Change in minutes per diary-day",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D09_change_avg_minutes_location_activity_bar.png",
  p_diff_locact_minutes,
  width = 12,
  height = 6,
  dpi = 300
)

# Figure D10: change in ICT minutes per diary-day by location x activity
p_diff_locact_ict_minutes <- ggplot(
  locact_diff_plot,
  aes(x = location_simple, y = diff_ict_minutes_per_day, fill = activity, pattern = sig_flag_ict_minutes)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  labs(
    title = "Change in ICT minutes per diary-day by location and activity",
    x = "Location",
    y = "Change in ICT-involved minutes per diary-day",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D10_change_ict_minutes_location_activity_bar.png",
  p_diff_locact_ict_minutes,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 12.35 Overall time-share change by location x activity
# Not ICT-specific
# ============================================================

p_diff_locact_time_share <- ggplot(
  locact_diff_plot,
  aes(x = location_simple, y = diff_time_share, fill = activity, pattern = sig_flag_time_share)
) +
  geom_col_sig(position = "dodge") +
  sig_pattern_scale +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Change in overall time share by location and activity (2023 - 2014-15)",
    x = "Location",
    y = "Change in share of total weighted time",
    fill = "Activity purpose"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D11_change_overall_time_share_location_activity.png",
  p_diff_locact_time_share,
  width = 12,
  height = 6,
  dpi = 300
)


# -------------------------
# 12.4 Hourly difference: 2023 minus 2014-2015
# -------------------------
hour_ict_diff <- hour_ict %>%
  select(period, hour, ict_share) %>%
  pivot_wider(names_from = period, values_from = ict_share) %>%
  mutate(diff_ict_share = `2023` - `2014-2015`)

write_csv(hour_ict_diff, "combined_outputs_harmonised/tables/diff_hourly_ict_share_2023_minus_2014_2015.csv")

p_diff_hour_ict <- ggplot(hour_ict_diff, aes(x = hour, y = diff_ict_share)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Hourly change in ICT share (2023 - 2014-15)",
    x = "Hour of day",
    y = "Change in ICT time share"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D11_hourly_change_ict_share.png",
  p_diff_hour_ict,
  width = 10,
  height = 6,
  dpi = 300
)

# -------------------------
# 12.5 NTS difference plots
# -------------------------
nts_purpose_diff <- nts_purpose %>%
  select(nts_period, purpose_group, trip_share) %>%
  pivot_wider(names_from = nts_period, values_from = trip_share) %>%
  mutate(diff_trip_share = `2023` - `2014-2015`)

write_csv(nts_purpose_diff, "combined_outputs_harmonised/tables/diff_nts_trip_purpose_share.csv")

p_diff_nts_purpose <- ggplot(
  nts_purpose_diff,
  aes(x = fct_reorder(purpose_group, diff_trip_share), y = diff_trip_share)
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "NTS: change in trip purpose share (2023 - 2014-15)",
    x = "Trip purpose group",
    y = "Change in trip share"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D12_nts_change_trip_purpose_share.png",
  p_diff_nts_purpose,
  width = 10,
  height = 6,
  dpi = 300
)

nts_mode_diff <- nts_mode %>%
  select(nts_period, mode_group, mode_share) %>%
  pivot_wider(names_from = nts_period, values_from = mode_share) %>%
  mutate(diff_mode_share = `2023` - `2014-2015`)

write_csv(nts_mode_diff, "combined_outputs_harmonised/tables/diff_nts_mode_share.csv")

p_diff_nts_mode <- ggplot(
  nts_mode_diff,
  aes(x = fct_reorder(mode_group, diff_mode_share), y = diff_mode_share)
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "NTS: change in mode share (2023 - 2014-15)",
    x = "Mode group",
    y = "Change in mode share"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D13_nts_change_mode_share.png",
  p_diff_nts_mode,
  width = 10,
  height = 6,
  dpi = 300
)

nts_day_diff <- nts_person_day_summary %>%
  select(nts_period, avg_trips_per_person_day, avg_distance_per_person_day, avg_travel_time_per_person_day) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = nts_period, values_from = value) %>%
  mutate(diff_value = `2023` - `2014-2015`)

write_csv(nts_day_diff, "combined_outputs_harmonised/tables/diff_nts_person_day_metrics.csv")

p_diff_nts_day <- ggplot(
  nts_day_diff,
  aes(x = metric, y = diff_value)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "NTS: change in person-day travel metrics (2023 - 2014-15)",
    x = "Metric",
    y = "Change in weighted mean per person-day"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_differences/D14_nts_change_person_day_metrics.png",
  p_diff_nts_day,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 12. Additional figures for descriptive analysis mechanism
# Purpose:
#   1) Decompose activity changes
#   2) Show activity relocation across locations
#   3) Show episode duration distribution
#   4) Show ICT across locations, especially travel ICT
# ============================================================

dir.create("combined_outputs_harmonised/figures_mechanism", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# M1. Decomposition table / chart
# Average minutes per diary-day =
# participation rate × episodes per active diary-day × average duration
# ------------------------------------------------------------

activity_day <- timeuse_all %>%
  group_by(period, person_day, wt, activity) %>%
  summarise(
    n_episodes = n(),
    total_minutes = sum(time, na.rm = TRUE),
    weighted_minutes = sum(weighted_time, na.rm = TRUE),
    .groups = "drop"
  )

activity_decomp <- activity_day %>%
  group_by(period, activity) %>%
  summarise(
    active_weighted_days = sum(wt, na.rm = TRUE),
    weighted_episodes = sum(n_episodes * wt, na.rm = TRUE),
    total_weighted_time = sum(weighted_minutes, na.rm = TRUE),
    avg_duration_per_episode = total_weighted_time / weighted_episodes,
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    participation_rate = active_weighted_days / weighted_diary_days,
    episodes_per_active_day = weighted_episodes / active_weighted_days,
    avg_minutes_per_diary_day = total_weighted_time / weighted_diary_days
  )

write_csv(
  activity_decomp,
  "combined_outputs_harmonised/tables/M1_activity_decomposition_by_period.csv"
)

activity_decomp_change <- activity_decomp %>%
  select(
    period, activity,
    participation_rate,
    episodes_per_active_day,
    avg_duration_per_episode,
    avg_minutes_per_diary_day
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(
      participation_rate,
      episodes_per_active_day,
      avg_duration_per_episode,
      avg_minutes_per_diary_day
    )
  ) %>%
  mutate(
    diff_participation_rate =
      participation_rate_2023 - `participation_rate_2014-2015`,
    diff_episodes_per_active_day =
      episodes_per_active_day_2023 - `episodes_per_active_day_2014-2015`,
    diff_avg_duration_per_episode =
      avg_duration_per_episode_2023 - `avg_duration_per_episode_2014-2015`,
    diff_avg_minutes_per_diary_day =
      avg_minutes_per_diary_day_2023 - `avg_minutes_per_diary_day_2014-2015`
  )

write_csv(
  activity_decomp_change,
  "combined_outputs_harmonised/tables/M1_activity_decomposition_change_2023_minus_2014_2015.csv"
)

# Significance tests for M1 indicators
activity_day_full <- timeuse_all %>%
  distinct(period, person_day, wt) %>%
  tidyr::crossing(activity = factor(activity_levels, levels = activity_levels)) %>%
  left_join(
    activity_day %>%
      mutate(activity = factor(activity, levels = activity_levels)),
    by = c("period", "person_day", "wt", "activity")
  ) %>%
  mutate(
    n_episodes = replace_na(n_episodes, 0),
    total_minutes = replace_na(total_minutes, 0),
    participation_rate = ifelse(n_episodes > 0, 1, 0),
    episodes_per_active_day = ifelse(n_episodes > 0, n_episodes, NA_real_),
    avg_duration_per_episode = ifelse(n_episodes > 0, total_minutes / n_episodes, NA_real_),
    avg_minutes_per_diary_day = total_minutes
  )

activity_decomp_sig <- activity_day_full %>%
  pivot_longer(
    cols = c(participation_rate, episodes_per_active_day,
             avg_duration_per_episode, avg_minutes_per_diary_day),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  group_by(activity, metric_raw) %>%
  summarise(
    p_value = sig_p(value, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  ) %>%
  mutate(
    metric = recode(
      metric_raw,
      participation_rate = "Participation rate",
      episodes_per_active_day = "Episodes per active diary-day",
      avg_duration_per_episode = "Average duration per episode",
      avg_minutes_per_diary_day = "Average minutes per diary-day"
    )
  )

write_csv(
  activity_decomp_sig,
  "combined_outputs_harmonised/tables/M1_activity_decomposition_significance_tests.csv"
)

activity_decomp_change_long <- activity_decomp_change %>%
  select(
    activity,
    diff_participation_rate,
    diff_episodes_per_active_day,
    diff_avg_duration_per_episode,
    diff_avg_minutes_per_diary_day
  ) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "metric",
    values_to = "change"
  ) %>%
  mutate(
    metric = recode(
      metric,
      diff_participation_rate = "Participation rate",
      diff_episodes_per_active_day = "Episodes per active diary-day",
      diff_avg_duration_per_episode = "Average duration per episode",
      diff_avg_minutes_per_diary_day = "Average minutes per diary-day"
    )
  ) %>%
  left_join(
    activity_decomp_sig %>% select(activity, metric, p_value, sig),
    by = c("activity", "metric")
  ) %>%
  group_by(metric) %>%
  mutate(
    label_y = change + ifelse(change >= 0, 1, -1) *
      0.04 * max(abs(change), na.rm = TRUE)
  ) %>%
  ungroup()

p_m1_decomp <- ggplot(
  activity_decomp_change_long,
  aes(x = activity, y = change)
) +
  geom_col() +
  geom_text(aes(y = label_y, label = sig), size = 5) +
  facet_wrap(~ metric, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Decomposition of activity change: 2023 minus 2014-2015",
    x = "Activity purpose",
    y = "Change (minutes / episodes / percentage points)"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M1_activity_decomposition_change.png",
  p_m1_decomp,
  width = 11,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# M1a. Same decomposition, but saved as four separate figures
# Easier to use in PPT than one four-panel figure
# ------------------------------------------------------------

m1_metric_order <- c(
  "Average minutes per diary-day",
  "Average duration per episode",
  "Episodes per active diary-day",
  "Participation rate"
)

activity_decomp_change_long <- activity_decomp_change_long %>%
  mutate(metric = factor(metric, levels = m1_metric_order))

for (m in m1_metric_order) {
  p_m1_single <- activity_decomp_change_long %>%
    filter(metric == m) %>%
    ggplot(aes(x = activity, y = change)) +
    geom_col() +
    geom_text(aes(y = label_y, label = sig), size = 5) +
    coord_flip() +
    labs(
      title = paste0("Change in ", m),
      subtitle = "2023 minus 2014-2015",
      x = "Activity purpose",
      y = metric_axis_label(m)
    ) +
    presentation_theme
  
  ggsave(
    paste0(
      "combined_outputs_harmonised/figures_mechanism/M1_split_",
      str_replace_all(str_to_lower(m), "[^a-z0-9]+", "_"),
      ".png"
    ),
    p_m1_single,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# ------------------------------------------------------------
# M1b. Decomposition with location added
# This follows the same idea as M2, but uses the decomposition indicators
# by activity x location.
# ------------------------------------------------------------

locact_day <- timeuse_all %>%
  group_by(period, person_day, wt, activity, location) %>%
  summarise(
    n_episodes = n(),
    total_minutes = sum(time, na.rm = TRUE),
    weighted_minutes = sum(weighted_time, na.rm = TRUE),
    .groups = "drop"
  )

locact_decomp <- locact_day %>%
  group_by(period, activity, location) %>%
  summarise(
    active_weighted_days = sum(wt, na.rm = TRUE),
    weighted_episodes = sum(n_episodes * wt, na.rm = TRUE),
    total_weighted_time = sum(weighted_minutes, na.rm = TRUE),
    avg_duration_per_episode = total_weighted_time / weighted_episodes,
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    participation_rate = active_weighted_days / weighted_diary_days,
    episodes_per_active_day = weighted_episodes / active_weighted_days,
    avg_minutes_per_diary_day = total_weighted_time / weighted_diary_days
  )

write_csv(
  locact_decomp,
  "combined_outputs_harmonised/tables/M1b_location_activity_decomposition_by_period.csv"
)

locact_decomp_change <- locact_decomp %>%
  select(
    period, activity, location,
    participation_rate,
    episodes_per_active_day,
    avg_duration_per_episode,
    avg_minutes_per_diary_day
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(
      participation_rate,
      episodes_per_active_day,
      avg_duration_per_episode,
      avg_minutes_per_diary_day
    ),
    values_fill = 0
  ) %>%
  mutate(
    diff_participation_rate =
      participation_rate_2023 - `participation_rate_2014-2015`,
    diff_episodes_per_active_day =
      episodes_per_active_day_2023 - `episodes_per_active_day_2014-2015`,
    diff_avg_duration_per_episode =
      avg_duration_per_episode_2023 - `avg_duration_per_episode_2014-2015`,
    diff_avg_minutes_per_diary_day =
      avg_minutes_per_diary_day_2023 - `avg_minutes_per_diary_day_2014-2015`
  )

write_csv(
  locact_decomp_change,
  "combined_outputs_harmonised/tables/M1b_location_activity_decomposition_change_2023_minus_2014_2015.csv"
)

# Significance tests for M1b location x activity indicators
locact_day_full <- timeuse_all %>%
  distinct(period, person_day, wt) %>%
  tidyr::crossing(
    activity = factor(activity_levels, levels = activity_levels),
    location = factor(location_levels, levels = location_levels)
  ) %>%
  left_join(
    locact_day %>%
      mutate(
        activity = factor(activity, levels = activity_levels),
        location = factor(location, levels = location_levels)
      ),
    by = c("period", "person_day", "wt", "activity", "location")
  ) %>%
  mutate(
    n_episodes = replace_na(n_episodes, 0),
    total_minutes = replace_na(total_minutes, 0),
    participation_rate = ifelse(n_episodes > 0, 1, 0),
    episodes_per_active_day = ifelse(n_episodes > 0, n_episodes, NA_real_),
    avg_duration_per_episode = ifelse(n_episodes > 0, total_minutes / n_episodes, NA_real_),
    avg_minutes_per_diary_day = total_minutes
  )

locact_decomp_sig <- locact_day_full %>%
  pivot_longer(
    cols = c(participation_rate, episodes_per_active_day,
             avg_duration_per_episode, avg_minutes_per_diary_day),
    names_to = "metric_raw",
    values_to = "value"
  ) %>%
  group_by(activity, location, metric_raw) %>%
  summarise(
    p_value = sig_p(value, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  ) %>%
  mutate(
    metric = recode(
      metric_raw,
      participation_rate = "Participation rate",
      episodes_per_active_day = "Episodes per active diary-day",
      avg_duration_per_episode = "Average duration per episode",
      avg_minutes_per_diary_day = "Average minutes per diary-day"
    )
  )

write_csv(
  locact_decomp_sig,
  "combined_outputs_harmonised/tables/M1b_location_activity_decomposition_significance_tests.csv"
)

locact_decomp_change_long <- locact_decomp_change %>%
  select(
    activity, location,
    diff_participation_rate,
    diff_episodes_per_active_day,
    diff_avg_duration_per_episode,
    diff_avg_minutes_per_diary_day
  ) %>%
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "metric",
    values_to = "change"
  ) %>%
  mutate(
    metric = recode(
      metric,
      diff_participation_rate = "Participation rate",
      diff_episodes_per_active_day = "Episodes per active diary-day",
      diff_avg_duration_per_episode = "Average duration per episode",
      diff_avg_minutes_per_diary_day = "Average minutes per diary-day"
    ),
    metric = factor(metric, levels = m1_metric_order)
  ) %>%
  left_join(
    locact_decomp_sig %>% select(activity, location, metric, p_value, sig),
    by = c("activity", "location", "metric")
  )

for (m in m1_metric_order) {
  p_m1_loc_single <- plot_location_decomposition(locact_decomp_change_long, m) +
    labs(x = metric_axis_label(m))

  ggsave(
    paste0(
      "combined_outputs_harmonised/figures_mechanism/M1b_location_split_",
      str_replace_all(str_to_lower(m), "[^a-z0-9]+", "_"),
      ".png"
    ),
    p_m1_loc_single,
    width = 10,
    height = 6,
    dpi = 300
  )
}


# ------------------------------------------------------------
# M2. Activity relocation difference plot
# Shows whether activities moved between home, work/school, travel, and other locations
# ------------------------------------------------------------

location_activity_summary <- timeuse_all %>%
  group_by(period, activity, location) %>%
  summarise(
    total_weighted_time = sum(weighted_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator, by = "period") %>%
  mutate(
    avg_minutes_per_diary_day = total_weighted_time / weighted_diary_days
  )

location_activity_change <- location_activity_summary %>%
  select(period, activity, location, avg_minutes_per_diary_day) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_minutes_per_diary_day,
    values_fill = 0
  ) %>%
  mutate(
    diff_minutes_per_day = `2023` - `2014-2015`
  )

write_csv(
  location_activity_change,
  "combined_outputs_harmonised/tables/M2_location_activity_relocation_change.csv"
)

# Significance tests for M2 relocation differences
location_activity_daily <- locact_day_full %>%
  mutate(avg_minutes_per_diary_day = total_minutes)

location_activity_sig <- location_activity_daily %>%
  group_by(activity, location) %>%
  summarise(
    p_value = sig_p(avg_minutes_per_diary_day, wt, person_day, period),
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = stats::p.adjust(p_value, method = "BH"),
    sig = p_to_stars(p_adj)
  )

write_csv(
  location_activity_sig,
  "combined_outputs_harmonised/tables/M2_location_activity_relocation_significance_tests.csv"
)

location_activity_change <- location_activity_change %>%
  left_join(location_activity_sig, by = c("activity", "location")) %>%
  mutate(sig_flag = ifelse(sig %in% c("***", "**", "*"), "p < 0.05", "not significant"))

# Use the same location-decomposition plotting function as the M1b figures.
# This keeps M2 visually and numerically consistent with
# M1b_location_split_average_minutes_per_diary_day.
p_m2_relocation <- plot_location_decomposition(
  locact_decomp_change_long,
  "Average minutes per diary-day"
) +
  labs(
    title = "Activity relocation: change in average minutes per diary-day",
    x = "Change in minutes per diary-day"
  )

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M2_activity_relocation_difference_plot.png",
  p_m2_relocation,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# M3. Episode duration distribution
# Standardised as weighted episodes per diary-day
# ------------------------------------------------------------

duration_plot_data <- timeuse_all %>%
  filter(!is.na(time), time > 0, time <= 240) %>%
  mutate(
    duration_bin = cut(
      time,
      breaks = seq(0, 240, by = 10),
      include.lowest = TRUE,
      right = FALSE
    ),
    duration_mid = as.numeric(sub("\\[(\\d+),.*", "\\1", duration_bin)) + 5
  )

# denominator: weighted diary-days by period
duration_day_denominator <- timeuse_all %>%
  distinct(period, person_day, wt) %>%
  group_by(period) %>%
  summarise(
    weighted_diary_days = sum(wt, na.rm = TRUE),
    .groups = "drop"
  )

# Overall: episodes per diary-day
duration_dist_overall <- duration_plot_data %>%
  group_by(period, duration_mid) %>%
  summarise(
    weighted_episodes = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(duration_day_denominator, by = "period") %>%
  mutate(
    episodes_per_diary_day = weighted_episodes / weighted_diary_days
  )

write_csv(
  duration_dist_overall,
  "combined_outputs_harmonised/tables/M3_episode_duration_distribution_overall_per_day.csv"
)

p_m3_duration_overall <- ggplot(
  duration_dist_overall,
  aes(x = duration_mid, y = episodes_per_diary_day, colour = period)
) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.2) +
  labs(
    title = "Episode duration distribution",
    subtitle = "Weighted episodes per diary-day; episodes longer than 240 minutes excluded",
    x = "Episode duration (minutes)",
    y = "Weighted episodes per diary-day",
    colour = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M3_episode_duration_distribution_overall_per_day.png",
  p_m3_duration_overall,
  width = 10,
  height = 6,
  dpi = 300
)

# By activity: episodes per diary-day
duration_dist_by_activity <- duration_plot_data %>%
  group_by(period, activity, duration_mid) %>%
  summarise(
    weighted_episodes = sum(wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(duration_day_denominator, by = "period") %>%
  mutate(
    episodes_per_diary_day = weighted_episodes / weighted_diary_days
  )

write_csv(
  duration_dist_by_activity,
  "combined_outputs_harmonised/tables/M3_episode_duration_distribution_by_activity_per_day.csv"
)

p_m3_duration_by_activity <- ggplot(
  duration_dist_by_activity,
  aes(x = duration_mid, y = episodes_per_diary_day, colour = period)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  facet_wrap(~ activity, scales = "free_y") +
  labs(
    title = "Episode duration distribution by activity",
    subtitle = "Weighted episodes per diary-day; episodes longer than 240 minutes excluded",
    x = "Episode duration (minutes)",
    y = "Weighted episodes per diary-day",
    colour = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M3_episode_duration_distribution_by_activity_per_day.png",
  p_m3_duration_by_activity,
  width = 12,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------
# M4. ICT involvement across locations
# ICT time-based indicators use harmonised pre-merge recorded segments.
# ------------------------------------------------------------

ict_location_summary <- timeuse_premerge_primary %>%
  group_by(period, location) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator_premerge, by = "period") %>%
  mutate(
    ict_segment_minutes_per_diary_day =
      ict_recorded_segment_time / weighted_diary_days,
    ict_segment_time_share =
      ict_recorded_segment_time / total_recorded_segment_time
  )

write_csv(
  ict_location_summary,
  "combined_outputs_harmonised/tables/M4_ict_involvement_across_locations_by_period.csv"
)

p_m4_ict_minutes_location <- ggplot(
  ict_location_summary %>% mutate(location = label_locations(location)),
  aes(x = location, y = ict_segment_minutes_per_diary_day, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "ICT involvement across locations",
    subtitle = "Weighted average recorded segment minutes involving ICT per diary-day",
    x = "Location",
    y = "Episode minutes involving a device per diary-day",
    fill = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4_ict_recorded_segment_minutes_across_locations.png",
  p_m4_ict_minutes_location,
  width = 10,
  height = 6,
  dpi = 300
)

p_m4_ict_intensity_location <- ggplot(
  ict_location_summary %>% mutate(location = label_locations(location)),
  aes(x = location, y = ict_segment_time_share, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "ICT intensity across locations",
    subtitle = "Share of episode time involving a device",
    x = "Location",
    y = "Share of episode time involving a device",
    fill = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4_ict_recorded_segment_intensity_across_locations.png",
  p_m4_ict_intensity_location,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# M4a. ICT involvement across locations with activity added
# ------------------------------------------------------------

ict_location_activity_summary <- timeuse_premerge_primary %>%
  group_by(period, location, activity) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator_premerge, by = "period") %>%
  mutate(
    ict_segment_minutes_per_diary_day =
      ict_recorded_segment_time / weighted_diary_days,
    ict_segment_time_share =
      ict_recorded_segment_time / total_recorded_segment_time
  )

write_csv(
  ict_location_activity_summary,
  "combined_outputs_harmonised/tables/M4a_ict_involvement_by_location_and_activity_by_period.csv"
)

p_m4a_ict_minutes_locact <- ggplot(
  ict_location_activity_summary,
  aes(x = activity, y = ict_segment_minutes_per_diary_day, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ location, scales = "free_y") +
  labs(
    title = "ICT involvement by location and activity",
    subtitle = "Episode minutes involving a device per diary-day",
    x = "Activity purpose",
    y = "Episode minutes involving a device per diary-day",
    fill = "Period"
  ) +
  presentation_theme +
  theme(strip.text = element_text(size = 12, face = "bold"))

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4a_ict_recorded_segment_minutes_by_location_and_activity.png",
  p_m4a_ict_minutes_locact,
  width = 14,
  height = 9,
  dpi = 300
)

p_m4a_ict_intensity_locact <- ggplot(
  ict_location_activity_summary,
  aes(x = activity, y = ict_segment_time_share, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ location, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "ICT intensity by location and activity",
    subtitle = "Share of episode time involving a device",
    x = "Activity purpose",
    y = "Share of episode time involving a device",
    fill = "Period"
  ) +
  presentation_theme +
  theme(strip.text = element_text(size = 12, face = "bold"))

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M4a_ict_recorded_segment_intensity_by_location_and_activity.png",
  p_m4a_ict_intensity_locact,
  width = 14,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# M5. Travel ICT detail
# ------------------------------------------------------------

travel_ict_summary <- timeuse_premerge_primary %>%
  filter(location == "travel") %>%
  group_by(period, activity) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(period_day_denominator_premerge, by = "period") %>%
  mutate(
    travel_ict_segment_minutes_per_day =
      ict_recorded_segment_time / weighted_diary_days,
    travel_total_recorded_segment_minutes_per_day =
      total_recorded_segment_time / weighted_diary_days,
    travel_ict_segment_time_share =
      ict_recorded_segment_time / total_recorded_segment_time
  )

write_csv(
  travel_ict_summary,
  "combined_outputs_harmonised/tables/M5_travel_ict_involvement_by_activity.csv"
)

p_m5_travel_ict <- ggplot(
  travel_ict_summary,
  aes(x = activity, y = travel_ict_segment_minutes_per_day, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "ICT involvement in travel contexts",
    subtitle = "Episode minutes involving a device per diary-day",
    x = "Activity purpose",
    y = "Episode minutes involving a device per diary-day",
    fill = "Period"
  ) +
  presentation_theme

ggsave(
  "combined_outputs_harmonised/figures_mechanism/M5_travel_ict_recorded_segment_minutes.png",
  p_m5_travel_ict,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 12B. ICT use within theoretically non-ICT activities
# Measurement rule:
#   - Share of merged spells involving a device uses formally merged data.
#   - Share of episode time involving a device uses pre-merge data.
# ============================================================
# Purpose:
#   This check tests whether ICT devices are used during activities whose
#   recorded activity labels do not explicitly require ICT, communication,
#   media use, online shopping, or computing.
#
# Interpretation:
#   This does NOT prove within-episode fragmentation directly. However, if ICT
#   use is observed during theoretically non-ICT activities, it provides evidence
#   that extra ICT-related behaviour may occur inside recorded activity episodes
#   without appearing as a separate activity code.
#
# Important:
#   Use PRIMARY rows only, because the duplicated secondary rows are created for
#   activity accounting and would otherwise double-count original diary episodes.

# Output folders
dir.create("combined_outputs_harmonised/figures_unexpected_ict", showWarnings = FALSE, recursive = TRUE)

# Helper: identify activities that explicitly involve ICT/media/online behaviour.
# These are excluded from this check because ICT use is expected by the activity label.
is_explicit_ict_or_media_activity <- function(activity_text) {
  str_detect(
    str_to_lower(as.character(activity_text)),
    paste(
      c(
        "internet", "online", "e-shopping", "comput", "software",
        "social media", "text", "sms", "email", "phone", "video call",
        "tv", "television", "video", "dvd", "radio", "recording", "music",
        "communication", "information search", "games", "watching",
        "shopping", "bank", "commercial", "administrative services"
      ),
      collapse = "|"
    )
  )
}

# Helper: classify theoretically non-ICT activities into interpretable groups.
# The activity names differ across 2014-2015 and 2023, so this uses keyword rules
# after excluding explicit ICT/media/online labels above.
classify_theoretical_non_ict_activity <- function(activity_text) {
  a <- str_to_lower(as.character(activity_text))

  case_when(
    str_detect(a, "sleep|sick in bed|wash|dress|personal care|eating|drinking|meal|resting") ~
      "personal care and rest",

    str_detect(a, paste(
      c(
        "food preparation", "baking", "dish", "cleaning", "tidying",
        "laundry", "ironing", "textile", "household upkeep",
        "household management not using the internet", "recycling", "disposal",
        "gardening", "tending domestic animals", "caring for pets", "walking the dog",
        "repairs", "construction", "vehicle maintenance", "heating dwelling", "arranging household"
      ),
      collapse = "|"
    )) ~ "household maintenance",

    str_detect(a, paste(
      c(
        "physical care", "supervision of child", "feeding the child",
        "reading playing", "reading, playing", "talking with child",
        "caring for", "childcare", "help to an adult", "care of", "accompanying"
      ),
      collapse = "|"
    )) ~ "care activities",

    str_detect(a, paste(
      c(
        "travelling", "travel", "passenger car", "on foot", "bus", "train", "taxi",
        "public transport", "walking,jogging", "cycle", "bicycle", "coach", "tram", "underground"
      ),
      collapse = "|"
    )) ~ "travel and mobility",

    str_detect(a, paste(
      c(
        "walking/dog walking", "walking and hiking", "walk or hike", "jogging", "running",
        "sports", "exercise", "fitness", "cycling", "biking", "skating", "swimming"
      ),
      collapse = "|"
    )) ~ "outdoor and physical leisure",

    TRUE ~ NA_character_
  )
}

# Helper: identify blank / missing / other secondary activities.
# These are allowed as secondary activities in the non-ICT sample because they do
# not indicate an explicit ICT/media/online activity.
is_blank_or_other_activity <- function(activity_text) {
  a <- str_to_lower(str_trim(as.character(activity_text)))

  is.na(activity_text) |
    a == "" |
    a %in% c(
      "na", "missing", "other", "other time use", "no secondary activity",
      "no main activity", "unspecified time use", "no answer/refused",
      "item not applicable", "schedule not applicable", "don't know",
      "not applicable", "unspecified", "unknown"
    ) |
    str_detect(a, "no secondary|no other|not applicable|missing")
}

# Build two analysis samples using the same activity definition.
#
# Important definition update:
#   An episode/spell is treated as theoretically non-ICT only when:
#     1) the PRIMARY activity is a theoretically non-ICT activity; and
#     2) the SECONDARY activity is not explicitly ICT/media/online.
#
# This avoids counting episodes such as:
#   primary = food preparation, secondary = watching TV
# as "non-ICT activity with ICT", because the secondary activity is already media-related.
#
# MERGED sample:
#   Used for the share of harmonised activity spells involving ICT.
#
# PRE-MERGE sample:
#   Used for the share of recorded segment time involving ICT. This avoids
#   assigning ICT use to the full merged spell when ICT status changes between
#   otherwise identical adjacent records.

non_ict_activity_ict_sample_merged <- timeuse_all %>%
  filter(activity_role == "primary") %>%
  mutate(
    primary_activity_name = as.character(activity_name),
    secondary_activity_name = as.character(sec),

    primary_explicit_ict_or_media =
      is_explicit_ict_or_media_activity(primary_activity_name),
    secondary_explicit_ict_or_media =
      ifelse(
        is_blank_or_other_activity(secondary_activity_name),
        FALSE,
        is_explicit_ict_or_media_activity(secondary_activity_name)
      ),

    non_ict_category =
      classify_theoretical_non_ict_activity(primary_activity_name),

    theoretically_non_ict =
      !primary_explicit_ict_or_media &
      !secondary_explicit_ict_or_media &
      !is.na(non_ict_category),

    # Keep activity_name as the primary activity label for the existing tables/figures.
    activity_name_chr = primary_activity_name
  ) %>%
  filter(theoretically_non_ict) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    non_ict_category = factor(
      non_ict_category,
      levels = c(
        "personal care and rest",
        "household maintenance",
        "care activities",
        "travel and mobility",
        "outdoor and physical leisure"
      )
    )
  )

non_ict_activity_ict_sample_premerge <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(
    primary_activity_name = as.character(pri),
    secondary_activity_name = as.character(sec),

    primary_explicit_ict_or_media =
      is_explicit_ict_or_media_activity(primary_activity_name),
    secondary_explicit_ict_or_media =
      ifelse(
        is_blank_or_other_activity(secondary_activity_name),
        FALSE,
        is_explicit_ict_or_media_activity(secondary_activity_name)
      ),

    non_ict_category =
      classify_theoretical_non_ict_activity(primary_activity_name),

    theoretically_non_ict =
      !primary_explicit_ict_or_media &
      !secondary_explicit_ict_or_media &
      !is.na(non_ict_category),

    # Keep activity_name as the primary activity label for the existing tables/figures.
    activity_name = primary_activity_name,
    activity_name_chr = primary_activity_name,

    weighted_time = time * wt,
    period = factor(period, levels = c("2014-2015", "2023")),
    non_ict_category = factor(
      non_ict_category,
      levels = c(
        "personal care and rest",
        "household maintenance",
        "care activities",
        "travel and mobility",
        "outdoor and physical leisure"
      )
    )
  ) %>%
  filter(theoretically_non_ict)

# Keep the old object name as the merged-spell sample for episode-level exports.
non_ict_activity_ict_sample <- non_ict_activity_ict_sample_merged

write_csv(
  non_ict_activity_ict_sample_merged,

  "combined_outputs_harmonised/tables/non_ict_activity_ict_sample_episode_level.csv"
)

# Smaller ICT-only files for Excel checking
write_csv(
  non_ict_activity_ict_sample %>%
    filter(period == "2014-2015", any_ict == 1) %>%
    select(period, mainid, diaryord, person_day, start, time, wt,
           activity_name, pri, sec, loc, act_group2, loc6, non_ict_category, any_ict),
  "combined_outputs_harmonised/tables/df2014_theoretical_non_ict_ict_only_small.csv"
)

write_csv(
  non_ict_activity_ict_sample %>%
    filter(period == "2023", any_ict == 1) %>%
    select(period, mainid, diaryord, person_day, start, time, wt,
           activity_name, pri, sec, loc, act_group2, loc6, non_ict_category, any_ict),
  "combined_outputs_harmonised/tables/df2023_theoretical_non_ict_ict_only_small.csv"
)

# Overall ICT indicators.
# Episode share uses merged harmonised spells; time share uses pre-merge segments.
non_ict_episode_overall <- non_ict_activity_ict_sample_merged %>%
  group_by(period) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

non_ict_time_overall <- non_ict_activity_ict_sample_premerge %>%
  group_by(period) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

non_ict_ict_overall <- non_ict_episode_overall %>%
  left_join(non_ict_time_overall, by = "period")

write_csv(
  non_ict_ict_overall,
  "combined_outputs_harmonised/tables/non_ict_activity_ict_overall.csv"
)

# ICT indicators by non-ICT activity category.
non_ict_episode_by_category <- non_ict_activity_ict_sample_merged %>%
  group_by(period, non_ict_category) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

non_ict_time_by_category <- non_ict_activity_ict_sample_premerge %>%
  group_by(period, non_ict_category) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

non_ict_ict_by_category <- non_ict_episode_by_category %>%
  left_join(non_ict_time_by_category, by = c("period", "non_ict_category"))

write_csv(
  non_ict_ict_by_category,
  "combined_outputs_harmonised/tables/non_ict_activity_ict_by_category.csv"
)

# Detailed activity-level table.
# Spell involvement comes from merged spells; recorded-time involvement comes
# from pre-merge segments.
non_ict_episode_by_activity <- non_ict_activity_ict_sample_merged %>%
  group_by(period, non_ict_category, activity_name) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

non_ict_time_by_activity <- non_ict_activity_ict_sample_premerge %>%
  group_by(period, non_ict_category, activity_name) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

non_ict_ict_by_activity <- non_ict_episode_by_activity %>%
  left_join(
    non_ict_time_by_activity,
    by = c("period", "non_ict_category", "activity_name")
  ) %>%
  arrange(period, desc(ict_weighted_episodes))

write_csv(
  non_ict_ict_by_activity,
  "combined_outputs_harmonised/tables/non_ict_activity_ict_by_activity.csv"
)

# High-frequency 2023 examples for presentation/discussion.
# ------------------------------------------------------------
# A minimum unweighted episode count is imposed. Without it the ranking is
# dominated by rare codes whose ICT share is estimated from a handful of
# episodes and can reach 100% by chance; "Resting (unspecified)" appeared at
# the top of the previous version on exactly that basis. The count is also
# printed on each bar so the reader can judge the precision directly.
NON_ICT_EXAMPLE_MIN_EPISODES <- 30

non_ict_ict_examples_2023 <- non_ict_ict_by_activity %>%
  filter(period == "2023") %>%
  filter(ict_weighted_episodes > 0) %>%
  filter(is.finite(raw_episodes), raw_episodes >= NON_ICT_EXAMPLE_MIN_EPISODES) %>%
  arrange(desc(ict_time_share)) %>%
  slice_head(n = 12)

message(
  "Top-12 non-ICT example activities: ",
  nrow(non_ict_ict_examples_2023),
  " retained at a minimum of ", NON_ICT_EXAMPLE_MIN_EPISODES,
  " unweighted episodes (excluded ",
  sum(non_ict_ict_by_activity$period == "2023" &
        non_ict_ict_by_activity$ict_weighted_episodes > 0 &
        non_ict_ict_by_activity$raw_episodes < NON_ICT_EXAMPLE_MIN_EPISODES,
      na.rm = TRUE),
  " rarer activities)."
)

write_csv(
  non_ict_ict_examples_2023,
  "combined_outputs_harmonised/tables/non_ict_activity_ict_examples_2023_top12.csv"
)

# Comparison table: 2023 minus 2014-2015 by category
non_ict_ict_category_change <- non_ict_ict_by_category %>%
  select(period, non_ict_category, ict_episode_share, ict_time_share) %>%
  pivot_wider(
    names_from = period,
    values_from = c(ict_episode_share, ict_time_share)
  ) %>%
  mutate(
    diff_ict_episode_share = `ict_episode_share_2023` - `ict_episode_share_2014-2015`,
    diff_ict_time_share = `ict_time_share_2023` - `ict_time_share_2014-2015`
  )

write_csv(
  non_ict_ict_category_change,
  "combined_outputs_harmonised/tables/non_ict_activity_ict_category_change_2023_minus_2014.csv"
)

# ------------------------------------------------------------
# Figures
# ------------------------------------------------------------

p_non_ict_ict_overall <- non_ict_ict_overall %>%
  pivot_longer(
    cols = c(ict_episode_share, ict_time_share),
    names_to = "measure",
    values_to = "ict_share"
  ) %>%
  mutate(
    measure = recode(
      measure,
      ict_episode_share = "Share of merged spells involving a device",
      ict_time_share = "Share of episode time involving a device"
    )
  ) %>%
  ggplot(aes(x = measure, y = ict_share, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "ICT use within theoretically non-ICT activities",
    subtitle = "Spell share uses merged episodes; time share uses pre-merge recorded segments",
    x = NULL,
    y = "ICT share",
    fill = "Period"
  ) +
  presentation_theme +
  theme(plot.title.position = "plot")

ggsave(
  "combined_outputs_harmonised/figures_unexpected_ict/non_ict_activity_ict_overall.png",
  p_non_ict_ict_overall,
  width = 10,
  height = 5.5,
  dpi = 300
)

p_non_ict_ict_by_category <- ggplot(
  non_ict_ict_by_category,
  aes(x = non_ict_category, y = ict_time_share, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Episode-time share involving a device within theoretically non-ICT activities",
    subtitle = "Spell share uses merged episodes; time share uses pre-merge recorded segments",
    x = NULL,
    y = "Share of episode time involving a device",
    fill = "Period"
  ) +
  presentation_theme +
  theme(plot.title.position = "plot")

ggsave(
  "combined_outputs_harmonised/figures_unexpected_ict/non_ict_activity_ict_by_category.png",
  p_non_ict_ict_by_category,
  width = 11,
  height = 6.5,
  dpi = 300
)

# Top 12 2023 examples by ICT time share
p_non_ict_ict_examples_2023 <- non_ict_ict_examples_2023 %>%
  mutate(
    activity_name = fct_reorder(activity_name, ict_time_share)
  ) %>%
  ggplot(aes(x = activity_name, y = ict_time_share)) +
  geom_col(fill = "grey35") +
  geom_text(
    aes(label = paste0("n = ", format(raw_episodes, big.mark = ","))),
    hjust = -0.12, size = 4.6, colour = "grey25"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "2023 examples: ICT use within activities not explicitly ICT-related",
    subtitle = paste0(
      "Top 12 activities by share of episode time involving a device; ",
      "activities with fewer than ", NON_ICT_EXAMPLE_MIN_EPISODES,
      " recorded episodes excluded"
    ),
    x = NULL,
    y = "Share of episode time involving a device"
  ) +
  presentation_theme +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 17, hjust = 0),
    plot.title.position = "plot",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16)
  )

ggsave(
  "combined_outputs_harmonised/figures_unexpected_ict/non_ict_activity_ict_examples_2023_top12.png",
  p_non_ict_ict_examples_2023,
  width = 11,
  height = 6,
  dpi = 300
)



# ============================================================
# 12C. Corresponding checks for explicitly ICT-related activities
#      and detailed activity-level figures
# ============================================================
# Interpretation of the figures:
#   1) The existing "by category" figure is a broad-category comparison
#      (e.g. travel and mobility, care activities), not a detailed
#      individual-activity plot.
#   2) This section adds:
#      - an overall figure for clearly explicit ICT/media/online activities;
#      - a broad explicit-ICT category figure;
#      - detailed activity-level figures for both theoretically non-ICT
#        and clearly explicit ICT activities.
#
# Important:
#   Use PRIMARY rows only to avoid double-counting duplicated secondary rows.

dir.create(
  "combined_outputs_harmonised/figures_explicit_ict",
  showWarnings = FALSE,
  recursive = TRUE
)

# A stricter definition of activities whose labels clearly indicate ICT,
# media, communication, online activity, or computing.
# Generic offline shopping/banking labels are not treated as clearly ICT
# unless the activity label also states online/internet use.
is_clearly_explicit_ict_activity <- function(activity_text) {
  str_detect(
    str_to_lower(as.character(activity_text)),
    paste(
      c(
        "internet",
        "online",
        "e-shopping",
        "computer",
        "computing",
        "software",
        "social media",
        "texting",
        "\\btext\\b",
        "\\bsms\\b",
        "email",
        "e-mail",
        "phone",
        "video call",
        "television",
        "\\btv\\b",
        "\\bvideo\\b",
        "\\bdvd\\b",
        "radio",
        "recording",
        "music",
        "communication",
        "information search",
        "computer game",
        "video game",
        "gaming",
        "watching"
      ),
      collapse = "|"
    )
  )
}

# Classify clearly explicit ICT activities into broad, interpretable groups.
classify_explicit_ict_activity <- function(activity_text) {
  a <- str_to_lower(as.character(activity_text))

  case_when(
    str_detect(
      a,
      "social media|texting|\\btext\\b|\\bsms\\b|email|e-mail|phone|video call|communication"
    ) ~ "communication and social media",

    str_detect(
      a,
      "internet|online|e-shopping|information search|computer|computing|software"
    ) ~ "internet and computing",

    str_detect(
      a,
      "computer game|video game|gaming"
    ) ~ "digital gaming",

    str_detect(
      a,
      "television|\\btv\\b|\\bvideo\\b|\\bdvd\\b|watching"
    ) ~ "television and video",

    str_detect(
      a,
      "radio|recording|music"
    ) ~ "audio media",

    TRUE ~ "other explicit ICT/media"
  )
}

# Build merged-spell and pre-merge segment samples for clearly explicit ICT activities.
explicit_ict_activity_sample_merged <- timeuse_all %>%
  filter(activity_role == "primary") %>%
  mutate(
    activity_name_chr = as.character(activity_name),
    clearly_explicit_ict = is_clearly_explicit_ict_activity(activity_name_chr),
    explicit_ict_category = classify_explicit_ict_activity(activity_name_chr)
  ) %>%
  filter(clearly_explicit_ict) %>%
  mutate(
    period = factor(period, levels = c("2014-2015", "2023")),
    explicit_ict_category = factor(
      explicit_ict_category,
      levels = c(
        "communication and social media",
        "internet and computing",
        "digital gaming",
        "television and video",
        "audio media",
        "other explicit ICT/media"
      )
    )
  )

explicit_ict_activity_sample_premerge <- bind_rows(
  df2014_premerge,
  df2023_premerge
) %>%
  mutate(
    activity_name = as.character(pri),
    activity_name_chr = as.character(pri),
    weighted_time = time * wt,
    clearly_explicit_ict = is_clearly_explicit_ict_activity(activity_name_chr),
    explicit_ict_category = classify_explicit_ict_activity(activity_name_chr),
    period = factor(period, levels = c("2014-2015", "2023")),
    explicit_ict_category = factor(
      explicit_ict_category,
      levels = c(
        "communication and social media",
        "internet and computing",
        "digital gaming",
        "television and video",
        "audio media",
        "other explicit ICT/media"
      )
    )
  ) %>%
  filter(clearly_explicit_ict)

explicit_ict_activity_sample <- explicit_ict_activity_sample_merged

write_csv(
  explicit_ict_activity_sample_merged,
  "combined_outputs_harmonised/tables/explicit_ict_activity_sample_episode_level.csv"
)

# Overall ICT indicators: merged spells for episode share, pre-merge segments for time share.
explicit_episode_overall <- explicit_ict_activity_sample_merged %>%
  group_by(period) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

explicit_time_overall <- explicit_ict_activity_sample_premerge %>%
  group_by(period) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

explicit_ict_overall <- explicit_episode_overall %>%
  left_join(explicit_time_overall, by = "period")

write_csv(
  explicit_ict_overall,
  "combined_outputs_harmonised/tables/explicit_ict_activity_ict_overall.csv"
)

# ICT indicators by broad explicit-ICT category.
explicit_episode_by_category <- explicit_ict_activity_sample_merged %>%
  group_by(period, explicit_ict_category) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

explicit_time_by_category <- explicit_ict_activity_sample_premerge %>%
  group_by(period, explicit_ict_category) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

explicit_ict_by_category <- explicit_episode_by_category %>%
  left_join(explicit_time_by_category, by = c("period", "explicit_ict_category"))

write_csv(
  explicit_ict_by_category,
  "combined_outputs_harmonised/tables/explicit_ict_activity_ict_by_category.csv"
)

# Detailed activity-level table for clearly explicit ICT activities.
explicit_episode_by_activity <- explicit_ict_activity_sample_merged %>%
  group_by(period, explicit_ict_category, activity_name) %>%
  summarise(
    raw_episodes = n(),
    weighted_episodes = sum(wt, na.rm = TRUE),
    ict_weighted_episodes = sum(wt * any_ict, na.rm = TRUE),
    ict_episode_share = ict_weighted_episodes / weighted_episodes,
    .groups = "drop"
  )

explicit_time_by_activity <- explicit_ict_activity_sample_premerge %>%
  group_by(period, explicit_ict_category, activity_name) %>%
  summarise(
    total_recorded_segment_time = sum(weighted_time, na.rm = TRUE),
    ict_recorded_segment_time = sum(weighted_time * any_ict, na.rm = TRUE),
    ict_time_share = ict_recorded_segment_time / total_recorded_segment_time,
    .groups = "drop"
  )

explicit_ict_by_activity <- explicit_episode_by_activity %>%
  left_join(
    explicit_time_by_activity,
    by = c("period", "explicit_ict_category", "activity_name")
  ) %>%
  arrange(period, desc(ict_recorded_segment_time))

write_csv(
  explicit_ict_by_activity,
  "combined_outputs_harmonised/tables/explicit_ict_activity_ict_by_activity.csv"
)

# ------------------------------------------------------------
# Figure A: overall ICT shares within clearly explicit ICT activities
# This is the direct counterpart to the existing non-ICT overall figure.
# ------------------------------------------------------------

p_explicit_ict_overall <- explicit_ict_overall %>%
  pivot_longer(
    cols = c(ict_episode_share, ict_time_share),
    names_to = "measure",
    values_to = "ict_share"
  ) %>%
  mutate(
    measure = recode(
      measure,
      ict_episode_share = "Share of merged spells involving a device",
      ict_time_share = "Share of episode time involving a device"
    )
  ) %>%
  ggplot(aes(x = measure, y = ict_share, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "ICT use within clearly ICT-related activities",
    subtitle = "Primary activity episodes only; labels explicitly indicate ICT/media/online activity",
    x = NULL,
    y = "ICT share",
    fill = "Period"
  ) +
  presentation_theme +
  theme(plot.title.position = "plot")

ggsave(
  "combined_outputs_harmonised/figures_explicit_ict/explicit_ict_activity_ict_overall.png",
  p_explicit_ict_overall,
  width = 10,
  height = 5.5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure B: broad explicit-ICT activity categories
# ------------------------------------------------------------

p_explicit_ict_by_category <- ggplot(
  explicit_ict_by_category,
  aes(x = explicit_ict_category, y = ict_time_share, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Episode-time share involving a device within clearly ICT-related activities",
    subtitle = "Primary activity episodes only",
    x = NULL,
    y = "Share of episode time involving a device",
    fill = "Period"
  ) +
  presentation_theme +
  theme(plot.title.position = "plot")

ggsave(
  "combined_outputs_harmonised/figures_explicit_ict/explicit_ict_activity_ict_by_category.png",
  p_explicit_ict_by_category,
  width = 11,
  height = 6.5,
  dpi = 300
)

# ------------------------------------------------------------
# Figure C: direct contrast between theoretically non-ICT and
# clearly explicit ICT activities
# ------------------------------------------------------------

ict_activity_type_overall <- bind_rows(
  non_ict_ict_overall %>%
    mutate(activity_type = "Theoretically non-ICT activities"),
  explicit_ict_overall %>%
    mutate(activity_type = "Clearly ICT-related activities")
) %>%
  pivot_longer(
    cols = c(ict_episode_share, ict_time_share),
    names_to = "measure",
    values_to = "ict_share"
  ) %>%
  mutate(
    measure = recode(
      measure,
      ict_episode_share = "Share of merged spells involving a device",
      ict_time_share = "Share of episode time involving a device"
    ),
    activity_type = factor(
      activity_type,
      levels = c(
        "Theoretically non-ICT activities",
        "Clearly ICT-related activities"
      )
    )
  )

write_csv(
  ict_activity_type_overall,
  "combined_outputs_harmonised/tables/ict_activity_type_overall_comparison.csv"
)

p_ict_activity_type_comparison <- ggplot(
  ict_activity_type_overall,
  aes(x = activity_type, y = ict_share, fill = period)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    # Shortened: the previous title overflowed the canvas and the final
    # character was clipped. The detail now sits in the subtitle, and
    # plot.title.position = "plot" left-aligns both to the panel edge.
    title = "Recorded ICT use by activity type",
    subtitle = paste0(
      "Theoretically non-ICT activities against clearly ICT-related ",
      "activities; primary activity records only"
    ),
    x = NULL,
    y = NULL,
    fill = "Period"
  ) +
  presentation_theme +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    plot.title.position = "plot"
  )

ggsave(
  "combined_outputs_harmonised/figures_explicit_ict/ict_non_ict_vs_explicit_ict_overall.png",
  p_ict_activity_type_comparison,
  width = 13,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# Figure D: detailed theoretically non-ICT activities
# This is the requested individual-activity figure.
# The existing Figure 2 is by BROAD CATEGORY, not individual activity.
# Select the 10 activities with the greatest weighted ICT time in each period.
# ------------------------------------------------------------

non_ict_top_activities_each_period <- non_ict_ict_by_activity %>%
  filter(ict_recorded_segment_time > 0) %>%
  group_by(period) %>%
  slice_max(
    order_by = ict_recorded_segment_time,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    activity_period = paste(activity_name, period, sep = "___"),
    activity_period = fct_reorder(activity_period, ict_time_share)
  )

write_csv(
  non_ict_top_activities_each_period,
  "combined_outputs_harmonised/tables/non_ict_top_activities_each_period.csv"
)

p_non_ict_by_detailed_activity <- ggplot(
  non_ict_top_activities_each_period,
  aes(x = activity_period, y = ict_time_share, fill = period)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~period, scales = "free_y") +
  scale_x_discrete(
    labels = function(x) sub("___.*$", "", x)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "ICT time share within detailed non-ICT activities",
    subtitle = "Top 10 activities in each period by episode time involving a device",
    x = NULL,
    y = "Share of episode time involving a device"
  ) +
  presentation_theme +
  theme(
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "combined_outputs_harmonised/figures_unexpected_ict/non_ict_activity_ict_by_detailed_activity.png",
  p_non_ict_by_detailed_activity,
  width = 13,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------
# Figure E: detailed clearly explicit ICT activities
# ------------------------------------------------------------

explicit_ict_top_activities_each_period <- explicit_ict_by_activity %>%
  filter(ict_recorded_segment_time > 0) %>%
  group_by(period) %>%
  slice_max(
    order_by = ict_recorded_segment_time,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    activity_period = paste(activity_name, period, sep = "___"),
    activity_period = fct_reorder(activity_period, ict_time_share)
  )

write_csv(
  explicit_ict_top_activities_each_period,
  "combined_outputs_harmonised/tables/explicit_ict_top_activities_each_period.csv"
)

p_explicit_ict_by_detailed_activity <- ggplot(
  explicit_ict_top_activities_each_period,
  aes(x = activity_period, y = ict_time_share, fill = period)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~period, scales = "free_y") +
  scale_x_discrete(
    labels = function(x) sub("___.*$", "", x)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "ICT time share within detailed ICT-related activities",
    subtitle = "Top 10 activities in each period by episode time involving a device",
    x = NULL,
    y = "Share of activity time involving recorded ICT device use"
  ) +
  presentation_theme +
  theme(
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold")
  )

ggsave(
  "combined_outputs_harmonised/figures_explicit_ict/explicit_ict_activity_ict_by_detailed_activity.png",
  p_explicit_ict_by_detailed_activity,
  width = 13,
  height = 8,
  dpi = 300
)


# ============================================================
# 13. Notes for interpretation
# ============================================================

notes <- tibble::tibble(
  point = c(
    "Survey weights are used throughout the time-use and NTS analyses.",
    "Time-use comparisons focus mainly on shares, ICT intensity, and average minutes per diary-day rather than raw totals.",
    "2014-2015 and 2023 time-use data are harmonised into common activity and location groups.",
    "A separate robustness check merges adjacent episodes that are likely to be over-split, including same-activity spells, primary-secondary swaps, and same-activity location changes.",
    "NTS is used as a travel context only; it does not identify ICT use and should not be treated as causal evidence of ICT effects.",
    "NTS is a 7-day travel diary while time-use data are diary-day based; therefore NTS metrics are reported per person-day and separately from time-use totals."
  )
)

write_csv(notes, "combined_outputs_harmonised/tables/interpretation_notes.csv")

cat("\nDONE: Combined comparison analysis finished.\n")
cat("Outputs saved in: combined_outputs_harmonised/\n")
cat("Key figures: combined_outputs_harmonised/figures_timeuse/, combined_outputs_harmonised/figures_nts/, and combined_outputs_harmonised/figures_differences/\n")
cat("Key tables: combined_outputs_harmonised/tables/\n")

readr::write_csv(
  dplyr::bind_rows(df2014_premerge, df2023_premerge) %>%
    dplyr::select(
      period, mainid, diaryord, person_day, original_episode_number,
      start, time, pri_code_harmonised, sec_code_harmonised,
      pri, sec, loc, loc6, any_ict, wt
    ),
  "combined_outputs_harmonised/tables/ict_analysis_premerge_check.csv"
)

readr::write_csv(
  dplyr::bind_rows(df2014_primary_merged, df2023_primary_merged) %>%
    dplyr::select(
      period, mainid, diaryord, person_day, harmonised_spell_id,
      start, time, pri_code_harmonised, sec_code_harmonised,
      pri, sec, loc, loc6, any_ict, n_original_episodes, wt
    ),
  "combined_outputs_harmonised/tables/ict_analysis_postmerge_check.csv"
)

