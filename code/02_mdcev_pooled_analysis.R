# ============================================================
# MDCEV estimation: UKTUS 2014-2015 vs ELIDDI/CTUR 2023
# Separate models + pooled period-interaction model
# ------------------------------------------------------------
# This version implements the supervisor-suggested two-channel specification:
#
#   ict_share       = overall share of the diary day recorded in ICT-involved episodes
#   ictact_leisure  = share of leisure time recorded in ICT-involved episodes
#   ictact_travel   = share of travel time recorded in ICT-involved episodes
#   ... similarly for maintenance, shopping, work, other
#
# MDCEV interpretation used here:
#
#   1) ict_share enters psi_k (baseline utility / participation propensity)
#      for each inside activity. This captures whether an ICT-heavy diary day is
#      associated with allocating any time to activity k.
#
#   2) the matching ictact_<activity> enters gamma_k (translation / duration-
#      satiation component) for each inside activity. This captures whether ICT
#      embedded within the activity is associated with more or less time allocated
#      to that activity, conditional on participation.
#
# Important interpretation:
#   These ICT variables are PROPORTIONS / SHARES, not exact device-use minutes.
#   They are based on whether the recorded episode involved ICT, not on measured
#   screen/device duration.
# ============================================================

# Run this script from the repository root so relative paths resolve correctly.

library(dplyr)
library(tidyr)
library(readr)
library(haven)     # to read the raw .dta covariate files
library(apollo)    # tested against apollo 0.3.8

dir.create("combined_outputs_harmonised/mdcev", recursive = TRUE, showWarnings = FALSE)

# ---- Configurable paths to the raw covariate files -------------------------
# These must point to the SAME diary files the main script uses, plus the
# 2014-15 individual file. Adjust if yours live in sub-folders.
PATH_2023_EPISODE <- "data/eliddi_episode_long.dta"      # 2023: age, econstat, nchild, dday
PATH_2014_DIARY   <- "data/uktus15_diary_ep_long.dta"    # 2014: DVAge, ddayw (day-of-week)
PATH_2014_INDIV   <- "data/uktus15_individual.dta"       # 2014: dilodefr (ILO), dhhtype

# ============================================================
# 0.5 ICT SCALING CONFIGURATION
# ------------------------------------------------------------
# WHY THIS EXISTS
#
# The two waves do not measure device use with the same instrument. 2014-15
# asks a single device question, with an explicit "not reported" category
# covering 6.67% of episodes; 2023 asks three separate device columns
# (computer, tablet, phone) with no missing data. Three prompts elicit more
# affirmative answers than one, and the share of travel episodes involving
# ICT rises from 11.5% to 25.4% between the waves.
#
# That matters here more than anywhere else in the project. The pooled model
# estimates theta_*_d2023 terms, which are the DIFFERENCE IN THE COEFFICIENT
# ON ict_share (and on the ictact_* variables) between the two waves. If the
# variable's own scale changed, that difference mixes behavioural change with
# instrument change, and the two cannot be separated. Every period-comparison
# result in the model is exposed to this.
#
# FOUR SPECIFICATIONS
#
#   "raw"              the original: untransformed shares in [0, 1].
#                      Retained as the headline, because the coefficients are
#                      interpretable in share units.
#   "within_period_z"  each ICT variable standardised to mean 0 and sd 1
#                      WITHIN its own wave. Coefficients then compare
#                      relative ICT intensity, and any common shift in the
#                      instrument is absorbed. This is the key robustness
#                      check: a period difference that survives it cannot be
#                      attributed to the change in question wording.
#   "single_prompt"    2023 restricted to the phone column, so that both
#                      waves rest on one device question. Requires
#                      any_ict_single_prompt in the episode file.
#   "single_prompt_z"  the two combined.
#
# Standardisation is applied AFTER the person-day file is built and
# SEPARATELY for each wave, which is what makes it within-period. The
# activity-specific variables are standardised only over person-days that
# actually undertook the activity, since a structural zero for a
# non-participant is not a low-ICT observation.
#
# Report "raw" as the headline and "within_period_z" alongside it.
# ============================================================

ICT_SCALING <- "raw"   # "raw" | "within_period_z" | "single_prompt" | "single_prompt_z"


# ============================================================
# 0.6 MODEL CACHE
# ------------------------------------------------------------
# Estimation itself takes about two minutes; computing the robust covariance
# matrix for 168 parameters takes about eighteen. Nothing downstream of the
# model objects - the coefficient tables, the interaction tests, every figure -
# requires re-estimation, so a cached model makes those steps near-instant.
#
# The cache is keyed on a fingerprint of the actual estimation inputs: the
# number of person-days, the column names, the parameter names, and the column
# sums of every numeric variable. If the harmonised data are regenerated, the
# ICT scaling is switched, or the specification changes, the fingerprint no
# longer matches and the model is re-estimated automatically. A silently stale
# result is therefore not possible; the script reports which of the two paths
# it took for each model.
#
# Set REUSE_SAVED_MODELS to FALSE to force re-estimation regardless.
# ============================================================

REUSE_SAVED_MODELS <- TRUE
MODEL_CACHE_DIR    <- "combined_outputs_harmonised/mdcev/model_cache"

dir.create(MODEL_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# Cheap dependency-free fingerprint of the estimation inputs.
model_fingerprint <- function(db, beta_names) {
  num_cols <- vapply(db, is.numeric, logical(1))
  col_sums <- vapply(
    db[num_cols],
    function(x) sum(as.numeric(x), na.rm = TRUE),
    numeric(1)
  )
  paste0(
    "rows=", nrow(db),
    "|cols=", paste(sort(names(db)), collapse = ","),
    "|npar=", length(beta_names),
    "|par=", paste(sort(beta_names), collapse = ","),
    "|sums=", paste(sprintf("%.6f", col_sums), collapse = ","),
    "|ict=", ICT_SCALING
  )
}

ICT_VARS_PSI   <- "ict_share"
ICT_VARS_GAMMA <- c("ictact_leisure", "ictact_maintenance", "ictact_travel",
                    "ictact_shopping", "ictact_work", "ictact_other")

# Every output is tagged with the specification, so that running a robustness
# variant does not overwrite the headline results.
ICT_TAG <- if (identical(ICT_SCALING, "raw")) "" else paste0("_", ICT_SCALING)

tag_path <- function(path) {
  if (identical(ICT_TAG, "")) return(path)
  sub("(\\.[A-Za-z0-9]+)$", paste0(ICT_TAG, "\\1"), path)
}

message("ICT scaling specification: ", ICT_SCALING,
        if (identical(ICT_TAG, "")) "" else paste0(" (outputs tagged ", ICT_TAG, ")"))


# ============================================================
# 1. Episode CSV -> person-day time matrix + ICT + out-of-home
# ============================================================
episodes <- read_csv(
  "combined_outputs_harmonised/tables/timeuse_formally_merged_and_expanded_episode_level.csv",
  show_col_types = FALSE
)

# Internal MDCEV names keep "leisure" because parameter names cannot safely use "&".
# The harmonisation script may now write the broad group as "social & leisure".
# We recode it back to the internal model label "leisure" before constructing the time matrix.
activity_groups <- c("personal care", "leisure", "maintenance",
                     "travel", "shopping", "work", "other")

# One row per person-day. PRIMARY episodes only, so the day sums to ~1440 and
# nothing is double-counted from the secondary-activity expansion.
build_personday <- function(df) {
  prim <- df %>%
    filter(activity_role == "primary") %>%
    mutate(
      # Compatibility with the latest harmonisation outputs:
      # "social & leisure" is the presentation label, while "leisure" is the
      # internal MDCEV alternative name used in parameter names and formulas.
      act_group2 = dplyr::case_when(
        act_group2 %in% c("social & leisure", "Social & leisure", "Social and leisure") ~ "leisure",
        TRUE ~ act_group2
      )
    )

  # Total minutes by activity.
  wide <- prim %>%
    group_by(person_day, act_group2) %>%
    summarise(minutes = sum(time, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = act_group2, values_from = minutes,
                       values_fill = 0)

  for (g in activity_groups) if (!g %in% names(wide)) wide[[g]] <- 0

  # ICT-involved minutes by activity.
  # IMPORTANT: these are NOT exact ICT/device minutes. They are minutes of
  # recorded activity episodes for which any_ict == 1.
  # Which ICT flag to use. The single-prompt variants restrict 2023 to the
  # phone column so that both waves rest on one device question; if the
  # column is absent the script falls back to the standard flag and says so.
  if (grepl("single_prompt", ICT_SCALING) &&
      "any_ict_single_prompt" %in% names(prim)) {
    prim$ict_flag <- prim$any_ict_single_prompt
  } else {
    if (grepl("single_prompt", ICT_SCALING)) {
      warning("any_ict_single_prompt not found in the episode file; ",
              "falling back to any_ict. Re-run the harmonisation script to ",
              "produce it.")
    }
    prim$ict_flag <- prim$any_ict
  }
  prim$ict_flag <- ifelse(is.na(prim$ict_flag), 0, prim$ict_flag)

  ict_wide <- prim %>%
    group_by(person_day, act_group2) %>%
    summarise(ict_episode_minutes = sum(time * ict_flag, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = act_group2,
      values_from = ict_episode_minutes,
      values_fill = 0,
      names_prefix = "ict_min_"
    )

  for (g in activity_groups) {
    nm <- paste0("ict_min_", g)
    if (!nm %in% names(ict_wide)) ict_wide[[nm]] <- 0
  }

  # Day-level overall ICT exposure + out-of-home exposure + weight.
  day_meta <- prim %>%
    group_by(person_day) %>%
    summarise(
      budget_min = sum(time, na.rm = TRUE),
      # Overall ICT exposure: share of the whole diary day in ICT-involved episodes.
      ict_share  = sum(time * ict_flag, na.rm = TRUE) / sum(time, na.rm = TRUE),
      # Out-of-home: share of the day spent NOT at home.
      # loc6 lumps own + other people's home into "home"; travel counts as out.
      out_min    = sum(time * (loc6 != "home"), na.rm = TRUE),
      w          = dplyr::first(wt),
      .groups = "drop"
    ) %>%
    mutate(out_share = ifelse(budget_min > 0, out_min / budget_min, 0))

  out <- wide %>%
    left_join(ict_wide, by = "person_day") %>%
    left_join(day_meta, by = "person_day")

  # Activity-specific ICT intensity:
  # share of each activity's time that was recorded as ICT-involved.
  # If a diary-day has zero time in that activity, set the share to 0.
  out <- out %>%
    mutate(
      ictact_leisure     = ifelse(leisure     > 0, ict_min_leisure     / leisure,     0),
      ictact_maintenance = ifelse(maintenance > 0, ict_min_maintenance / maintenance, 0),
      ictact_travel      = ifelse(travel      > 0, ict_min_travel      / travel,      0),
      ictact_shopping    = ifelse(shopping    > 0, ict_min_shopping    / shopping,    0),
      ictact_work        = ifelse(work        > 0, ict_min_work        / work,        0),
      ictact_other       = ifelse(other       > 0, ict_min_other       / other,       0)
    )

  # minutes -> HOURS for numerical stability.
  out <- out %>%
    transmute(
      person_day,
      t_personal_care = `personal care` / 60,
      t_leisure       = leisure / 60,
      t_maintenance   = maintenance / 60,
      t_travel        = travel / 60,
      t_shopping      = shopping / 60,
      t_work          = work / 60,
      t_other         = other / 60,
      budget          = budget_min / 60,

      # ICT exposures are shares/proportions in [0, 1], not minutes.
      ict_share,
      ictact_leisure,
      ictact_maintenance,
      ictact_travel,
      ictact_shopping,
      ictact_work,
      ictact_other,

      out_share,
      w
    ) %>%
    filter(t_personal_care > 0, budget > 0)   # outside good must be > 0

  # Apollo MDCEV requires the sum of all continuous choices to equal the budget.
  # After harmonisation-label changes, this catches any unmapped activity group early.
  out <- out %>%
    mutate(
      expend_check = t_personal_care + t_leisure + t_maintenance +
        t_travel + t_shopping + t_work + t_other,
      budget = expend_check
    ) %>%
    select(-expend_check)

  out$w <- out$w / mean(out$w, na.rm = TRUE)  # normalise weights to mean 1
  out
}


db_2014 <- episodes %>% filter(period == "2014-2015") %>% build_personday()
db_2023 <- episodes %>% filter(period == "2023")      %>% build_personday()

# ------------------------------------------------------------
# Within-period standardisation of the ICT variables.
# Applied to each wave separately, which is what removes the common shift in
# the measuring instrument. The activity-specific variables are standardised
# over participants only: a structural zero for someone who did not do the
# activity is not an observation of low ICT intensity, and including those
# zeros would make the standardised variable a participation proxy.
# ------------------------------------------------------------
standardise_ict_within_period <- function(db, label) {
  z_stats <- list()

  m <- mean(db[[ICT_VARS_PSI]], na.rm = TRUE)
  sdv <- stats::sd(db[[ICT_VARS_PSI]], na.rm = TRUE)
  if (is.finite(sdv) && sdv > 0) {
    db[[ICT_VARS_PSI]] <- (db[[ICT_VARS_PSI]] - m) / sdv
  }
  z_stats[[ICT_VARS_PSI]] <- c(mean = m, sd = sdv, n = sum(!is.na(db[[ICT_VARS_PSI]])))

  for (v in ICT_VARS_GAMMA) {
    if (!v %in% names(db)) next
    act <- sub("^ictact_", "t_", v)
    participants <- if (act %in% names(db)) db[[act]] > 0 else rep(TRUE, nrow(db))
    m <- mean(db[[v]][participants], na.rm = TRUE)
    sdv <- stats::sd(db[[v]][participants], na.rm = TRUE)
    if (is.finite(sdv) && sdv > 0) {
      # Non-participants keep a value of 0 on the original scale, which maps
      # to the participant mean once centred, so that they contribute no
      # spurious signal to the satiation term.
      db[[v]] <- ifelse(participants, (db[[v]] - m) / sdv, 0)
    }
    z_stats[[v]] <- c(mean = m, sd = sdv, n = sum(participants, na.rm = TRUE))
  }

  attr(db, "ict_z_stats") <- z_stats
  message("  ", label, ": ICT variables standardised within period (",
          length(z_stats), " variables)")
  db
}

if (grepl("_z$", ICT_SCALING)) {
  message("Standardising ICT variables within each wave...")
  db_2014 <- standardise_ict_within_period(db_2014, "2014-2015")
  db_2023 <- standardise_ict_within_period(db_2023, "2023")

  # Record the centring and scaling constants so that coefficients on the
  # standardised variables can be translated back into share units.
  z_out <- dplyr::bind_rows(
    tibble::as_tibble(do.call(rbind, attr(db_2014, "ict_z_stats")),
                      rownames = "variable") %>% mutate(period = "2014-2015"),
    tibble::as_tibble(do.call(rbind, attr(db_2023, "ict_z_stats")),
                      rownames = "variable") %>% mutate(period = "2023")
  )
  dir.create("combined_outputs_harmonised/mdcev", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(
    z_out,
    "combined_outputs_harmonised/mdcev/mdcev_ict_standardisation_constants.csv"
  )
  print(as.data.frame(z_out))
}

# Descriptive check of what the model is actually being fed.
ict_input_summary <- dplyr::bind_rows(
  tibble::tibble(period = "2014-2015",
                 variable = c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                 mean = vapply(c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                               function(v) mean(db_2014[[v]], na.rm = TRUE), 0),
                 sd = vapply(c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                             function(v) stats::sd(db_2014[[v]], na.rm = TRUE), 0)),
  tibble::tibble(period = "2023",
                 variable = c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                 mean = vapply(c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                               function(v) mean(db_2023[[v]], na.rm = TRUE), 0),
                 sd = vapply(c(ICT_VARS_PSI, ICT_VARS_GAMMA),
                             function(v) stats::sd(db_2023[[v]], na.rm = TRUE), 0))
) %>%
  mutate(ict_scaling = ICT_SCALING)

dir.create("combined_outputs_harmonised/mdcev", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(
  ict_input_summary,
  "combined_outputs_harmonised/mdcev/mdcev_ict_input_summary.csv"
)
message("ICT variables as fed to the model:")
print(as.data.frame(ict_input_summary))

# ============================================================
# 2. Socio-demographic covariates from the raw files
#    age (categorical), employment, household, day type -- harmonised across years
# ============================================================

# ---- 2023: everything is attached per episode in the eliddi file ----
cov_2023 <- read_dta(PATH_2023_EPISODE,
                     col_select = c(mainid, diaryord, age, sex, econstat, nchild, dday)) %>%
  mutate(person_day = paste(as.character(mainid), as.character(diaryord), sep = "_")) %>%
  group_by(person_day) %>%
  summarise(across(c(age, sex, econstat, nchild, dday),
                   ~ dplyr::first(as.numeric(.x))), .groups = "drop") %>%
  mutate(
    age_band = dplyr::case_when(
      age == 1 ~ "18-29", age == 2 ~ "30-39", age == 3 ~ "40-49",
      age == 4 ~ "50-59", age == 5 ~ "60-69", age == 6 ~ "70+",
      TRUE ~ NA_character_),
    female    = dplyr::if_else(sex %in% c(1, 2), as.integer(sex == 2), NA_integer_),
    in_work   = as.integer(econstat %in% c(1, 2, 3, 4, 6)),  # self-emp / employed / casual
    has_child = as.integer(nchild > 0),
    weekend   = as.integer(dday %in% c(6, 7))
  ) %>%
  select(person_day, age_band, female, in_work, has_child, weekend)

# ---- 2014-15: diary file (DVAge, ddayw) + individual file (dilodefr, dhhtype) ----
band_age_years <- function(x) dplyr::case_when(
  x >= 18 & x < 30 ~ "18-29", x >= 30 & x < 40 ~ "30-39",
  x >= 40 & x < 50 ~ "40-49", x >= 50 & x < 60 ~ "50-59",
  x >= 60 & x < 70 ~ "60-69", x >= 70          ~ "70+",
  TRUE ~ NA_character_
)

diary14 <- read_dta(PATH_2014_DIARY,
                    col_select = c(serial, pnum, daynum, DVAge, ddayw)) %>%
  mutate(person_day = paste(serial, pnum, daynum, sep = "_")) %>%
  group_by(person_day) %>%
  summarise(serial = dplyr::first(serial), pnum = dplyr::first(pnum),
            DVAge = dplyr::first(as.numeric(DVAge)),
            ddayw = dplyr::first(as.numeric(ddayw)), .groups = "drop")

indiv14 <- read_dta(PATH_2014_INDIV,
                    col_select = c(serial, pnum, DMSex, dilodefr, dhhtype)) %>%
  mutate(DMSex = as.numeric(DMSex),
         dilodefr = as.numeric(dilodefr), dhhtype = as.numeric(dhhtype)) %>%
  distinct(serial, pnum, .keep_all = TRUE)

cov_2014 <- diary14 %>%
  left_join(indiv14, by = c("serial", "pnum")) %>%
  mutate(
    age_band  = band_age_years(DVAge),
    female    = dplyr::if_else(DMSex %in% c(1, 2), as.integer(DMSex == 2), NA_integer_),
    in_work   = as.integer(dilodefr == 1),               # ILO: in employment
    has_child = as.integer(dhhtype %in% c(2, 4)),         # couple / lone parent w/ child <=15
    weekend   = as.integer(ddayw %in% c(2, 3))            # 2 = Saturday, 3 = Sunday
  ) %>%
  select(person_day, age_band, female, in_work, has_child, weekend)

# ---- Age dummies: full 6 native bands, reference = 18-29 (5 dummies) ----
# Matches the 2023 survey's native age banding exactly.
add_age_dummies <- function(db) {
  db %>% mutate(
    age_30_39  = as.integer(age_band == "30-39"),
    age_40_49  = as.integer(age_band == "40-49"),
    age_50_59  = as.integer(age_band == "50-59"),
    age_60_69  = as.integer(age_band == "60-69"),
    age_70plus = as.integer(age_band == "70+")
  )
}

# ---- Merge covariates onto the time matrices; keep complete cases ----
attach_covariates <- function(db, cov) {
  n0 <- nrow(db)
  db2 <- db %>%
    left_join(cov, by = "person_day") %>%
    add_age_dummies() %>%
    filter(if_all(all_of(c("age_30_39", "age_40_49", "age_50_59",
                           "age_60_69", "age_70plus",
                           "female", "in_work", "has_child", "weekend")),
                  ~ !is.na(.)))
  cat(sprintf("  covariate merge: %d -> %d person-days (%d dropped for missing covariates)\n",
              n0, nrow(db2), n0 - nrow(db2)))
  db2
}

cat("2014-15:\n"); db_2014 <- attach_covariates(db_2014, cov_2014)
cat("2023:\n");    db_2023 <- attach_covariates(db_2023, cov_2023)
cat("Person-days for estimation: 2014-15 =", nrow(db_2014),
    " | 2023 =", nrow(db_2023), "\n")

# ============================================================
# 3. MDCEV specification (programmatic, so covariates are easy to toggle)
# ============================================================
alternatives <- c("personal_care", "leisure", "maintenance",
                  "travel", "shopping", "work", "other")
inside       <- c("leisure", "maintenance", "travel", "shopping", "work", "other")

# --- Covariates that enter every inside good's baseline utility --------------
# These are general day/person controls.
common_covs <- c("ict_share", "female",
                 "age_30_39", "age_40_49", "age_50_59", "age_60_69", "age_70plus",
                 "in_work", "has_child", "weekend")   # out_share removed: endogenous (a deterministic function of the out-of-home activity durations being modelled)

# Activity-specific ICT variable used by each inside good.
# In this specification, it enters gamma_k (duration/satiation), NOT V/psi.
activity_ict_vars <- c(
  leisure     = "ictact_leisure",
  maintenance = "ictact_maintenance",
  travel      = "ictact_travel",
  shopping    = "ictact_shopping",
  work        = "ictact_work",
  other       = "ictact_other"
)

# Pooled model interactions.
# To make the pooled-period coefficients directly comparable with the two
# separately estimated models, all baseline-utility covariates are interacted
# with d2023. The pooled model also includes a period interaction in the
# log-gamma intercept for every activity. Without that log-gamma shift, the
# model forces the 2014-15 and 2023 gamma intercepts to be identical and may
# incorrectly load period differences onto the ICT_activity interaction.
pooled_common_interactions <- common_covs

# starting values: alpha_base, one log-gamma per inside good, one constant per
# inside good, common psi covariate coefficients, and one gamma/duration ICT
# coefficient per inside good.
make_beta <- function(common_covs,
                      include_period_interactions = FALSE,
                      pooled_common_interactions = common_covs) {
  # Default is now the FULL covariate set: a pooled call that forgets to pass
  # pooled_common_interactions will still be fully interacted, never silently
  # constrain the controls across periods.
  b <- c(alpha_base = -5)                       # fixed -> alpha ≈ 0.0067

  # log_gamma is exponentiated inside apollo_probabilities so gamma_k stays positive.
  for (k in inside) b <- c(b, setNames(0, paste0("log_gamma_", k)))

  # Baseline utility constants for inside goods, relative to personal care.
  for (k in inside) b <- c(b, setNames(0, paste0("b_", k)))

  # General covariates enter psi_k / baseline utility for each inside good.
  # ict_share is included here: ICT_all -> participation / baseline utility.
  for (k in inside) {
    for (cc in common_covs) {
      b <- c(b, setNames(0, paste0("b_", cc, "_", k)))
    }

    # Matching activity-specific ICT enters gamma_k / duration-satiation.
    # This is on log-gamma scale because gamma_k = exp(...).
    b <- c(b, setNames(0, paste0("theta_gamma_ict_activity_", k)))
  }

  if (include_period_interactions) {
    for (k in inside) {
      # Period-specific shift in the baseline log-gamma. This is essential for
      # aligning the pooled gamma specification with the separate-period models.
      b <- c(b, setNames(0, paste0("log_gamma_d2023_", k)))

      b <- c(b, setNames(0, paste0("b_d2023_", k)))

      for (cc in pooled_common_interactions) {
        b <- c(b, setNames(0, paste0("b_", cc, "_d2023_", k)))
      }

      # Period interaction for the matching activity-specific ICT in gamma_k.
      b <- c(b, setNames(0, paste0("theta_gamma_ict_activity_d2023_", k)))
    }
  }

  b
}

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()

  inside       <- apollo_inputs$inside
  common_covs  <- apollo_inputs$common_covs
  activity_ict_vars <- apollo_inputs$activity_ict_vars
  alternatives <- apollo_inputs$alternatives
  include_period_interactions <- apollo_inputs$include_period_interactions
  pooled_common_interactions  <- apollo_inputs$pooled_common_interactions

  V <- list(); V[["personal_care"]] <- 0
  gamma <- list()

  for (k in inside) {
    # ---------------- psi_k / baseline utility ----------------
    # General controls, including ict_share (ICT_all), enter psi_k.
    common_part <- Reduce(
      `+`,
      lapply(common_covs, function(cc) {
        get(paste0("b_", cc, "_", k)) * get(cc)
      })
    )

    V[[k]] <- get(paste0("b_", k)) + common_part

    # Optional pooled 2023 interactions in psi_k.
    if (include_period_interactions) {
      V[[k]] <- V[[k]] + get(paste0("b_d2023_", k)) * d2023

      for (cc in pooled_common_interactions) {
        V[[k]] <- V[[k]] +
          get(paste0("b_", cc, "_d2023_", k)) * get(cc) * d2023
      }
    }

    # ---------------- gamma_k / duration-satiation -------------
    # Matching activity-specific ICT exposure enters gamma_k.
    # Example: when k == "travel", this uses ictact_travel.
    # gamma is exponentiated to keep the translation parameter positive.
    ict_var_name <- activity_ict_vars[[k]]
    gamma_linear <- get(paste0("log_gamma_", k)) +
      get(paste0("theta_gamma_ict_activity_", k)) * get(ict_var_name)

    # Optional pooled 2023 interaction in gamma_k.
    if (include_period_interactions) {
      gamma_linear <- gamma_linear +
        get(paste0("log_gamma_d2023_", k)) * d2023 +
        get(paste0("theta_gamma_ict_activity_d2023_", k)) * get(ict_var_name) * d2023
    }

    gamma[[k]] <- exp(gamma_linear)
  }

  a     <- 1 / (1 + exp(-alpha_base))           # single alpha (gamma-profile)
  alpha <- setNames(rep(list(a), length(alternatives)), alternatives)
  cost  <- setNames(as.list(rep(1, length(alternatives))), alternatives)
  avail <- setNames(as.list(rep(1, length(alternatives))), alternatives)

  continuousChoice <- list(
    personal_care = t_personal_care, leisure = t_leisure,
    maintenance = t_maintenance, travel = t_travel,
    shopping = t_shopping, work = t_work, other = t_other
  )

  mdcev_settings <- list(
    alternatives     = alternatives,
    avail            = avail,
    continuousChoice = continuousChoice,
    V                = V,
    alpha            = alpha,
    gamma            = gamma,
    sigma            = 1,
    cost             = cost,
    budget           = budget,
    outside          = "personal_care"
  )

  P[["model"]] <- apollo_mdcev(mdcev_settings, functionality)
  P <- apollo_weighting(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

run_mdcev <- function(db, model_name,
                      common_covs_model = common_covs,
                      include_period_interactions = FALSE,
                      pooled_common_interactions_model = pooled_common_interactions) {
  apollo_initialise()
  apollo_control <<- list(
    modelName       = model_name,
    indivID         = "person_day",
    outputDirectory = "combined_outputs_harmonised/mdcev",
    weights         = "w",
    panelData       = FALSE,
    nCores          = 1
  )
  database     <<- as.data.frame(db)
  apollo_beta  <<- make_beta(
    common_covs_model,
    include_period_interactions = include_period_interactions,
    pooled_common_interactions = pooled_common_interactions_model
  )
  apollo_fixed <<- c("alpha_base")

  # ---- cache lookup -------------------------------------------------
  cache_file <- file.path(MODEL_CACHE_DIR, paste0(model_name, "_model.rds"))
  fp <- model_fingerprint(db, names(apollo_beta))

  if (isTRUE(REUSE_SAVED_MODELS) && file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$fingerprint, fp) &&
        !is.null(cached$model$estimate)) {
      message("  [cache] ", model_name,
              ": reusing the saved model (estimated ",
              format(cached$created, "%Y-%m-%d %H:%M"), ").")
      return(cached$model)
    }
    if (!is.null(cached)) {
      message("  [cache] ", model_name,
              ": saved model found but the inputs have changed; re-estimating.")
    }
  }
  message("  [cache] ", model_name, ": estimating from scratch...")
  # -------------------------------------------------------------------

  apollo_inputs <<- apollo_validateInputs()

  apollo_inputs$inside       <<- inside
  apollo_inputs$common_covs  <<- common_covs_model
  apollo_inputs$activity_ict_vars <<- activity_ict_vars
  apollo_inputs$alternatives <<- alternatives
  apollo_inputs$include_period_interactions <<- include_period_interactions
  apollo_inputs$pooled_common_interactions  <<- pooled_common_interactions_model

  model <- apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs)
  apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
  apollo_saveOutput(model)

  # Store the model with the fingerprint of the inputs that produced it.
  saveRDS(
    list(model = model, fingerprint = fp, ict_scaling = ICT_SCALING,
         created = Sys.time()),
    cache_file
  )
  message("  [cache] ", model_name, ": saved to ", cache_file)

  model
}


# ============================================================
# 4. Estimate separately for each period
# ============================================================
model_2014 <- run_mdcev(db_2014,
  paste0("mdcev_2014_2015_ictall_psi_ictact_gamma_fixed_budget", ICT_TAG))
model_2023 <- run_mdcev(db_2023,
  paste0("mdcev_2023_ictall_psi_ictact_gamma_fixed_budget", ICT_TAG))

# ============================================================
# 5. Compare coefficients across periods
# ============================================================
tidy_est <- function(model, suffix) {
  est <- model$estimate
  se  <- tryCatch(model$robse, error = function(e) model$se)
  tibble(parameter = names(est),
         !!paste0("est_", suffix) := as.numeric(est),
         !!paste0("se_",  suffix) := as.numeric(se[names(est)]))
}

comparison <- full_join(
  tidy_est(model_2014, "2014"),
  tidy_est(model_2023, "2023"),
  by = "parameter"
) %>%
  mutate(
    diff_2023_minus_2014 = est_2023 - est_2014,
    type = dplyr::case_when(
      grepl("^theta_gamma_ict_activity_", parameter) ~ "activity-specific ICT effect in gamma",
      grepl("^b_ict_share_",  parameter) ~ "ICT-overall effect",
      grepl("^b_out_share_",  parameter) ~ "out-of-home effect",
      grepl("^b_age_",        parameter) ~ "age effect",
      grepl("^b_female_",     parameter) ~ "sex effect",
      grepl("^b_in_work_",    parameter) ~ "employment effect",
      grepl("^b_has_child_",  parameter) ~ "household effect",
      grepl("^b_weekend_",    parameter) ~ "day-type effect",
      grepl("^b_",            parameter) ~ "baseline utility",
      grepl("^log_gamma_",    parameter) ~ "baseline log-gamma (translation)",
      TRUE                               ~ "other"
    )
  ) %>%
  arrange(type, parameter)

print(comparison, n = 200)
write_csv(comparison,
          tag_path("combined_outputs_harmonised/mdcev/mdcev_coefficient_comparison_2014_vs_2023_ictall_psi_ictact_gamma.csv"))

cat("\nParameter naming: b_<covariate>_<activity>. Covariates:\n")
cat("  ict_share  = overall diary-day ICT exposure share, not minutes\n")
cat("  ictact_<activity> = share of that activity time recorded in ICT-involved episodes; enters gamma_k/duration  [NEW]\n")
cat("  out_share  = share of the day spent out of home\n")
cat("  age_30_39 ... age_70plus = 6 native age bands (ref 18-29)  [NEW]\n")
cat("  female     = sex (1 = female)  [ADDED BACK]\n")
cat("  in_work    = ILO in-employment (2014) / employed-or-self-employed (2023)\n")
cat("  has_child  = child in household\n")
cat("  weekend    = Saturday/Sunday diary day\n")
cat("Compare SIGN and pattern across 2014 vs 2023. Note: ict_share coefficients are in psi; ictact coefficients are in log-gamma.\n")

# ============================================================
# 5c. Figures for the slide deck
#     P7 = ICT_all in psi vs ICT_activity in gamma
#     P8 = travel-focus ICT coefficient plot
# ============================================================
library(ggplot2)

fig_dir     <- "combined_outputs_harmonised/mdcev"
period_cols <- c("2014-2015" = "#F8766D", "2023" = "#00BFC4")
act_levels  <- c("Social & leisure", "Maintenance", "Travel", "Shopping", "Work", "Other")

deck_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", hjust = 0),
    plot.subtitle    = element_text(hjust = 0),
    plot.title.position = "plot",
    strip.text       = element_text(face = "bold"),
    legend.position  = "top"
  )

# Long format: one row per activity x ICT channel x period, with 95% CI.
# Parameter names expected:
#   b_ict_share_<activity>                 [ICT_all in psi_k]
#   theta_gamma_ict_activity_<activity>    [ICT_activity in gamma_k]
ict_long <- comparison %>%
  filter(
    grepl("^b_ict_share_", parameter) |
      grepl("^theta_gamma_ict_activity_", parameter)
  ) %>%
  mutate(
    channel = case_when(
      grepl("^b_ict_share_", parameter) ~
        "ICT_all in baseline utility (psi)",
      grepl("^theta_gamma_ict_activity_", parameter) ~
        "ICT_activity in duration/satiation (gamma)",
      TRUE ~ NA_character_
    ),
    activity = case_when(
      grepl("^b_ict_share_", parameter) ~
        sub("^b_ict_share_", "", parameter),
      grepl("^theta_gamma_ict_activity_", parameter) ~
        sub("^theta_gamma_ict_activity_", "", parameter),
      TRUE ~ NA_character_
    )
  ) %>%
  select(activity, channel, est_2014, se_2014, est_2023, se_2023) %>%
  tidyr::pivot_longer(
    cols = c(est_2014, se_2014, est_2023, se_2023),
    names_to = c(".value", "period"),
    names_pattern = "(est|se)_([0-9]+)"
  ) %>%
  mutate(
    period = recode(period, "2014" = "2014-2015", "2023" = "2023"),
    lo = est - 1.96 * se,
    hi = est + 1.96 * se,
    channel = factor(
      channel,
      levels = c("ICT_all in baseline utility (psi)",
                 "ICT_activity in duration/satiation (gamma)")
    ),
    activity = dplyr::recode(
      activity,
      leisure = "Social & leisure",
      maintenance = "Maintenance",
      travel = "Travel",
      shopping = "Shopping",
      work = "Work",
      other = "Other",
      .default = NA_character_
    ),
    activity = factor(activity, levels = act_levels)
  ) %>%
  filter(!is.na(activity), !is.na(est), !is.na(se))

write_csv(
  ict_long,
  tag_path(file.path(fig_dir, "mdcev_ict_coefficients_long_for_figures_ictall_psi_ictact_gamma.csv"))
)

# ---- P7: two-channel ICT coefficient plot ----
p_ict_two_channel <- ggplot(
  ict_long,
  aes(x = est, y = activity, colour = period)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.25,
    position = position_dodge(width = 0.6),
    linewidth = 0.6
  ) +
  geom_point(size = 2.6, position = position_dodge(width = 0.6)) +
  facet_wrap(~ channel) +
  scale_y_discrete(limits = rev(act_levels)) +
  scale_colour_manual(values = period_cols) +
  labs(
    title = "Two ICT channels in the MDCEV model",
    subtitle = "ICT_all enters baseline utility (psi); ICT_activity enters gamma/duration; 95% CI",
    x = "MDCEV coefficient (psi coefficient or log-gamma coefficient)",
    y = NULL,
    colour = "Period"
  ) +
  deck_theme

ggsave(
  tag_path(file.path(fig_dir, "P7_ictall_psi_vs_ictactivity_gamma.png")),
  p_ict_two_channel,
  width = 11,
  height = 5.5,
  dpi = 300
)

# ---- P8: travel-focus plot ----
travel_focus <- ict_long %>%
  filter(activity == "Travel") %>%
  mutate(lab = sprintf("%+.2f", est))

p_travel_focus <- ggplot(
  travel_focus,
  aes(x = est, y = channel, colour = period)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(
    aes(xmin = lo, xmax = hi),
    height = 0.18,
    position = position_dodge(width = 0.5),
    linewidth = 0.7
  ) +
  geom_point(size = 3.6, position = position_dodge(width = 0.5)) +
  geom_text(
    aes(x = hi, label = lab),
    position = position_dodge(width = 0.5),
    hjust = -0.25,
    size = 4,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = period_cols) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.22))) +
  labs(
    title = "The travel story: ICT_all vs ICT within travel",
    subtitle = "ICT_all affects travel baseline utility; ICT within travel affects gamma/duration; 95% CI",
    x = "MDCEV coefficient",
    y = NULL,
    colour = "Period"
  ) +
  deck_theme

ggsave(
  tag_path(file.path(fig_dir, "P8_travel_ictall_psi_vs_ictactivity_gamma.png")),
  p_travel_focus,
  width = 9,
  height = 4.2,
  dpi = 300
)

message(
  "Deck figures written to ", fig_dir,
  ": P7_ictall_psi_vs_ictactivity_gamma.png, ",
  "P8_travel_ictall_psi_vs_ictactivity_gamma.png"
)

# ============================================================
# 6. Fully interacted pooled model with period interactions
# ============================================================
# This provides a direct test of whether the ICT associations changed in 2023.
# All nuisance coefficients in psi and the baseline log-gamma intercepts are
# allowed to differ by period. This makes the pooled ICT coefficients align with
# the separate models, while the d2023 interactions provide formal change tests.

make_pooled_db <- function(db_2014, db_2023) {
  bind_rows(
    db_2014 %>% mutate(period_model = "2014-2015", d2023 = 0),
    db_2023 %>% mutate(period_model = "2023",      d2023 = 1)
  )
}

db_pooled <- make_pooled_db(db_2014, db_2023)

# Enforce the "fully interacted" specification: every psi covariate that is free
# in the separate models must also carry a d2023 interaction in the pooled model.
# (log_gamma intercepts, activity constants and the gamma ICT term are always
# interacted below.) alpha_base is fixed in both separate models, so it is
# intentionally left common here.
stopifnot(setequal(pooled_common_interactions, common_covs))

model_pooled <- run_mdcev(
  db_pooled,
  paste0("mdcev_pooled_2014_2023_ictall_psi_ictact_gamma_fully_interacted", ICT_TAG),
  common_covs_model = common_covs,
  include_period_interactions = TRUE,
  pooled_common_interactions_model = pooled_common_interactions
)

# ------------------------------------------------------------
# 7. Extract pooled interaction tests
# ------------------------------------------------------------
pooled_est <- model_pooled$estimate
pooled_se  <- tryCatch(model_pooled$robse, error = function(e) model_pooled$se)

pooled_common_tests <- tidyr::expand_grid(
  covariate = pooled_common_interactions,
  activity = inside
) %>%
  mutate(
    base_parameter = paste0("b_", covariate, "_", activity),
    interaction_parameter = paste0("b_", covariate, "_d2023_", activity),
    effect_2014 = as.numeric(pooled_est[base_parameter]),
    interaction_2023_minus_2014 = as.numeric(pooled_est[interaction_parameter]),
    interaction_rob_se = as.numeric(pooled_se[interaction_parameter]),
    effect_2023 = effect_2014 + interaction_2023_minus_2014,
    interaction_t = interaction_2023_minus_2014 / interaction_rob_se,
    type = ifelse(covariate == "ict_share", "overall ICT interaction", "out-of-home interaction")
  )

pooled_activity_ict_tests <- tibble(activity = inside) %>%
  mutate(
    covariate = "ict_activity_in_gamma",
    base_parameter = paste0("theta_gamma_ict_activity_", activity),
    interaction_parameter = paste0("theta_gamma_ict_activity_d2023_", activity),
    effect_2014 = as.numeric(pooled_est[base_parameter]),
    interaction_2023_minus_2014 = as.numeric(pooled_est[interaction_parameter]),
    interaction_rob_se = as.numeric(pooled_se[interaction_parameter]),
    effect_2023 = effect_2014 + interaction_2023_minus_2014,
    interaction_t = interaction_2023_minus_2014 / interaction_rob_se,
    type = "activity-specific ICT interaction in gamma"
  )

pooled_interaction_tests <- bind_rows(
  pooled_common_tests,
  pooled_activity_ict_tests
) %>%
  mutate(
    interpretation = dplyr::case_when(
      is.na(interaction_t) ~ "not estimated / no SE",
      abs(interaction_t) >= 1.96 & interaction_2023_minus_2014 > 0 ~
        "significantly stronger in 2023",
      abs(interaction_t) >= 1.96 & interaction_2023_minus_2014 < 0 ~
        "significantly weaker in 2023",
      TRUE ~ "no clear change"
    )
  ) %>%
  arrange(type, covariate, activity)

# Diagnostic: the fully interacted pooled-period effects should be close to
# the estimates from the two separate models. Small numerical differences can
# remain, but opposite time trends should no longer be created by a constrained
# gamma intercept.
pooled_vs_separate_check <- pooled_interaction_tests %>%
  filter(covariate %in% c("ict_share", "ict_activity_in_gamma")) %>%
  mutate(
    separate_parameter = dplyr::if_else(
      covariate == "ict_share",
      paste0("b_ict_share_", activity),
      paste0("theta_gamma_ict_activity_", activity)
    )
  ) %>%
  left_join(
    comparison %>%
      select(parameter, separate_2014 = est_2014, separate_2023 = est_2023),
    by = c("separate_parameter" = "parameter")
  ) %>%
  mutate(
    pooled_minus_separate_2014 = effect_2014 - separate_2014,
    pooled_minus_separate_2023 = effect_2023 - separate_2023
  )

write_csv(
  pooled_vs_separate_check,
  tag_path("combined_outputs_harmonised/mdcev/mdcev_pooled_vs_separate_ICT_alignment_check.csv")
)

# Guardrail: with a fully interacted pooled model the pooled period effects must
# reproduce the separate-model estimates up to numerical tolerance. A large gap
# (especially one that appears only in 2023) means some period-varying parameter
# is still being held common across periods.
.align_gap <- max(abs(c(
  pooled_vs_separate_check$pooled_minus_separate_2014,
  pooled_vs_separate_check$pooled_minus_separate_2023
)), na.rm = TRUE)
if (is.finite(.align_gap) && .align_gap > 0.05) {
  warning(sprintf(
    paste0("Pooled vs separate ICT estimates differ by up to %.3f. ",
           "The pooled model may not be fully interacted (check log_gamma_d2023 ",
           "and that pooled_common_interactions == common_covs)."),
    .align_gap
  ))
} else {
  message(sprintf(
    "Alignment OK: max |pooled - separate| = %.3f (fully interacted pooled).",
    .align_gap
  ))
}

print(pooled_interaction_tests, n = 100)

write_csv(
  pooled_interaction_tests,
  tag_path("combined_outputs_harmonised/mdcev/mdcev_pooled_interaction_tests_ictall_psi_ictact_gamma_aligned.csv")
)

# ------------------------------------------------------------
# 7b. Controls: pooled 2014 -> 2023 interaction tests (psi / baseline utility)
# ------------------------------------------------------------
# The fully interacted pooled model already interacts EVERY psi covariate with
# d2023, so the demographic / context controls (sex, age bands, employment,
# household, day type) are tested for change exactly like ICT above. Here we
# just pull them out of pooled_interaction_tests, label them clearly, and export
# them for their own "how demographic gaps changed" slide. No re-estimation.
control_covs <- setdiff(common_covs, "ict_share")   # out_share no longer a covariate

pooled_control_tests <- pooled_interaction_tests %>%
  filter(covariate %in% control_covs) %>%
  mutate(type = "control interaction (psi / baseline utility)") %>%
  arrange(covariate, activity)

write_csv(
  pooled_control_tests,
  tag_path("combined_outputs_harmonised/mdcev/mdcev_pooled_control_interaction_tests.csv")
)

# Console summary: which control -> activity gradients significantly shifted in 2023
message("\nControl interaction tests (2023 vs 2014-15) - significant shifts only:")
pooled_control_tests %>%
  filter(interpretation %in% c("significantly stronger in 2023",
                               "significantly weaker in 2023")) %>%
  transmute(covariate, activity,
            change_2023_minus_2014 = round(interaction_2023_minus_2014, 3),
            t = round(interaction_t, 2),
            interpretation) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

cat("
Pooled interaction interpretation:
")
cat("  effect_2014 = base coefficient in the pooled model
")
cat("  interaction_2023_minus_2014 = direct test of whether the association changed in 2023
")
cat("  effect_2023 = effect_2014 + interaction_2023_minus_2014
")
cat("Focus especially on theta_gamma_ict_activity_d2023_travel: whether the duration/gamma association of ICT within travel changed in 2023.
")
cat("
ICT variable note:
")
cat("  ICT measures are shares/proportions, not exact ICT minutes.
")
cat("  ictact_travel means: travel episode minutes involving ICT / total travel episode minutes.
")
# ============================================================


# ============================================================
# 8. Forest plot of pooled 2023 interaction effects
#    Two channels only:
#      psi channel  = ICT_all interaction in baseline utility
#      gamma channel = activity-specific ICT interaction in gamma
# ============================================================

pooled_forest <- pooled_interaction_tests %>%
  filter(
    covariate == "ict_share" |
      covariate == "ict_activity_in_gamma"
  ) %>%
  mutate(
    channel = dplyr::case_when(
      covariate == "ict_share" ~
        "psi channel: ICT_all in baseline utility",
      covariate == "ict_activity_in_gamma" ~
        "gamma channel: ICT_activity in duration/translation",
      TRUE ~ NA_character_
    ),
    activity_label = dplyr::recode(
      activity,
      leisure = "Social & leisure",
      maintenance = "Maintenance",
      travel = "Travel",
      shopping = "Shopping",
      work = "Work",
      other = "Other"
    ),
    lo = interaction_2023_minus_2014 - 1.96 * interaction_rob_se,
    hi = interaction_2023_minus_2014 + 1.96 * interaction_rob_se,
    significance_group = dplyr::case_when(
      interpretation == "significantly stronger in 2023" ~ "Significantly stronger in 2023",
      interpretation == "significantly weaker in 2023" ~ "Significantly weaker in 2023",
      interpretation == "no clear change" ~ "No clear change",
      TRUE ~ "No standard error"
    ),
    significance_group = factor(
      significance_group,
      levels = c(
        "Significantly stronger in 2023",
        "Significantly weaker in 2023",
        "No clear change",
        "No standard error"
      )
    ),
    activity_label = factor(
      activity_label,
      levels = rev(c("Social & leisure", "Maintenance", "Travel", "Shopping", "Work", "Other"))
    )
  ) %>%
  filter(
    !is.na(interaction_2023_minus_2014),
    !is.na(interaction_rob_se)
  )

# Use plain-text facet labels to avoid device-specific parsing issues.
pooled_forest <- pooled_forest %>%
  mutate(
    channel_label = dplyr::case_when(
      covariate == "ict_share" ~ "ψ channel: ICT_all in baseline utility",
      covariate == "ict_activity_in_gamma" ~ "γ channel: ICT_activity in duration/translation",
      TRUE ~ NA_character_
    ),
    channel_label = factor(
      channel_label,
      levels = c(
        "ψ channel: ICT_all in baseline utility",
        "γ channel: ICT_activity in duration/translation"
      )
    )
  )

write_csv(
  pooled_forest %>%
    select(
      channel_label, activity = activity_label,
      interaction_2023_minus_2014, interaction_rob_se,
      lo, hi, significance_group, interpretation
    ),
  tag_path(file.path(fig_dir, "mdcev_pooled_ict_interactions_for_forest_plot_aligned.csv"))
)

pooled_interaction_cols <- c(
  "Significantly stronger in 2023" = "#2C7FB8",
  "Significantly weaker in 2023" = "#D95F0E",
  "No clear change" = "#7A7A7A",
  "No standard error" = "#BDBDBD"
)

p_pooled_forest <- ggplot(
  pooled_forest,
  aes(
    x = interaction_2023_minus_2014,
    y = activity_label,
    colour = significance_group
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_errorbar(
    aes(xmin = lo, xmax = hi),
    orientation = "y",
    width = 0.20,
    linewidth = 0.75
  ) +
  geom_point(size = 3.1) +
  facet_wrap(~ channel_label, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = pooled_interaction_cols, drop = FALSE) +
  labs(
    title = "How ICT associations changed in 2023",
    subtitle = "Pooled-model interaction estimates (2023 minus 2014–15) with 95% confidence intervals",
    x = "Interaction coefficient: 2023 minus 2014–15",
    y = NULL,
    colour = "Change classification",
    caption = "Positive values indicate a stronger coefficient in 2023; negative values indicate a weaker coefficient."
  ) +
  deck_theme +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing.x = grid::unit(1.2, "lines")
  )

ggsave(
  tag_path(file.path(fig_dir, "P9_pooled_ICT_interaction_forest_plot_aligned.png")),
  p_pooled_forest,
  width = 12,
  height = 5.8,
  dpi = 300
)

message(
  "Pooled-model forest plot written to ",
  tag_path(file.path(fig_dir, "P9_pooled_ICT_interaction_forest_plot_aligned.png"))
)


# ============================================================
# 10. CONTROL interaction forest plots (demographic / context)
#     Same style as the ICT pooled forest (P9). Three themed panels:
#       P11 = work & schedule (employment, day type)
#       P12 = age gradients (leisure & other across age bands)
#       P13 = gender & household (female, has_child)
#     Reuses fig_dir, deck_theme, pooled_interaction_cols from above.
# ============================================================

control_forest_base <- pooled_control_tests %>%
  filter(!is.na(interaction_2023_minus_2014), !is.na(interaction_rob_se)) %>%
  mutate(
    pretty_cov = dplyr::recode(
      covariate,
      female     = "Female",
      in_work    = "Employed",
      has_child  = "Has child",
      weekend    = "Weekend",
      age_30_39  = "Age 30-39",
      age_40_49  = "Age 40-49",
      age_50_59  = "Age 50-59",
      age_60_69  = "Age 60-69",
      age_70plus = "Age 70+"
    ),
    pretty_act = dplyr::recode(
      activity,
      leisure = "Social & leisure", maintenance = "Maintenance",
      travel = "Travel", shopping = "Shopping", work = "Work", other = "Other"
    ),
    row_label = paste0(pretty_cov, " \u2192 ", pretty_act),
    lo = interaction_2023_minus_2014 - 1.96 * interaction_rob_se,
    hi = interaction_2023_minus_2014 + 1.96 * interaction_rob_se,
    significance_group = factor(
      dplyr::case_when(
        interpretation == "significantly stronger in 2023" ~ "Significantly stronger in 2023",
        interpretation == "significantly weaker in 2023"   ~ "Significantly weaker in 2023",
        interpretation == "no clear change"                ~ "No clear change",
        TRUE ~ "No standard error"
      ),
      levels = c("Significantly stronger in 2023", "Significantly weaker in 2023",
                 "No clear change", "No standard error")
    )
  )

.control_caption <- "Positive = stronger association in 2023; negative = weaker. Baseline-utility (psi) coefficients; reference = personal care."

# ---- P11: work & schedule (employment + day type), all activities ----
# Split into two narrative blocks (Employment / Weekend); within each block, order activities to match the narrative and place grey rows at the bottom,
# use vertical facet_grid panels, with the left strip serving as the subsection heading and matching the two text blocks on the right.

# Order within each family: listed here from bottom to top = factor levels (put the item intended for the top last).
emp_bottom_to_top <- c("Other", "Maintenance", "Social & leisure",
                       "Shopping", "Travel", "Work")           # Top: Work/Travel/Shopping
wkn_bottom_to_top <- c("Other", "Maintenance", "Shopping",
                       "Travel", "Social & leisure", "Work")   # Top: Work/Leisure/Travel

p11_df <- control_forest_base %>%
  filter(covariate %in% c("in_work", "weekend")) %>%
  mutate(
    family = dplyr::recode(covariate,
                           in_work = "Employment's grip loosened",
                           weekend = "The weekday/weekend divide sharpened"),
    family = factor(family, levels = c("Employment's grip loosened",
                                       "The weekday/weekend divide sharpened")),
    # Prefix with covariate so the two blocks can be ordered independently (activities with the same name do not interfere with each other).
    y_key = paste(covariate, pretty_act, sep = "___"),
    y_key = factor(y_key, levels = c(
      paste("in_work", emp_bottom_to_top, sep = "___"),
      paste("weekend", wkn_bottom_to_top, sep = "___")
    ))
  )

p_ctrl_work <- ggplot(p11_df,
                      aes(x = interaction_2023_minus_2014, y = y_key, colour = significance_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.20, linewidth = 0.75) +
  geom_point(size = 3.1) +
  facet_grid(family ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +   # Axis shows activity names only
  scale_colour_manual(values = pooled_interaction_cols, drop = FALSE) +
  labs(title = "Work and schedule structure loosened by 2023",
       subtitle = "Pooled-model interaction estimates (2023 minus 2014-15) with 95% CI",
       x = "Interaction coefficient: 2023 minus 2014-15", y = NULL,
       colour = "Change classification", caption = .control_caption) +
  deck_theme +
  theme(legend.position   = "bottom", legend.box = "vertical",
        strip.placement   = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 0),
        panel.spacing.y   = grid::unit(0.9, "lines"))

ggsave(tag_path(file.path(fig_dir, "P11_controls_work_schedule_forest.png")),
       p_ctrl_work, width = 12, height = 6.5, dpi = 300)
message("Control forest written to ", tag_path(file.path(fig_dir, "P11_controls_work_schedule_forest.png")))
# ---- P12: age gradients, leisure & other across age bands (facet by activity) ----
age_levels <- c("Age 30-39", "Age 40-49", "Age 50-59", "Age 60-69", "Age 70+")
p_ctrl_age <- control_forest_base %>%
  filter(grepl("^age_", covariate), activity %in% c("leisure", "other")) %>%
  mutate(pretty_cov = factor(pretty_cov, levels = rev(age_levels)),
         pretty_act = factor(pretty_act, levels = c("Social & leisure", "Other"))) %>%
  ggplot(aes(x = interaction_2023_minus_2014, y = pretty_cov,
             colour = significance_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.20, linewidth = 0.75) +
  geom_point(size = 3.1) +
  facet_wrap(~ pretty_act, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = pooled_interaction_cols, drop = FALSE) +
  labs(title = "Age gradients shifted: mid-life leisure compressed, 'Other' rose",
       subtitle = "Interaction estimates vs the 18-29 reference (2023 minus 2014-15), 95% CI",
       x = "Interaction coefficient: 2023 minus 2014-15", y = NULL,
       colour = "Change classification", caption = .control_caption) +
  deck_theme +
  theme(legend.position = "bottom", legend.box = "vertical",
        strip.text = element_text(face = "bold", size = 11),
        panel.spacing.x = grid::unit(1.2, "lines"))

ggsave(tag_path(file.path(fig_dir, "P12_controls_age_forest.png")),
       p_ctrl_age, width = 12, height = 5.0, dpi = 300)
message("Control forest written to ", tag_path(file.path(fig_dir, "P12_controls_age_forest.png")))

# ---- P13: gender & household (female + has_child), all activities ----
p_ctrl_gh <- control_forest_base %>%
  filter(covariate %in% c("female", "has_child")) %>%
  ggplot(aes(x = interaction_2023_minus_2014,
             y = reorder(row_label, interaction_2023_minus_2014),
             colour = significance_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.6) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.20, linewidth = 0.75) +
  geom_point(size = 3.1) +
  scale_colour_manual(values = pooled_interaction_cols, drop = FALSE) +
  labs(title = "Gender and household: durable, with two exceptions",
       subtitle = "Pooled-model interaction estimates (2023 minus 2014-15) with 95% CI",
       x = "Interaction coefficient: 2023 minus 2014-15", y = NULL,
       colour = "Change classification", caption = .control_caption) +
  deck_theme +
  theme(legend.position = "bottom", legend.box = "vertical")

ggsave(tag_path(file.path(fig_dir, "P13_controls_gender_household_forest.png")),
       p_ctrl_gh, width = 12, height = 6.0, dpi = 300)
message("Control forest written to ", tag_path(file.path(fig_dir, "P13_controls_gender_household_forest.png")))

# ============================================================
# 11. Per-activity ICT two-channel forests (one figure per activity)
#     Same style as P8 (travel): psi row + gamma row, coloured by period,
#     with the coefficient value labelled. Reuses ict_long, period_cols,
#     deck_theme, fig_dir. Depends only on the two SEPARATE models
#     (via ict_long) -- the pooled model is NOT needed for these.
# ============================================================

activity_slugs <- c(
  "Social & leisure" = "leisure",
  "Maintenance"      = "maintenance",
  "Travel"           = "travel",
  "Shopping"         = "shopping",
  "Work"             = "work",
  "Other"            = "other"
)

for (act in names(activity_slugs)) {
  slug <- activity_slugs[[act]]

  df_act <- ict_long %>%
    filter(activity == act) %>%
    mutate(lab = sprintf("%+.2f", est))

  if (nrow(df_act) == 0) {
    message("Skipping ", act, " (no rows in ict_long)")
    next
  }

  p_act <- ggplot(df_act, aes(x = est, y = channel, colour = period)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_errorbarh(
      aes(xmin = lo, xmax = hi),
      height = 0.18,
      position = position_dodge(width = 0.5),
      linewidth = 0.7
    ) +
    geom_point(size = 3.6, position = position_dodge(width = 0.5)) +
    geom_text(
      aes(x = hi, label = lab),
      position = position_dodge(width = 0.5),
      hjust = -0.25,
      size = 4,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = period_cols) +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.22))) +
    labs(
      title = NULL,
      subtitle = sprintf(
        "ICT_all affects %s baseline utility (psi); ICT within %s affects gamma/duration; 95%% CI",
        slug, slug
      ),
      x = "MDCEV coefficient",
      y = NULL,
      colour = "Period"
    ) +
    deck_theme

  ggsave(
    file.path(fig_dir, paste0("P14_by_activity_", slug, ICT_TAG, ".png")),
    p_act,
    width = 9,
    height = 4.2,
    dpi = 300
  )
  message("Per-activity forest written: ",
          file.path(fig_dir, paste0("P14_by_activity_", slug, ICT_TAG, ".png")))
}

