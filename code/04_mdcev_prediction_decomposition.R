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
  ict_wide <- prim %>%
    group_by(person_day, act_group2) %>%
    summarise(ict_episode_minutes = sum(time * any_ict, na.rm = TRUE), .groups = "drop") %>%
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
      ict_share  = sum(time * any_ict, na.rm = TRUE) / sum(time, na.rm = TRUE),
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
common_covs <- c("ict_share", "out_share", "female",
                 "age_30_39", "age_40_49", "age_50_59", "age_60_69", "age_70plus",
                 "in_work", "has_child", "weekend")

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
  model
}


# ============================================================
# 4. Estimate separately for each period
# ============================================================



# ============================================================
# 5. FROM COEFFICIENTS TO SUBSTANCE
#    STEP 1: baseline forecast validation
#    STEP 2: psi/gamma channel decomposition
# ------------------------------------------------------------
# Exact compatibility with the final model above:
#   alternatives = personal_care, leisure, maintenance,
#                  travel, shopping, work, other
#   outside good = personal_care
#   ICT_all      = ict_share in psi
#   ICT_activity = ictact_k in log(gamma_k)
#
# This section does NOT re-estimate any model.
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)

POST_DIR <- "combined_outputs_harmonised/mdcev/prediction_analysis"
dir.create(POST_DIR, recursive = TRUE, showWarnings = FALSE)

MODEL_2014_RDS <- file.path(
  "combined_outputs_harmonised", "mdcev",
  "mdcev_2014_2015_ictall_psi_ictact_gamma_fixed_budget_model.rds"
)

MODEL_2023_RDS <- file.path(
  "combined_outputs_harmonised", "mdcev",
  "mdcev_2023_ictall_psi_ictact_gamma_fixed_budget_model.rds"
)

stopifnot(file.exists(MODEL_2014_RDS))
stopifnot(file.exists(MODEL_2023_RDS))

model_2014_saved <- readRDS(MODEL_2014_RDS)
model_2023_saved <- readRDS(MODEL_2023_RDS)

# 500 is suitable for checking. Use 1000 for final reported results.
N_REP <- 500L
PREDICTION_SEED <- 97132L

# Main scenario specification:
# add 10 percentage points, bounded to [0,1].
ICT_SHOCK <- 0.10

activity_map <- tribble(
  ~activity,        ~activity_label,       ~observed_time_var,
  "personal_care",  "Personal care",       "t_personal_care",
  "leisure",        "Social & leisure",    "t_leisure",
  "maintenance",    "Maintenance",         "t_maintenance",
  "travel",         "Travel",              "t_travel",
  "shopping",       "Shopping",            "t_shopping",
  "work",           "Work",                "t_work",
  "other",          "Other",               "t_other"
)

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  if (!any(ok) || sum(w[ok]) <= 0) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# ------------------------------------------------------------
# Prediction inputs
#
# apollo_probabilities() necessarily calls apollo_weighting().
# To obtain observation-level, unweighted predictions, we create
# a prediction-only weight equal to 1 for every person-day.
# Survey weights w are then applied exactly once in aggregation.
# ------------------------------------------------------------
make_prediction_inputs <- function(db, model, period_label) {
  apollo_initialise()

  db_pred <- as.data.frame(db)
  db_pred$prediction_weight <- 1

  apollo_control <<- list(
    modelName       = paste0("prediction_", gsub("[^0-9A-Za-z]+", "_", period_label)),
    indivID         = "person_day",
    outputDirectory = POST_DIR,
    weights         = "prediction_weight",
    panelData       = FALSE,
    nCores          = 1,
    seed            = PREDICTION_SEED
  )

  database <<- db_pred
  apollo_beta <<- model$estimate

  # The final separate-period models fix alpha_base.
  apollo_fixed <<- if (!is.null(model$apollo_fixed)) {
    model$apollo_fixed
  } else {
    "alpha_base"
  }

  apollo_inputs <<- apollo_validateInputs()

  # Inject the exact custom objects used by apollo_probabilities().
  apollo_inputs$inside <<- inside
  apollo_inputs$common_covs <<- common_covs
  apollo_inputs$activity_ict_vars <<- activity_ict_vars
  apollo_inputs$alternatives <<- alternatives
  apollo_inputs$include_period_interactions <<- FALSE
  apollo_inputs$pooled_common_interactions <<- character(0)

  apollo_inputs
}

standardise_prediction_output <- function(pred) {
  if (is.list(pred) && !is.data.frame(pred)) {
    if ("model" %in% names(pred)) {
      pred <- pred[["model"]]
    } else if (length(pred) == 1L) {
      pred <- pred[[1]]
    }
  }

  pred <- as.data.frame(pred)

  # Apollo labels the outside good generically as "outside" in
  # prediction output, even when the model alternative is named
  # "personal_care". Rename only the returned prediction columns.
  outside_rename <- c(
    outside_cont_mean = "personal_care_cont_mean",
    outside_disc_mean = "personal_care_disc_mean",
    outside_cont_sd   = "personal_care_cont_sd",
    outside_disc_sd   = "personal_care_disc_sd",
    outside_expe_mean = "personal_care_expe_mean",
    outside_expe_sd   = "personal_care_expe_sd"
  )

  for (old_name in names(outside_rename)) {
    new_name <- outside_rename[[old_name]]
    if (old_name %in% names(pred) && !new_name %in% names(pred)) {
      names(pred)[names(pred) == old_name] <- new_name
    }
  }

  required <- unlist(lapply(
    alternatives,
    function(k) c(
      paste0(k, "_cont_mean"),
      paste0(k, "_disc_mean")
    )
  ))

  missing_required <- setdiff(required, names(pred))

  if (length(missing_required) > 0) {
    stop(
      "Prediction output is missing expected final-model columns:\n",
      paste(missing_required, collapse = ", "),
      "\n\nAvailable columns:\n",
      paste(names(pred), collapse = ", ")
    )
  }

  pred
}

run_prediction <- function(db, model, period_label, scenario_label) {
  pred_inputs <- make_prediction_inputs(db, model, period_label)

  set.seed(PREDICTION_SEED)

  pred <- apollo_prediction(
    model = model,
    apollo_probabilities = apollo_probabilities,
    apollo_inputs = pred_inputs,
    prediction_settings = list(
      modelComponent = "model",
      nRep = N_REP,
      runs = 1,
      silent = FALSE,
      summary = FALSE
    )
  )

  pred <- standardise_prediction_output(pred)

  if (nrow(pred) != nrow(db)) {
    stop(
      "Prediction rows = ", nrow(pred),
      "; database rows = ", nrow(db),
      ". The final model should return one row per person-day."
    )
  }

  keep_db <- db %>%
    select(
      person_day, w, age_band, in_work,
      female, has_child, weekend,
      all_of(paste0("t_", alternatives)),
      ict_share,
      all_of(unname(activity_ict_vars))
    )

  bind_cols(
    keep_db,
    pred %>% select(-any_of(c("ID", "Observation")))
  ) %>%
    mutate(
      period = period_label,
      scenario = scenario_label
    )
}

# ============================================================
# STEP 1. BASELINE FORECAST VALIDATION
# ============================================================

message("STEP 1: baseline prediction for 2014-15...")
pred_baseline_2014 <- run_prediction(
  db_2014,
  model_2014_saved,
  "2014-2015",
  "Baseline"
)

message("STEP 1: baseline prediction for 2023...")
pred_baseline_2023 <- run_prediction(
  db_2023,
  model_2023_saved,
  "2023",
  "Baseline"
)

step1_personday <- bind_rows(
  pred_baseline_2014,
  pred_baseline_2023
)

write_csv(
  step1_personday,
  file.path(POST_DIR, "step1_baseline_personday_predictions.csv")
)

summarise_baseline_fit <- function(df) {
  map_dfr(seq_len(nrow(activity_map)), function(i) {
    k <- activity_map$activity[i]
    observed_var <- activity_map$observed_time_var[i]
    predicted_time_var <- paste0(k, "_cont_mean")
    predicted_part_var <- paste0(k, "_disc_mean")

    observed_minutes <- weighted_mean_safe(
      df[[observed_var]] * 60,
      df$w
    )

    predicted_minutes <- weighted_mean_safe(
      df[[predicted_time_var]] * 60,
      df$w
    )

    observed_participation <- weighted_mean_safe(
      as.numeric(df[[observed_var]] > 0),
      df$w
    )

    predicted_participation <- weighted_mean_safe(
      df[[predicted_part_var]],
      df$w
    )

    tibble(
      period = unique(df$period),
      activity = k,
      activity_label = activity_map$activity_label[i],
      observed_mean_minutes = observed_minutes,
      predicted_mean_minutes = predicted_minutes,
      mean_minutes_error = predicted_minutes - observed_minutes,
      observed_participation_rate = observed_participation,
      predicted_participation_rate = predicted_participation,
      participation_error_pp =
        100 * (predicted_participation - observed_participation)
    )
  })
}

step1_fit <- bind_rows(
  summarise_baseline_fit(filter(step1_personday, period == "2014-2015")),
  summarise_baseline_fit(filter(step1_personday, period == "2023"))
)

write_csv(
  step1_fit,
  file.path(
    POST_DIR,
    "step1_baseline_observed_vs_predicted_by_activity.csv"
  )
)

step1_fit_summary <- step1_fit %>%
  group_by(period) %>%
  summarise(
    mean_minutes_MAE =
      mean(abs(mean_minutes_error), na.rm = TRUE),
    mean_minutes_RMSE =
      sqrt(mean(mean_minutes_error^2, na.rm = TRUE)),
    participation_MAE_pp =
      mean(abs(participation_error_pp), na.rm = TRUE),
    participation_RMSE_pp =
      sqrt(mean(participation_error_pp^2, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(
  step1_fit_summary,
  file.path(POST_DIR, "step1_baseline_fit_summary.csv")
)

step1_minutes_plot_data <- step1_fit %>%
  select(
    period, activity_label,
    observed_mean_minutes,
    predicted_mean_minutes
  ) %>%
  pivot_longer(
    cols = c(observed_mean_minutes, predicted_mean_minutes),
    names_to = "series",
    values_to = "minutes"
  ) %>%
  mutate(
    series = recode(
      series,
      observed_mean_minutes = "Observed",
      predicted_mean_minutes = "Predicted"
    ),
    activity_label = factor(
      activity_label,
      levels = rev(activity_map$activity_label)
    )
  )

p_step1_minutes <- ggplot(
  step1_minutes_plot_data,
  aes(x = minutes, y = activity_label, colour = series)
) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.8
  ) +
  facet_wrap(~period) +
  labs(
    title = "Step 1: baseline validation of mean activity duration",
    subtitle = paste0(
      "Weighted observed and MDCEV-predicted minutes per diary-day; nRep = ",
      N_REP
    ),
    x = "Minutes per diary-day",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(
    POST_DIR,
    "Step1_baseline_mean_minutes_observed_vs_predicted.png"
  ),
  p_step1_minutes,
  width = 11,
  height = 5.8,
  dpi = 300
)

step1_part_plot_data <- step1_fit %>%
  select(
    period, activity_label,
    observed_participation_rate,
    predicted_participation_rate
  ) %>%
  pivot_longer(
    cols = c(
      observed_participation_rate,
      predicted_participation_rate
    ),
    names_to = "series",
    values_to = "participation_rate"
  ) %>%
  mutate(
    series = recode(
      series,
      observed_participation_rate = "Observed",
      predicted_participation_rate = "Predicted"
    ),
    activity_label = factor(
      activity_label,
      levels = rev(activity_map$activity_label)
    )
  )

p_step1_participation <- ggplot(
  step1_part_plot_data,
  aes(
    x = participation_rate,
    y = activity_label,
    colour = series
  )
) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 2.8
  ) +
  facet_wrap(~period) +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(
    title = "Step 1: baseline validation of activity participation",
    subtitle = paste0(
      "Weighted observed and MDCEV-predicted positive-allocation rates; nRep = ",
      N_REP
    ),
    x = "Participation rate",
    y = NULL,
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(
    POST_DIR,
    "Step1_baseline_participation_observed_vs_predicted.png"
  ),
  p_step1_participation,
  width = 11,
  height = 5.8,
  dpi = 300
)

# ============================================================
# STEP 2. COHERENT ICT SCENARIOS + psi/gamma DECOMPOSITION
# ------------------------------------------------------------
# WHY THIS REPLACES THE OLD A/B/C SHOCK
#
# ict_share and ictact_k are NOT free to move independently. By
# construction (see build_personday):
#
#     ict_share = [ ICT minutes over the whole day ] / budget
#     ictact_k  = [ ICT minutes in activity k ]      / (minutes in k)
#
# so, mechanically,
#
#     ict_share = (ICT_personal_care / budget)
#                 + SUM_over_inside_k  (t_k / budget) * ictact_k
#
# The old design moved ict_share by +0.10 while holding ictact_k
# fixed (scenario A), and moved every ictact_k by +0.10 while
# holding ict_share fixed (scenario B). Both are algebraically
# impossible points: they sit off the data manifold, because the
# two ICT measures are tied by the identity above.
#
# COHERENT DESIGN (supervisor's point)
#
# The behavioural lever is the ACTIVITY-LEVEL intensity ictact_k
# ("what fraction of time in activity k involved a device"). That
# is the quantity with a real-world interpretation. When we raise
# ictact_k for one or more activities, ict_share must follow by
# EXACTLY the accounting amount that change implies:
#
#     Delta_ict_share = SUM_over_targeted_k (t_k / budget)
#                       * (ictact_k_new - ictact_k_old)
#
# Example (supervisor's): raising ictact_work by +0.10 raises the
# overall ict_share by only ~ (t_work / budget) * 0.10, i.e. a
# small number, because work is a small slice of the day.
#
# Each coherent scenario is then run three ways so the two model
# channels can still be reported as an ADDITIVE decomposition of the
# single self-consistent move (not as independent counterfactuals):
#
#   leg = "total"  : move ict_share AND the targeted ictact_k
#                    together  -> the real, self-consistent scenario
#   leg = "psi"    : move ONLY ict_share (to its coherent new value);
#                    hold ictact_k at baseline -> isolates the
#                    participation / baseline-utility channel
#   leg = "gamma"  : move ONLY the targeted ictact_k; hold ict_share
#                    at baseline -> isolates the duration / satiation
#                    channel
#
# Reported result = the "total" leg. psi and gamma are shown only as
# its decomposition, and we check psi + gamma ~= total.
#
# Off-support guard: ictact_k is only defined where the activity is
# actually observed (t_k > 0). Where t_k == 0 the construction sets
# ictact_k = 0 as "not applicable", not "no device use". We therefore
# apply the ictact_k shock ONLY on person-days with t_k > 0. Those
# rows also contribute zero to Delta_ict_share automatically, so the
# psi and gamma legs stay referenced to the same realised move.
# ============================================================

# ICT_SHOCK (defined earlier, = 0.10) is now the shock applied to the
# ACTIVITY-LEVEL ictact_k lever(s). ict_share is a DERIVED quantity and
# is never shocked directly.

clip01 <- function(x) pmin(1, pmax(0, x))

# Day total (= budget) rebuilt from the observed activity minutes so we
# do not depend on a particular column surviving upstream edits. t_* are
# in hours; the ratio t_k / day_total is a unitless time share.
day_time_share <- function(db, k) {
  t_cols   <- paste0("t_", alternatives)
  day_total <- rowSums(as.matrix(db[, t_cols, drop = FALSE]), na.rm = TRUE)
  tk <- db[[paste0("t_", k)]]
  share <- ifelse(day_total > 0, tk / day_total, 0)
  share
}

# ------------------------------------------------------------
# Build one coherent scenario leg.
#   target_activities : character vector of inside goods whose
#                       ictact_k is raised (e.g. "work", or all six)
#   leg               : "total" | "psi" | "gamma"
# Returns a copy of db with ict_share and/or ictact_k overwritten in
# an accounting-consistent way.
# ------------------------------------------------------------
apply_coherent_scenario <- function(
    db,
    target_activities,
    leg   = c("total", "psi", "gamma"),
    shock = ICT_SHOCK
) {
  leg <- match.arg(leg)
  out <- db

  # Realised change in ict_share implied by raising the targeted
  # ictact_k, computed with observed time shares and bounded moves.
  delta_ict_share <- rep(0, nrow(db))

  for (k in target_activities) {
    ict_var <- activity_ict_vars[[k]]
    tk      <- db[[paste0("t_", k)]]
    observed <- tk > 0                     # off-support guard

    old_val <- db[[ict_var]]
    new_val <- old_val
    new_val[observed] <- clip01(old_val[observed] + shock)
    realised_delta <- new_val - old_val    # 0 where not observed / already at 1

    # Accounting contribution of activity k to the overall ICT share.
    share_k <- day_time_share(db, k)
    delta_ict_share <- delta_ict_share + share_k * realised_delta

    # The gamma channel (and the total) move the activity-level intensity.
    if (leg %in% c("total", "gamma")) {
      out[[ict_var]] <- new_val
    }
  }

  # The psi channel (and the total) move the overall ICT share by exactly
  # the accounting amount the ictact_k change implies.
  if (leg %in% c("total", "psi")) {
    out$ict_share <- clip01(db$ict_share + delta_ict_share)
  }

  out
}

# ------------------------------------------------------------
# Scenario catalogue.
# Each row is one coherent scenario, defined by which activities'
# in-activity ICT intensity is raised. ict_share follows by identity.
#   S1 travel  : device use within travel rises        (core travel story)
#   S2 work    : device use within work rises           (digital/remote work)
#   S3 leisure : device use within leisure rises         (second-screen leisure)
#   S4 uniform : device use rises across all activities  (society-wide shift;
#                the self-consistent analogue of the old "C: both")
# (An optional S5 could transplant 2023 observed ictact_k onto 2014 people;
#  omitted here to keep this block self-contained and re-estimation-free.)
# ------------------------------------------------------------
scenario_catalogue <- tibble::tibble(
  scenario_code = c("S1_travel", "S2_work", "S3_leisure", "S4_uniform"),
  scenario      = c("S1: ICT within travel",
                    "S2: ICT within work",
                    "S3: ICT within leisure",
                    "S4: ICT across all activities"),
  targets       = list(
    "travel",
    "work",
    "leisure",
    inside                       # all six inside goods
  )
)

leg_catalogue <- tibble::tibble(
  leg       = c("total", "psi", "gamma"),
  leg_label = c("Total",
                "Participation (ψ)",
                "Duration (γ)")
)

# ------------------------------------------------------------
# Run every (scenario x leg) plus a single shared baseline, for one period.
# ------------------------------------------------------------
run_coherent_scenarios <- function(db, model, period_label) {

  # Baseline: observed ICT structure, run once and shared by all scenarios.
  message("STEP 2: ", period_label, " -- Baseline (observed ICT)")
  baseline_pred <- run_prediction(db, model, period_label, "Baseline") %>%
    mutate(scenario_code = "Baseline",
           leg = "baseline",
           leg_label = "Baseline")

  scenario_preds <- map_dfr(seq_len(nrow(scenario_catalogue)), function(si) {
    code    <- scenario_catalogue$scenario_code[si]
    label   <- scenario_catalogue$scenario[si]
    targets <- scenario_catalogue$targets[[si]]

    map_dfr(seq_len(nrow(leg_catalogue)), function(li) {
      leg       <- leg_catalogue$leg[li]
      leg_label <- leg_catalogue$leg_label[li]

      message("STEP 2: ", period_label, " -- ", label, " [", leg_label, "]")

      db_scn <- apply_coherent_scenario(db, targets, leg = leg)

      run_prediction(db_scn, model, period_label, label) %>%
        mutate(scenario_code = code,
               leg = leg,
               leg_label = leg_label)
    })
  })

  bind_rows(baseline_pred, scenario_preds)
}

step2_personday <- bind_rows(
  run_coherent_scenarios(db_2014, model_2014_saved, "2014-2015"),
  run_coherent_scenarios(db_2023, model_2023_saved, "2023")
)

write_csv(
  step2_personday,
  file.path(POST_DIR, "step2_coherent_personday_predictions.csv")
)

# ------------------------------------------------------------
# Aggregate to weighted period x scenario x leg levels.
# ------------------------------------------------------------
summarise_scenario_levels <- function(df) {
  map_dfr(seq_len(nrow(activity_map)), function(i) {
    k <- activity_map$activity[i]
    tibble(
      period         = unique(df$period),
      scenario       = unique(df$scenario),
      scenario_code  = unique(df$scenario_code),
      leg            = unique(df$leg),
      leg_label      = unique(df$leg_label),
      activity       = k,
      activity_label = activity_map$activity_label[i],
      predicted_mean_minutes = weighted_mean_safe(
        df[[paste0(k, "_cont_mean")]] * 60, df$w
      ),
      predicted_participation_rate = weighted_mean_safe(
        df[[paste0(k, "_disc_mean")]], df$w
      )
    )
  })
}

step2_levels <- step2_personday %>%
  group_by(period, scenario_code, leg) %>%
  group_split() %>%
  map_dfr(summarise_scenario_levels)

write_csv(
  step2_levels,
  file.path(POST_DIR, "step2_coherent_predicted_levels.csv")
)

# ------------------------------------------------------------
# Changes vs the shared baseline (same period, same activity).
# ------------------------------------------------------------
step2_baseline <- step2_levels %>%
  filter(scenario_code == "Baseline") %>%
  select(period, activity,
         baseline_minutes = predicted_mean_minutes,
         baseline_participation = predicted_participation_rate)

step2_changes <- step2_levels %>%
  filter(scenario_code != "Baseline") %>%
  left_join(step2_baseline, by = c("period", "activity")) %>%
  mutate(
    minutes_change = predicted_mean_minutes - baseline_minutes,
    participation_change_pp =
      100 * (predicted_participation_rate - baseline_participation)
  )

write_csv(
  step2_changes,
  file.path(POST_DIR, "step2_coherent_changes_vs_baseline.csv")
)

# ------------------------------------------------------------
# Fixed-budget identity: within any scenario x leg, minutes changes
# across all seven activities must sum to ~ 0.
# ------------------------------------------------------------
step2_budget_check <- step2_changes %>%
  group_by(period, scenario_code, scenario, leg, leg_label) %>%
  summarise(total_minutes_change = sum(minutes_change, na.rm = TRUE),
            .groups = "drop")

write_csv(
  step2_budget_check,
  file.path(POST_DIR, "step2_coherent_time_budget_check.csv")
)

# ------------------------------------------------------------
# Additivity check: for each scenario, does psi + gamma ~= total?
# (The two channels are decomposition terms of one coherent move, so a
#  small residual is expected from the model's non-linearity.)
# ------------------------------------------------------------
step2_additivity <- step2_changes %>%
  select(period, scenario_code, scenario, activity, activity_label,
         leg, minutes_change, participation_change_pp) %>%
  pivot_wider(
    names_from  = leg,
    values_from = c(minutes_change, participation_change_pp)
  ) %>%
  mutate(
    minutes_total_minus_psi_minus_gamma =
      minutes_change_total - minutes_change_psi - minutes_change_gamma,
    participation_pp_total_minus_psi_minus_gamma =
      participation_change_pp_total - participation_change_pp_psi -
      participation_change_pp_gamma
  )

write_csv(
  step2_additivity,
  file.path(POST_DIR, "step2_coherent_additivity_total_vs_psi_plus_gamma.csv")
)

# ------------------------------------------------------------
# Diagnostic: how large is the induced ict_share move in each scenario?
# This is the "work +10pp -> overall +~1pp" number the supervisor asked
# for. Reported as weighted means over person-days.
# ------------------------------------------------------------
induced_ict_share_move <- function(db, targets, period_label, scenario_label) {
  delta <- rep(0, nrow(db))
  for (k in targets) {
    ict_var <- activity_ict_vars[[k]]
    tk <- db[[paste0("t_", k)]]
    observed <- tk > 0
    old_val <- db[[ict_var]]
    new_val <- old_val
    new_val[observed] <- clip01(old_val[observed] + ICT_SHOCK)
    delta <- delta + day_time_share(db, k) * (new_val - old_val)
  }
  tibble(
    period = period_label,
    scenario = scenario_label,
    lever_shock_pp = 100 * ICT_SHOCK,
    induced_ict_share_move_pp = 100 * weighted_mean_safe(delta, db$w)
  )
}

step2_induced_share <- bind_rows(
  map_dfr(seq_len(nrow(scenario_catalogue)), function(si)
    induced_ict_share_move(
      db_2014, scenario_catalogue$targets[[si]],
      "2014-2015", scenario_catalogue$scenario[si]
    )),
  map_dfr(seq_len(nrow(scenario_catalogue)), function(si)
    induced_ict_share_move(
      db_2023, scenario_catalogue$targets[[si]],
      "2023", scenario_catalogue$scenario[si]
    ))
)

write_csv(
  step2_induced_share,
  file.path(POST_DIR, "step2_coherent_induced_ict_share_move.csv")
)

# ============================================================
# Figures
# ============================================================
activity_levels <- rev(activity_map$activity_label)

# ---- Figure 1: coherent (total-leg) minutes reallocated, by scenario ----
step2_total_plot <- step2_changes %>%
  filter(leg == "total") %>%
  mutate(
    activity_label = factor(activity_label, levels = activity_levels),
    scenario = factor(scenario, levels = scenario_catalogue$scenario)
  )

p_step2_total <- ggplot(
  step2_total_plot,
  aes(x = minutes_change, y = activity_label, colour = scenario)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(position = position_dodge(width = 0.6), size = 2.6) +
  facet_wrap(~period) +
  labs(
    title = "Model-implied reallocation under four coherent ICT scenarios",
    subtitle = paste0(
      "Each scenario raises in-activity ICT by +",
      100 * ICT_SHOCK, " percentage points for its target activities; ",
      "overall ICT exposure follows by identity"
    ),
    x = "Change from baseline (minutes per diary-day)",
    y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(
  file.path(POST_DIR, "Step2_coherent_scenarios_minutes.png"),
  p_step2_total, width = 12, height = 6, dpi = 300
)

# ---- Figure 2: psi/gamma/total decomposition for the uniform scenario ----
# (S4 is the self-consistent analogue of the old A/B/C picture.)
step2_decomp_plot <- step2_changes %>%
  filter(scenario_code == "S4_uniform") %>%
  mutate(
    activity_label = factor(activity_label, levels = activity_levels),
    leg_label = factor(
      leg_label,
      levels = c("Participation (ψ)",
                 "Duration (γ)",
                 "Total")
    )
  )

p_step2_decomp <- ggplot(
  step2_decomp_plot,
  aes(x = minutes_change, y = activity_label, colour = leg_label)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(position = position_dodge(width = 0.6), size = 2.8) +
  facet_wrap(~period) +
  labs(
    title = "Participation (ψ) and duration (γ) decomposition of the uniform ICT scenario",
    subtitle = paste0(
      "The uniform scenario raises in-activity ICT across all activities by +", 100 * ICT_SHOCK,
      " percentage points;\n",
      "the participation (ψ) and duration (γ) parts sum to the total"
    ),
    x = "Change from baseline (minutes per diary-day)",
    y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(
  file.path(POST_DIR, "Step2_coherent_uniform_psi_gamma_decomposition.png"),
  p_step2_decomp, width = 12, height = 6, dpi = 300
)

# ---- Figure 3: participation-margin decomposition, uniform scenario ----
p_step2_decomp_part <- ggplot(
  step2_decomp_plot,
  aes(x = participation_change_pp, y = activity_label, colour = leg_label)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(position = position_dodge(width = 0.6), size = 2.8) +
  facet_wrap(~period) +
  labs(
    title = "Participation changes under the uniform ICT scenario",
    subtitle = "Change in probability of any positive time; participation (ψ), duration (γ) and total",
    x = "Change from baseline (percentage points)",
    y = NULL, colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(
  file.path(POST_DIR, "Step2_coherent_uniform_participation_decomposition.png"),
  p_step2_decomp_part, width = 12, height = 6, dpi = 300
)

message("\nSTEP 1 and STEP 2 (coherent) complete.")
message("Outputs written to: ", POST_DIR)
message("Main Step 2 result: ",
        file.path(POST_DIR, "step2_coherent_changes_vs_baseline.csv"))
message("Induced ict_share move (lever -> overall): ",
        file.path(POST_DIR, "step2_coherent_induced_ict_share_move.csv"))
message("Fixed-budget identity check: ",
        file.path(POST_DIR, "step2_coherent_time_budget_check.csv"))
message("psi + gamma vs total additivity check: ",
        file.path(POST_DIR, "step2_coherent_additivity_total_vs_psi_plus_gamma.csv"))

