# ============================================================
# TWO-STAGE ROBUSTNESS CHECK: endogeneity of in-activity ICT
# ------------------------------------------------------------
# PROBLEM (raised by Jacek, restated by Aruna):
#
#   ictact_k = (ICT-involved minutes in activity k) / (total minutes in k)
#
# The denominator IS the dependent variable of the MDCEV duration equation
# for k, so ictact_k is mechanically simultaneous with t_k. The sharpest form
# of this is the forced zero: whenever t_k = 0, ictact_k = 0 by construction
# rather than by behaviour.
#
# APPROACH (supervisor's second suggestion):
#   Stage 1: predict ictact_k from covariates that are NOT constructed from
#            the same diary day's time allocation.
#   Stage 2: re-estimate the MDCEV with ictact_hat_k in place of ictact_k in
#            log(gamma_k). Everything else identical.
#
# ------------------------------------------------------------
# HOW TO RUN
# ------------------------------------------------------------
#   1. Run code/02_mdcev_pooled_analysis.R from the top down to the END OF
#      SECTION 3, i.e. through the closing brace of run_mdcev() (~line 707).
#      Do NOT run Section 4 onwards.
#   2. source("code/03_mdcev_2stage_crossfit.R")
#
# It must be that script, not mdcev_STEP1_STEP2_coherent1.R. Only the pooled
# script has the current specification (out_share removed) and the
# cache-aware run_mdcev().
# ============================================================

library(dplyr)
library(readr)
library(tibble)

.needed <- c("db_2014", "db_2023", "inside", "common_covs", "alternatives",
             "activity_ict_vars", "run_mdcev", "apollo_probabilities",
             "make_beta", "ICT_SCALING")
.missing <- .needed[!vapply(.needed, exists, logical(1))]
if (length(.missing)) {
  stop("Missing objects: ", paste(.missing, collapse = ", "),
       "\nRun 02_mdcev_pooled_analysis.R through the end of Section 3 first.")
}

# Stage 1 is a fractional logit, which needs the dependent variable in [0, 1].
# Under the _z variants the ICT variables are standardised and this breaks.
if (!identical(ICT_SCALING, "raw")) {
  stop("ICT_SCALING must be \"raw\" for the two-stage check. ",
       "Set ICT_SCALING <- \"raw\", re-run Sections 1-3, then source this file.")
}

if (!exists("ICT_TAG")) ICT_TAG <- ""

# Cross-fit the first stage. A person-day's predicted value is then produced by
# a model estimated without that person, so its own outcome contributes nothing
# to its own regressor.
#
# Strictly this is not required for consistency: the standard two-stage
# estimator fits both stages on the same sample, and the own-observation
# contribution to the coefficient vector is O(1/n), which is negligible at
# n = 3,000 to 14,000. It is done because it costs seconds, it removes the
# question entirely, and it is preferable to splitting the sample by diary day,
# which would halve the second-stage sample and unbalance the day types.
#
# Folds are assigned by respondent, not by person-day, so that the other-day
# predictors cannot cross a fold boundary.
CROSSFIT <- TRUE
N_FOLDS  <- 5

OUT_DIR <- paste0("combined_outputs_harmonised/mdcev/endogeneity_2stage",
                  if (CROSSFIT) "_crossfit" else "")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Two-stage endogeneity check. Controls in psi: ",
        paste(common_covs, collapse = ", "))


# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

# Stage-1 predictors: exogenous with respect to the SAME DAY's time
# allocation. Deliberately EXCLUDED are ict_share and out_share, both of which
# are constructed from the modelled durations of THIS day.
STAGE1_COVS <- c("female",
                 "age_30_39", "age_40_49", "age_50_59",
                 "age_60_69", "age_70plus",
                 "in_work", "has_child", "weekend")

# Use the respondent's OTHER diary day. Two predictors are built:
#
#   oth_ictact_k  ICT intensity in activity k on the other day. Strong where
#                 participation is high (leisure, maintenance, travel), but
#                 undefined when the person did not do k on the other day.
#
#   oth_ict_share overall ICT share of the other day. Available whenever the
#                 person has a second diary, regardless of what they did on
#                 it. This is what carries the low-participation activities
#                 (work, shopping, other), which matters here because UKTUS
#                 2014-15 pairs one weekday with one weekend day, so the
#                 other day is systematically the opposite day type.
USE_OTHER_DAY <- TRUE

# Fit stage 1 on participants only (t_k > 0), then predict for everyone.
# TRUE -> E[ictact_k | participates in k, X], a "potential ICT intensity"
#         defined for all person-days, with the forced zero removed. This most
#         directly answers the critique.
STAGE1_FIT_ON_PARTICIPANTS <- TRUE

# ============================================================
# 1. Other-day predictors (leave-one-out within person)
# ============================================================
# person_day is "mainid_diaryord" (2023) or "serial_pnum_daynum" (2014-15),
# so stripping the final "_x" recovers the person identifier in both waves.

add_otherday_ict <- function(db, acts = inside) {
  db <- db %>% mutate(.person_id = sub("_[^_]+$", "", person_day))

  # ---- overall ICT share on the other day ----
  t0 <- db %>%
    group_by(.person_id) %>%
    mutate(.n = n(), .num = sum(ict_share) - ict_share) %>%
    ungroup()
  db$oth_ict_share <- ifelse(t0$.n > 1, t0$.num / (t0$.n - 1), NA_real_)
  db$has_oth_day   <- as.integer(t0$.n > 1)
  # Single-diary respondents are given the weighted sample mean rather than
  # zero. Zero would assert that they are the least ICT-intensive people in
  # the sample, which is an assertion the data does not support.
  .mu <- weighted.mean(db$oth_ict_share, db$w, na.rm = TRUE)
  db$oth_ict_share[is.na(db$oth_ict_share)] <- .mu

  # ---- activity-specific ICT intensity on the other day ----
  for (k in acts) {
    v  <- paste0("ictact_", k)
    tv <- paste0("t_", k)

    tmp <- db %>%
      mutate(.part = as.numeric(.data[[tv]] > 0),
             .val  = .data[[v]] * as.numeric(.data[[tv]] > 0)) %>%
      group_by(.person_id) %>%
      mutate(.num = sum(.val)  - .val,
             .den = sum(.part) - .part) %>%
      ungroup()

    db[[paste0("oth_ictact_", k)]] <- ifelse(tmp$.den > 0, tmp$.num / tmp$.den, 0)
    db[[paste0("has_oth_", k)]]    <- as.integer(tmp$.den > 0)
  }

  db %>% select(-.person_id)
}


# ============================================================
# 2. Stage 1: fractional logit for each activity
# ============================================================
# ictact_k is a share in [0,1], so a quasi-binomial (fractional logit) GLM is
# the right functional form: predictions stay inside (0,1) and no
# distributional assumption on the error is required. This matters here
# because the variable has a large mass at zero even among participants.

broom_like <- function(m, period_label, activity) {
  s <- summary(m)$coefficients
  tibble(period = period_label, activity = activity,
         term = rownames(s), estimate = s[, 1], std_error = s[, 2],
         t_value = s[, 3], p_value = s[, 4])
}

fit_stage1 <- function(db, period_label, acts = inside) {

  if (USE_OTHER_DAY) {
    db <- add_otherday_ict(db, acts)
    message(sprintf("  %s: %.1f%% of person-days have a second diary day.",
                    period_label, 100 * mean(db$has_oth_day)))
  }

  # Cross-fitting folds, assigned by PERSON rather than by person-day, so that
  # both diary days of a respondent land in the same fold. Assigning by
  # person-day would let the other-day predictors carry information across the
  # fold boundary and defeat the purpose.
  if (CROSSFIT) {
    set.seed(20260827)
    pid  <- sub("_[^_]+$", "", db$person_day)
    upid <- unique(pid)
    db$.fold <- as.integer(setNames(
      sample(rep_len(seq_len(N_FOLDS), length(upid))), upid)[pid])
    message(sprintf("  %s: %d-fold cross-fitting over %d respondents.",
                    period_label, N_FOLDS, length(upid)))
  }

  diag_rows <- list(); coef_rows <- list()

  for (k in acts) {
    y  <- paste0("ictact_", k)
    tv <- paste0("t_", k)
    hk <- paste0("has_oth_", k)

    xs <- STAGE1_COVS
    if (USE_OTHER_DAY) {
      xs <- c(xs, "oth_ict_share", "has_oth_day",
              paste0("oth_ictact_", k), hk)
    }

    fit_data <- if (STAGE1_FIT_ON_PARTICIPANTS) {
      db[db[[tv]] > 0, , drop = FALSE]
    } else db

    # --- rank-deficiency guards, evaluated on the ESTIMATION sample ---
    # Drop constant regressors, and binary regressors whose minority class is
    # too small to estimate. UKTUS 2014-15 has only 22 single-diary
    # respondents, which separated has_oth_day in the "other" equation
    # (coefficient 11.6, standard error 244) and pushed those rows'
    # predicted values to 1.
    MIN_CELL <- 50
    .keep <- vapply(xs, function(z) {
      u <- unique(fit_data[[z]])
      if (length(u) < 2) return(FALSE)
      if (length(u) == 2 && all(sort(u) %in% c(0, 1))) {
        return(min(sum(fit_data[[z]] == 0),
                   sum(fit_data[[z]] == 1)) >= MIN_CELL)
      }
      TRUE
    }, logical(1))
    if (any(!.keep)) {
      message("    ", period_label, " / ", k, ": dropped ",
              paste(xs[!.keep], collapse = ", "),
              " (constant or minority cell < ", MIN_CELL, ")")
    }
    .dropped <- xs[!.keep]
    xs <- xs[.keep]
    # has_oth_day and has_oth_k coincide when participation in k is near
    # universal (e.g. leisure at 98.8%). Keep only the activity-specific one.
    if (all(c("has_oth_day", hk) %in% xs) &&
        mean(fit_data$has_oth_day == fit_data[[hk]]) > 0.99) {
      xs <- setdiff(xs, "has_oth_day")
      .dropped <- c(.dropped, "has_oth_day (duplicates has_oth_k)")
    }

    f <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))
    m <- glm(f, family = quasibinomial(link = "logit"),
             data = fit_data, weights = fit_data$w)

    if (any(is.na(coef(m)))) {
      warning(period_label, " / ", k,
              ": aliased terms dropped by glm: ",
              paste(names(coef(m))[is.na(coef(m))], collapse = ", "))
    }

    hat_insample <- suppressWarnings(
      as.numeric(predict(m, newdata = db, type = "response")))

    if (CROSSFIT) {
      # Predict each fold from a model estimated without it, so that a
      # person-day's own outcome contributes nothing to its own predicted
      # value. The full-sample fit above is retained only for the reported
      # first-stage coefficient table.
      hat <- rep(NA_real_, nrow(db))
      for (fo in seq_len(N_FOLDS)) {
        tr <- fit_data[fit_data$.fold != fo, , drop = FALSE]
        te <- which(db$.fold == fo)
        if (!nrow(tr) || !length(te)) next
        mf <- tryCatch(
          glm(f, family = quasibinomial(link = "logit"),
              data = tr, weights = tr$w),
          error = function(e) NULL)
        if (is.null(mf)) next
        hat[te] <- suppressWarnings(
          as.numeric(predict(mf, newdata = db[te, , drop = FALSE],
                             type = "response")))
      }
      # Any fold that failed to fit falls back to the full-sample prediction.
      nbad <- sum(!is.finite(hat))
      if (nbad) {
        warning(period_label, " / ", k, ": ", nbad,
                " rows fell back to the full-sample prediction.")
        hat[!is.finite(hat)] <- hat_insample[!is.finite(hat)]
      }
    } else {
      hat <- hat_insample
    }

    db[[paste0("ictact_hat_", k)]] <- hat

    obs <- db[[y]]
    diag_rows[[k]] <- tibble(
      period = period_label, activity = k,
      participation  = mean(db[[tv]] > 0),
      n_stage1       = nrow(fit_data),
      oth_coverage   = if (USE_OTHER_DAY) mean(fit_data[[hk]]) else NA_real_,
      zero_share_par = mean(fit_data[[y]] == 0),
      sd_observed    = sd(obs),
      sd_predicted   = sd(hat),
      sd_ratio       = sd(hat) / sd(obs),
      min_predicted  = min(hat),
      max_predicted  = max(hat),
      dropped_terms  = if (length(.dropped)) paste(.dropped, collapse = "; ") else "",
      cor_obs_hat    = suppressWarnings(cor(obs, hat)),
      pseudo_R2      = 1 - m$deviance / m$null.deviance,
      # How far cross-fitting moves the generated regressor. If these are
      # ~1.000 and ~0, the own-observation contribution was negligible, which
      # is what theory predicts at this sample size.
      cor_cf_insample = if (CROSSFIT) suppressWarnings(cor(hat, hat_insample)) else NA_real_,
      max_abs_diff    = if (CROSSFIT) max(abs(hat - hat_insample)) else NA_real_
    )
    coef_rows[[k]] <- broom_like(m, period_label, k)
  }

  list(db = db, diagnostics = bind_rows(diag_rows), coefs = bind_rows(coef_rows))
}


# ============================================================
# 3. Build the stage-2 datasets
# ============================================================

s1_2014 <- fit_stage1(db_2014, "2014-2015")
s1_2023 <- fit_stage1(db_2023, "2023")

db_2014_2s <- s1_2014$db
db_2023_2s <- s1_2023$db

stage1_diag  <- bind_rows(s1_2014$diagnostics, s1_2023$diagnostics)
stage1_coefs <- bind_rows(s1_2014$coefs,       s1_2023$coefs)

write_csv(stage1_diag,  file.path(OUT_DIR, "stage1_diagnostics.csv"))
write_csv(stage1_coefs, file.path(OUT_DIR, "stage1_coefficients.csv"))

cat("\n================ STAGE 1 DIAGNOSTICS ================\n")
print(as.data.frame(stage1_diag), digits = 3)
cat("\nsd_ratio is the key number. Below about 0.10 the stage-2 coefficient\n")
cat("for that activity is weakly identified and may be large and unstable.\n")
cat("Report it as such rather than dropping it.\n")
cat("====================================================\n\n")


# ============================================================
# 4. Stage 2: re-estimate with the predicted values
# ============================================================
# run_mdcev() reads activity_ict_vars from the global environment, so the
# cleanest swap is to overwrite that object temporarily. No edits to the
# pooled script are needed. The cache fingerprint covers column names and
# column sums, so the extra ictact_hat_* columns force a fresh estimation.

activity_ict_vars_hat  <- setNames(paste0("ictact_hat_", inside), inside)
activity_ict_vars_orig <- activity_ict_vars

activity_ict_vars <<- activity_ict_vars_hat

model_2014_2s <- run_mdcev(db_2014_2s,
  paste0("mdcev_2014_2015_2stage_ictact_hat", if (CROSSFIT) "_cf" else "", ICT_TAG))
model_2023_2s <- run_mdcev(db_2023_2s,
  paste0("mdcev_2023_2stage_ictact_hat", if (CROSSFIT) "_cf" else "", ICT_TAG))

activity_ict_vars <<- activity_ict_vars_orig


# ============================================================
# 5. Comparison tables: main vs two-stage
# ============================================================

load_main_model <- function(base_name) {
  p1 <- file.path("combined_outputs_harmonised", "mdcev",
                  paste0(base_name, "_model.rds"))
  p2 <- file.path(MODEL_CACHE_DIR, paste0(base_name, "_model.rds"))
  for (p in c(p1, p2)) {
    if (file.exists(p)) {
      obj <- readRDS(p)
      if (!is.null(obj$estimate))       return(obj)
      if (!is.null(obj$model$estimate)) return(obj$model)
    }
  }
  stop("Could not find an estimated main model for: ", base_name)
}

get_se <- function(model) {
  se <- tryCatch(model$robse, error = function(e) NULL)
  if (is.null(se)) se <- model$se
  se
}

pull_params <- function(model, prefix, period_label, spec_label) {
  est <- model$estimate; se <- get_se(model)
  nms <- paste0(prefix, inside); nms <- nms[nms %in% names(est)]
  tibble(period = period_label, spec = spec_label,
         activity = sub(paste0("^", prefix), "", nms),
         estimate = as.numeric(est[nms]),
         std_err  = as.numeric(se[nms])) %>%
    mutate(t_stat = estimate / std_err,
           p_value = 2 * pnorm(-abs(t_stat)))
}

main_2014 <- load_main_model(
  paste0("mdcev_2014_2015_ictall_psi_ictact_gamma_fixed_budget", ICT_TAG))
main_2023 <- load_main_model(
  paste0("mdcev_2023_ictall_psi_ictact_gamma_fixed_budget", ICT_TAG))

theta_comparison <- bind_rows(
  pull_params(main_2014,     "theta_gamma_ict_activity_", "2014-2015", "Main (observed)"),
  pull_params(model_2014_2s, "theta_gamma_ict_activity_", "2014-2015", "Two-stage (predicted)"),
  pull_params(main_2023,     "theta_gamma_ict_activity_", "2023",      "Main (observed)"),
  pull_params(model_2023_2s, "theta_gamma_ict_activity_", "2023",      "Two-stage (predicted)")
)
write_csv(theta_comparison, file.path(OUT_DIR, "theta_gamma_main_vs_2stage.csv"))

# The participation channel, which Aruna expects to be less affected.
psi_comparison <- bind_rows(
  pull_params(main_2014,     "b_ict_share_", "2014-2015", "Main (observed)"),
  pull_params(model_2014_2s, "b_ict_share_", "2014-2015", "Two-stage (predicted)"),
  pull_params(main_2023,     "b_ict_share_", "2023",      "Main (observed)"),
  pull_params(model_2023_2s, "b_ict_share_", "2023",      "Two-stage (predicted)")
)
write_csv(psi_comparison, file.path(OUT_DIR, "b_ict_share_main_vs_2stage.csv"))

cat("\n===== DURATION CHANNEL (gamma): main vs two-stage =====\n")
print(as.data.frame(theta_comparison), digits = 3)
cat("\n===== PARTICIPATION CHANNEL (psi): main vs two-stage =====\n")
print(as.data.frame(psi_comparison), digits = 3)
cat("\n")

fit_table <- tibble(
  period = c("2014-2015", "2014-2015", "2023", "2023"),
  spec   = c("Main", "Two-stage", "Main", "Two-stage"),
  LL     = c(main_2014$maximum, model_2014_2s$maximum,
             main_2023$maximum, model_2023_2s$maximum),
  n_par  = c(length(main_2014$estimate), length(model_2014_2s$estimate),
             length(main_2023$estimate), length(model_2023_2s$estimate))
)
write_csv(fit_table, file.path(OUT_DIR, "model_fit_main_vs_2stage.csv"))

message("Done. Outputs in: ", OUT_DIR)


# ------------------------------------------------------------
# CAVEATS THAT MUST APPEAR IN THE THESIS
# ------------------------------------------------------------
# 1. GENERATED REGRESSOR. ictact_hat_k is estimated, and the stage-2 standard
#    errors reported by Apollo ignore stage-1 estimation uncertainty. They are
#    therefore too small. Valid inference would require a bootstrap over both
#    stages, which is not feasible within the submission timetable. Read this
#    check as evidence on the SIGN and rough MAGNITUDE of the duration-channel
#    coefficients, not as a basis for formal significance testing.
#
# 2. IDENTIFYING ASSUMPTION. Conditional on the demographics, the
#    respondent's ICT behaviour on their other diary day is uncorrelated with
#    the day-specific shock to their time allocation on this day. This severs
#    the mechanical simultaneity Jacek raised. It does NOT rule out
#    person-level unobserved heterogeneity persisting across both diary days.
#
# 3. UNEVEN STRENGTH ACROSS ACTIVITIES. UKTUS 2014-15 pairs one weekday with
#    one weekend day per respondent, so oth_ictact_k is often unavailable for
#    the low-participation activities (work, shopping, other); oth_ict_share
#    carries those. Quote oth_coverage and sd_ratio from stage1_diagnostics.csv
#    and say plainly which activities the check identifies well and which it
#    does not.
# ------------------------------------------------------------
