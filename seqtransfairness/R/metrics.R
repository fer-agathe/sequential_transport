#' Metrics for a Scoring Classifier
#'
#' @description
#' From predicted scores of a binary scoring classifier and corresponding
#' observed outcome (\eqn{Y\in\{0,1\}}), computes various metrics. The scoring
#' classifier is turned into a binary classifier by setting a threshold above
#' which individuals are predicted as 1 (or as the second level of the binary
#' outcome if it is a factor variable).
#'
#' @param obs Vector of observed binary (0/1) variable.
#' @param pred Vector of predicted scores.
#' @param threshold Threshold above which predicted scores are classified as 1.
#'
#' @returns A named numeric vector with the following metrics:
#' * `mean_pred`: Mean predicted scores
#' * `n_obs`: Number of observations
#' * `TP`: Count of true positives
#' * `FP`: Count of false positives
#' * `FN`: Count of false negatives
#' * `TN`: Count of true negatives
#' * `P`: Count of positives (`TP` + `FN`)
#' * `N`: Count of positives (`FP` + `TN`)
#' * `TPR`: True positive rate (`TP` / `P`)
#' * `FPR`: False positive rate (`FP` / `N`)
#' * `TNR`: True negative rate (`TN` / `N`)
#' * `FNR`: False negative rate (`FN` / `P`)
#' @export
#' @md
#' @examples
#' obs <-  c( 0,  0,  0,  0,  1,  1,  1)
#' pred <- c(.2, .3, .8, .1, .6, .5, .1)
#' metrics_bin_classif(obs, pred, threshold =  .5)
metrics_bin_classif <- function(obs,
                                pred,
                                threshold = .5) {
  # Values for positive / negative classes
  if (is.numeric(obs)) {
    lvl_neg <- 0
    lvl_pos <- 1
  } else if (is.factor(obs)) {
    lvl_neg <- levels(obs)[1]
    lvl_pos <- levels(obs)[2]
  } else {
    stop("Observed values muste either 0/1 or a binary factor.")
  }
  pred_class <- ifelse(pred > threshold, lvl_pos, lvl_neg)

  mean_pred <- mean(pred)
  TP <- sum(obs == lvl_pos & pred_class == lvl_pos)
  FP <- sum(obs == lvl_neg & pred_class == lvl_pos)
  FN <- sum(obs == lvl_pos & pred_class == lvl_neg)
  TN <- sum(obs == lvl_neg & pred_class == lvl_neg)
  P <- TP + FN
  N <- FP + TN
  TPR <- TP / P
  FPR <- FP / N
  TNR <- TN / N
  FNR <- FN / P
  n_obs <- length(obs)

  c(
    "mean_pred" = mean_pred,
    "n_obs" = n_obs,
    "TP" = TP,
    "FP" = FP,
    "FN" = FN,
    "TN" = TN,
    "P" = P,
    "N" = N,
    "TPR" = TPR,
    "FPR" = FPR,
    "TNR" = TNR
  )
}

#' Metrics Within Groups for a Scoring Classifier
#'
#' @description
#' From predicted scores of a binary scoring classifier in two groups (group 0,
#' and group 1) and corresponding observed outcome (\eqn{Y\in\{0,1\}}), computes
#' various metrics within each group. The scoring classifier is turned into a
#' binary classifier by setting a threshold above which individuals are
#' predicted as 1 (or as the second level of the binary outcome if it is a
#' factor variable).
#'
#' @param obs_0 Vector of observed binary (0/1) variable in group 0.
#' @param obs_1 Vector of observed binary (0/1) variable in group 1.
#' @param pred_0 Vector of predicted scores in group 0.
#' @param pred_1 Vector of predicted scores in group 1.
#' @param threshold Threshold above which predicted scores are classified as 1.
#'
#' @returns A tibble with the metrics in rows in each group (in column). The
#'  following metrics are returned:
#' * `mean_pred`: Mean predicted scores
#' * `n_obs`: Number of observations
#' * `TP`: Count of true positives
#' * `FP`: Count of false positives
#' * `FN`: Count of false negatives
#' * `TN`: Count of true negatives
#' * `P`: Count of positives (`TP` + `FN`)
#' * `N`: Count of positives (`FP` + `TN`)
#' * `TPR`: True positive rate (`TP` / `P`)
#' * `FPR`: False positive rate (`FP` / `N`)
#' * `TNR`: True negative rate (`TN` / `N`)
#' * `FNR`: False negative rate (`FN` / `P`)
#' @export
#' @md
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @examples
#' obs_0 <-    c( 0,  0,  0,  0,  1,  1,  1)
#' pred_0 <-   c(.2, .3, .8, .1, .6, .5, .1)
#' obs_1 <-    c( 0,  1,  0,  1,  1,  1,  1)
#' pred_1 <-   c(.6, .8, .2, .8, .5, .9, .8)
#' group_metrics_classif(obs_0, obs_1, pred_0, pred_1, threshold =  .5)
group_metrics_classif <- function(obs_0,
                                  obs_1,
                                  pred_0,
                                  pred_1,
                                  threshold = .5) {
  metrics_group_0 <- metrics_bin_classif(
    obs = obs_0, pred = pred_0, threshold = threshold
  )
  metrics_group_1 <- metrics_bin_classif(
    obs = obs_1, pred = pred_1, threshold = threshold
  )
  tibble::enframe(metrics_group_0, name = "metric", value = "group_0") |>
    left_join(
      tibble::enframe(metrics_group_1, name = "metric", value = "group_1"),
      by = "metric"
    )
}

#' Counterfactual Fairness Metrics for Binary Scoring Classifier
#'
#' @description
#' From predicted scores of a binary scoring classifier in two groups (group 0,
#' and group 1) and corresponding observed outcome (\eqn{Y\in\{0,1\}}), as well
#' as predicted scores for counterfactual individuals from group 0, computes
#' various metrics within each group. The scoring classifier is turned into a
#' binary classifier by setting a threshold above which individuals are
#' predicted as 1 (or as the second level of the binary outcome if it is a
#' factor variable).
#'
#' @param obs_0 Vector of observed binary (0/1) variable in group 0.
#' @param obs_1 Vector of observed binary (0/1) variable in group 1.
#' @param pred_0 Vector of predicted scores in group 0.
#' @param pred_0_t Vector of predicted scores computed based on counterfactual
#'        characteristics of individuals from group 0.
#' @param pred_1 Vector of predicted scores in group 1.
#' @param threshold Threshold above which predicted scores are classified as 1.
#'
#' @returns A tibble with the metrics in rows in each group (in column). The
#'  following metrics are returned:
#' * `mean_pred`: Mean predicted scores
#' * `n_obs`: Number of observations
#' * `TP`: Count of true positives
#' * `FP`: Count of false positives
#' * `FN`: Count of false negatives
#' * `TN`: Count of true negatives
#' * `P`: Count of positives (`TP` + `FN`)
#' * `N`: Count of positives (`FP` + `TN`)
#' * `TPR`: True positive rate (`TP` / `P`)
#' * `FPR`: False positive rate (`FP` / `N`)
#' * `TNR`: True negative rate (`TN` / `N`)
#' * `FNR`: False negative rate (`FN` / `P`)
#' @export
#' @md
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @examples
#' obs_0 <-    c( 0,  0,  0,  0,  1,  1,  1)
#' pred_0 <-   c(.2, .3, .8, .1, .6, .5, .1)
#' pred_0_t <- c(.2, .2, .7, .6, .8, .7, .3)
#' obs_1 <-    c( 0,  1,  0,  1,  1,  1,  1)
#' pred_1 <-   c(.6, .8, .2, .8, .5, .9, .8)
#' counter_fair_metrics(obs_0, obs_1, pred_0, pred_0_t, pred_1, threshold =  .5)
counter_fair_metrics <- function(obs_0,
                                 obs_1,
                                 pred_0,
                                 pred_0_t,
                                 pred_1,
                                 threshold) {
  m_0 <- metrics_bin_classif(
    obs = obs_0, pred = pred_0, threshold = threshold
  )
  m_0_t <- metrics_bin_classif(
    obs = obs_0, pred = pred_0_t, threshold = threshold
  )
  m_1 <- metrics_bin_classif(
    obs = obs_1, pred = pred_1, threshold = threshold
  )

  # Group 1 vs. Group 0
  metrics_1_vs_0 <- c(
    demog_par = m_0[["P"]]/m_0[["n_obs"]] - m_1[["P"]]/m_1[["n_obs"]],
    eq_opp = m_0[["TPR"]] - m_1[["TPR"]],
    class_bal_true = m_0[["FPR"]] / m_1[["FPR"]],
    class_bal_false = m_0[["TNR"]] / m_1[["TNR"]],
    eq_treatment = (m_0[["FPR"]] / m_0[["FN"]]) - (m_1[["FPR"]] / m_1[["FN"]])
  )
  # Group 1 vs. Group 0_t
  metrics_1_vs_0_t <- c(
    demog_par = m_0_t[["P"]]/m_0_t[["n_obs"]] - m_1[["P"]]/m_1[["n_obs"]],
    eq_opp = m_0_t[["TPR"]] - m_1[["TPR"]],
    class_bal_true = m_0_t[["FPR"]] / m_1[["FPR"]],
    class_bal_false = m_0_t[["TNR"]] / m_1[["TNR"]],
    eq_treatment = (m_0_t[["FPR"]] / m_0_t[["FN"]]) -
      (m_1[["FPR"]] / m_1[["FN"]])
  )

  # Group 0 vs Group 0_t
  metrics_0_vs_0_t <- c(
    c_demog_parity = m_0_t[["mean_pred"]] - m_0[["mean_pred"]],
    c_eq_op = m_0_t[["TPR"]] - m_0[["TPR"]],
    c_class_bal_true = m_0_t[["FPR"]] / m_0[["FPR"]],
    c_class_bal_false = m_0_t[["TNR"]] / m_0[["TNR"]],
    c_eq_treatment = (m_0_t[["FPR"]] / m_0_t[["FN"]]) -
      (m_0[["FPR"]] / m_0[["FN"]])
  )

  group_metrics <- tibble::enframe(m_0, name = "metric", value = "group_0") |>
    left_join(
      tibble::enframe(m_0_t, name = "metric", value = "group_0_t"),
      by = "metric"
    ) |>
    left_join(
      tibble::enframe(m_1, name = "metric", value = "group_1"),
      by = "metric"
    )
  list(
    group_metrics = group_metrics,
    factuals_metrics = metrics_1_vs_0,
    counter_metrics_1 = metrics_1_vs_0_t,
    counter_metrics_0 = metrics_0_vs_0_t
  )
}
