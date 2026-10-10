# =====================================================================
# utils.R  -  shared settings and helper functions
# Every other script starts with:  source("R/utils.R")
# IMPORTANT: run all scripts with the PROJECT FOLDER as working directory
# (in RStudio: Session > Set Working Directory > To Project Directory).
# =====================================================================

# ---- 1. Packages ----------------------------------------------------
REQUIRED_PACKAGES <- c("tidyverse", "caret", "randomForest", "kernlab",
                       "nnet", "pROC", "shiny", "DT", "reshape2", "scales")

# Stops with a clear install message if a package is missing
check_packages <- function(pkgs = REQUIRED_PACKAGES) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         "\nInstall them with:\n  install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- 2. Paths (all relative to the project folder) -------------------
PATHS <- list(
  raw_csv     = "data/industrial_fault_detection_data_1000.csv",
  clean_csv   = "data/cleaned_fault_data.csv",
  train_csv   = "data/train_data.csv",
  test_csv    = "data/test_data.csv",
  sample_new  = "data/sample_new_records.csv",
  plots       = "outputs/plots",
  results     = "outputs/results",
  models      = "models"
)

# ---- 3. Column names -------------------------------------------------
# Original names exactly as they appear in the uploaded CSV
RAW_TIME   <- "Timestamp"
# "\u00b0" is the degree sign; written as an escape so the file works on every OS/locale
RAW_SENSOR <- c("Vibration (mm/s)", "Temperature (\u00b0C)", "Pressure (bar)")
RAW_CONST  <- c("RMS Vibration", "Mean Temp")      # constant columns (see cleaning)
RAW_TARGET <- "Fault Label"

# Syntax-safe names used inside R after cleaning
SENSOR_COLS <- c("vibration_mm_s", "temperature_c", "pressure_bar")
TARGET_COL  <- "fault_label"
CLASS_LEVELS <- c("class_0", "class_1", "class_2")  # numeric labels 0, 1, 2

# Reads the raw CSV as UTF-8 (readr is locale-independent) and keeps original names
read_raw <- function(path = PATHS$raw_csv) {
  as.data.frame(readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal",
                                locale = readr::locale(encoding = "UTF-8")), check.names = FALSE)
}

SEED <- 123          # one random seed used everywhere
TRAIN_FRACTION <- 0.80

# ---- 4. Evaluation helpers --------------------------------------------

# PR-AUC computed as "average precision" (no extra package needed)
pr_auc <- function(is_positive, score) {
  o <- order(score, decreasing = TRUE)
  y <- is_positive[o]
  if (sum(y) == 0) return(NA_real_)
  tp <- cumsum(y); fp <- cumsum(!y)
  precision <- tp / (tp + fp)
  recall    <- tp / sum(y)
  sum(diff(c(0, recall)) * precision)
}

# Safe ROC-AUC (NA if only one class present)
roc_auc <- function(is_positive, score) {
  if (length(unique(is_positive)) < 2) return(NA_real_)
  as.numeric(pROC::auc(pROC::roc(is_positive, score, levels = c(FALSE, TRUE),
                                 direction = "<", quiet = TRUE)))
}

safe_div <- function(a, b) ifelse(b == 0, NA_real_, a / b)

# Multiclass evaluation: per-class (one-vs-rest) and overall metrics.
# truth, pred = factors with the same levels; prob = matrix of class probabilities
evaluate_multiclass <- function(truth, pred, prob, majority_accuracy = NA) {
  lv <- levels(truth)
  cm <- table(Predicted = pred, Actual = truth)
  per <- do.call(rbind, lapply(lv, function(k) {
    tp <- sum(pred == k & truth == k); fp <- sum(pred == k & truth != k)
    fn <- sum(pred != k & truth == k); tn <- sum(pred != k & truth != k)
    prec <- safe_div(tp, tp + fp); rec <- safe_div(tp, tp + fn)
    # Convention: if a metric is undefined (no predictions of that class) -> F1 = 0
    f1 <- ifelse(is.na(prec) | is.na(rec) | (prec + rec) == 0, 0, 2 * prec * rec / (prec + rec))
    data.frame(class = k, support = tp + fn, TP = tp, FP = fp, FN = fn, TN = tn,
               precision = prec, recall_sensitivity = rec,
               specificity = safe_div(tn, tn + fp), f1 = f1,
               roc_auc = roc_auc(truth == k, prob[, k]),
               pr_auc  = pr_auc(truth == k, prob[, k]))
  }))
  acc <- mean(pred == truth)
  pe  <- sum(table(pred) / length(pred) * table(truth) / length(truth))
  overall <- data.frame(
    accuracy = acc,
    majority_class_accuracy = majority_accuracy,
    kappa = (acc - pe) / (1 - pe),
    macro_precision = mean(ifelse(is.na(per$precision), 0, per$precision)),
    macro_recall = mean(per$recall_sensitivity, na.rm = TRUE),
    macro_specificity = mean(per$specificity, na.rm = TRUE),
    macro_f1 = mean(per$f1),
    macro_roc_auc = mean(per$roc_auc, na.rm = TRUE),
    macro_pr_auc = mean(per$pr_auc, na.rm = TRUE),
    total_missed_per_class_FN = sum(per$FN))
  list(confusion = cm, per_class = per, overall = overall)
}

# Binary "fault vs normal" view.  ASSUMPTION: class_0 = normal operation,
# class_1 / class_2 = fault types.  The dataset has no documentation of the
# label meaning, so this view is clearly labelled as an assumption.
evaluate_binary_view <- function(truth, pred, prob) {
  is_fault  <- truth != "class_0"
  pred_fault <- pred != "class_0"
  tp <- sum(pred_fault & is_fault); fp <- sum(pred_fault & !is_fault)
  fn <- sum(!pred_fault & is_fault); tn <- sum(!pred_fault & !is_fault)
  prec <- safe_div(tp, tp + fp); rec <- safe_div(tp, tp + fn)
  data.frame(TP = tp, FP = fp, FN_missed_faults = fn, TN = tn,
             accuracy = (tp + tn) / length(truth), precision = prec,
             recall_sensitivity = rec, specificity = safe_div(tn, tn + fp),
             f1 = ifelse(is.na(prec) | is.na(rec) | (prec + rec) == 0, 0, 2 * prec * rec / (prec + rec)),
             roc_auc = roc_auc(is_fault, 1 - prob[, "class_0"]),
             pr_auc  = pr_auc(is_fault, 1 - prob[, "class_0"]))
}

save_plot <- function(p, filename, w = 8, h = 5) {
  ggplot2::ggsave(file.path(PATHS$plots, filename), p, width = w, height = h, dpi = 150)
}
