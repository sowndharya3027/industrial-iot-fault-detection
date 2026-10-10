# =====================================================================
# 04_feature_preparation_split.R  -  Stage 4: predictors, split, scaling
# Output: data/train_data.csv, data/test_data.csv, data/sample_new_records.csv,
#         models/preprocess.rds, outputs/results/split_summary.csv
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages({library(tidyverse); library(caret)})
set.seed(SEED)

d <- read_csv(PATHS$clean_csv, show_col_types = FALSE) %>%
  mutate(fault_label = factor(fault_label, levels = CLASS_LEVELS)) %>% arrange(timestamp)

# ---- Column roles -----------------------------------------------------------
# predictors : vibration_mm_s, temperature_c, pressure_bar (all numeric: no encoding needed)
# target     : fault_label (3-class factor)
# timestamp  : used ONLY to order the data (not a predictor)
# dropped    : RMS Vibration, Mean Temp (constant whole-dataset summaries = leakage-prone)

# ---- Chronological split ---------------------------------------------------------
# Readings are one minute apart, so neighbouring rows are time-dependent.
# A random shuffle would mix "future" rows into training, so the FIRST 80% of
# time is used for training and the LAST 20% for testing.
n_train <- floor(TRAIN_FRACTION * nrow(d))
train <- d[seq_len(n_train), ]
test  <- d[(n_train + 1):nrow(d), ]
write.csv(train, PATHS$train_csv, row.names = FALSE)
write.csv(test,  PATHS$test_csv,  row.names = FALSE)

# ---- Scaling: fitted on TRAINING data only -----------------------------------------
pp <- preProcess(train[SENSOR_COLS], method = c("center", "scale"))
saveRDS(pp, file.path(PATHS$models, "preprocess.rds"))

# ---- Class imbalance -------------------------------------------------------------
cat("Train class counts:\n"); print(table(train$fault_label))
cat("Test class counts:\n");  print(table(test$fault_label))
cat("\nImbalance decision: classes are imbalanced (class_2 is the smallest) but not extreme.\n",
    "No resampling is applied, so the test set keeps its real class mix.\n",
    "Per-class recall/F1 and the majority-class baseline are reported instead.\n")

summary_tbl <- bind_rows(
  train %>% count(fault_label) %>% mutate(set = "train", start = min(train$timestamp), end = max(train$timestamp)),
  test  %>% count(fault_label) %>% mutate(set = "test",  start = min(test$timestamp),  end = max(test$timestamp))) %>%
  group_by(set) %>% mutate(percent = round(100 * n / sum(n), 2)) %>% ungroup() %>%
  mutate(seed = SEED, split_method = "chronological 80/20")
write.csv(summary_tbl, file.path(PATHS$results, "split_summary.csv"), row.names = FALSE)
print(as.data.frame(summary_tbl))

# ---- Sample file for the prediction script and the dashboard -----------------------
# The first 20 REAL test records, sensor columns only, with the ORIGINAL column names.
sample_new <- test[1:20, SENSOR_COLS]; names(sample_new) <- RAW_SENSOR
write.csv(sample_new, PATHS$sample_new, row.names = FALSE, fileEncoding = "UTF-8")
cat("\nTrain rows:", nrow(train), " Test rows:", nrow(test), " Seed:", SEED, "\n")
