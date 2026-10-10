# =====================================================================
# 05_model_training.R  -  Stage 5: train and compare three models
# Models : Multinomial logistic regression (baseline), Random Forest, SVM (RBF)
# CV     : rolling-origin ("timeslice") cross-validation on TRAINING data only
# Output : models/*.rds, outputs/results/cv_results.csv, models/model_bundle.rds
# The test set is NOT touched in this script.
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages({library(tidyverse); library(caret)})
set.seed(SEED)

train <- read_csv(PATHS$train_csv, show_col_types = FALSE) %>%
  mutate(fault_label = factor(fault_label, levels = CLASS_LEVELS))
pp <- readRDS(file.path(PATHS$models, "preprocess.rds"))
x_train <- as.data.frame(predict(pp, as.data.frame(train[SENSOR_COLS])))   # scaled predictors
y_train <- train$fault_label

# Rolling-origin CV respects time order: always train on the past, validate on the next 100 rows
ctrl <- trainControl(method = "timeslice", initialWindow = 400, horizon = 100,
                     skip = 99, fixedWindow = FALSE, classProbs = TRUE,
                     summaryFunction = defaultSummary, savePredictions = "final")

models <- list()
cat("Training Logistic Regression ...\n"); set.seed(SEED)
models$logistic_regression <- train(x_train, y_train, method = "multinom", trace = FALSE,
  trControl = ctrl, metric = "Kappa", tuneGrid = data.frame(decay = c(0, 0.01, 0.1, 1)))

cat("Training Random Forest ...\n"); set.seed(SEED)
models$random_forest <- train(x_train, y_train, method = "rf", ntree = 300, importance = TRUE,
  trControl = ctrl, metric = "Kappa", tuneGrid = data.frame(mtry = 1:3))

cat("Training SVM (RBF kernel) ...\n"); set.seed(SEED)
models$svm_radial <- train(x_train, y_train, method = "svmRadial", prob.model = TRUE,
  trControl = ctrl, metric = "Kappa", tuneLength = 4)

# ---- CV summary: best tuning setting of each model ------------------------------------
cv <- map_dfr(names(models), function(nm) {
  m <- models[[nm]]; best <- m$results[best_row <- which.max(replace(m$results$Kappa, is.na(m$results$Kappa), -Inf)), ]
  tibble(model = nm, best_tuning = paste(names(m$bestTune), unlist(m$bestTune), sep = "=", collapse = ", "),
         cv_accuracy = best$Accuracy, cv_kappa = best$Kappa)
})
majority <- max(prop.table(table(y_train)))
cv$majority_baseline_accuracy <- majority
print(as.data.frame(cv))
write.csv(cv, file.path(PATHS$results, "cv_results.csv"), row.names = FALSE)

# ---- Select the final model by CV Kappa (NOT by test results) ----------------------------
sel <- cv$model[which.max(replace(cv$cv_kappa, is.na(cv$cv_kappa), -Inf))]
cat("\nSelected model by cross-validation Kappa:", sel, "\n")

for (nm in names(models)) saveRDS(models[[nm]], file.path(PATHS$models, paste0(nm, ".rds")))
bundle <- list(model = models[[sel]], model_name = sel, preprocess = pp,
               class_levels = CLASS_LEVELS, sensor_clean = SENSOR_COLS, sensor_raw = RAW_SENSOR,
               train_min = sapply(train[SENSOR_COLS], min), train_max = sapply(train[SENSOR_COLS], max),
               seed = SEED, trained_on_rows = nrow(train))
saveRDS(bundle, file.path(PATHS$models, "model_bundle.rds"))
cat("Saved models/ and model_bundle.rds\n")
