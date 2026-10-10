# =====================================================================
# 06_model_evaluation.R  -  Stages 6 & 7: test-set evaluation + feature importance
# This is the FIRST time the held-out test set is used.
# Output: outputs/results/*.csv and outputs/plots/*.png
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages({library(tidyverse); library(caret)})

train <- read_csv(PATHS$train_csv, show_col_types = FALSE) %>% mutate(fault_label = factor(fault_label, levels = CLASS_LEVELS))
test  <- read_csv(PATHS$test_csv,  show_col_types = FALSE) %>% mutate(fault_label = factor(fault_label, levels = CLASS_LEVELS))
pp <- readRDS(file.path(PATHS$models, "preprocess.rds"))
bundle <- readRDS(file.path(PATHS$models, "model_bundle.rds"))
x_test <- as.data.frame(predict(pp, as.data.frame(test[SENSOR_COLS]))); y_test <- test$fault_label

model_files <- c(logistic_regression = "logistic_regression.rds", random_forest = "random_forest.rds", svm_radial = "svm_radial.rds")
models <- lapply(model_files, function(f) readRDS(file.path(PATHS$models, f)))

# Majority-class baseline: always predict the most common TRAINING class
majority_class <- names(which.max(table(train$fault_label)))
majority_acc_test <- mean(y_test == majority_class)

all_preds <- list(); comparison <- list(); per_class_all <- list(); binary_all <- list(); roc_data <- list()
for (nm in names(models)) {
  prob <- predict(models[[nm]], x_test, type = "prob")[, CLASS_LEVELS]
  # Class = highest estimated probability (first class wins a tie). This is deterministic and
  # identical to what the prediction script and dashboard return. (predict(type = "raw") on a
  # Random Forest breaks vote ties at RANDOM, which would make results change between runs.)
  pred <- factor(CLASS_LEVELS[max.col(as.matrix(prob), ties.method = "first")], levels = CLASS_LEVELS)
  all_preds[[nm]] <- pred
  ev <- evaluate_multiclass(y_test, pred, as.matrix(prob), majority_acc_test)
  comparison[[nm]] <- cbind(model = nm, selected_by_cv = (nm == bundle$model_name), ev$overall)
  per_class_all[[nm]] <- cbind(model = nm, ev$per_class)
  binary_all[[nm]] <- cbind(model = nm, evaluate_binary_view(y_test, pred, as.matrix(prob)))
  cm_df <- cbind(Predicted = rownames(ev$confusion), as.data.frame.matrix(ev$confusion))   # first column = predicted class
  write.csv(cm_df, file.path(PATHS$results, paste0("confusion_matrix_", nm, ".csv")), row.names = FALSE)
  cat("\n====", nm, "====\n"); print(ev$confusion); print(round(ev$overall, 4))
  # data for ROC curves (one-vs-rest)
  for (k in CLASS_LEVELS) {
    r <- pROC::roc(y_test == k, prob[[k]], levels = c(FALSE, TRUE), direction = "<", quiet = TRUE)
    roc_data[[paste(nm, k)]] <- data.frame(model = nm, class = k, fpr = 1 - r$specificities, tpr = r$sensitivities)
  }
}
comparison <- bind_rows(comparison); per_class <- bind_rows(per_class_all); binary <- bind_rows(binary_all)
# add the majority baseline as its own row for context
comparison <- bind_rows(comparison, tibble(model = "majority_class_baseline", selected_by_cv = NA,
  accuracy = majority_acc_test, majority_class_accuracy = majority_acc_test, kappa = 0))
write.csv(comparison, file.path(PATHS$results, "model_comparison.csv"), row.names = FALSE)
write.csv(per_class,  file.path(PATHS$results, "per_class_metrics.csv"), row.names = FALSE)
write.csv(binary,     file.path(PATHS$results, "binary_fault_vs_normal_assumption.csv"), row.names = FALSE)
cat("\nBinary view (ASSUMES class_0 = normal):\n"); print(as.data.frame(binary))

# ---- Plots ---------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
# confusion matrices
cms <- map_dfr(names(models), function(nm) {
  as.data.frame(table(Predicted = all_preds[[nm]], Actual = y_test)) %>% mutate(model = nm) })
p_cm <- ggplot(cms, aes(Actual, Predicted, fill = Freq)) + geom_tile(colour = "white") + geom_text(aes(label = Freq), size = 5) +
  facet_wrap(~model) + scale_fill_gradient(low = "#EAF2F8", high = "#2E86AB") +
  labs(title = "Confusion matrices on the held-out test set", subtitle = "Rows = predicted class, columns = actual class")
save_plot(p_cm, "09_confusion_matrices.png", 11, 4)

p_roc <- ggplot(bind_rows(roc_data), aes(fpr, tpr, colour = model)) + geom_line() +
  geom_abline(linetype = "dashed", colour = "grey50") + facet_wrap(~class) +
  labs(title = "One-vs-rest ROC curves (test set)", subtitle = "Dashed line = random guessing", x = "False positive rate", y = "True positive rate")
save_plot(p_roc, "10_roc_curves.png", 11, 4)

p_cmp <- comparison %>% filter(!is.na(accuracy)) %>% select(model, accuracy, kappa, macro_f1) %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>% filter(!is.na(value)) %>%
  ggplot(aes(model, value, fill = metric)) + geom_col(position = "dodge") +
  labs(title = "Model comparison on the test set", x = NULL, y = "Value", fill = "Metric") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_plot(p_cmp, "11_model_comparison.png", 9, 5)

# ---- Stage 7: feature importance (Random Forest, model-specific) ----------------------
rf <- models$random_forest$finalModel
imp <- as.data.frame(randomForest::importance(rf)); imp$feature <- rownames(imp)
imp <- imp %>% select(feature, MeanDecreaseAccuracy, MeanDecreaseGini) %>% arrange(desc(MeanDecreaseAccuracy))
imp$feature_label <- c(vibration_mm_s = "Vibration (mm/s)", temperature_c = "Temperature (\u00b0C)", pressure_bar = "Pressure (bar)")[imp$feature]
print(imp); write.csv(imp, file.path(PATHS$results, "feature_importance_rf.csv"), row.names = FALSE)
p_imp <- imp %>% pivot_longer(c(MeanDecreaseAccuracy, MeanDecreaseGini), names_to = "measure", values_to = "value") %>%
  ggplot(aes(reorder(feature_label, value), value)) + geom_col(fill = "#2E86AB") + coord_flip() +
  facet_wrap(~measure, scales = "free_x") +
  labs(title = "Random Forest feature importance (training data)",
       subtitle = "Importance shows what the model used - it does not prove cause and effect", x = NULL, y = "Importance")
save_plot(p_imp, "12_feature_importance.png", 10, 4)
cat("\nEvaluation complete.\n")
