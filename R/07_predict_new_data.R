# =====================================================================
# 07_predict_new_data.R  -  predict fault class for new sensor records
# Usage (from the project folder):
#   Rscript R/07_predict_new_data.R data/sample_new_records.csv outputs/results/new_predictions.csv
# Required input columns: Vibration (mm/s), Temperature (°C), Pressure (bar)
# NOTE: probabilities are the model's estimated class probabilities (not calibrated).
# =====================================================================
source("R/utils.R"); check_packages(c("caret", "randomForest", "kernlab", "nnet", "readr"))
source("R/predict_functions.R")

args <- commandArgs(trailingOnly = TRUE)
in_file  <- if (length(args) >= 1) args[1] else PATHS$sample_new
out_file <- if (length(args) >= 2) args[2] else file.path(PATHS$results, "new_predictions.csv")

if (!file.exists(in_file)) stop("Input file not found: ", in_file, call. = FALSE)
new_data <- as.data.frame(readr::read_csv(in_file, show_col_types = FALSE, name_repair = "minimal"), check.names = FALSE)
bundle <- load_model_bundle()
cat("Model used:", bundle$model_name, "\n")
result <- predict_fault(new_data, bundle)
write.csv(result, out_file, row.names = FALSE)
print(head(result, 10)); cat("\nPredicted", sum(!is.na(result$predicted_class)), "of", nrow(result),
                              "rows. Saved to", out_file, "\n")
