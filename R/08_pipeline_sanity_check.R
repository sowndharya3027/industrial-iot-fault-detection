# =====================================================================
# 08_pipeline_sanity_check.R  -  OPTIONAL: is the modelling code itself working?
# The real Fault Label shows no relationship with the sensors, so this script uses the
# REAL sensor values but replaces the label with a deliberately rule-based test label
# ("high temperature AND high vibration").  If the code is correct, a Random Forest
# must score far above chance here.  This is a code test ONLY - these numbers are
# NOT results about your dataset and must never be reported as such.
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages({library(tidyverse); library(caret)})
set.seed(SEED)
train <- read_csv(PATHS$train_csv, show_col_types = FALSE); test <- read_csv(PATHS$test_csv, show_col_types = FALSE)
make_label <- function(d) factor(ifelse(d$temperature_c > 100 & d$vibration_mm_s > 0.5, "fault", "normal"), levels = c("normal", "fault"))
pp <- readRDS(file.path(PATHS$models, "preprocess.rds"))
rf <- train(as.data.frame(predict(pp, as.data.frame(train[SENSOR_COLS]))), make_label(train), method = "rf", ntree = 200,
            trControl = trainControl(method = "none"), tuneGrid = data.frame(mtry = 2))
pred <- predict(rf, as.data.frame(predict(pp, as.data.frame(test[SENSOR_COLS])))); truth <- make_label(test)
out <- data.frame(purpose = "pipeline code test with a rule-based label (NOT real results)",
                  test_accuracy = mean(pred == truth),
                  majority_accuracy = max(prop.table(table(truth))))
print(out); write.csv(out, file.path(PATHS$results, "pipeline_sanity_check.csv"), row.names = FALSE)
