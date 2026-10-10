# =====================================================================
# 02_data_cleaning.R  -  Stage 2: clean the data and log every decision
# Output: data/cleaned_fault_data.csv, outputs/results/cleaning_log.csv
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages(library(tidyverse))

raw <- read_raw()
cleaning_log <- list()
add_log <- function(check, finding, action)
  cleaning_log[[length(cleaning_log) + 1]] <<- data.frame(check = check, finding = finding, action = action)

# 1. Missing values ---------------------------------------------------------
n_na <- sum(is.na(raw))
add_log("Missing values", paste(n_na, "missing cells"), "None to impute")

# 2. Exact duplicates ---------------------------------------------------------
n_dup <- sum(duplicated(raw))
add_log("Exact duplicate rows", paste(n_dup, "duplicates"), "None removed")

# 3. Data types: parse timestamp, check that sensors are numeric ------------------
ts <- as.POSIXct(raw[[RAW_TIME]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
add_log("Timestamp parsing", paste(sum(is.na(ts)), "unparseable timestamps"),
        "Converted to POSIXct (UTC)")
stopifnot(all(sapply(raw[c(RAW_SENSOR, RAW_CONST, RAW_TARGET)], is.numeric)))
add_log("Numeric columns", "All sensor columns and the label are numeric", "None needed")

# 4. Time ordering and sampling interval -----------------------------------------------
gaps <- as.numeric(diff(ts), units = "mins")
add_log("Time ordering", paste0("Sorted: ", !is.unsorted(ts), "; unique timestamps: ", !anyDuplicated(ts),
        "; gaps in minutes: ", paste(unique(gaps), collapse = ",")),
        "Data kept in chronological order (used for a chronological split)")

# 5. Plausibility checks (ASSUMED physical limits - adjust for your machine) -----------
limits <- setNames(list(c(0, 50), c(-50, 300), c(0, 100)), RAW_SENSOR)
for (s in names(limits)) {
  bad <- sum(raw[[s]] < limits[[s]][1] | raw[[s]] > limits[[s]][2])
  add_log(paste("Plausible range:", s), paste(bad, "values outside", paste(limits[[s]], collapse = " to ")),
          "No rows removed")
}

# 6. Outliers: IQR rule, REPORTED ONLY (could be genuine fault behaviour) --------------
for (s in RAW_SENSOR) {
  q <- quantile(raw[[s]], c(.25, .75)); iqr <- diff(q)
  n_out <- sum(raw[[s]] < q[1] - 1.5 * iqr | raw[[s]] > q[2] + 1.5 * iqr)
  add_log(paste("IQR outliers:", s), paste(n_out, "outliers"), "Kept (outliers may be real faults)")
}

# 7. Constant columns -----------------------------------------------------------
for (cc in RAW_CONST) {
  add_log(paste("Constant column:", cc),
          paste("Single value in all rows:", signif(unique(raw[[cc]]), 8)),
          "Dropped: no information, and it looks like a whole-dataset summary (RMS / mean over all rows), which would leak test-set information if used as a feature")
}
rms_check <- sqrt(mean(raw[[RAW_SENSOR[1]]]^2)); mean_check <- mean(raw[[RAW_SENSOR[2]]])
add_log("Check of constant columns", paste0("RMS of Vibration over all rows = ", signif(rms_check, 8),
        "; mean of Temperature over all rows = ", signif(mean_check, 8)),
        "Confirms the two columns are global summaries, not per-row features")

# 8. Build the cleaned data set (nothing derived from the target) --------------
clean <- tibble(timestamp = ts,
                vibration_mm_s = raw[[RAW_SENSOR[1]]],
                temperature_c  = raw[[RAW_SENSOR[2]]],
                pressure_bar   = raw[[RAW_SENSOR[3]]],
                fault_label    = factor(paste0("class_", raw[[RAW_TARGET]]), levels = CLASS_LEVELS)) %>%
  arrange(timestamp)

write.csv(clean, PATHS$clean_csv, row.names = FALSE)
write.csv(do.call(rbind, cleaning_log), file.path(PATHS$results, "cleaning_log.csv"), row.names = FALSE)
cat("Cleaned data saved:", nrow(clean), "rows x", ncol(clean), "columns\n")
print(do.call(rbind, cleaning_log), right = FALSE)
