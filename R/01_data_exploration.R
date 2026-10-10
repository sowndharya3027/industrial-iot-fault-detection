# =====================================================================
# 01_data_exploration.R  -  Stage 1: look at the RAW uploaded dataset
# Output: outputs/results/dataset_profile.csv, target_distribution.csv,
#         sensor_vs_label_tests.csv, raw_head.csv
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages(library(tidyverse))

# read_raw() keeps the original column names such as "Vibration (mm/s)"
raw <- read_raw()

cat("Rows:", nrow(raw), "  Columns:", ncol(raw), "\n\nColumn names:\n")
print(names(raw))
cat("\nStructure:\n"); str(raw)
cat("\nSummary statistics:\n"); print(summary(raw))
cat("\nFirst 6 records:\n"); print(head(raw))

# ---- dataset profile: one row per column --------------------------------
profile <- tibble(
  column = names(raw),
  type = sapply(raw, function(x) class(x)[1]),
  n_missing = sapply(raw, function(x) sum(is.na(x))),
  n_unique = sapply(raw, function(x) n_distinct(x)),
  min  = sapply(raw, function(x) if (is.numeric(x)) min(x) else NA),
  mean = sapply(raw, function(x) if (is.numeric(x)) mean(x) else NA),
  max  = sapply(raw, function(x) if (is.numeric(x)) max(x) else NA),
  sd   = sapply(raw, function(x) if (is.numeric(x)) sd(x) else NA))
print(as.data.frame(profile))
write.csv(profile, file.path(PATHS$results, "dataset_profile.csv"), row.names = FALSE)
write.csv(head(raw, 10), file.path(PATHS$results, "raw_head.csv"), row.names = FALSE)

cat("\nExact duplicate rows:", sum(duplicated(raw)), "\n")

# ---- target variable ----------------------------------------------------
target <- raw[[RAW_TARGET]]
tdist <- as.data.frame(table(label = target)) %>%
  mutate(percent = round(100 * Freq / sum(Freq), 2))
cat("\nTarget variable:", RAW_TARGET, "\n"); print(tdist)
write.csv(tdist, file.path(PATHS$results, "target_distribution.csv"), row.names = FALSE)

# ---- do the sensors differ between classes? (Kruskal-Wallis test) ---------
tests <- map_dfr(RAW_SENSOR, function(s) {
  kt <- kruskal.test(raw[[s]] ~ factor(target))
  tibble(sensor = s, kruskal_chi_sq = unname(kt$statistic), p_value = kt$p.value)
})
print(tests)
write.csv(tests, file.path(PATHS$results, "sensor_vs_label_tests.csv"), row.names = FALSE)

# ---- columns with no variation -----------------------------------------
cat("\nColumns with zero variance:\n")
print(names(raw)[sapply(raw, function(x) is.numeric(x) && sd(x) == 0)])
