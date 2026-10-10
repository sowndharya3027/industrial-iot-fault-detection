# =====================================================================
# 03_visualisation.R  -  Stage 3: exploratory data analysis plots
# Output: PNG files in outputs/plots/
# =====================================================================
source("R/utils.R"); check_packages(); suppressMessages(library(tidyverse))

d <- read_csv(PATHS$clean_csv, show_col_types = FALSE) %>%
  mutate(fault_label = factor(fault_label, levels = CLASS_LEVELS))
nice <- c(vibration_mm_s = "Vibration (mm/s)", temperature_c = "Temperature (\u00b0C)", pressure_bar = "Pressure (bar)")
long <- d %>% pivot_longer(all_of(SENSOR_COLS), names_to = "sensor", values_to = "value") %>%
  mutate(sensor = factor(nice[sensor], levels = nice))
cols <- c(class_0 = "#2E86AB", class_1 = "#F6A01A", class_2 = "#C73E1D")
theme_set(theme_minimal(base_size = 12))

# 1. Target-class distribution ------------------------------------------------
p1 <- d %>% count(fault_label) %>% mutate(pct = round(100 * n / sum(n), 1)) %>%
  ggplot(aes(fault_label, n, fill = fault_label)) + geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), vjust = -0.4) +
  scale_fill_manual(values = cols) + expand_limits(y = max(table(d$fault_label)) * 1.12) +
  labs(title = "Distribution of Fault Label classes", x = "Fault Label", y = "Number of records")
save_plot(p1, "01_class_distribution.png")

# 2. Sensor distributions ------------------------------------------------------
p2 <- ggplot(long, aes(value)) + geom_histogram(bins = 30, fill = "#2E86AB", colour = "white") +
  facet_wrap(~sensor, scales = "free", nrow = 1) +
  labs(title = "Distribution of each sensor reading", x = "Sensor value", y = "Count")
save_plot(p2, "02_sensor_distributions.png", 10, 4)

# 3. Sensor comparison across classes (density) ----------------------------------
p3 <- ggplot(long, aes(value, fill = fault_label, colour = fault_label)) +
  geom_density(alpha = 0.25) + facet_wrap(~sensor, scales = "free", nrow = 1) +
  scale_fill_manual(values = cols) + scale_colour_manual(values = cols) +
  labs(title = "Sensor densities by Fault Label", x = "Sensor value", y = "Density", fill = "Class", colour = "Class")
save_plot(p3, "03_sensor_density_by_class.png", 10, 4)

# 4. Boxplots by class -----------------------------------------------------------
p4 <- ggplot(long, aes(fault_label, value, fill = fault_label)) + geom_boxplot(show.legend = FALSE) +
  facet_wrap(~sensor, scales = "free_y", nrow = 1) + scale_fill_manual(values = cols) +
  labs(title = "Sensor boxplots by Fault Label", x = "Fault Label", y = "Sensor value")
save_plot(p4, "04_sensor_boxplots_by_class.png", 10, 4)

# 5. Correlation heatmap (sensor features only; constant columns excluded) ----------
cm <- cor(d[SENSOR_COLS]); rownames(cm) <- colnames(cm) <- nice[SENSOR_COLS]
p5 <- reshape2::melt(cm) %>%
  ggplot(aes(Var1, Var2, fill = value)) + geom_tile(colour = "white") +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2(low = "#C73E1D", mid = "white", high = "#2E86AB", limits = c(-1, 1)) +
  labs(title = "Correlation between sensor features", x = NULL, y = NULL, fill = "r")
save_plot(p5, "05_correlation_heatmap.png", 7, 5)
write.csv(round(cm, 4), file.path(PATHS$results, "sensor_correlation.csv"))

# 6. Time-series plot (valid 1-minute timestamps exist) -------------------------------
roll <- function(x, k = 30) as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
ts_long <- long %>% group_by(sensor) %>% arrange(timestamp) %>% mutate(rolling_mean = roll(value)) %>% ungroup()
p6 <- ggplot(ts_long, aes(timestamp)) + geom_line(aes(y = value), colour = "grey70", linewidth = 0.3) +
  geom_line(aes(y = rolling_mean), colour = "#2E86AB", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~sensor, scales = "free_y", ncol = 1) +
  labs(title = "Sensor readings over time", subtitle = "Grey = raw value, blue = 30-minute rolling mean",
       x = "Time (2023-03-10)", y = "Sensor value")
save_plot(p6, "06_sensor_time_series.png", 9, 7)

# 7. Fault label over time ----------------------------------------------------------
p7 <- ggplot(d, aes(timestamp, fault_label, colour = fault_label)) + geom_point(alpha = 0.6, size = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = cols) + labs(title = "Fault Label over time", x = "Time", y = "Fault Label")
save_plot(p7, "07_fault_label_timeline.png", 9, 3.5)

# 8. Share of each class per time block (is the label drifting in time?) -------------
p8 <- d %>% mutate(block = cut(row_number(), breaks = 5, labels = paste("Block", 1:5))) %>%
  count(block, fault_label) %>% group_by(block) %>% mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(block, pct, fill = fault_label)) + geom_col() + scale_fill_manual(values = cols) +
  labs(title = "Class mix across five consecutive time blocks", x = NULL, y = "% of records", fill = "Class")
save_plot(p8, "08_class_mix_over_time.png", 8, 4)

cat("Plots saved:\n"); print(list.files(PATHS$plots))
