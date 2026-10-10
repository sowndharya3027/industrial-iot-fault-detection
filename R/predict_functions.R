# =====================================================================
# predict_functions.R  -  reusable prediction code (used by the script AND the Shiny app)
# =====================================================================
MODEL_BUNDLE_PATH <- "models/model_bundle.rds"

load_model_bundle <- function(path = MODEL_BUNDLE_PATH) {
  if (!file.exists(path)) stop("Model file not found: ", path, ". Run run_all.R first.", call. = FALSE)
  readRDS(path)
}

# Make column names comparable: "Temperature (°C)" and "temperature_c" both -> "temperaturec"
norm_name <- function(x) gsub("[^a-z]", "", tolower(x))

# Plausible physical limits (same ASSUMED limits as the cleaning script)
PLAUSIBLE <- list(vibration_mm_s = c(0, 50), temperature_c = c(-50, 300), pressure_bar = c(0, 100))

# Check the input columns. Returns the data with clean column names, or stops with a clear message.
validate_input_columns <- function(new_data, bundle) {
  if (!is.data.frame(new_data) || nrow(new_data) == 0) stop("The input is empty (no rows).", call. = FALSE)
  matched <- sapply(seq_along(bundle$sensor_raw), function(i) {
    hit <- which(norm_name(names(new_data)) %in% c(norm_name(bundle$sensor_raw[i]), norm_name(bundle$sensor_clean[i])))
    if (length(hit) == 0) NA_integer_ else hit[1] })
  if (anyNA(matched)) {
    stop("Missing required column(s): ", paste(bundle$sensor_raw[is.na(matched)], collapse = ", "),
         ".\nExpected columns: ", paste(bundle$sensor_raw, collapse = ", "),
         ".\nColumns found: ", paste(names(new_data), collapse = ", "), call. = FALSE)
  }
  out <- as.data.frame(new_data[, matched, drop = FALSE]); names(out) <- bundle$sensor_clean
  out
}

# Predict fault class for each row. Invalid rows are NOT predicted; they get a clear status message.
predict_fault <- function(new_data, bundle = load_model_bundle()) {
  x <- validate_input_columns(new_data, bundle)
  n <- nrow(x); lv <- bundle$class_levels
  status <- rep("OK", n)
  for (col in bundle$sensor_clean) {
    v <- suppressWarnings(as.numeric(as.character(x[[col]])))
    bad_type <- is.na(v) | !is.finite(v)
    lim <- PLAUSIBLE[[col]]
    bad_range <- !bad_type & (v < lim[1] | v > lim[2])
    status[bad_type & status == "OK"]  <- paste0("Not predicted: missing/non-numeric ", col)
    status[bad_range & status == "OK"] <- paste0("Not predicted: implausible ", col, " value")
    x[[col]] <- v
  }
  ok <- status == "OK"
  # Warn (but still predict) if a value is outside the range seen in training
  for (col in bundle$sensor_clean) {
    out <- ok & (x[[col]] < bundle$train_min[col] | x[[col]] > bundle$train_max[col])
    status[out] <- paste0("OK (", col, " outside training range - treat with caution)")
  }
  res <- data.frame(row = seq_len(n), predicted_class = NA_character_, estimated_probability = NA_real_,
                    matrix(NA_real_, n, length(lv), dimnames = list(NULL, paste0("prob_", lv))), status = status,
                    check.names = FALSE, stringsAsFactors = FALSE)
  if (any(ok)) {
    scaled <- predict(bundle$preprocess, x[ok, bundle$sensor_clean, drop = FALSE])
    prob <- as.matrix(predict(bundle$model, scaled, type = "prob")[, lv, drop = FALSE])
    res$predicted_class[ok] <- lv[max.col(prob, ties.method = "first")]
    res$estimated_probability[ok] <- round(apply(prob, 1, max), 4)
    res[ok, paste0("prob_", lv)] <- round(prob, 4)
  }
  res
}
