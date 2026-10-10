# Industrial IoT Fault Detection and Predictive Maintenance Using R

## 1. Project overview
An R project that cleans, explores and models an industrial sensor dataset to classify the `Fault Label` of a machine record from vibration, temperature and pressure readings. It includes a reproducible analysis pipeline, a prediction script, a Shiny dashboard (file-based demonstration), an academic report and presentation outline.

> **Key finding (from the uploaded dataset):** the sensor columns carry no learnable signal for `Fault Label`. The models do not beat a "always predict class 0" baseline. See section 9. The pipeline itself is verified; a better dataset is needed for a useful detector.

## 2. Problem statement
Can machine-learning models in R classify the fault label of a machine record from its sensor readings?
No claims are made about remaining useful life, real-time monitoring or forecasting future failures: the data is one day of 1-minute readings.

## 3. Dataset source and attribution
File: `data/industrial_fault_detection_data_1000.csv` (uploaded by the project author). **Original source/licence: [add here].** The meaning of the label values is undocumented.

## 4. Features and target (actual dataset)
1000 rows x 7 columns, no missing values, no duplicate rows.

| Column | Role |
|---|---|
| `Timestamp` | 1-minute readings, 2023-03-10 00:00 to 16:39; used for ordering only |
| `Vibration (mm/s)`, `Temperature (°C)`, `Pressure (bar)` | **Predictors** |
| `RMS Vibration`, `Mean Temp` | Constant in every row (whole-dataset summaries) - **dropped** (no information, leakage-prone) |
| `Fault Label` | **Target**: 0 (609 rows), 1 (303), 2 (88) - multiclass |

## 5. Technologies
R, RStudio, tidyverse/ggplot2, caret, randomForest, kernlab (SVM via caret), nnet (multinomial logistic regression), pROC, Shiny, DT. See `requirements_or_package_notes.txt` for the documented deviations from the brief (kernlab instead of e1071; no vip/PRROC).

## 6. Installation
1. Install R (4.x) and RStudio.
2. Open the project folder in RStudio (set it as the working directory).
3. Install packages:
```r
install.packages(c("tidyverse", "caret", "randomForest", "kernlab", "nnet",
                   "pROC", "shiny", "DT", "reshape2", "scales", "rmarkdown", "knitr"))
```

## 7. How to run
From the project folder:
```r
source("run_all.R")                  # runs scripts 01-06 in order (about 1 minute)
shiny::runApp()                      # starts the dashboard
```
Terminal equivalents:
```bash
Rscript run_all.R
Rscript R/07_predict_new_data.R data/sample_new_records.csv outputs/results/new_predictions.csv
Rscript R/08_pipeline_sanity_check.R   # optional code test (not a result)
Rscript report/render_report.R         # builds report/Project_Report.html and .docx
Rscript -e 'shiny::runApp()'
```

| Script | Purpose |
|---|---|
| `R/utils.R`, `R/predict_functions.R` | shared settings, metrics, prediction/validation functions |
| `R/01_data_exploration.R` | dimensions, structure, profile, target distribution |
| `R/02_data_cleaning.R` | checks, cleaning log, `data/cleaned_fault_data.csv` |
| `R/03_visualisation.R` | 8 EDA plots |
| `R/04_feature_preparation_split.R` | chronological split (seed 123), scaling fitted on training data |
| `R/05_model_training.R` | 3 models, rolling-origin CV, saves models |
| `R/06_model_evaluation.R` | test metrics, ROC, confusion matrices, feature importance |
| `R/07_predict_new_data.R` | predictions for a new CSV |
| `R/08_pipeline_sanity_check.R` | proves the code can learn when a real pattern exists |

Prediction input columns: `Vibration (mm/s)`, `Temperature (°C)`, `Pressure (bar)`. Missing or non-numeric values, implausible values and missing columns are reported in a `status` column or an error message instead of crashing.

## 8. Method summary
* **Split:** chronological 80/20 (800 train / 200 test) because readings are time-dependent; seed 123.
* **Preprocessing:** centre/scale fitted on training data only.
* **Models:** multinomial logistic regression (baseline), Random Forest, SVM (RBF).
* **Validation:** rolling-origin cross-validation on the training set; model chosen by CV Kappa; test set used once.

## 9. Model evaluation results (held-out test set, 200 records; from `outputs/results/`)
| Model | Accuracy | Kappa | Macro F1 | Macro ROC-AUC | Macro PR-AUC |
|---|---|---|---|---|---|
| Logistic regression | 0.630 | 0.000 | 0.258 | 0.476 | 0.329 |
| **Random Forest** (selected by CV Kappa) | 0.555 | 0.019 | 0.323 | 0.488 | 0.323 |
| SVM (RBF) | 0.630 | 0.000 | 0.258 | 0.498 | 0.352 |
| Majority-class baseline (always class 0) | 0.630 | 0.000 | - | - | - |

Logistic regression and the SVM predict class 0 for every record, so they miss all 74 records of class 1/2. The Random Forest catches some but is below baseline accuracy. Under the *assumption* that class 0 = normal, it misses 57 of 74 fault records (recall 0.23). Sensors show no significant difference between classes (Kruskal-Wallis p = 0.59, 0.84, 0.51). Feature importance (Random Forest): vibration > temperature > pressure by permutation importance, but with near-equal Gini scores; this is not evidence of causation.

**Pipeline check:** with a rule-based label built from the same sensor values, the same code reached 100% test accuracy (baseline 80.5%), so the lack of performance on the real label comes from the data, not from a coding error. That test is *not* a result about your dataset.

Results are reproducible with seed 123 (identical outputs on repeated runs).

## 10. Screenshots
Add your own after running the app: `screenshots/dashboard_overview.png`, `screenshots/prediction_tab.png`. Static plots are in `outputs/plots/`.

## 11. Limitations and future work
* One day / 1000 readings; undocumented label meaning; data may be simulated.
* No time-window or frequency-domain features; imbalance reported, not corrected.
* File-based demo only - no live sensor connection.
* Future: documented dataset with real fault labels, rolling-window features, gradient boosting, probability calibration, live data feed.
