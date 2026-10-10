# =====================================================================
# run_all.R  -  runs the whole analysis in the correct order
# From RStudio: open this file and click Source  (working directory = project folder)
# From a terminal: Rscript run_all.R
# =====================================================================
if (!file.exists("R/utils.R")) stop("Set the working directory to the project folder first.")
steps <- c("R/01_data_exploration.R", "R/02_data_cleaning.R", "R/03_visualisation.R",
           "R/04_feature_preparation_split.R", "R/05_model_training.R", "R/06_model_evaluation.R")
for (s in steps) { cat("\n########## Running", s, "##########\n"); source(s, echo = FALSE) }
cat("\nAll steps finished. Next: shiny::runApp()   (or Rscript R/07_predict_new_data.R)\n")
