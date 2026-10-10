# =====================================================================
# app.R - Industrial IoT Fault Detection dashboard (file-based demo)
# Run from the project folder:  shiny::runApp()
# Needs the files created by run_all.R (results, models, cleaned data).
# =====================================================================
source("R/utils.R")
check_packages(c("shiny", "DT", "tidyverse", "caret", "randomForest", "kernlab", "nnet", "readr"))
suppressMessages({ library(shiny); library(DT); library(tidyverse) })
source("R/predict_functions.R")

# ---- Data loaded once at start-up -------------------------------------
read_result <- function(file) {
  path <- file.path(PATHS$results, file)
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else NULL
}
clean  <- if (file.exists(PATHS$clean_csv)) readr::read_csv(PATHS$clean_csv, show_col_types = FALSE) else NULL
bundle <- tryCatch(load_model_bundle(), error = function(e) NULL)
sensor_names <- c(vibration_mm_s = "Vibration (mm/s)", temperature_c = "Temperature (\u00b0C)", pressure_bar = "Pressure (bar)")

# Colour palette (navy / teal / light blue / grey)
navy <- "#1d3a47"; teal <- "#2f6179"; sky <- "#5fa0bd"; grey <- "#8b9199"
class_colors <- c(class_0 = navy, class_1 = sky, class_2 = grey)
sensor_colors <- c(navy, sky, grey)

# ---- Look and feel (CSS only) ---------------------------------------------
app_css <- "
body { background:#c9dde8; font-family:'Segoe UI',Helvetica,Arial,sans-serif; color:#1d3a47; }
.container-fluid { max-width:1250px; margin:auto; padding:20px 25px 40px; }
.title { text-align:center; font-size:36px; font-weight:800; color:#111; margin:10px 0 4px; }
.subtitle { text-align:center; color:#3d5766; margin-bottom:22px; }
.card { background:#fff; border-radius:16px; padding:16px 20px; margin-bottom:18px; box-shadow:0 2px 6px rgba(29,58,71,.12); }
.card h4 { margin:0 0 10px; font-size:16px; font-weight:700; }
.kpi { background:#fff; border-radius:16px; padding:14px 20px; margin-bottom:18px; box-shadow:0 2px 6px rgba(29,58,71,.12); }
.kpi .label { font-size:14px; font-weight:700; }
.kpi .value { font-size:28px; font-weight:800; margin:2px 0; }
.kpi .delta { font-size:12px; color:#555; }
.up { color:#1e8e5a; font-weight:700; } .down { color:#d33; font-weight:700; }
.note { background:#fff; border-left:6px solid #5fa0bd; border-radius:10px; padding:10px 16px; margin-bottom:18px; box-shadow:0 2px 6px rgba(29,58,71,.12); }
.nav-tabs { border:none; text-align:center; margin-bottom:18px; }
.nav-tabs > li { float:none; display:inline-block; }
.nav-tabs > li > a { border:none !important; border-radius:20px; background:#e6f0f5; color:#1d3a47; font-weight:600; margin:0 4px; }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover { background:#1d3a47 !important; color:#fff !important; }
.btn-primary { background:#1d3a47; border-color:#1d3a47; border-radius:20px; }
.btn-default { border-radius:20px; }
"

# ---- Small helpers --------------------------------------------------------------
kpi <- function(label, value, delta = NULL) {
  column(3, div(class = "kpi", div(class = "label", label), div(class = "value", value), div(class = "delta", delta)))
}
card <- function(title, ...) div(class = "card", h4(title), ...)
app_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(), legend.position = "bottom")
no_files <- div(class = "note", "Result files not found. Run ", code("source('run_all.R')"), " first.")

# ---- User interface -----------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  div(class = "title", "Industrial IoT Fault Detection Dashboard"),
  div(class = "subtitle", "Sensor-based fault classification and predictive maintenance using R (file-based demonstration)"),
  tabsetPanel(
    tabPanel("Dashboard", br(),
             uiOutput("kpis"),
             fluidRow(column(4, card("Fault-class share", plotOutput("class_plot", height = 280))),
                      column(8, card("Sensor trends over time (standardised, 30-minute average)", plotOutput("trend_plot", height = 280)))),
             fluidRow(column(6, card("Feature importance (Random Forest)", plotOutput("imp_plot", height = 260),
                                     p(em("Shows what the model used, not what causes a fault.")))),
                      column(6, card("Average sensor value by class", plotOutput("mean_plot", height = 260))))),
    
    tabPanel("Sensors", br(),
             card("Explore a sensor",
                  fluidRow(column(4, selectInput("sensor", "Sensor", setNames(names(sensor_names), sensor_names))),
                           column(8, radioButtons("ptype", "View", inline = TRUE,
                                                  c("Boxplot by class" = "box", "Density by class" = "dens", "Histogram" = "hist", "Over time" = "time")))),
                  plotOutput("sensor_plot", height = 380))),
    
    tabPanel("Model performance", br(),
             uiOutput("perf_note"),
             card("Test-set comparison", DTOutput("compare_tbl")),
             card("Confusion matrix",
                  selectInput("cm_model", "Model", c("logistic_regression", "random_forest", "svm_radial")),
                  plotOutput("cm_plot", height = 300))),
    
    tabPanel("Data and cleaning", br(),
             card("Dataset profile", DTOutput("profile_tbl")),
             card("Data-cleaning log", DTOutput("clean_log"))),
    
    tabPanel("Predict new records", br(),
             card("Upload sensor data",
                  p("CSV columns required: ", code(paste(RAW_SENSOR, collapse = ", ")), "."),
                  fluidRow(column(5, fileInput("upload", NULL, accept = ".csv", buttonLabel = "Browse...", placeholder = "Choose a CSV file")),
                           column(4, actionButton("use_sample", "Use sample file", class = "btn-primary")))),
             uiOutput("pred_msg"),
             card("Predictions", DTOutput("pred_tbl"), br(),
                  downloadButton("download", "Download predictions"),
                  p(em("Probabilities are the model's estimates and are not calibrated."))))
  )
)

# ---- Server ----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Summary cards
  output$kpis <- renderUI({
    cmp <- read_result("model_comparison.csv")
    if (is.null(clean) || is.null(cmp) || is.null(bundle)) return(no_files)
    chosen <- cmp[cmp$model == bundle$model_name, ]
    diff_pts <- 100 * (chosen$accuracy - chosen$majority_class_accuracy)
    delta <- tags$span(class = if (diff_pts >= 0) "up" else "down",
                       sprintf("%s %.1f pts", if (diff_pts >= 0) "\u25b2" else "\u25bc", abs(diff_pts)), " vs baseline")
    other_share <- 100 * mean(clean$fault_label != "class_0")
    fluidRow(kpi("Total records", format(nrow(clean), big.mark = ","), "1-minute sensor readings"),
             kpi("Selected model", gsub("_", " ", bundle$model_name), "chosen by cross-validation"),
             kpi("Test accuracy", sprintf("%.1f%%", 100 * chosen$accuracy), delta),
             kpi("Class 1 + 2 records", sprintf("%.1f%%", other_share), "of all records"))
  })
  
  # Dashboard charts
  output$class_plot <- renderPlot({
    req(clean)
    d <- count(clean, fault_label) %>% mutate(pct = n / sum(n))
    ggplot(d, aes(x = 2, y = pct, fill = fault_label)) +
      geom_col(width = 1, colour = "white") + coord_polar("y") + xlim(0.8, 2.5) +
      geom_text(aes(label = scales::percent(pct, accuracy = 1)), position = position_stack(vjust = 0.5), colour = "white", fontface = "bold") +
      scale_fill_manual(values = class_colors) + labs(fill = NULL) + theme_void(base_size = 12) + theme(legend.position = "right")
  })
  output$trend_plot <- renderPlot({
    req(clean)
    roll <- function(x, k = 30) as.numeric(stats::filter(x, rep(1 / k, k), sides = 2))
    clean %>% arrange(timestamp) %>%
      mutate(across(all_of(names(sensor_names)), ~ roll(as.numeric(scale(.x))))) %>%
      pivot_longer(all_of(names(sensor_names)), names_to = "sensor", values_to = "value") %>%
      mutate(sensor = sensor_names[sensor]) %>%
      ggplot(aes(timestamp, value, colour = sensor)) + geom_line(linewidth = 0.8, na.rm = TRUE) +
      scale_colour_manual(values = setNames(sensor_colors, sensor_names)) +
      labs(x = NULL, y = "Standardised value", colour = NULL) + app_theme
  })
  output$imp_plot <- renderPlot({
    d <- read_result("feature_importance_rf.csv"); req(d)
    ggplot(d, aes(reorder(feature_label, MeanDecreaseAccuracy), MeanDecreaseAccuracy)) +
      geom_col(fill = navy, width = 0.6) + coord_flip() +
      labs(x = NULL, y = "Mean decrease in accuracy") + app_theme
  })
  output$mean_plot <- renderPlot({
    req(clean)
    clean %>% group_by(fault_label) %>% summarise(across(all_of(names(sensor_names)), mean), .groups = "drop") %>%
      pivot_longer(-fault_label, names_to = "sensor", values_to = "mean") %>%
      mutate(sensor = sensor_names[sensor]) %>%
      ggplot(aes(fault_label, mean, fill = fault_label)) + geom_col(show.legend = FALSE, width = 0.6) +
      facet_wrap(~ sensor, scales = "free_y") + scale_fill_manual(values = class_colors) +
      labs(x = "Fault label", y = "Mean") + app_theme
  })
  
  # Sensors tab
  output$sensor_plot <- renderPlot({
    req(clean)
    s <- input$sensor; label <- sensor_names[[s]]
    d <- mutate(clean, value = .data[[s]])
    p <- switch(input$ptype,
                box  = ggplot(d, aes(fault_label, value, fill = fault_label)) + geom_boxplot(show.legend = FALSE) + labs(x = "Fault label"),
                dens = ggplot(d, aes(value, fill = fault_label)) + geom_density(alpha = 0.4) + labs(x = label, fill = "Class"),
                hist = ggplot(d, aes(value)) + geom_histogram(bins = 30, fill = sky, colour = "white") + labs(x = label),
                time = ggplot(d, aes(timestamp, value)) + geom_line(colour = teal, linewidth = 0.4) + labs(x = "Time"))
    p + scale_fill_manual(values = class_colors) + labs(y = label, title = label) + app_theme
  })
  
  # Model performance tab
  output$perf_note <- renderUI({
    if (is.null(read_result("model_comparison.csv"))) return(no_files)
    div(class = "note", strong("How to read this: "), "metrics come from the held-out, time-ordered test set. ",
        "A model that does not beat the majority-class baseline has not learned a useful rule.")
  })
  output$compare_tbl <- renderDT({
    d <- read_result("model_comparison.csv"); req(d)
    d <- d %>% select(model, accuracy, majority_class_accuracy, kappa, macro_precision, macro_recall, macro_f1, macro_roc_auc, macro_pr_auc) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$cm_plot <- renderPlot({
    d <- read_result(paste0("confusion_matrix_", input$cm_model, ".csv")); req(d)
    pivot_longer(d, -Predicted, names_to = "Actual", values_to = "Count") %>%
      ggplot(aes(Actual, Predicted, fill = Count)) + geom_tile(colour = "white") +
      geom_text(aes(label = Count), size = 6) +
      scale_fill_gradient(low = "#EAF2F8", high = sky, guide = "none") + app_theme
  })
  
  # Data tab
  output$profile_tbl <- renderDT({
    d <- read_result("dataset_profile.csv"); req(d)
    datatable(mutate(d, across(where(is.numeric), ~ round(.x, 3))), rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, pageLength = 10))
  })
  output$clean_log <- renderDT({
    d <- read_result("cleaning_log.csv"); req(d)
    datatable(d, rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Prediction tab
  new_data <- reactiveVal(NULL)
  read_upload <- function(path) {
    d <- tryCatch(as.data.frame(readr::read_csv(path, show_col_types = FALSE, name_repair = "minimal"), check.names = FALSE),
                  error = function(e) NULL)
    new_data(if (is.null(d)) data.frame() else d)   # an empty table triggers a clear message below
  }
  observeEvent(input$upload,     read_upload(input$upload$datapath))
  observeEvent(input$use_sample, read_upload(PATHS$sample_new))
  
  # Returns list(result = data frame or NULL, error = message or NULL)
  prediction <- reactive({
    d <- new_data(); req(d)
    if (is.null(bundle)) return(list(result = NULL, error = "Model file not found. Run run_all.R first."))
    tryCatch(list(result = predict_fault(d, bundle), error = NULL),
             error = function(e) list(result = NULL, error = conditionMessage(e)))
  })
  output$pred_msg <- renderUI({
    p <- prediction()
    if (!is.null(p$error)) return(div(class = "note", style = "border-left-color:#d33; white-space:pre-wrap;", p$error))
    ok <- sum(!is.na(p$result$predicted_class))
    div(class = "note", style = "border-left-color:#1e8e5a;",
        sprintf("%d of %d rows predicted. Rows that could not be predicted are explained in the 'status' column.", ok, nrow(p$result)))
  })
  output$pred_tbl <- renderDT({
    p <- prediction(); req(p$result)
    datatable(p$result, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download <- downloadHandler(
    filename = function() paste0("fault_predictions_", Sys.Date(), ".csv"),
    content  = function(file) { p <- prediction(); req(p$result); write.csv(p$result, file, row.names = FALSE) })
}

shinyApp(ui, server)