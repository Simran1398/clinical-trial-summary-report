## app.R — Standalone Clinical Trial Dashboard

# Load necessary libraries
library(shiny)
library(DT)
library(plotly)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

# Simulated data (replace with real data if available)
set.seed(123)
biomarker_data <- expand.grid(
  treatment_arm = c("Placebo", "Dapagliflozin 5mg", "Dapagliflazin 10mg"),
  subject_id = 1:50
) %>%
  mutate(
    responder_status = sample(c("Responder", "Non-Responder"), n(), replace = TRUE),
    biomarker_level = round(rnorm(n(), mean = ifelse(responder_status == "Responder", 5.5, 4.2), sd = 1), 2)
  )

baseline_clean <- data.frame(
  treatment_arm = sample(c("Placebo","Dapagliflozin 5mg", "Dapagliflazin 10mg"), 150, replace = TRUE),
  age_mean = rnorm(150, mean = 60, sd = 10),
  bmi_mean = rnorm(150, mean = 27, sd = 4)
)

ae_clean <- data.frame(
  treatment_arm = sample(c("Placebo", "Dapagliflozin 5mg", "Dapagliflazin 10mg"), 100, replace = TRUE),
  ae_category = sample(c("Headache", "Nausea", "Fatigue"), 100, replace = TRUE),
  n_events = sample(1:10, 100, replace = TRUE),
  ae_severity = sample(c("Mild", "Moderate", "Severe"), 100, replace = TRUE)
)

surv_data <- data.frame(
  time = rexp(150, 0.1),
  status = sample(0:1, 150, replace = TRUE),
  treatment = baseline_clean$treatment_arm
)

# UI
ui <- fluidPage(
  titlePanel("Clinical Trial Summary Dashboard - Simranpreet Singh"),
  sidebarLayout(
    sidebarPanel(
      selectInput("treatment", "Select Treatment Arm:",
                  choices = unique(baseline_clean$treatment_arm)),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Baseline Table", DTOutput("baselineTable")),
        tabPanel("Adverse Events", plotlyOutput("aeBarPlot")),
        tabPanel("Biomarker Violin Plot", plotlyOutput("biomarkerPlot")),
        tabPanel("Survival Curve", plotOutput("kmPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$baselineTable <- renderDT({
    df <- baseline_clean %>% filter(treatment_arm == input$treatment)
    if (nrow(df) == 0) return(datatable(data.frame(Note = "No data available")))
    datatable(df)
  })
  
  output$aeBarPlot <- renderPlotly({
    df <- ae_clean %>% filter(treatment_arm == input$treatment)
    p <- ggplot(df, aes(x = ae_category, y = n_events, fill = ae_severity)) +
      geom_col(position = "dodge") +
      labs(title = paste("AEs in", input$treatment), x = "AE Category", y = "Event Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$biomarkerPlot <- renderPlotly({
    df <- biomarker_data %>% filter(treatment_arm == input$treatment)
    p <- ggplot(df, aes(x = responder_status, y = biomarker_level, fill = responder_status)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white") +
      labs(title = paste("Biomarker Levels -", input$treatment), x = "Response", y = "Level") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$kmPlot <- renderPlot({
    fit <- survfit(Surv(time, status) ~ treatment, data = surv_data)
    ggsurvplot(fit,
               data = surv_data,
               risk.table = TRUE,
               pval = TRUE,
               conf.int = TRUE,
               legend.title = "Treatment Arm",
               legend.labs = unique(surv_data$treatment),
               palette = "Dark2")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("clinical_trial_filtered_", input$treatment, ".csv")
    },
    content = function(file) {
      df <- baseline_clean %>% filter(treatment_arm == input$treatment)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
