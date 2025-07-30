# PHASE 1: Data Cleaning and Structuring
---
  
  # Install required packages
  # `tidyverse`: A collection of R packages designed for data science (dplyr, ggplot2, etc.).
  # `janitor`: Provides functions for cleaning data (e.g., `clean_names()`).
  # `here`: Helps in constructing file paths in a robust way, making scripts portable.
  # `gtsummary`: For creating beautiful summary tables (e.g., descriptive statistics).
  # `ggpubr`: Simplifies ggplot2 for creating publication-ready plots.
  # `patchwork`: For easily combining multiple ggplot2 plots.
  # `gt`: For creating highly customizable and beautiful tables.
  # `flextable`: For creating searchable, sortable, and exportable tables.
  # `shiny`: For building interactive web applications (dashboards).
  # `DT`: Provides an R interface to the DataTables JavaScript library for interactive HTML tables.
  # `plotly`: For creating interactive web-based graphs.
  # `cardx`: Likely for creating "cards" or summary statistics.
  # `rmarkdown`: For creating dynamic reports from R.
  # `officer`: For working with Microsoft Word and PowerPoint documents.
  # `webshot`: For taking web screenshots (often used with `gt` for saving tables as images).
  # `tinytex`: A lightweight LaTeX distribution for R Markdown PDF output.
  # `survival` and `survminer`: For survival analysis (Kaplan-Meier plots, etc.).
  # `remotes`: For installing R packages from GitHub or other remote repositories.
  # `admiral`: An R package for creating ADaM (Analysis Data Model) datasets.
  # `Tplyr`: For creating clinical trial tables with ease.
  # `bslib`: For building custom Shiny themes.
  
  install.packages(c("tidyverse", "janitor", "here", "gtsummary", "ggpubr", "patchwork",
                     "gt", "flextable", "shiny", "DT", "plotly", "cardx",
                     "rmarkdown", "officer", "webshot", "tinytex","survival","survminer","remotes","admiral","Tplyr"))

# Load libraries
library(tidyverse)
library(janitor)
library(here)
library(gtsummary)
library(ggpubr)
library(patchwork)
library(gt)
library(flextable)
library(shiny)
library(DT)
library(plotly)
library(cardx)
library(rmarkdown)
library(officer)
library(webshot)
library(tinytex)
library(survival)
library(survminer)
library(admiral)
library(Tplyr)
library(bslib)

# Load and clean datasets
# `read_csv()` from `readr` (part of tidyverse) reads CSV files.
# `here()` ensures that file paths are correctly constructed regardless of where the script is run from.
# `show_col_types = FALSE` suppresses messages about column type inference.
# `clean_names()` from `janitor` standardizes column names (e.g., converts to snake_case).
baseline <- read_csv("baseline.csv"), show_col_types = FALSE) %>% clean_names()
ae_summary <- read_csv("ae_summary.csv"), show_col_types = FALSE) %>% clean_names()

# Clean baseline data
# This section performs specific cleaning and transformation steps on the `baseline` data.
baseline_clean <- baseline %>%
  mutate(
    # Converts 'treatment_arm' to a factor for categorical analysis.
    treatment_arm = factor(treatment_arm),
    # Converts 'sex_male_percent' and 'sex_female_percent' to numeric.
    sex_male_percent = as.numeric(sex_male_percent),
    sex_female_percent = as.numeric(sex_female_percent),
    # Rounds 'bmi_mean' to one decimal place.
    bmi_mean = round(bmi_mean, 1)
  ) %>%
  # Selects only the relevant columns for the cleaned baseline data.
  select(treatment_arm, n, age_mean, age_sd, sex_male_percent, sex_female_percent, bmi_mean)

# Clean AE data (Adverse Events)
# This section cleans and transforms the `ae_summary` data.
ae_clean <- ae_summary %>%
  mutate(
    # Converts 'treatment_arm' to a factor.
    treatment_arm = factor(treatment_arm),
    # Capitalizes the first letter of each word in 'ae_category' for consistent display.
    ae_category = str_to_title(ae_category),
    # Converts 'ae_severity' to a factor with specific order (Mild, Moderate, Severe).
    ae_severity = factor(ae_severity, levels = c("Mild", "Moderate", "Severe")),
    # Rounds 'percent_percent' (renamed to 'percent_events') to one decimal place.
    percent_events = round(percent_percent, 1)
  ) %>%
  # Selects relevant columns for the cleaned AE data.
  select(treatment_arm, ae_category, ae_severity, n_events, percent_events)

# Save cleaned files
# This section creates an 'outputs/cleaned' directory if it doesn't exist and saves the cleaned data.
if (!dir.exists(here("outputs", "cleaned"))) {
  dir.create(here("outputs", "cleaned"), recursive = TRUE) # `recursive = TRUE` creates parent directories if they don't exist.
}

write_csv(baseline_clean, here("outputs", "cleaned", "baseline_clean.csv")) # Saves cleaned baseline data.
write_csv(ae_clean, here("outputs", "cleaned", "ae_summary_clean.csv"))     # Saves cleaned AE data.

---
  # PHASE 2: Statistical Summary Tables
  ---
  
  # Load cleaned data
  # Loads the previously cleaned data files. This is useful if running Phase 2 independently.
  baseline_clean <- read_csv(here("outputs", "cleaned", "baseline_clean.csv"), show_col_types = FALSE)
ae_clean <- read_csv(here("outputs", "cleaned", "ae_summary_clean.csv"), show_col_types = FALSE)

# Summary statistics with gtsummary
# Creates a descriptive summary table of the baseline data, grouped by `treatment_arm`.
baseline_clean %>%
  tbl_summary(by = treatment_arm,
              # Defines how continuous variables (mean (SD)) and categorical variables (n (%)) should be summarized.
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p() %>% # Adds p-values to test for differences between treatment arms.
  modify_header(label ~ "**Variable**") %>% # Renames the 'label' column header.
  bold_labels() # Bolds the row labels (variable names).

# Simulated survival data 
# Creates dummy survival data for demonstration purposes 
set.seed(100) # Sets the seed for reproducibility of random data generation.
survival_data <- tibble(
  treatment_arm = rep(c("A", "B"), each = 50), # 50 subjects for each treatment arm.
  time = round(rexp(100, rate = 0.1), 1),     # Simulated event times (exponential distribution).
  status = sample(0:1, 100, replace = TRUE)   # Simulated event status (0 = censored, 1 = event).
)

# Kaplan-Meier plot
# Fits a Kaplan-Meier survival curve model.
km_fit <- survfit(Surv(time, status) ~ treatment_arm, data = survival_data)

# Generates and displays a Kaplan-Meier survival plot.
km_plot <- ggsurvplot(
  km_fit,
  data = survival_data,
  pval = TRUE,       # Displays log-rank test p-value.
  conf.int = TRUE,   # Adds confidence intervals to the curves.
  risk.table = TRUE, # Adds a risk table below the plot.
  title = "Kaplan-Meier Survival by Treatment Arm", # Plot title.
  xlab = "Time (days)",      # X-axis label.
  ylab = "Survival Probability", # Y-axis label.
  legend.title = "Treatment" # Legend title.
)
km_plot # Prints the plot object.

# Chi-square test
# Prepares data for a chi-square test to examine the association between treatment arm and AE category.
chisq_table <- ae_clean %>%
  count(treatment_arm, ae_category) %>% # Counts occurrences of each AE category per treatment arm.
  pivot_wider(names_from = ae_category, values_from = n, values_fill = 0) # Reshapes data for chi-square.

chisq.test(chisq_table[,-1]) # Performs chi-square test (excluding the treatment_arm column).
chisq_table # Displays the contingency table.
fisher.test(chisq_table[,-1]) # Performs Fisher's exact test, suitable for small cell counts.

# AE plot (Adverse Events)
# Creates a stacked or dodged bar plot of adverse events by category and severity, faceted by treatment arm.
ae_plot <- ggplot(ae_clean, aes(x = ae_category, y = n_events, fill = ae_severity)) +
  geom_col(position = "dodge") + # Creates bar columns; `position = "dodge"` separates bars by fill.
  facet_wrap(~ treatment_arm) +  # Creates separate panels for each treatment arm.
  labs(title = "Adverse Events by Treatment Arm", x = "AE Category", y = "Number of Events") + # Adds labels.
  theme_minimal() # Uses a minimalist theme for the plot.
ae_plot # Prints the plot.

# Patchwork layout for age and BMI
# Creates two separate bar plots for mean age and mean BMI by treatment arm.
age_plot <- ggplot(baseline_clean, aes(x = treatment_arm, y = age_mean)) +
  geom_col(fill = "skyblue") + # Blue bar columns.
  labs(title = "Mean Age by Treatment Arm", y = "Mean Age")

bmi_plot <- ggplot(baseline_clean, aes(x = treatment_arm, y = bmi_mean)) +
  geom_col(fill = "orange") + # Orange bar columns.
  labs(title = "Mean BMI by Treatment Arm", y = "Mean BMI")

age_plot + bmi_plot # Uses `patchwork` to combine the two plots side-by-side.

# Flextable report
# Creates a flexible, formatted table summarizing baseline characteristics.
baseline_clean %>%
  group_by(treatment_arm) %>% # Groups data by treatment arm.
  summarise(
    N = sum(n), # Calculates total sample size per arm.
    Age_Mean = round(mean(age_mean, na.rm = TRUE), 1), # Calculates mean age, handles NAs.
    BMI_Mean = round(mean(bmi_mean, na.rm = TRUE), 1), # Calculates mean BMI, handles NAs.
    Male_Percent = round(mean(sex_male_percent, na.rm = TRUE), 1), # Calculates mean male percentage.
    Female_Percent = round(mean(sex_female_percent, na.rm = TRUE), 1) # Calculates mean female percentage.
  ) %>%
  flextable() %>% # Converts the summarized data frame to a flextable object.
  set_header_labels( # Sets user-friendly column headers.
    treatment_arm = "Treatment Arm",
    N = "Sample Size",
    Age_Mean = "Mean Age",
    BMI_Mean = "Mean BMI",
    Male_Percent = "% Male",
    Female_Percent = "% Female"
  ) %>%
  autofit() # Adjusts column widths to fit content.

# Tplyr table
# Uses Tplyr to create a clinical trial-style summary table for age and BMI.
tplyr_table <- tplyr_table(baseline_clean, treatment_arm) %>% # Initializes a Tplyr table, grouped by treatment_arm.
  add_layer(
    group_desc(age_mean, by = "Age (Mean ± SD)") # Adds a descriptive layer for age_mean.
  ) %>%
  add_layer(
    group_desc(bmi_mean, by = "BMI (Mean ± SD)") # Adds a descriptive layer for bmi_mean.
  ) %>%
  build() # Builds the Tplyr table.

print(tplyr_table) # Prints the Tplyr table.

---
  # PHASE 3: Interactive Shiny Dashboard
  ---
  
  # Simulated data for Shiny dashboard
  # Generates new simulated data specifically for the Shiny app.
  # This is important because the previous data might have been filtered or summarized.
  set.seed(123) # Ensures reproducibility of simulated data for the dashboard.
biomarker_data <- expand.grid(
  treatment_arm = c("Placebo", "Dapagliflozin 5mg", "Dapagliflazin 10mg"), # Defines treatment arms.
  subject_id = 1:50 # Defines subject IDs.
) %>%
  mutate(
    responder_status = sample(c("Responder", "Non-Responder"), n(), replace = TRUE), # Assigns responder status.
    biomarker_level = round(rnorm(n(), mean = ifelse(responder_status == "Responder", 5.5, 4.2), sd = 1), 2) # Simulates biomarker levels.
  )

# Simulated baseline data (for Shiny)
baseline_clean <- data.frame(
  treatment_arm = sample(c("Placebo","Dapagliflozin 5mg", "Dapagliflazin 10mg"), 150, replace = TRUE),
  age_mean = rnorm(150, mean = 60, sd = 10),
  bmi_mean = rnorm(150, mean = 27, sd = 4)
)

# Simulated AE data (for Shiny)
ae_clean <- data.frame(
  treatment_arm = sample(c("Placebo", "Dapagliflozin 5mg", "Dapagliflazin 10mg"), 100, replace = TRUE),
  ae_category = sample(c("Headache", "Nausea", "Fatigue"), 100, replace = TRUE),
  n_events = sample(1:10, 100, replace = TRUE),
  ae_severity = sample(c("Mild", "Moderate", "Severe"), 100, replace = TRUE)
)

# Simulated survival data (for Shiny)
surv_data <- data.frame(
  time = rexp(150, 0.1), # Simulated event times.
  status = sample(0:1, 150, replace = TRUE), # Simulated event status.
  treatment = baseline_clean$treatment_arm # Assigns treatment arm from simulated baseline.
)

# UI (User Interface)
# Defines the layout and appearance of the Shiny dashboard.
ui <- fluidPage( # Creates a fluid page layout that adjusts to browser width.
  titlePanel("Clinical Trial Summary Dashboard - Simranpreet Singh"), # Main title of the dashboard.
  sidebarLayout( # Divides the page into a sidebar and a main panel.
    sidebarPanel( # Content for the sidebar.
      selectInput("treatment", "Select Treatment Arm:", # Dropdown menu for selecting treatment arm.
                  choices = unique(baseline_clean$treatment_arm)), # Populates choices from unique treatment arms.
      downloadButton("downloadData", "Download Filtered Data") # Button to download filtered data.
    ),
    mainPanel( # Content for the main panel.
      tabsetPanel( # Creates a tabbed interface.
        tabPanel("Baseline Table", DTOutput("baselineTable")), # Tab for interactive baseline table.
        tabPanel("Adverse Events", plotlyOutput("aeBarPlot")), # Tab for interactive AE bar plot.
        tabPanel("Biomarker Violin Plot", plotlyOutput("biomarkerPlot")), # Tab for interactive biomarker violin plot.
        tabPanel("Survival Curve", plotOutput("kmPlot")), # Tab for static Kaplan-Meier plot.
      )
    )
  )
)

# SERVER
# Defines the logic and reactivity of the Shiny dashboard.
server <- function(input, output) {
  
  # Baseline Table output
  # Renders an interactive DataTable for the baseline data, filtered by selected treatment arm.
  output$baselineTable <- renderDT({
    df <- baseline_clean %>% filter(treatment_arm == input$treatment) # Filters data based on user selection.
    if (nrow(df) == 0) return(DT::datatable(data.frame(Note = "No data available"))) # Handles no data case.
    datatable(df) # Renders the data as an interactive table.
  })
  
  # AE Plot output
  # Renders an interactive Plotly bar chart for adverse events.
  output$aeBarPlot <- renderPlotly({
    df <- ae_clean %>% filter(treatment_arm == input$treatment) # Filters AE data.
    p <- ggplot(df, aes(x = ae_category, y = n_events, fill = ae_severity)) +
      geom_col(position = "dodge") +
      labs(title = paste("AEs in", input$treatment), x = "AE", y = "Events") +
      theme_minimal()
    ggplotly(p) # Converts ggplot to an interactive Plotly graph.
  })
  
  # Biomarker Violin Plot output
  # Renders an interactive Plotly violin plot for biomarker levels.
  output$biomarkerPlot <- renderPlotly({
    df <- biomarker_data %>% filter(treatment_arm == input$treatment) # Filters biomarker data.
    p <- ggplot(df, aes(x = responder_status, y = biomarker_level, fill = responder_status)) +
      geom_violin(trim = FALSE) + # Creates violin plot.
      geom_boxplot(width = 0.1, fill = "white") + # Adds boxplot inside violin.
      labs(title = paste("Biomarker Levels -", input$treatment),
           x = "Response", y = "Biomarker Level") +
      theme_minimal()
    ggplotly(p) # Converts ggplot to an interactive Plotly graph.
  })
  
  # KM Plot output
  # Renders a Kaplan-Meier survival plot. Note: This plot is not filtered by the `input$treatment`
  # because `surv_data` is defined globally and `ggsurvplot` always plots all treatments in `surv_data$treatment`.
  # To make it react to the `treatment` input, you'd need to filter `surv_data` first.
  output$kmPlot <- renderPlot({
    fit <- survfit(Surv(time, status) ~ treatment, data = surv_data) # Fits KM model on all treatments in surv_data.
    ggsurvplot(fit,
               data = surv_data,
               risk.table = TRUE,
               pval = TRUE,
               conf.int = TRUE,
               legend.title = "Treatment Arm",
               legend.labs = unique(surv_data$treatment),
               palette = "Dark2")
  })
  
  
  # Download Handler
  # Defines the logic for the download button, allowing users to download filtered data.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("clinical_trial_filtered_", input$treatment, ".csv") # Sets the filename based on selected treatment.
    },
    content = function(file) {
      df <- baseline_clean %>% filter(treatment_arm == input$treatment) # Filters baseline data for download.
      write_csv(df, file) # Writes the filtered data to a CSV file.
    }
  )
}

shinyApp(ui, server) # Runs the Shiny application.