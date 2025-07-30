#  Clinical Trial Summary Report – Dapagliflozin (NCT01400884)

This project simulates a real-world clinical trial summary analysis using public data from a Phase III trial of **Dapagliflozin in Type 2 Diabetes (NCT01400884)**. The objective was to analyze and visualize key clinical metrics, adverse events, and survival trends using R to emulate workflows relevant to roles in clinical research, pharmacovigilance, and clinical data management.

---

## Project Objectives

- Explore, clean, and summarize clinical trial data
- Perform descriptive statistics on baseline and treatment groups
- Profile **Adverse Events (AEs)** and serious AEs
- Conduct **Kaplan-Meier survival analysis**
- Build **interactive visualizations** using `plotly` and `DT`
- Simulate end-to-end reporting for internal review or publication

---

## Tools & Packages Used

- `tidyverse`, `dplyr`, `janitor` – Data cleaning & wrangling  
- `ggplot2`, `plotly`, `ggpubr` – Data visualization  
- `survival`, `survminer` – Survival analysis (Kaplan-Meier curves)  
- `DT` – Interactive tables  
- `knitr`, `rmarkdown` – Reporting  

---
## Folder Structure
```
Clinical-Trial-Summary-Report/
├── data/           # Cleaned dataset (simulated from public sources)
├── scripts/        # R scripts for each stage of the analysis
├── outputs/        # Plots, summary tables, interactive HTML outputs
├── report/         
│   └── Clinical_Report.html   # Final rendered summary report
└── README.md       # Project README file
```
---

##  Key Analyses Performed

| Analysis Type               | Description |
|----------------------------|-------------|
| **Descriptive Statistics** | Age, weight, gender distribution, etc. |
| **Adverse Event Analysis** | AE frequency by severity and treatment group |
| **Survival Analysis**      | Kaplan-Meier curves to evaluate event-free survival |
| **Interactive Reporting**  | AE summaries and visualizations in dynamic tables and plots |

---

##  Relevant For

- Clinical Trial Assistant (CTA)
- Pharmacovigilance Officer
- Clinical Data Manager (CDM)
- Biostatistics / Medical Writer

---

## Report Preview

You can view the full HTML report [here](./report/Clinical_Report.html) *(open locally or render using RStudio)*

---

##  Contact

**Simranpreet Singh**  
simran1661998@gmail.com 
 

---
License
This project is for educational and portfolio use only.

> ⚠️ *Disclaimer: This project uses simulated or publicly available data and is for educational/portfolio purposes only. No real patient data is involved.*


