# Greenfield FDI Dashboard (Demo Using Synthetic Data)

This repository contains a Shiny application that replicates the structure and functionality of a dashboard originally developed for a European central bank.  
The dashboard provides an interactive and comprehensive overview of **Greenfield FDI inflows into the United States**, enabling quantitative researchers to explore trends across countries, sectors, and time.

---

### ⚠️ Important: Synthetic Data

All data included in this repository is **AI-generated synthetic data**, created solely from a written prompt with **no access** to the original confidential dataset.  
The original data cannot be shared due to confidentiality restrictions.

The purpose of this public version is to demonstrate:

- the **architecture of the Shiny dashboard**,  
- the workflow for **data exploration**,  
- and how the tool can be **easily adapted** to other datasets.

---

## 📊 Data Generation Details

The synthetic dataset consists of **5,000 observations**, designed to mimic realistic FDI patterns:

- The **top 9 countries** account for approximately **70%** of all entries.  
- Each country shows a **persistent preference** for certain sectors (randomly assigned to introduce structured variation).  
- All other variables are randomly generated while maintaining plausible distributions.

This ensures that:

- No confidential or proprietary information is exposed.  
- The dashboard behaves similarly to the original version.

---

## 🛠️ Application Structure

For convenience, all application components are contained within a **single R script**, including:

- Shiny UI  
- Server logic  
- Data generation  
- Custom HTML  
- CSS styling  
- JavaScript functions  

While modularization is usually recommended for larger projects, this structure makes the demo easy to transfer, review, and customize.

---

## 🌐 Live Demo

A deployed version of the dashboard is available at:

👉 **https://l2ea7x-dario-cavarretta.shinyapps.io/dashboard/**  

---


## ▶️ How to Run Locally

You can run the dashboard locally on any machine with R + RStudio installed.

### 1. Clone or download the repository

```sh
git clone https://github.com/dariocavarretta/Bank-dashboard-US-Greenfield-FDI-fake-data.git
```

```r
### 2. Install all necessary packages 
install.packages(c(
  "readr", "dplyr", "tidyr", "ggplot2", "lubridate", "highcharter",
  "htmltools", "RColorBrewer", "jsonlite", "scales", "shiny",
  "shinyjs", "purrr", "shinycssloaders", "shinyWidgets"
))
```

### 3. Ensure the following files are present

app.R
fake_us_fdi_inflow.csv
www/
   ├── background.jpg
   └── company_logo.jpg

(When first run, the app automatically downloads the world map GeoJSON and saves it locally (worldgeojson.rds))

### 4. Run the app

```r
shiny::runApp()
```

---


## 🚧 Future Updates

A broader dashboard covering **FDI flows between all countries** is currently in development.  
A version using synthetic data will be published on GitHub **by the end of 2025**.

The updated version will include:

- A more modern and responsive layout  
- Greater customization options  
- Additional visualization capabilities  
- Improved architecture and modular design  

---

