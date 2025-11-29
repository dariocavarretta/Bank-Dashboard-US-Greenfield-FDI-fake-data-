### AUTHOR: Dario Cavarretta

### this script creates a shiny app that contains a dashboard that summarizes
# in an exhaustive way the FDI inflow data to the US 

#### as data is confidential, this project includes AI - generated fake data
#### the purpose of this exercise is to show on github the architecture of my 
#### dashboard which can be easily used and customized to other types of data 

### data generation: 5000 rows, top 9 countries have 70% of all entries
### specific trends of countries to invest more persistently in some sectors (randomly assigned)
### everything else randomly assigned 

### all code including html, css, JS styling is contained in this script 
### this was done as it was more convenient to have it this way at the time

### the original project was made for a central bank to analyze recent trends 
### on US FDI inflows

#setwd("C:/Users/Dario/Desktop/dashboard")

#LIBRARIES

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(highcharter)
library(htmltools)
library(RColorBrewer)
library(jsonlite)
library(scales)
library(shiny)
library(shinyjs)
library(purrr)
library(shinycssloaders)
library(shinyWidgets)



# (update path with newer downloaded versions when available)

####load in fake simulated data 


us_fdi <- read.csv("fake_us_fdi_inflow.csv")

us_fdi <- us_fdi %>%
  rename(`Jobs created` = Jobs.created,
         `Capital investment` = Capital.investment,
         `Project status` = Project.status,
         `Sub-sector` = Sub.sector,
         `Source country` = Source.country)%>%
  mutate(`Capital investment` = `Capital investment`/1000000)


### download world map

json <- jsonlite::fromJSON("https://code.highcharts.com/mapdata/custom/world.geo.json", simplifyVector = FALSE)
saveRDS(json, "worldgeojson.rds")

worldgeojson <- readRDS("worldgeojson.rds")



################################################################################
###############################################################################
###############################################################################


##### Shiny App #####


# UI 


ui <- navbarPage(
  title = "",
  id = "main_nav",
  collapsible = TRUE,
  header = tags$head(
    tags$style(HTML("
      .navbar-brand {
        display: none !important;
      }
    "))
  ),
  
# css + html script for styling of the dashboard
  
tags$head(
  tags$style(HTML("
  /* --- General Body --- */
  body {
    background-color: #f9f9f9;
    font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  }

  /* --- Top Panel --- */
  .top-panel {
  background-color: white;
  color: #1f4e79;
  padding: 20px 30px;
  display: flex;
  justify-content: space-between;
  align-items: center;
  border-radius: 0; /* no rounding */
  margin: 0; /* attach to tabs */
  box-shadow: 0 4px 10px rgba(0,0,0,0.15);
  border: 2px solid transparent; 
  transition: border 0.3s ease; 
}

.top-panel:hover {
  border: 2px solid #1f4e79; 
}

.top-panel h1 {
  margin: 0;
  font-size: 36px;
  font-weight: bold;
}

.top-panel img {
  height: 100px;
  border-radius: 0; /* no rounding */
}
  /* --- Modern Tabs Single-Bar --- */
  .nav-pills {
    display: flex;
    justify-content: flex-start;
    background-color: #1f4e79;
    border-radius: 0; /* remove rounding */
    margin: 0;  /* attach to top panel above */
    flex-wrap: nowrap;
    overflow: visible; /* allow dropdowns to show */
    height: 40px;
    align-items: center; /* vertically center the text */
    box-shadow: none; /* shadow handled by top panel */
  }
  .nav-pills > li {
    flex: 1;
    position: relative; /* needed for dropdown menu positioning */
  }
  .nav-pills > li > a {
    border-radius: 0 !important; /* remove pill rounding */
    margin: 0 !important;
    padding: 0 20px;
    line-height: 40px; /* vertically center text */
    font-weight: 600;
    font-size: 16px;
    color: white;
    background-color: #1f4e79;
    border: none;
    text-align: center;
    white-space: nowrap;
    transition: all 0.2s ease;
  }
  .nav-pills > li.active > a {
    background-color: #2a6ebd;
    box-shadow: inset 0 -3px 0 #ffffff; /* active underline in white */
    color: white;
  }
  .nav-pills > li > a:hover {
    background-color: #2a6ebd;
    color: white;
  }

  /* --- Dropdown fix --- */
  .nav-pills > li.dropdown > ul.dropdown-menu {
    display: none;
    position: absolute;
    top: 100%;
    left: 0;
    background-color: #1f4e79;
    border-radius: 0 0 12px 12px;
    padding: 0;
    min-width: 160px;
    z-index: 1000;
  }
  .nav-pills > li.dropdown:hover > ul.dropdown-menu {
    display: block;
  }
  .nav-pills > li.dropdown > ul.dropdown-menu > li > a {
    color: #fff !important;
    padding: 10px 15px;
    font-weight: 500;
    background-color: #1f4e79;
  }
  .nav-pills > li.dropdown > ul.dropdown-menu > li > a:hover {
    background-color: #2a6ebd;
    color: #fff !important;
  }

 /* --- Banner Panel (Main Content) --- */
.content-panel {
  position: relative;
  background-image: url('background.jpg');
  background-size: cover;
  background-repeat: no-repeat;
  background-position: center;
  border-radius: 0; /* no rounding */
  color: white;
  padding: 70px;
  text-align: center;
  margin: 0; /* attach to tabs above */
  box-shadow: 0 4px 10px rgba(0,0,0,0.15); 
  transition: box-shadow 0.3s ease, border 0.3s ease; 
}

.content-panel:hover {
  box-shadow: 0 0 0 3px rgba(255, 255, 255, 0.8), 0 4px 10px rgba(0,0,0,0.15);
}
  .content-panel h2 {
    font-size: 46px;
    font-weight: bold;
    text-shadow: 1px 1px 5px rgba(0,0,0,0.6);
    letter-spacing: 2px;   
    line-height: 1.9;  
  }

  /* --- Card Styling --- */
  .card {
    background-color: white;
    border-radius: 10px;
    padding: 15px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.05); 
    margin-bottom: 20px;
  }

  /* --- Modern Panels --- */
  .glass-panel {
    background: white;
    backdrop-filter: blur(10px);
    border-radius: 15px;
    padding: 25px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.05); 
    color: #1f4e79;
    margin-bottom: 20px;
  }
  .floating-panel {
    background: white;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 10px 25px rgba(0,0,0,0.15);
    transition: transform 0.2s, box-shadow 0.2s;
    margin-bottom: 20px;
  }
  .floating-panel:hover {
    transform: translateY(-5px);
    box-shadow: 0 15px 30px rgba(0,0,0,0.25);
  }
  .gradient-panel {
    background: linear-gradient(135deg, #1f4e79, #2a6ebd);
    color: white;
    border-radius: 6px;
    padding: 6px;
    box-shadow: 0 1px 2px rgba(0,0,0,0.15);
    margin-bottom: 20px;
  }

  /* --- Text shadows for headings --- */
  .glass-panel h2, .gradient-panel h2, .floating-panel h3 {
    text-shadow: 1px 1px 4px rgba(0,0,0,0.2);
  }
  
  /* Container for all tab outputs */
.main-panel-container {
  background-color: #1f4e79;   
  border-radius: 0 0 12px 12px; 
  padding-top: 15px;           
  padding-left: 20px;
  padding-right: 20px;
  padding-bottom: 20px;
}

.tab-pane {
  padding-top: 10px;  
  padding-bottom: 20px;
}

.tab-pane .card, 
.tab-pane .floating-panel, 
.tab-pane .gradient-panel, 
.tab-pane .glass-panel {
  margin-top: 15px;
}
  "))
),

  
  
  # Title + logo panel

    fluidRow(
    column(
      width = 12,
      div(class = "top-panel",
          h1("US Greenfield FDI Dashboard"),
          img(src = "company_logo.jpg", alt = "Company logo")
      )
    )
  ),
  
  # Banner Panel
  fluidRow(
    column(width = 12,
           div(class = "content-panel",
               h2("Monitoring Greenfield FDI Inflows to the United States")
           )
    )
  ),
  
  # Individual Tabs 
div(class = "main-panel-container",
    
 tabsetPanel(
    id = "main_tab",
    type = "pills",
    
    
    # general analysis
    
    tabPanel("General Analysis", value = "general",
             
             fluidRow(
               column(width = 12,
                      div(class = "glass-panel",
                          h2("General Overview"),
                          p("This is a series of charts and graphs that offer a general overview of the data. Use the filters below to select the type of metric, project status and graph for general analysis insights.")
                      )
               )
             ),
             
             fluidRow(
               column(width = 3,
                      div(class = "floating-panel",
                          radioButtons("metric", "Select Metric:",
                                       choices = c("Capital Investment", "Number of Projects", "Jobs Created"),
                                       selected = "Capital Investment"),
                          radioButtons("project_type", "Select Project Type:",
                                       choices = c("Opened", "Announced", "Opened + Announced"),
                                       selected = "Opened + Announced"),
                          radioButtons("chart_type", "Select Chart Type:",
                                       choices = c("Pie Chart" = "pie", "Area Over Time" = "area", 
                                                   "Sankey Chart" = "sankey", "Cumulative FDI" = "cumulative",
                                                   "Monthly FDI" = "monthly", "World Trend" = "world_trend",
                                                   "FDI Heatmap" = "heatmap" ),
                                       selected = "pie")
                      )
               ),
               column(width = 9,
                      div(class = "gradient-panel",
                          withSpinner(highchartOutput("fdi_chart", height = "600px"), type = 7, color = "#FFF")
                      )
               )
             )
    ),
    
    #country tab
    
    tabPanel("Country Analysis", value = "countryal",
             
             fluidRow(
               column(width = 3,
                      div(class = "floating-panel",
                          h3("Country Analysis"),
                          tags$h5("Explore FDI inflows by source country across different metrics, project statuses, and time frames. Adjust filters to visualize top performing countries."),
                          
                          tags$div(
                            class = "mb-3",
                            airMonthpickerInput(
                              inputId = "country_start",
                              label = "Start Month:",
                              minDate = "2016-01-01",
                              maxDate = "2025-06-01",
                              value = "2016-01-01",
                              view = "months",
                              minView = "months",
                              dateFormat = "yyyy-MM"
                            ),
                            airMonthpickerInput(
                              inputId = "country_end",
                              label = "End Month:",
                              minDate = "2016-01-01",
                              maxDate = "2025-06-01",
                              value = "2025-06-01",
                              view = "months",
                              minView = "months",
                              dateFormat = "yyyy-MM"
                            ),
                            tags$script(HTML("
                          $(document).on('shiny:connected', function() {
                            $('.airdatepicker-input').each(function() {
                              var val = $(this).val();
                              if(val) $(this).val(val.substring(0, 7));
                            });
                          });
                        "))
                          ),
                          
                          radioButtons(
                            inputId = "country_chart_type",
                            label = "Select Chart Type:",
                            choices = c("Bar Chart" = "bar",
                                        "Pie Chart" = "pie",
                                        "Line over Time" = "line_time",
                                        "Cumulative Line" = "cum_line"),
                            selected = "bar",
                            inline = TRUE
                          ),
                          
                          radioButtons("country_metric", "Select Metric:",
                                       choices = c("Capital Investment", "Number of Projects", "Jobs Created"),
                                       selected = "Capital Investment",
                                       inline = FALSE),
                          
                          radioButtons("country_project_status", "Select Project Status:",
                                       choices = c("Opened", "Announced", "Opened + Announced"),
                                       selected = "Opened + Announced",
                                       inline = FALSE),
                          
                          numericInput("country_top_n", "Number of Top Countries to Display:",
                                       value = 10, min = 3, max = 30, step = 1)
                      )
               ),
               
               column(width = 9,
                      div(class = "gradient-panel",
                          withSpinner(
                            highchartOutput("country_chart", height = "670px"),
                            type = 7, color = "#FFF"
                          )
                      )
               )
             )
    ),
    
    #sector tab
    
    
    tabPanel("Sectoral Analysis", value = "sectoral",
             
             fluidRow(
               column(width = 3,
                      div(class = "floating-panel",
                          h3("Sectoral Analysis"),
                          tags$h5("Explore FDI inflows by sector across different metrics, project statuses, and time frames. Adjust filters to visualize top performing sectors. Find Industry classification info at the bottom"),
                          
                          tags$div(
                            class = "mb-3",
                            airMonthpickerInput(
                              inputId = "sector_start",
                              label = "Start Month:",
                              minDate = "2016-01-01",
                              maxDate = "2025-06-01",
                              value = "2016-01-01",
                              view = "months",
                              minView = "months",
                              dateFormat = "yyyy-MM"
                            ),
                            airMonthpickerInput(
                              inputId = "sector_end",
                              label = "End Month:",
                              minDate = "2016-01-01",
                              maxDate = "2025-06-01",
                              value = "2025-06-01",
                              view = "months",
                              minView = "months",
                              dateFormat = "yyyy-MM"
                            ),
                            tags$script(HTML("
                           $(document).on('shiny:connected', function() {
                             $('.airdatepicker-input').each(function() {
                               var val = $(this).val();
                               if(val) $(this).val(val.substring(0, 7)); // keep only YYYY-MM
                             });
                           });
                         "))
                          ),
                          
                          # Dimension selection: Sector / Sub-sector / Cluster / Activity
                          radioButtons(
                            inputId = "sector_dimension",
                            label = "Choose Dimension:",
                            choices = c("Cluster", "Sector", "Sub-sector", "Activity"),
                            selected = "Sector",
                            inline = FALSE
                          ),
                          
                          radioButtons(
                            inputId = "sector_chart_type",
                            label = "Select Chart Type:",
                            choices = c("Bar Chart" = "bar",
                                        "Pie Chart" = "pie",
                                        "Line over Time" = "line_time",
                                        "Cumulative Line" = "cum_line"),
                            selected = "bar",
                            inline = TRUE
                          ),
                          
                          # Metric selection
                          radioButtons("sector_metric", "Select Metric:",
                                       choices = c("Capital Investment", "Number of Projects", "Jobs Created"),
                                       selected = "Capital Investment",
                                       inline = FALSE),
                          
                          # Project status selection
                          radioButtons("sector_project_status", "Select Project Status:",
                                       choices = c("Opened", "Announced", "Opened + Announced"),
                                       selected = "Opened + Announced",
                                       inline = FALSE),
                          
                          # Top N sectors
                          numericInput("sector_top_n", "Number of Top Sectors to Display:",
                                       value = 10, min = 3, max = 30, step = 1)
                      )
               ),
               
               column(width = 9,
                      div(class = "gradient-panel",
                          tags$div(style = "height: 40px;"),                          
                          withSpinner(
                            highchartOutput("sector_chart", height = "700px"),
                            type = 7, color = "#FFF"
                          )
                      )
               )
             ),
             
             fluidRow(
               column(width = 12,
                      div(style = "background-color:#f8f9fa; border-radius:8px; padding:15px; margin-top:20px; font-size:0.9em; color:#444;",
                          tags$strong("NOTE on Classifications: "),
                          "All sectors, sub-sectors, clusters, and activities follow the ",
                          tags$em("fDi Markets proprietary industry classification system"),
                          ", developed by ",
                          tags$em("fDi Intelligence (Financial Times)"),
                          ". This taxonomy is designed for cross-border investment project tracking and is ",
                          "loosely aligned with ISIC and NAICS standards but not identical. ",
                          "Each project in fDi Markets is tagged at four levels: ",
                          tags$strong("Cluster → Sector → Sub-Sector → Business Activity."),
                          " Full classification details are available to subscribers within the fDi Markets data dictionary."
                      )
               )
             )
    ),
    
    
    # trump tab
    
    tabPanel("Trump Effect", value = "trump",
             fluidRow(
               column(width = 4,
                      div(class = "floating-panel",
                          h3("Trump Effect - Monthly FDI"),
                          p("Compare FDI trends to the US by country, sector, trend, project and metric from 2023-01. Vertical red line marks Trump’s presidency start (20/01/25). Based on selected metrics, both the graph and summary statistics in the table below get dynamically adjusted for in depth analysis and comparisons."),
                          radioButtons("trump_group_by", "Group By:",
                                       choices = c("Country", "Sector"),
                                       selected = "Country",
                                       inline = TRUE),
                          uiOutput("trump_area_selector"),
                          checkboxGroupInput("trump_project_type", "Select Project Status:",
                                             choices = c("Opened", "Announced", "Opened + Announced"),
                                             selected = c("Opened", "Announced")),
                          radioButtons("trump_metric", "Select Metric:",
                                       choices = c("Capital Investment", "Jobs Created", "Number of Projects"),
                                       selected = "Capital Investment",
                                       inline = TRUE),
                          radioButtons("trump_trend_type", "Trend Type:",
                                       choices = c("Monthly", "Cumulative"),
                                       selected = "Monthly",
                                       inline = TRUE)
                      )
               ),
               column(width = 8,
                      div(class = "gradient-panel",
                          withSpinner(highchartOutput("trump_chart", height = "500px"),
                                      type = 7, color = "#FFF")
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      div(class = "glass-panel",
                          h4("Summary Statistics Pre and Post Trump"),
                          tableOutput("trump_summary_stats"),
                          tags$div(
                            HTML("<p><em>Note:</em> The table shows summary metrics for pre-Trump and post-Trump periods.</p>")
                          ),
                          tags$div(style = "margin-top: 15px;",
                                   tags$p(strong("Column Explanations:"), 
                                          "Each column shows the following summary metrics:"),
                                   tags$ul(
                                     tags$li(strong("Group:"), " The selected area or country."),
                                     tags$li(strong("Series:"), " The trend type (Monthly or Cumulative)."),
                                     tags$li(strong("Metric:"), " The selected FDI metric (Capital Investment, Jobs Created, or Projects)."),
                                     tags$li(strong("Label:"), " Label combining project type and change type."),
                                     tags$li(strong("Mean:"), " Average value of the metric."),
                                     tags$li(strong("SD:"), " Standard deviation of the metric."),
                                     tags$li(strong("Min / Max:"), " Minimum and maximum observed values."),
                                     tags$li(strong("Trend:"), " Monthly average change in the metric over the period (only for cumulative trend)."),
                                     tags$li(strong("CV:"), " Coefficient of variation (SD divided by Mean)."),
                                     tags$li(strong("MaxMinDiff:"), " Difference between maximum and minimum values."),
                                     tags$li(strong("Abs_Change:"), " Absolute change between post- and pre-Trump periods (only for monthly trend)."),
                                     tags$li(strong("Pct_Change:"), " Percent change between post- and pre-Trump periods (only for monthly trend)."),
                                     tags$li(strong("Trend_Ratio:"), " Ratio of post-Trump trend to pre-Trump trend (only for cumulative trend).")
                                   )
                          )
                      )
               )
             )
    ),
    
    

    navbarMenu("More Analysis",
               
    
    
    # capital vs jobs tab 
    
    tabPanel("Capital vs Jobs", value = "capjobs",
             fluidRow(
               column(width = 3,
                      div(class = "floating-panel",
                          h3("Capital Investment vs Jobs Created"),
                          tags$h5("Compare how Capital Investment correlates with Jobs Created across countries or sectors. Each point represents a project, and the regression slope (β) quantifies the strength of the relationship."),
                          radioButtons("cap_jobs_group", "Group by:",
                                       choices = c("Sector", "Country"),
                                       selected = "Sector",
                                       inline = TRUE),
                          radioButtons("cap_jobs_project_status", "Select Project Type:",
                                       choices = c("Opened", "Announced", "Opened + Announced"),
                                       selected = "Opened + Announced",
                                       inline = TRUE),
                          selectInput("cap_jobs_index", "Select Group(s) to Show:",
                                      choices = NULL, selected = NULL, multiple = TRUE),
                          h4("Regression Slopes (β)"),
                          tableOutput("cap_jobs_betas")
                      )
               ),
               column(width = 9,
                      div(class = "gradient-panel",
                          withSpinner(highchartOutput("cap_jobs_chart", height = "500px"),
                                      type = 7, color = "#FFF")
                      )
               )
             )
    ),
    
    #world focus tab
    
    tabPanel(
      "World Focus", value = "world",
      
      useShinyjs(),
      
      fluidRow(
        column(
          width = 12,
          div(
            class = "card", 
            style = "background-color:#1f4e79; color: white; padding: 15px; border-radius: 10px; position: relative;", 
            
            h3("World Focus - FDI into the US"),
            p("Select the metric, project status and timeframe (either cumulated or specific year - for 2025 data shown only for the first 6 months). Hover and/or click on individual countries to explore total FDI value and composition by sector."),
            
            
            fluidRow(
              column(
                width = 3,
                radioButtons("world_metric", "Select Metric:",
                             choices = c("Capital Investment", "Number of Projects", "Jobs Created"),
                             selected = "Capital Investment",
                             inline = FALSE)
              ),
              column(
                width = 3,
                radioButtons("world_project_status", "Project Status:",
                             choices = c("Opened", "Announced", "Opened + Announced"),
                             selected = "Opened + Announced",
                             inline = FALSE)
              ),
              column(
                width = 6,
                div(
                  class = "card",
                  style = "background-color:#1f4e79; color: white; padding: 15px; border-radius: 10px;",
                  
                  h4("Time Selection"),
                  checkboxInput("world_cumulative", "Show cumulative (2016–2025-06)", value = TRUE),
                  
                  conditionalPanel(
                    condition = "!input.world_cumulative",
                    sliderInput(
                      "world_year", "Select Year:",
                      min = 2016, max = 2025,
                      value = 2025, step = 1, sep = ""
                    )
                  )
                )
              )
            ),
            
            
            div(
              class = "card",
              style = "position: relative; background-color:#1f4e79; color:white; padding:15px; border-radius:10px;",
              
              hidden(
                actionButton(
                  "back_to_map", "Back to World Map",
                  style = "position: absolute; top: 15px; right: 15px; z-index: 1000;"
                )
              )
              ),
            
            withSpinner(
              highchartOutput("world_map_chart", height = "600px"),
              type = 7, color = "#FFFFFF"
            )
          )
        )
      )
    ),
    
    tabPanel("Aggregates", value = "aggregates",
             
             fluidRow(
               column(width = 12,
                      div(class = "floating-panel",
                          h3("Aggregates Analysis"),
                          tags$h5("Filter Metrics, Project Status and Aggregates for comparisons over three dynamic charts")
                          )
               )
             ),

             fluidRow(
               column(width = 6,
                      div(class = "gradient-panel",
                          style = "height: 450px;",  
                          h3("Option selection"),
                          selectInput("aggregate_index", "Select Aggregates to Show:",
                                      choices = c("EU", "China", "NAFTA (-US)", "UK", "ASEAN",
                                                  "MERCOSUR", "AFR", "CIS", "BRICS"),
                                      selected = c("EU", "China"), multiple = TRUE),
                          radioButtons("agg_metric", "Select Metric:",
                                       choices = c("Capital Investment", "Number of Projects", "Jobs Created"),
                                       selected = "Capital Investment",
                                       inline = FALSE),
                          radioButtons("agg_project_status", "Project Status:",
                                       choices = c("Opened", "Announced", "Opened + Announced"),
                                       selected = "Opened + Announced",
                                       inline = FALSE)
                      )
               ),
               
               column(width = 6,  
                      div(class = "card",
                          style = "height: 450px;",
                          withSpinner(highchartOutput("agg_scat", height = "100%"),
                                      type = 7, color = "#FFF")
                      )
               )
               
               
               
             ),
             
             fluidRow(
               column(width = 6,  
                      div(class = "card",
                          style = "height: 450px;",
                          withSpinner(highchartOutput("agg_sankey", height = "100%"),
                                      type = 7, color = "#FFF")
                      )
               ),
               
               column(width = 6,
                      div(class = "gradient-panel",
                          style = "height: 450px;",
                          withSpinner(highchartOutput("agg_bar", height = "100%"),
                                      type = 7, color = "#FFF")
                      )
               )
               
             )
    ),
    
    
    # About Tab 
    tabPanel("How to use / About", value = "about",
             
             fluidRow(
               column(width = 12,
                      div(class = "floating-panel",
                          h3("Introduction to the Dashboard"),
                          p("This dashboard showcases a public, fully reproducible replica of the original Foreign Direct Investment (FDI) dashboard built using confidential data. 
                        Because the original dataset cannot be shared publicly, this GitHub version uses a synthetic dataset randomly generated by ChatgPT 5.1. without ANY access to original dataset."),
                          tags$ul(
                            tags$li("Synthetic FDI dataset structured at the monthly level"),
                            tags$li("Randomly generated FDI inflows by source country, sector, and project status"),
                            tags$li("Time frame covered: January 2016 to June 2025"),
                            tags$li("Values generated to mimic cross-country imbalance, country-specific sector patterns, and time trends"),
                            tags$li("All graphs are interactive: charts can be zoomed, exported, or downloaded using the menu in the upper right corner of each panel"),
                            tags$li("Includes a reproduction of the ‘Trump effect’ analysis from the original dashboard")
                          ),
                          p("Use the filters to explore different metrics, project types, countries, and chart formats.")
                      )
               )
             ),
             
             fluidRow(
               column(width = 12,
                      div(class = "floating-panel",
                          h3("Technical Details"),
                          tags$ul(
                            tags$li("This dashboard was developed using R 4.4.1 on R Studio 2024.9.0.375."),
                            tags$li("This GitHub version is fully standalone and runs entirely with random data."),
                            tags$li("Random data generation includes (fed into ChatGPT as prompt):"),
                            tags$ul(
                              tags$li("Monthly timestamps from 01-2016 to 06-2025"),
                              tags$li("Strong imbalance across countries (top contributors receive most entries)"),
                              tags$li("Country-specific random sector preferences and time trends")                            
                              ),
                            tags$li("The script contained in this repository can be modified to generate new synthetic datasets or expanded for demonstration purposes."),
                            tags$li("No real data is used in this public version.")
                          )
                      )
               )
             ),
             
             fluidRow(
               column(width = 12,
                      div(class = "floating-panel",
                          h3("Source"),
                          p("For demonstration purposes in this GitHub version, please cite:"),
                          tags$ul(
                            tags$li(style = "font-weight: bold; color: #2c3e50; font-size: 16px;",
                                    "‘Synthetic FDI Dataset, own simulations’")
                          )
                          )
                      )
               )
             )
    )
    
    )
    
  )
)




########################



# Server 


server <- function(input, output, session) {

  # custom color vectors 
    my_colors <- c(
    "#1f4e79",  "#2ca02c",  "#ff7f0e", "#9467bd", "#d62728",
    "#8c564b",  "#17becf",  "#bcbd22", "#e377c2", "#7f7f7f",
    "#393b79"
  )
  
  manual_colors <- list(
    "Australia" = "#001E3DE5",
    "Canada" = "#30283E",
    "Germany" = "#46AAAF",
    "Italy" = "#95AF83",
    "Japan" = "#BC8034",
    "South Korea" = "#6A5988",
    "Spain" = "#D36A5F",
    "Taiwan" = "#D1D9DD",
    "UAE" = "#004289",
    "United Kingdom" = "#F2E5C0"
  )
  
  
  
  # Filter data based on project (for general analysis)
  filtered_data <- reactive({
    if(input$project_type == "Opened") {
      us_fdi %>% filter(`Project status` == "Opened")
    } else if(input$project_type == "Announced") {
      us_fdi %>% filter(`Project status` == "Announced")
    } else {
      us_fdi
    }
  })%>% bindCache(input$project_type)
  
  #data for caps vs jobs (faster loading)
  
  cap_jobs_data <- reactive({
    us_fdi   
  })%>% bindCache(TRUE)
  
  
  #### general analysis rendering
  
  output$fdi_chart <- renderHighchart({
    df <- filtered_data()
    
    #  PIE CHART
    if(input$chart_type == "pie") {
      
      if(input$metric == "Capital Investment") {
        country_summary <- df %>%
          group_by(`Source country`) %>%
          summarise(value = sum(`Capital investment`, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(value))
        
        drilldown_data <- df %>%
          filter(`Source country` %in% head(country_summary$`Source country`, 10)) %>%
          group_by(`Source country`, Sector) %>%
          summarise(value = sum(`Capital investment`, na.rm = TRUE), .groups = "drop") %>%
          group_by(`Source country`) %>%
          arrange(`Source country`, desc(value))
        
        y_axis_title <- "Capital Investment (M USD)"
        tooltip_suffix <- " M"
        
      } else if(input$metric == "Number of Projects") {
        country_summary <- df %>%
          group_by(`Source country`) %>%
          summarise(value = n(), .groups = "drop") %>%
          arrange(desc(value))
        
        drilldown_data <- df %>%
          filter(`Source country` %in% head(country_summary$`Source country`, 10)) %>%
          group_by(`Source country`, Sector) %>%
          summarise(value = n(), .groups = "drop") %>%
          group_by(`Source country`) %>%
          arrange(`Source country`, desc(value))
        
        y_axis_title <- "Number of Projects"
        tooltip_suffix <- ""
        
      } else if(input$metric == "Jobs Created") {
        country_summary <- df %>%
          group_by(`Source country`) %>%
          summarise(value = sum(`Jobs created`, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(value))
        
        drilldown_data <- df %>%
          filter(`Source country` %in% head(country_summary$`Source country`, 10)) %>%
          group_by(`Source country`, Sector) %>%
          summarise(value = sum(`Jobs created`, na.rm = TRUE), .groups = "drop") %>%
          group_by(`Source country`) %>%
          arrange(`Source country`, desc(value))
        
        y_axis_title <- "Jobs Created"
        tooltip_suffix <- ""
      }
      
      # Top 10 countries + Others
      top_countries <- head(country_summary, 10)
      others <- sum(country_summary$value) - sum(top_countries$value)
      
      main_pie <- top_countries %>%
        mutate(drilldown = `Source country`) %>%
        bind_rows(tibble(`Source country` = "Others", value = others, drilldown = NA))
      
      drilldown_list <- drilldown_data %>%
        group_by(`Source country`) %>%
        summarise(
          data = list(map2(Sector, value, ~list(name = .x, y = round(.y)))),  
          .groups = "drop"
        )
      
      # Build pie chart with dynamic main and drilldown titles
      hchart(main_pie, "pie", hcaes(name = `Source country`, y = value, drilldown = drilldown)) %>%
        hc_colors(my_colors) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = paste("Top 10 Source Countries by FDI Inflow to the US - ",input$project_type, "-", input$metric)) %>%  # dynamic title
        hc_subtitle(text = "Click on any country to see their FDI decomposition by Sector") %>% 
        hc_plotOptions(
          pie = list(
            allowPointSelect = TRUE,
            cursor = "pointer",
            dataLabels = list(
              enabled = TRUE,
              formatter = JS(paste0("
            function() {
              var perc = Highcharts.numberFormat(this.percentage, 1);
              var value = Math.round(this.y);
              return '<b>' + this.point.name + '</b>: ' + value + '", tooltip_suffix, " (' + perc + '%)';
            }
          "))
            )
          )
        ) %>%
        hc_exporting(enabled = TRUE)%>%
        hc_drilldown(
          allowPointDrilldown = TRUE,
          series = list_parse(
            drilldown_list %>%
              rename(id = `Source country`, data = data) %>%
              mutate(
                type = "column",
                name = paste(input$metric, "by Sector")  
              )
          )
        ) %>%
        hc_yAxis(
          title = list(text = y_axis_title),
          labels = list(
            formatter = JS("function() { return Math.round(this.value); }")  
          )
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          pointFormatter = JS(paste0("
        function() {
          return '<b>' + this.name + '</b>: <b>' + Math.round(this.y) + '", tooltip_suffix, "</b>';
        }
      "))
        ) %>%
        hc_chart(events = list(
          drilldown = JS("
        function(e) {
          if(e.seriesOptions && e.seriesOptions.type === 'column') {
            this.xAxis[0].update({ 
              type: 'category', 
              labels: { rotation: -90 } 
            });
          }
        }
      "),
          drillup = JS("
        function(e) {
          this.xAxis[0].update({ type: 'linear', labels: { rotation: 0 } });
        }
      ")
        ))
    }
    
    # AREA PLOT 
    
    else if(input$chart_type == "area") {
      
      df <- df %>% mutate(date = as.Date(time)) %>% filter(!is.na(date))
      
      if(input$metric == "Capital Investment") {
        y_col <- "Capital investment"
        y_label <- "Share of Capital Investment (%)"
        tooltip_suffix <- " %"
        
      } else if(input$metric == "Number of Projects") {
        y_col <- "Project status"  
        y_label <- "Share of Number of Projects (%)"
        tooltip_suffix <- " %"
        
      } else if(input$metric == "Jobs Created") {
        y_col <- "Jobs created"
        y_label <- "Share of Jobs Created (%)"
        tooltip_suffix <- " %"
      }
      
      # Top 9 countries 
      top_countries <- df %>%
        group_by(`Source country`) %>%
        summarise(value = if(y_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) else if(y_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE) else n(), .groups = "drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = 9) %>%
        pull(`Source country`)
      
      df_area <- df %>%
        mutate(`Source country` = ifelse(`Source country` %in% top_countries, `Source country`, "Others")) %>%
        group_by(`Source country`, date) %>%
        summarise(value = if(y_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) else if(y_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE) else n(), .groups = "drop") %>%
        arrange(`Source country`, date)
      
      df_area <- df_area %>%
        mutate(`Source country` = ifelse(`Source country` %in% top_countries, `Source country`, "Others")) %>%
        group_by(date, `Source country`) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        arrange(date) %>%
        mutate(`Source country` = factor(`Source country`, levels = c(top_countries, "Others")))
      
      # Plot 100% stacked area chart 
      hchart(df_area, "area", hcaes(x = date, y = value, group = `Source country`)) %>%
        hc_colors(c(my_colors, "gray")) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_plotOptions(
          area = list(
            stacking = "percent",
            marker = list(enabled = FALSE)
          )
        ) %>%
        hc_title(text = paste("FDI Share over Time (Top 9 Countries + Others) -", input$project_type, "-", input$metric)) %>%
        hc_xAxis(title = list(text = "Date")) %>%
        hc_yAxis(
          title = list(text = y_label),
          max = 100,
          labels = list(
            formatter = JS("function() { return Highcharts.numberFormat(this.value, 0) + '%' }")
          )
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(
          shared = TRUE, 
          crosshairs = TRUE, 
          useHTML = TRUE,
          pointFormatter = JS(paste0("
        function() {
          return '<span style=\"color:' + this.color + '\">&#9679;</span> <b>' + this.series.name + '</b>: ' +
                 Highcharts.numberFormat(this.percentage, 1, '.', ',') + '", tooltip_suffix, "' + '<br/>';
        }
      "))
        )
    }
    
    # SANKEY PLOT (flows from column to column)
    
    else if(input$chart_type == "sankey") {
      
      if(input$metric == "Capital Investment") {
        agg_fun <- sum
        value_col <- "total_capital"
        tooltip_suffix <- "M"
        
      } else if(input$metric == "Number of Projects") {
        agg_fun <- length
        value_col <- "num_projects"
        tooltip_suffix <- ""
        
      } else if(input$metric == "Jobs Created") {
        agg_fun <- sum
        value_col <- "jobs_created"
        tooltip_suffix <- ""
      }
      
      # Top countries and subsectors
      top_countries <- df %>%
        group_by(`Source country`) %>%
        summarise(
          value = if(value_col == "total_capital") sum(`Capital investment`, na.rm = TRUE) else if(value_col == "jobs_created") sum(`Jobs created`, na.rm = TRUE) else length(`Project status`),
          .groups = "drop"
        ) %>%
        arrange(desc(value)) %>%
        slice_head(n = 10) %>%
        pull(`Source country`)
      
      top_subsectors <- df %>%
        group_by(`Sub-sector`) %>%
        summarise(
          value = if(value_col == "total_capital") sum(`Capital investment`, na.rm = TRUE) else if(value_col == "jobs_created") sum(`Jobs created`, na.rm = TRUE) else length(`Project status`),
          .groups = "drop"
        ) %>%
        arrange(desc(value)) %>%
        slice_head(n = 10) %>%
        pull(`Sub-sector`)
      
      fdi_flow_top <- df %>%
        filter(`Source country` %in% top_countries, `Sub-sector` %in% top_subsectors) %>%
        group_by(`Source country`, Sector, `Sub-sector`, Activity) %>%
        summarise(
          value = if(value_col == "total_capital") sum(`Capital investment`, na.rm = TRUE) else if(value_col == "jobs_created") sum(`Jobs created`, na.rm = TRUE) else length(`Project status`),
          .groups = "drop"
        )
      
      # Build links
      country_sector <- fdi_flow_top %>%
        group_by(`Source country`, Sector) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(from = `Source country`, to = Sector, country = `Source country`)
      
      sector_subsector <- fdi_flow_top %>%
        group_by(Sector, `Sub-sector`, `Source country`) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(from = Sector, to = `Sub-sector`, country = `Source country`)
      
      subsector_activity <- fdi_flow_top %>%
        group_by(`Sub-sector`, Activity, `Source country`) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(from = `Sub-sector`, to = Activity, country = `Source country`)
      
      links <- bind_rows(country_sector, sector_subsector, subsector_activity) %>%
        mutate(color = unname(sapply(country, function(x) manual_colors[[x]])),
               weight_m = round(weight, 0))
      
      # Render Sankey
      highchart() %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_chart(type = "sankey") %>%
        hc_title(text = paste("FDI Flows -", input$project_type, "-", input$metric)) %>%
        hc_add_series(
          keys = c("from", "to", "weight", "color"),
          data = list_parse(links),
          name = "FDI Flows",
          linkOpacity = 0.6
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(
          useHTML = TRUE,
          pointFormatter = JS(paste0("
        function() {
          if(this.from && this.to){
            return this.from + ' → ' + this.to + ': <b>' + Math.round(this.weight) + '", tooltip_suffix, "' + '</b>';
          }
        }
      ")),
          nodeFormatter = JS(paste0("
        function() {
          return '<b>' + this.name + '</b>: <b>' + Math.round(this.sum) + '", tooltip_suffix, "' + '</b>';
        }
      "))
        )
    }
    
    # CUMULATIVE TREND
    
    else if(input$chart_type == "cumulative") {
      
      #prepare data, filter choices
      
      df <- df %>% mutate(date = as.Date(time)) %>% filter(!is.na(date))
      
      if(input$metric == "Capital Investment") {
        agg_col <- "Capital investment"
        y_label <- "Cumulative Capital Investment (M USD)"
        tooltip_suffix <- " M"
      } else if(input$metric == "Number of Projects") {
        agg_col <- "Project status"
        y_label <- "Cumulative Number of Projects"
        tooltip_suffix <- ""
      } else if(input$metric == "Jobs Created") {
        agg_col <- "Jobs created"
        y_label <- "Cumulative Jobs Created"
        tooltip_suffix <- ""
      }
      
      top_countries <- df %>%
        group_by(`Source country`) %>%
        summarise(value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE) 
                  else n(),
                  .groups = "drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = 10) %>%
        pull(`Source country`)
      
      df_top <- df %>% filter(`Source country` %in% top_countries)
      
      cum_fdi <- df_top %>%
        group_by(`Source country`, date) %>%
        summarise(daily_value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop") %>%
        arrange(`Source country`, date) %>%
        group_by(`Source country`) %>%
        mutate(cumulative = cumsum(daily_value)) %>%
        ungroup()
      
      # Plot 
      hchart(cum_fdi, "line", hcaes(x = date, y = cumulative, group = `Source country`)) %>%
        hc_colors(c(my_colors, "black")) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = paste("Cumulative FDI over time (Top 10 countries) -", input$project_type, "-", input$metric)) %>%
        hc_yAxis(title = list(text = y_label),
                 labels = list(
                   formatter = JS(paste0("function() { return Math.round(this.value) + '", tooltip_suffix, "'; }"))
                 )) %>%
        hc_xAxis(title = list(text = "Date")) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, useHTML = TRUE,
                   pointFormatter = JS(paste0("
                 function() {
                   return '<span style=\"color:' + this.color + '\">&#9679;</span> <b>' + this.series.name + '</b>: ' +
                          Math.round(this.y) + '", tooltip_suffix, "' + '<br/>';
                 }
               ")))
    }
    
    # MONTHLY TREND
    else if(input$chart_type == "monthly") {
      
      #data preparation
      
      df <- df %>% mutate(month = lubridate::floor_date(as.Date(time), "month"))
      
      
      if(input$metric == "Capital Investment") {
        agg_col <- "Capital investment"
        y_label <- "Capital Investment (M USD)"
        tooltip_suffix <- " M"
      } else if(input$metric == "Number of Projects") {
        agg_col <- "Project status"
        y_label <- "Number of Projects"
        tooltip_suffix <- ""
      } else if(input$metric == "Jobs Created") {
        agg_col <- "Jobs created"
        y_label <- "Jobs Created"
        tooltip_suffix <- ""
      }
      
      
      top_countries <- df %>%
        group_by(`Source country`) %>%
        summarise(value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE) 
                  else n(),
                  .groups = "drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = 10) %>%
        pull(`Source country`)
      
      df_top <- df %>% filter(`Source country` %in% top_countries)
      
      monthly_fdi <- df_top %>%
        group_by(`Source country`, month) %>%
        summarise(monthly_value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE)
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop")
      
      # Plot
      hchart(monthly_fdi, "line", hcaes(x = month, y = monthly_value, group = `Source country`)) %>%
        hc_colors(my_colors) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = paste("Monthly FDI (Top 10 countries) -", input$project_type, "-", input$metric)) %>%
        hc_yAxis(title = list(text = y_label),
                 labels = list(
                   formatter = JS(paste0("function() { return Math.round(this.value) + '", tooltip_suffix, "'; }"))
                 )) %>%
        hc_xAxis(title = list(text = "Month"),
                 type = "datetime",
                 labels = list(format = "{value:%Y-%m}")) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE,
                   pointFormatter = JS(paste0("
                 function() {
                   return '<span style=\"color:' + this.color + '\">&#9679;</span> <b>' + this.series.name + '</b>: ' +
                          Math.round(this.y) + '", tooltip_suffix, "' + '<br/>';
                 }
               ")))
    }
    
    #WORLD TREND
    
    else if(input$chart_type == "world_trend") {
      
      df <- df %>% mutate(date = as.Date(time)) %>% filter(!is.na(date))
      
      if(input$metric == "Capital Investment") {
        agg_col <- "Capital investment"
        y_label <- "Capital Investment (M USD)"
        tooltip_suffix <- " M"
      } else if(input$metric == "Number of Projects") {
        agg_col <- "Project status"
        y_label <- "Number of Projects"
        tooltip_suffix <- ""
      } else if(input$metric == "Jobs Created") {
        agg_col <- "Jobs created"
        y_label <- "Jobs Created"
        tooltip_suffix <- ""
      }
      
      # World cumulative
      df_world_cum <- df %>%
        group_by(date) %>%
        summarise(daily_value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop") %>%
        arrange(date) %>%
        mutate(cumulative = cumsum(daily_value),
               drilldown = format(date, "%Y-%m-%d"))
      
      # World monthly
      df_world_monthly <- df %>%
        mutate(month = lubridate::floor_date(date, "month")) %>%
        group_by(month) %>%
        summarise(monthly_value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE)
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop") %>%
        arrange(month) %>%
        mutate(drilldown = format(month, "%Y-%m"))
      
      #drilldowns
        drilldown_cum <- df %>%
        group_by(date, Sector) %>%
        summarise(value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop") %>%
        group_by(date) %>%
        summarise(
          data = list(map2(Sector, value, ~list(name = .x, y = .y))),
          .groups = "drop"
        ) %>%
        mutate(id = format(date, "%Y-%m-%d"),
               type = "column",
               name = "By Sector")
      
      drilldown_month <- df %>%
        mutate(month = lubridate::floor_date(date, "month")) %>%
        group_by(month, Sector) %>%
        summarise(value = if(agg_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE) 
                  else if(agg_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
                  else n(),
                  .groups = "drop") %>%
        group_by(month) %>%
        summarise(
          data = list(map2(Sector, value, ~list(name = .x, y = .y))),
          .groups = "drop"
        ) %>%
        mutate(id = format(month, "%Y-%m"),
               type = "column",
               name = "By Sector")
      
      # Chart
      highchart() %>%
        hc_add_series(df_world_cum, "line",
                      hcaes(x = date, y = cumulative, drilldown = drilldown),
                      name = "Cumulative World Trend") %>%
        hc_add_series(df_world_monthly, "line",
                      hcaes(x = month, y = monthly_value, drilldown = drilldown),
                      name = "Monthly World Trend", visible = FALSE) %>%
        hc_chart(zoomType = "x") %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = paste("World FDI Trend -", input$project_type, "-", input$metric)) %>%
        hc_subtitle(text = "Click on any period to see sectoral composition of FDI at that point in time (cumulated or monthly)") %>%
        hc_yAxis(title = list(text = y_label),
                 labels = list(
                   formatter = JS(paste0("function() { return Math.round(this.value) + '", tooltip_suffix, "'; }"))
                 )) %>%
        hc_xAxis(title = list(text = "Date"), type = "datetime") %>%
        hc_exporting(enabled = TRUE) %>%
        hc_plotOptions(
          series = list(
            events = list(
              legendItemClick = JS("
            function() {
              var chart = this.chart;
              chart.series.forEach(function(s) {
                if (s !== this) { s.setVisible(false, false); }
              }, this);
              this.setVisible(true, false);
              chart.redraw();
              return false;
            }
          ")
            )
          )
        ) %>%
        hc_drilldown(
          allowPointDrilldown = TRUE,
          series = list_parse(bind_rows(drilldown_cum, drilldown_month))
        ) %>%
        hc_chart(events = list(
          drilldown = JS("
        function(e) {
          if(e.seriesOptions && e.seriesOptions.type === 'column') {
            this.xAxis[0].update({ type: 'category', labels: { rotation: -90 } });
            this.setSubtitle({ text: '' });
          }
        }
      "),
          drillup = JS("
        function(e) {
          this.xAxis[0].update({ type: 'datetime', labels: { rotation: 0 } });
          this.setSubtitle({ text: 'Click on any period to see sectoral composition of FDI at that point in time (cumulated or monthly)' });
        }
      ")
        )) %>%
        hc_tooltip(shared = FALSE, useHTML = TRUE,
                   pointFormatter = JS(paste0("
        function() {
          if(this.series.type === 'column') {
            return '<b>' + this.name + '</b>: ' +
                   Math.round(this.y) + '", tooltip_suffix, "' + '<br/>';
          } else {
            return '<b>' + Highcharts.dateFormat('%b %Y', this.x) + '</b>: ' +
                   Math.round(this.y) + '", tooltip_suffix, "' + '<br/>';
          }
        }
      "))
        )
    }
    
    #HEATMAP
    
    else if(input$chart_type == "heatmap") {
      
      if(input$metric == "Capital Investment") {
        value_col <- "Capital investment"
        tooltip_suffix <- " M"
      } else if(input$metric == "Number of Projects") {
        value_col <- "Project status"  # counting rows
        tooltip_suffix <- ""
      } else if(input$metric == "Jobs Created") {
        value_col <- "Jobs created"
        tooltip_suffix <- ""
      }
      
      heatmap_data <- df %>%
        group_by(`Source country`, Sector) %>%
        summarise(
          value = if(value_col == "Capital investment") sum(`Capital investment`, na.rm = TRUE)
          else if(value_col == "Jobs created") sum(`Jobs created`, na.rm = TRUE)
          else n(),
          .groups = "drop"
        )
      
      top_countries <- heatmap_data %>%
        group_by(`Source country`) %>%
        summarise(total = sum(value), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = 10) %>%
        pull(`Source country`)
      
      heatmap_data <- heatmap_data %>% filter(`Source country` %in% top_countries)
      
      top_sectors <- heatmap_data %>%
        group_by(Sector) %>%
        summarise(total = sum(value), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = 10) %>%
        pull(Sector)
      
      heatmap_data <- heatmap_data %>% filter(Sector %in% top_sectors)
      
      heatmap_data <- heatmap_data %>%
        tidyr::complete(`Source country` = top_countries, Sector = top_sectors, fill = list(value = 0))
      
      heatmap_matrix <- heatmap_data %>%
        mutate(
          value_log = log(value + 1),
          x = as.numeric(factor(Sector, levels = top_sectors)) - 1,
          y = as.numeric(factor(`Source country`, levels = rev(top_countries))) - 1
        ) %>%
        select(x, y, value = value_log, original_value = value)
      
      # Plot heatmap 
      highchart() %>%
        hc_chart(type = "heatmap", backgroundColor = "transparent") %>%
        hc_add_series(
          data = list_parse2(heatmap_matrix),
          name = paste(input$metric, "by Country and Sector"),
          borderWidth = 1
        ) %>%
        hc_xAxis(
          categories = top_sectors,
          title = list(
            text = "Sector",
            style = list(color = "#FFFFFF", fontWeight = "bold")
          ),
          labels = list(style = list(color = "#FFFFFF")),
          lineColor = "#FFFFFF",
          tickColor = "#FFFFFF"
        ) %>%
        hc_yAxis(
          categories = rev(top_countries),
          title = list(
            text = "Country",
            style = list(color = "#FFFFFF", fontWeight = "bold")
          ),
          labels = list(style = list(color = "#FFFFFF")),
          lineColor = "#FFFFFF",
          tickColor = "#FFFFFF"
        ) %>%
        hc_colorAxis(
          minColor = "#E0F2FF",
          maxColor = "#1f4e79",
          labels = list(style = list(color = "#FFFFFF")),
          title = list(style = list(color = "#FFFFFF"))
        ) %>%
        hc_title(
          text = paste("FDI Heatmap -", input$project_type, "-", input$metric),
          style = list(color = "#FFFFFF", fontWeight = "bold")
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        
        hc_legend(
          layout = "vertical",
          align = "right",
          verticalAlign = "middle",
          itemStyle = list(color = "#FFFFFF"),
          itemHoverStyle = list(color = "#FFFFFF"),
          title = list(style = list(color = "#FFFFFF"))
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          formatter = JS(paste0("
      function() {
        var original_value = Math.round(Math.exp(this.point.value) - 1);
        return '<b>' + this.series.yAxis.categories[this.point.y] + '</b><br>' +
               '<b>' + this.series.xAxis.categories[this.point.x] + '</b>: ' +
               original_value + '", tooltip_suffix, "';
      }
    "))
        )
    }
    
    #world map: upgraded version below in its own tab
    
    # else if(input$chart_type == "world_map") {
    #   
    #   # Prepare map data
    #   df_map <- df %>%
    #     filter(`Source country` != "Kosovo", `Source country` != "United States") %>%
    #     mutate(iso3 = countrycode(`Source country`, "country.name", "iso3c")) %>%
    #     group_by(`Source country`, iso3) %>%
    #     summarise(
    #       total_value = if(input$metric == "Capital Investment") sum(`Capital investment`, na.rm = TRUE)
    #       else if(input$metric == "Number of Projects") n()
    #       else sum(`Jobs created`, na.rm = TRUE),
    #       .groups = "drop"
    #     )
    #   
    #   # Define color stops (can keep same for simplicity)
    #   n_stops <- 7
    #   stop_colors <- c('#6d3040', '#cd624f', '#dabc41', '#e9d584', '#bcd285', '#84A76D', '#4c7d55')
    #   stops <- lapply(0:(n_stops-1), function(i) list(i/(n_stops-1), stop_colors[i+1]))
    #   
    #   # Dynamic color axis and tooltip based on metric
    #   if (input$metric == "Capital Investment") {
    #     min_val <- 0
    #     max_val <- 100000
    #     tooltip_suffix <- " M"
    #     formatter_js <- JS("
    #   function() {
    #     if (this.value >= this.axis.max) {
    #       return '100B+';
    #     } else {
    #       return (this.value / 1000) + ' B';
    #     }
    #   }
    # ")
    #   } else  {
    #     min_val <- 0
    #     max_val <- max(df_map$total_value, na.rm = TRUE)
    #     tooltip_suffix <- ""
    #     formatter_js <- JS("function() { return this.value; }")
    #   }
    #   
    #   # Build the highchart map
    #   highchart(type = "map") %>%
    #     hc_add_series_map(
    #       worldgeojson,
    #       df_map,
    #       value = "total_value",
    #       joinBy = c("iso-a3", "iso3"),
    #       name = input$metric,
    #       nullColor = "#F0F0F0",
    #       states = list(
    #         hover = list(
    #           enabled = TRUE,
    #           borderWidth = 3,          # thicker border to “pop out”
    #           borderColor = "#FFFFFF",  # white border
    #           brightness = 0.1,         # slightly lighter
    #           halo = list(
    #             size = 12,
    #             opacity = 0.3,
    #             attributes = list(fill = "#FFFFFF")
    #           )
    #         )
    #       )
    #     ) %>%
    #     hc_colorAxis(
    #       min = min_val,
    #       max = max_val,
    #       stops = stops,
    #       labels = list(
    #         style = list(color = "#FFFFFF", width = 'auto', whiteSpace = 'nowrap', textOverflow = 'clip'),
    #         formatter = formatter_js
    #       )
    #     ) %>%
    #     hc_legend(
    #       layout = "horizontal",
    #       align = "center",
    #       verticalAlign = "bottom",
    #       symbolWidth = 300,
    #       itemStyle = list(color = "#FFFFFF"),
    #       itemHoverStyle = list(color = "#FFFFFF"),
    #       title = list(style = list(color = "#FFFFFF"))
    #     ) %>%
    #     hc_exporting(enabled = TRUE) %>%
    #     hc_title(
    #       text = paste("World Map: Countries FDI to the US by", input$metric),
    #       style = list(color = "#FFFFFF", fontWeight = "bold")
    #     ) %>%
    #     hc_tooltip(
    #       useHTML = TRUE,
    #       headerFormat = "",
    #       formatter = JS(paste0("
    #   function() {
    #     var value = Math.round(Math.abs(this.point.value));
    #     return '<b>' + this.point.name + '</b>: ' + value + '", tooltip_suffix, "';
    #   }
    # "))
    #     ) %>%
    #     hc_mapNavigation(
    #       enabled = TRUE,
    #       enableMouseWheelZoom = TRUE,
    #       buttonOptions = list(
    #         verticalAlign = 'bottom',
    #         theme = list(fill = '#1f4e79', style = list(color = '#FFFFFF'))
    #       )
    #     )
    #   
    #   
    # }
    
    
    
  })%>%bindCache(input$metric, input$chart_type, input$project_type) 
  
  
  ################
  
  # CAPITAL VS JOBS
  
  

  df_capjobs <- reactive({ cap_jobs_data() })
  
  # Filter by Project Status
  df_capjobs_filtered <- reactive({
    df <- df_capjobs()
    if (input$cap_jobs_project_status == "Opened") {
      df <- df[df$`Project status` == "Opened", ]
    } else if (input$cap_jobs_project_status == "Announced") {
      df <- df[df$`Project status` == "Announced", ]
    }
    df  # "Opened + Announced" = no filter
  })
  
  # Update menu dynamically when changing from Sector/Country
  observe({
    req(input$cap_jobs_group)
    gv <- if (input$cap_jobs_group == "Sector") "Sector" else "Source country"
    
    # Preserve current selection if still valid
    current_sel <- isolate(input$cap_jobs_index)
    valid_sel <- current_sel[current_sel %in% df_capjobs_filtered()[[gv]]]
    
    updateSelectInput(
      session,
      "cap_jobs_index",
      choices = sort(unique(df_capjobs_filtered()[[gv]])),
      selected = valid_sel  
    )
  })
  
  # Reactive for selected groups
  selected_groups <- reactive({ input$cap_jobs_index })
  
  output$cap_jobs_chart <- renderHighchart({
    req(input$main_tab == "capjobs")
    
    gv <- if (input$cap_jobs_group == "Sector") "Sector" else "Source country"
    sel <- selected_groups()
    
    # If nothing selected, show empty chart
    if (length(sel) == 0) {
      return(
        highchart() %>%
          hc_chart(type = "scatter", backgroundColor = "transparent") %>%
          hc_title(
            text = paste("Capital Investment vs Jobs Created — grouped by", gv),
            style = list(color = "white")
          ) %>%
          hc_xAxis(
            type = "logarithmic",
            title = list(text = "Capital Investment (log)", style = list(color = "white")),
            labels = list(style = list(color = "white"))
          ) %>%
          hc_yAxis(
            type = "logarithmic",
            title = list(text = "Jobs Created (log)", style = list(color = "white")),
            labels = list(style = list(color = "white"))
          ) %>%
          hc_legend(itemStyle = list(color = "white"))
      )
    }
    
    # Build series for selected groups
    series_list <- lapply(sel, function(g) {
      dfg <- df_capjobs_filtered()[df_capjobs_filtered()[[gv]] == g, c("Capital investment","Jobs created")]
      list(
        name = g,
        type = "scatter",
        data = purrr::transpose(list(x = dfg$`Capital investment`, y = dfg$`Jobs created`))
      )
    })
    
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_title(
        text = paste("Capital Investment vs Jobs Created — grouped by", gv),
        style = list(color = "#FFFFFF", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        type = "logarithmic",
        title = list(text = "Capital Investment (log)", style = list(color = "#FFFFFF")),
        labels = list(
          style = list(color = "#FFFFFF"),
          formatter = JS("function(){return Highcharts.numberFormat(this.value,0,'.',',');}")
        )
      ) %>%
      hc_yAxis(
        type = "logarithmic",
        title = list(text = "Jobs Created (log)", style = list(color = "#FFFFFF")),
        labels = list(
          style = list(color = "#FFFFFF"),
          formatter = JS("function(){return Highcharts.numberFormat(this.value,0,'.',',');}")
        )
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_series_list(series_list) %>%
      hc_plotOptions(
        scatter = list(
          marker = list(symbol = "circle", radius = 5)
        )
      ) %>%
      hc_legend(
        itemStyle = list(color = "#FFFFFF"),       
        itemHoverStyle = list(color = "#FFFFFF")   
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormatter = JS(
          "function(){
        return '<span style=\"color:#FFFFFF\">Group: '+this.series.name+'<br>'+
               'Capital: '+Math.round(this.x)+' M<br>'+
               'Jobs: '+Highcharts.numberFormat(this.y,0,'.',',')+'</span>';
      }"
        )
      )
    
  })
  
  
  # Regression slopes table for cap vs jobs 
  
  
  output$cap_jobs_betas <- renderTable({
    req(input$main_tab == "capjobs")
    sel <- selected_groups()
    if (length(sel) == 0) return(data.frame(Note = "Select a group from the index above to show β here."))
    
    gv <- if (input$cap_jobs_group == "Sector") "Sector" else "Source country"
    
    df_capjobs_filtered()[df_capjobs_filtered()[[gv]] %in% sel, ] %>%
      dplyr::group_by(.data[[gv]]) %>%
      dplyr::summarise(
        beta = {
          x <- `Capital investment`; y <- `Jobs created`
          ok <- is.finite(x) & is.finite(y) & x > 0 & y > 0
          if (sum(ok) >= 2) stats::coef(lm(log10(y[ok]) ~ log10(x[ok])))[2] else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::mutate(beta = round(beta, 3)) %>%
      dplyr::arrange(dplyr::desc(beta)) %>%
      dplyr::rename(Group = !!gv, `β (log–log slope)` = beta)
  }, striped = TRUE, bordered = TRUE, digits = 3)
  
  
  
  ###########
  
  #### TRUMP EFFECT

  ### rendering by choice: country or sector
  
  output$trump_area_selector <- renderUI({
    if(input$trump_group_by == "Country") {
      selectInput("trump_area", "Select Country:",
                  choices = c("World", "EU", sort(unique(us_fdi$`Source country`))),
                  selected = "World")
    } else {
      selectInput("trump_area", "Select Sector:",
                  choices = sort(unique(us_fdi$Sector)),
                  selected = unique(us_fdi$Sector)[1])
    }
  })
  
  # Reactive filtered data for Trump chart based on users choices
  
  df_trump_filtered <- reactive({
    req(input$trump_area, input$trump_project_type)
    
    df <- us_fdi %>%
      mutate(month_year = as.Date(format(as.Date(time), "%Y-%m-01"))) %>%
      filter(time > as.Date("2022-12-31"))%>%
      filter(!is.na(month_year))
    
    if (input$trump_group_by == "Country") {
      
      if (input$trump_area == "World") {
        # no extra filter, keep all countries
        
      } else if (input$trump_area == "EU") {
        # filter by Aggregate column
        df <- df %>% filter(Aggregate == "EU")
        
      } else {
        # regular single country selection
        df <- df %>% filter(`Source country` == input$trump_area)
      }
      
    } else {
      df <- df %>% filter(Sector == input$trump_area)
    }
    
    proj_types <- input$trump_project_type
    df <- df %>%
      filter(`Project status` %in%
               if ("Opened + Announced" %in% proj_types) 
                 c("Opened", "Announced") 
             else proj_types
      )
    
    df
  }) %>% bindCache(input$trump_area, input$trump_project_type)

  output$trump_chart <- renderHighchart({
    req(input$main_tab == "trump")
    df <- df_trump_filtered()
    
    month_seq <- seq(min(df$month_year), max(df$month_year), by = "month")
    month_labels <- format(month_seq, "%b %Y")
    
    metric_col <- switch(input$trump_metric,
                         "Capital Investment" = "Capital investment",
                         "Jobs Created" = "Jobs created",
                         "Number of Projects" = "Project count")  
    
    series_list <- lapply(input$trump_project_type, function(proj_type){
      
      df_status <- if(proj_type == "Opened + Announced") df else df %>% filter(.data[["Project status"]] == proj_type)
      
      # Aggregate metric
      if(input$trump_metric == "Number of Projects") {
        df_status <- df_status %>%
          group_by(month_year) %>%
          summarise(value = n(), .groups = "drop")
      } else {
        df_status <- df_status %>%
          group_by(month_year) %>%
          summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
      }
      
      value_full <- sapply(month_seq, function(m){
        val <- df_status$value[df_status$month_year == m]
        if(length(val) == 0) 0 else val
      })
      
      if(input$trump_trend_type == "Cumulative") value_full <- cumsum(value_full)
      
      list(
        name = proj_type,
        data = value_full,
        type = "line"
      )
    })
    
    # Plot line for Trump presidency start
    plot_lines <- list()
    if(min(month_seq) <= as.Date("2025-02-01") & max(month_seq) >= as.Date("2025-02-01")){
      trump_pos <- which(month_seq == as.Date("2025-01-01")) - 1 + 0.5
      plot_lines <- c(plot_lines, list(
        list(color = "white", width = 2, dashStyle = "Dash",
             value = trump_pos,
             label = list(text = "Trump Start", style = list(color = "white", fontWeight = "bold")))
      ))
    }
    
    #  Highchart 
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(
        text = paste(input$trump_trend_type, input$trump_metric, "from", input$trump_area, "to the US"),
        style = list(color = "#FFFFFF", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        categories = month_labels,
        title = list(text = "Month", style = list(color = "#FFFFFF")),
        labels = list(style = list(color = "#FFFFFF")),
        plotLines = plot_lines
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_yAxis(
        title = list(text = input$trump_metric, style = list(color = "#FFFFFF")),
        labels = list(
          style = list(color = "#FFFFFF"),
          formatter = JS("
        function() { 
          if(this.axis.options.title.text === 'Capital Investment') {
            return Highcharts.numberFormat(this.value, 0, '.', ',') + ' M';
          } else { 
            return this.value; 
          }
        }
      ")
        )
      ) %>%
      hc_add_series_list(series_list) %>%
      hc_colors(RColorBrewer::brewer.pal(8, "Set2")) %>%
      hc_tooltip(
        shared = TRUE,
        crosshairs = TRUE,
        useHTML = TRUE,
        formatter = JS("
      function() {
        var date_label = this.x !== null ? this.series.xAxis.categories[this.point.index] : '';
        var val = this.y;
        if(this.series.name.includes('Capital Investment')) {
          return '<span style=\"color:#FFFFFF\"><b>'+ this.series.name +'</b>: '+ Highcharts.numberFormat(val, 0, '.', ',') + ' M<br>' + date_label + '</span>';
        } else {
          return '<span style=\"color:#FFFFFF\"><b>'+ this.series.name +'</b>: '+ val + '<br>' + date_label + '</span>';
        }
      }
    ")
      ) %>%
      hc_legend(
        itemStyle = list(color = "#FFFFFF"),
        itemHoverStyle = list(color = "#FFFFFF")
      )
    
  })
  
  
  ### render dynamic table with useful stats for comparison pre and post trump 
  
  output$trump_summary_stats <- renderTable({
    req(input$trump_area, input$trump_project_type, input$trump_metric, input$trump_trend_type, input$trump_group_by)
    
    df <- df_trump_filtered()
    area_selected <- input$trump_area
    trend_type <- input$trump_trend_type
    metric <- input$trump_metric
    
    pre_trump_end <- as.Date("2025-01-31")
    post_trump_start <- as.Date("2025-02-01")
    
    summarize_period <- function(df_sub, label) {
      if(nrow(df_sub) == 0) {
        return(data.frame(
          Group = area_selected,
          Series = trend_type,
          Metric = metric,
          Label = label,
          Mean = NA, SD = NA, Min = NA, Max = NA,
          Trend = NA, CV = NA, MaxMinDiff = NA
        ))
      }
      
      # Compute metric
      if(metric == "Number of Projects") {
        df_sub <- df_sub %>%
          group_by(month_year) %>%
          summarise(value = n(), .groups = "drop")
      } else {
        y_col <- switch(metric,
                        "Capital Investment" = "Capital investment",
                        "Jobs Created" = "Jobs created")
        df_sub <- df_sub %>%
          group_by(month_year) %>%
          summarise(value = sum(.data[[y_col]], na.rm = TRUE), .groups = "drop")
      }
      
      if(trend_type == "Cumulative") df_sub$value <- cumsum(df_sub$value)
      
      df_sub <- df_sub %>% arrange(month_year)
      time_index <- seq_len(nrow(df_sub))
      trend_val <- if(nrow(df_sub) >= 2) coef(lm(df_sub$value ~ time_index))[2] else NA_real_
      
      cv_val <- if(mean(df_sub$value, na.rm = TRUE) != 0) sd(df_sub$value, na.rm = TRUE)/mean(df_sub$value, na.rm = TRUE) else NA_real_
      
      max_min_diff <- max(df_sub$value, na.rm = TRUE) - min(df_sub$value, na.rm = TRUE)
      
      data.frame(
        Group = area_selected,
        Series = trend_type,
        Metric = metric,
        Label = label,
        Mean = round(mean(df_sub$value, na.rm = TRUE),2),
        SD   = round(ifelse(is.na(sd(df_sub$value, na.rm = TRUE)),0,sd(df_sub$value, na.rm = TRUE)),2),
        Min  = round(min(df_sub$value, na.rm = TRUE),2),
        Max  = round(max(df_sub$value, na.rm = TRUE),2),
        Trend = round(trend_val,3),
        CV = round(cv_val,3),
        MaxMinDiff = round(max_min_diff,3)
      )
    }
    
    results <- lapply(input$trump_project_type, function(proj_type) {
      df_status <- if(proj_type == "Opened + Announced") df else df %>% filter(`Project status` == proj_type)
      
      pre  <- summarize_period(df_status %>% filter(month_year <= pre_trump_end), paste0(proj_type, " Pre-Trump"))
      post <- summarize_period(df_status %>% filter(month_year >= post_trump_start), paste0(proj_type, " Post-Trump"))
      
      abs_change <- post$Mean - pre$Mean
      pct_change <- if(!is.na(pre$Mean) && pre$Mean != 0) abs_change / pre$Mean * 100 else NA_real_
      trend_ratio <- if(!is.na(pre$Trend) && pre$Trend != 0) post$Trend / pre$Trend else NA_real_
      
      comparison <- data.frame(
        Group = area_selected,
        Series = trend_type,
        Metric = metric,
        Label = paste0(proj_type, " Change"),
        Mean = "", SD = "", Min = "", Max = "",
        Trend = "", CV = "", MaxMinDiff = "",
        Abs_Change = round(abs_change,2),
        Pct_Change = round(pct_change,2),
        Trend_Ratio = round(trend_ratio,3)
      )
      
      pre$Abs_Change <- post$Abs_Change <- pre$Pct_Change <- post$Pct_Change <- pre$Trend_Ratio <- post$Trend_Ratio <- ""
      
      rbind(pre, post, comparison)
    })
    
    df_summary <- do.call(rbind, results)
    
    # Remove columns based on trend_type
    if(trend_type == "Cumulative") {
      df_summary <- df_summary %>% select(-Abs_Change, -Pct_Change)
    } else {
      df_summary <- df_summary %>% select(-Trend, -Trend_Ratio)
    }
    
    df_summary
  }, striped = TRUE, bordered = TRUE, digits = 3)
  
  
  
  
  ###############
  
  
  #### WORLD MAP 
  
  df_world <- reactive({
    req(us_fdi)
    
    df <- us_fdi %>%
      filter(`Project status` %in% switch(input$world_project_status,
                                          "Opened" = "Opened",
                                          "Announced" = "Announced",
                                          "Opened + Announced" = c("Opened", "Announced")),
             `Source country` != "Kosovo",
             `Source country` != "United States")
    
    # Filter by year if not cumulative
    if (!input$world_cumulative) {
      df <- df %>% filter(lubridate::year(time) == input$world_year)
    }
    
    df %>%
      group_by(`Source country`, iso3_cp) %>%
      summarise(
        total_value = case_when(
          input$world_metric == "Capital Investment" ~ sum(`Capital investment`, na.rm = TRUE),
          input$world_metric == "Number of Projects" ~ n(),
          input$world_metric == "Jobs Created" ~ sum(`Jobs created`, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  })
  
  # Reactive value to track chart state
  
  chart_state <- reactiveVal("map") 
  
  shinyjs::hide("back_to_map")  
  
  

  # Reactive selected country and drilldown data
  
  selected_country <- reactive({
    req(input$map_country_click)
    list(
      iso = input$map_country_click$iso,
      name = input$map_country_click$name
    )
  })
  
  df_country_reactive <- reactive({
    req(selected_country())
    
    df <- us_fdi %>%
      filter(
        iso3_cp == selected_country()$iso,
        `Project status` %in% switch(input$world_project_status,
                                     "Opened" = "Opened",
                                     "Announced" = "Announced",
                                     "Opened + Announced" = c("Opened", "Announced"))
      )
    
    if (!input$world_cumulative) {
      df <- df %>% filter(lubridate::year(time) == input$world_year)
    }
    
    df %>%
      group_by(Sector) %>%
      summarise(
        value = case_when(
          input$world_metric == "Capital Investment" ~ sum(`Capital investment`, na.rm = TRUE),
          input$world_metric == "Number of Projects" ~ n(),
          input$world_metric == "Jobs Created" ~ sum(`Jobs created`, na.rm = TRUE)
        ),
        .groups = "drop"
      ) %>%
      arrange(desc(value)) %>%
      mutate(Sector = as.character(Sector))
  })
  # Map rendering function
  
  render_map <- function(df) {
    
    n_stops <- 7
    stop_colors <- c('#6d3040', '#cd624f', '#dabc41', '#e9d584', '#bcd285', '#84A76D', '#4c7d55')
    stops <- lapply(0:(n_stops-1), function(i) list(i/(n_stops-1), stop_colors[i+1]))
    
    # ----- Legend & tooltip formatting -----
    if (input$world_metric == "Capital Investment") {
      if (isTRUE(input$world_cumulative)) {
        min_val <- 0; max_val <- 100000
      } else {
        min_val <- 0; max_val <- max(df$total_value, na.rm = TRUE)
      }
      legend_formatter <- JS("
      function() {
        var val = this.value;
        if (val >= 1000) return Math.round(val/1000) + 'B';
        return Math.round(val) + 'M';
      }
    ")
      tooltip_formatter <- JS("
      function() {
        var val = this.point.value;
        if (val >= 1000) val = Highcharts.numberFormat(val/1000, 1) + 'B';
        else val = Highcharts.numberFormat(val, 0) + 'M';
        return '<b>' + this.point.name + '</b>: ' + val;
      }
    ")
      
    } else {
      min_val <- 0; max_val <- max(df$total_value, na.rm = TRUE)
      legend_formatter <- JS("
      function() {
        var val = this.value;
        if (val >= 1000) return Math.round(val,0);
        return Math.round(val);
      }
    ")
      tooltip_formatter <- JS("
      function() {
        var val = this.point.value;
        if (val >= 1000) val = Highcharts.numberFormat(val,0) ;
        else val = Highcharts.numberFormat(val, 0);
        return '<b>' + this.point.name + '</b>: ' + val;
      }
    ")
    }
    
    # ----- Highchart -----
    highchart(type = "map") %>%
      hc_add_series_map(
        worldgeojson, df,
        value = "total_value",
        joinBy = c("iso-a3", "iso3_cp"),
        name = input$world_metric,
        nullColor = "#F0F0F0",
        states = list(hover = list(enabled = TRUE, borderWidth = 3, borderColor = "#FFFFFF"))
      ) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(min = min_val, max = max_val, stops = stops,
                   labels = list(style = list(color = "#FFFFFF"), formatter = legend_formatter)) %>%
      hc_title(text = paste("World Map: FDI into the US by", input$world_metric),
               style = list(color = "#FFFFFF")) %>%
      hc_tooltip(useHTML = TRUE, formatter = tooltip_formatter) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("
              function() {
                Shiny.setInputValue('map_country_click', 
                  { iso: this.iso3_cp, name: this.name }, {priority: 'event'});
              }
            ")
            )
          )
        )
      ) %>%
      hc_legend(itemStyle = list(color = "#FFFFFF")) %>%
      hc_exporting(enabled = TRUE)
  }
  
  
  
  # Render world map or drilldown

  output$world_map_chart <- renderHighchart({
    
    if(chart_state() == "map") {
      render_map(df_world())
      
    } else if(chart_state() == "drilldown") {
      
      df_country <- df_country_reactive()
      n_sectors <- nrow(df_country)
      set.seed(123)
      random_colors <- paste0("#", sprintf("%06X", sample(0:0xFFFFFF, n_sectors)))
      
      if(input$world_metric == "Capital Investment"){
        data_label_formatter <- "function() { return Math.round(this.y) + 'M'; }"
        tooltip_formatter <- "function() { return '<b>' + this.category + '</b>: ' + Math.round(this.y) + 'M'; }"
        yaxis_labels <- FALSE         
        yaxis_title <- NULL            
      } else {
        data_label_formatter <- "function() { return this.y; }"
        tooltip_formatter <- "function() { return '<b>' + this.category + '</b>: ' + this.y; }"
        yaxis_labels <- TRUE           
        yaxis_title <- input$world_metric
      }
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(
          text = paste("Sectoral Distribution for", selected_country()$name),
          style = list(color = "#FFFFFF")
        ) %>%
        hc_xAxis(
          categories = df_country$Sector,
          labels = list(style = list(color = "#FFFFFF")),
          lineColor = "#FFFFFF",
          tickColor = "#FFFFFF"
        ) %>%
        hc_yAxis(
          title = list(
            text = if(input$world_metric == "Capital Investment") "Capital Investment" else input$world_metric,
            style = list(color = "#FFFFFF", fontWeight = "bold", fontSize = "14px")
          ),
          labels = list(
            enabled = if(input$world_metric == "Capital Investment") FALSE else TRUE,
            style = list(color = "#FFFFFF")
          ),
          gridLineColor = "#444444"
        ) %>%
        hc_add_series(
          name = if(nrow(df_country) == 1) df_country$Sector else input$world_metric,
          data = df_country$value,
          colorByPoint = TRUE,
          colors = random_colors
        ) %>%
        hc_plotOptions(
          column = list(
            dataLabels = list(
              enabled = TRUE,
              style = list(color = "#FFFFFF"),
              formatter = JS(data_label_formatter)
            )
          )
        ) %>%
        hc_tooltip(
          pointFormatter = JS(tooltip_formatter)
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_legend(itemStyle = list(color = "#FFFFFF"))
    }
  })
  
  

  # Map click -> switch to drilldown --> observeEvent 
  
  observeEvent(input$map_country_click, {
    req(input$map_country_click)
    chart_state("drilldown")
  })
  
  
  observeEvent(input$back_to_map, {
    chart_state("map")
  })
  
  observe({
    if(chart_state() == "drilldown"){
      shinyjs::show("back_to_map")
    } else {
      shinyjs::hide("back_to_map")
    }
  })
  
  
  
  
  
  #### aggregates 
  
  
  
  # Reactive for filtered data
  aggregate_data <- reactive({
    req(input$aggregate_index, input$agg_metric, input$agg_project_status)
    
    df <- us_fdi %>%
      filter(Aggregate %in% input$aggregate_index)
    
    proj_types <- input$agg_project_status
    df <- df %>% filter(`Project status` %in%
                          if ("Opened + Announced" %in% proj_types) 
                            c("Opened", "Announced") 
                        else proj_types)
    
    df
  })
  
  aggregate_summary <- reactive({
    req(aggregate_data())
    metric_col <- switch(input$agg_metric,
                         "Capital Investment" = "Capital investment",
                         "Jobs Created" = "Jobs created",
                         "Number of Projects" = "Number of Projects")  
    
    df <- aggregate_data()
    
    if(input$agg_metric == "Number of Projects") {
      df_summary <- df %>%
        group_by(Aggregate) %>%
        summarise(value = n(), .groups = "drop")
    } else {
      df_summary <- df %>%
        group_by(Aggregate) %>%
        summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
    }
    
    df_summary
  })
  
  #fixed colors for aggregates
  
  aggregate_colors <- list(
    "EU" = "#1f77b4",      
    "China" = "#ff7f0e",    
    "NAFTA (-US)" = "#2ca02c",
    "UK" = "#d62728",      
    "ASEAN" = "#9467bd",    
    "MERCOSUR" = "#8c564b", 
    "AFR" = "#e377c2",     
    "CIS" = "#7f7f7f",      
    "BRICS" = "#bcbd22"     
  )
  
  #bar chart
  
  output$agg_bar <- renderHighchart({
    req(aggregate_summary())
    
    df_summary <- aggregate_summary()
    
    series_data <- lapply(seq_len(nrow(df_summary)), function(i) {
      list(
        name = df_summary$Aggregate[i],
        y = df_summary$value[i],
        color = aggregate_colors[[df_summary$Aggregate[i]]]
      )
    })
    
    tooltip_suffix <- if(input$agg_metric == "Capital Investment") "M" else ""
    
    yaxis_formatter <- JS(
      "function() {",
      if(input$agg_metric == "Capital Investment") 
        "return this.value + 'M';" 
      else 
        "return this.value;",
      "}"
    )
    
    highchart() %>%
      hc_chart(type = "column", backgroundColor = NULL) %>%
      hc_title(text = paste("Bar Chart: ", input$agg_metric),
               style = list(color = "white")) %>%
      hc_xAxis(categories = df_summary$Aggregate,
               labels = list(style = list(color = "white")),
               lineColor = "white",
               tickColor = "white") %>%
      hc_yAxis(
        title = list(text = input$agg_metric, style = list(color = "white")),
        labels = list(style = list(color = "white"), formatter = yaxis_formatter),
        lineColor = "white",
        tickColor = "white",
        allowDecimals = TRUE
      ) %>%
      hc_legend(itemStyle = list(color = "white")) %>%
      hc_add_series(
        name = input$agg_metric,
        data = series_data
      ) %>%
      hc_tooltip(pointFormatter = JS(
        "function() {",
        paste0("return '<b>' + this.name + '</b>: ' + Math.round(this.y) + '", tooltip_suffix, "';"),
        "}"
      )) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE)  
      ))
  })
  
  #data for plot over time
  
  aggregate_time_data <- reactive({
    req(aggregate_data())
    
    df <- aggregate_data() %>%
      mutate(month_year = as.Date(format(as.Date(time), "%Y-%m-01"))) %>%  
      filter(!is.na(month_year))
    
    metric_col <- switch(input$agg_metric,
                         "Capital Investment" = "Capital investment",
                         "Jobs Created" = "Jobs created",
                         "Number of Projects" = "Number of Projects")
    
    if(input$agg_metric == "Number of Projects") {
      df_summary <- df %>%
        group_by(Aggregate, month_year) %>%
        summarise(value = n(), .groups = "drop")
    } else {
      df_summary <- df %>%
        group_by(Aggregate, month_year) %>%
        summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop")
    }
    
    df_summary
  })
  
  #plot over time
  
  output$agg_scat <- renderHighchart({
    req(aggregate_time_data())
    
    df_summary <- aggregate_time_data()
    
    series_list <- lapply(unique(df_summary$Aggregate), function(agg) {
      df_agg <- df_summary %>% filter(Aggregate == agg) %>% arrange(month_year)
      list(
        name = agg,
        data = lapply(seq_len(nrow(df_agg)), function(i) {
          list(
            x = as.numeric(df_agg$month_year[i]) * 86400000, 
            y = df_agg$value[i]
          )
        }),
        color = aggregate_colors[[agg]]
      )
    })
    
    tooltip_suffix <- if(input$agg_metric == "Capital Investment") "M" else ""
    
    yaxis_formatter <- JS(
      "function() {",
      if(input$agg_metric == "Capital Investment") 
        "return this.value + 'M';" 
      else 
        "return this.value;",
      "}"
    )
    
    highchart() %>%
      hc_chart(type = "line", backgroundColor = "white") %>%
      hc_title(text = paste(input$agg_metric, " over time"),
               style = list(color = "black")) %>%
      hc_xAxis(type = "datetime",
               labels = list(style = list(color = "black")),
               lineColor = "black",
               tickColor = "black") %>%
      hc_yAxis(title = list(text = input$agg_metric, style = list(color = "black")),
               labels = list(style = list(color = "black"), formatter = yaxis_formatter),
               lineColor = "black",
               tickColor = "black") %>%
      hc_legend(itemStyle = list(color = "black")) %>%
      hc_add_series_list(series_list) %>%
      hc_tooltip(
        xDateFormat = "%Y-%m",
        pointFormatter = JS(
          "function() {",
          paste0("return '<b>' + this.series.name + '</b>: ' + Math.round(this.y) + '", tooltip_suffix, "';"),
          "}"
        )
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_plotOptions(line = list(
        marker = list(enabled = TRUE)
      ))
  })
  
  
  # data for sankey
  
  aggregate_sankey_data <- reactive({
    req(aggregate_data())
    
    df <- aggregate_data()
    
    sector_totals <- df %>%
      group_by(Sector) %>%
      summarise(value = n(), .groups = "drop") %>%  
      arrange(desc(value)) %>%
      slice_head(n = 8)  
    
    top_sectors <- sector_totals$Sector
    
    df_sankey <- df %>%
      filter(Sector %in% top_sectors) %>%
      group_by(Aggregate, Sector) %>%
      summarise(value = n(), .groups = "drop")
    
    df_sankey
  })
  
  
  #sankey plot
  
  output$agg_sankey <- renderHighchart({
    req(aggregate_sankey_data())
    
    df_sankey <- aggregate_sankey_data()
    
    sector_palette <- c("#ff9999","#66b3ff","#99ff99","#ffcc99",
                        "#c2c2f0","#ffb3e6","#c2f0c2","#f0b3b3")
    sector_colors <- setNames(sector_palette[seq_len(n_distinct(df_sankey$Sector))],
                              unique(df_sankey$Sector))
    
    nodes <- unique(c(df_sankey$Aggregate, df_sankey$Sector))
    
    node_list <- lapply(nodes, function(n) {
      list(
        id = n,
        color = ifelse(n %in% names(aggregate_colors), aggregate_colors[[n]],
                       sector_colors[[n]]),
        dataLabels = list(enabled = TRUE)
      )
    })
    
    links <- lapply(seq_len(nrow(df_sankey)), function(i) {
      list(
        from = df_sankey$Aggregate[i],
        to = df_sankey$Sector[i],
        weight = df_sankey$value[i]
      )
    })
    
    highchart() %>%
      hc_chart(type = "sankey", backgroundColor = "white") %>%
      hc_title(text = "Aggregates → Top 8 Sectors Flow", style = list(color = "black")) %>%
      hc_add_series(
        keys = c("from", "to", "weight"),
        type = "sankey",
        nodes = node_list,
        data = links,
        dataLabels = list(enabled = TRUE, color = "black")
      ) %>%
      hc_tooltip(pointFormatter = JS(
        "function() { return this.from + ' → ' + this.to + ': ' + this.weight; }"
      )) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_plotOptions(sankey = list(
        nodeWidth = 40  
      ))
  })
  
  #Sectoral Analysis
  
  sector_data <- reactive({

  # Base dataset
  df <- us_fdi %>% mutate(time = as.Date(time),
                          month_str = format(time, "%Y-%m"))
  
  # Filter by project status
  df <- switch(input$sector_project_status,
               "Opened" = df %>% filter(`Project status` == "Opened"),
               "Announced" = df %>% filter(`Project status` == "Announced"),
               "Opened + Announced" = df
  )
  
  # Filter by month range safely (strings YYYY-MM)
  req(input$sector_start, input$sector_end)
  start_month <- format(as.Date(paste0(input$sector_start, "-01")), "%Y-%m")
  end_month   <- format(as.Date(paste0(input$sector_end, "-01")), "%Y-%m")
  
  df <- df %>%
    filter(month_str >= start_month, month_str <= end_month)
  
  # Choose dimension
  dim_col <- switch(input$sector_dimension,
                    "Sector" = "Sector",
                    "Sub-sector" = "Sub-sector",
                    "Cluster" = "Cluster",
                    "Activity" = "Activity")
  
  df <- df %>% mutate(dim_label = .data[[dim_col]])
  
  # Aggregate by metric
  df <- switch(input$sector_metric,
               "Capital Investment" = df %>%
                 group_by(dim_label) %>%
                 summarise(value = sum(`Capital investment`, na.rm = TRUE), .groups = "drop") %>%
                 mutate(y_title = "Capital Investment (M USD)",
                        tooltip_suffix = " M"),
               
               "Number of Projects" = df %>%
                 group_by(dim_label) %>%
                 summarise(value = n(), .groups = "drop") %>%
                 mutate(y_title = "Number of Projects",
                        tooltip_suffix = ""),
               
               "Jobs Created" = df %>%
                 group_by(dim_label) %>%
                 summarise(value = sum(`Jobs created`, na.rm = TRUE), .groups = "drop") %>%
                 mutate(y_title = "Jobs Created",
                        tooltip_suffix = "")
  )
  
  # Top N and order
  df <- df %>%
    arrange(desc(value)) %>%
    slice_head(n = input$sector_top_n) %>%
    mutate(dim_label = factor(dim_label, levels = rev(dim_label)))
  
  df
}) %>% bindCache(input$sector_metric, input$sector_project_status,
                 input$sector_top_n, input$sector_start, input$sector_end,
                 input$sector_dimension)

  output$sector_chart <- renderHighchart({
    
    df <- sector_data()
    if (nrow(df) == 0) return(NULL)
    
    y_title <- unique(df$y_title)
    tooltip_suffix <- unique(df$tooltip_suffix)
    x_label <- input$sector_dimension
    
    # Only line/cumulative charts first
    if (input$sector_chart_type %in% c("line_time", "cum_line")) {
      
      # Filter us_fdi only to Top N dims from sector_data
      top_dims <- df$dim_label
      
      df_time <- us_fdi %>%
        mutate(time = as.Date(time),
               month_str = format(time, "%Y-%m"),
               dim_label = .data[[switch(input$sector_dimension,
                                         "Sector" = "Sector",
                                         "Sub-sector" = "Sub-sector",
                                         "Cluster" = "Cluster",
                                         "Activity" = "Activity")]]) %>%
        filter(
          month_str >= format(as.Date(paste0(input$sector_start, "-01")), "%Y-%m"),
          month_str <= format(as.Date(paste0(input$sector_end, "-01")), "%Y-%m"),
          dim_label %in% top_dims
        ) %>%
        {switch(input$sector_project_status,
                "Opened" = filter(., `Project status` == "Opened"),
                "Announced" = filter(., `Project status` == "Announced"),
                "Opened + Announced" = .)}
      
      df_ts <- switch(input$sector_metric,
                      "Capital Investment" = df_time %>%
                        group_by(month_str, dim_label) %>%
                        summarise(value = sum(`Capital investment`, na.rm = TRUE), .groups = "drop"),
                      
                      "Number of Projects" = df_time %>%
                        group_by(month_str, dim_label) %>%
                        summarise(value = n(), .groups = "drop"),
                      
                      "Jobs Created" = df_time %>%
                        group_by(month_str, dim_label) %>%
                        summarise(value = sum(`Jobs created`, na.rm = TRUE), .groups = "drop")
      )
      
      all_months <- seq.Date(
        from = as.Date(paste0(input$sector_start, "-01")),
        to   = as.Date(paste0(input$sector_end, "-01")),
        by   = "month"
      )
      all_months_str <- format(all_months, "%Y-%m")
      
      df_ts <- df_ts %>%
        tidyr::complete(dim_label, month_str = all_months_str, fill = list(value = 0)) %>%
        arrange(dim_label, month_str)
      
      if (input$sector_chart_type == "cum_line") {
        df_ts <- df_ts %>%
          group_by(dim_label) %>%
          arrange(month_str) %>%
          mutate(value = cumsum(value)) %>%
          ungroup()
      }
      
      series_list <- lapply(unique(df_ts$dim_label), function(lbl) {
        df_temp <- df_ts %>% filter(dim_label == lbl)
        list(name = lbl, data = list_parse2(df_temp %>% select(x = month_str, y = value)))
      })
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_add_series_list(series_list) %>%
        hc_colors(my_colors[seq_along(unique(df_ts$dim_label))]) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(
          text = paste0(
            x_label, " over time - ", input$sector_metric,
            " (", format(as.Date(paste0(input$sector_start, "-01")), "%Y-%m"),
            " to ", format(as.Date(paste0(input$sector_end, "-01")), "%Y-%m"), ")"
          )
        ) %>%
        hc_xAxis(categories = all_months_str, title = list(text = "Month")) %>%
        hc_yAxis(title = list(text = y_title)) %>%
        hc_tooltip(shared = TRUE) %>%
        hc_plotOptions(line = list(dataLabels = list(enabled = FALSE))) %>%
        hc_exporting(enabled = TRUE)
    }else {
      if (input$sector_chart_type == "bar") {
        # --- Bar chart ---
        hchart(df, "bar", hcaes(x = dim_label, y = value)) %>%
          hc_colors(my_colors) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text = paste0("Top ", input$sector_top_n, " ", x_label, "s by ", input$sector_metric,
                                 " - ", input$sector_project_status,
                                 " (", format(as.Date(paste0(input$sector_start, "-01")), "%Y-%m"),
                                 " to ", format(as.Date(paste0(input$sector_end, "-01")), "%Y-%m"), ")")) %>%
          hc_xAxis(title = list(text = x_label)) %>%
          hc_yAxis(title = list(text = y_title)) %>%
          hc_tooltip(
            useHTML = TRUE,
            pointFormatter = JS(paste0("
          function() {
            return '<b>' + this.category + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
          }
        "))
          ) %>%
          hc_plotOptions(series = list(colorByPoint = TRUE, dataLabels = list(enabled = TRUE, format = "{point.y:,.0f}"))) %>%
          hc_exporting(enabled = TRUE)
        
      } else if (input$sector_chart_type == "pie") {
        # --- Pie chart ---
        hchart(df, "pie", hcaes(name = dim_label, y = value)) %>%
          hc_colors(my_colors) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text = paste0("Top ", input$sector_top_n, " ", x_label, "s by ", input$sector_metric,
                                 " - ", input$sector_project_status,
                                 " (", format(as.Date(paste0(input$sector_start, "-01")), "%Y-%m"),
                                 " to ", format(as.Date(paste0(input$sector_end, "-01")), "%Y-%m"), ")")) %>%
          hc_tooltip(
            useHTML = TRUE,
            pointFormatter = JS(paste0("
          function() {
            return '<b>' + this.name + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
          }
        "))
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                formatter = JS(paste0("
              function() {
                return '<b>' + this.point.name + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
              }
            "))
              )
            )
          ) %>%
          hc_exporting(enabled = TRUE)
      }
    }
    
  })
  
  
  ### country analysis
  
  # --- Reactive filtered dataset for country analysis ---
  country_data <- reactive({
    
    df <- us_fdi %>% 
      mutate(time = as.Date(time),
             month_str = format(time, "%Y-%m"))
    
    # Filter by project status
    df <- switch(input$country_project_status,
                 "Opened" = df %>% filter(`Project status` == "Opened"),
                 "Announced" = df %>% filter(`Project status` == "Announced"),
                 "Opened + Announced" = df
    )
    
    # Filter by month range
    req(input$country_start, input$country_end)
    start_month <- format(as.Date(paste0(input$country_start, "-01")), "%Y-%m")
    end_month <- format(as.Date(paste0(input$country_end, "-01")), "%Y-%m")
    
    df <- df %>% filter(month_str >= start_month, month_str <= end_month)
    
    # Country dimension
    df <- df %>% mutate(dim_label = `Source country`)
    
    # Aggregate by metric
    df <- switch(input$country_metric,
                 "Capital Investment" = df %>%
                   group_by(dim_label) %>%
                   summarise(value = sum(`Capital investment`, na.rm=TRUE), .groups="drop") %>%
                   mutate(y_title="Capital Investment (M USD)", tooltip_suffix=" M"),
                 
                 "Number of Projects" = df %>%
                   group_by(dim_label) %>%
                   summarise(value = n(), .groups="drop") %>%
                   mutate(y_title="Number of Projects", tooltip_suffix=""),
                 
                 "Jobs Created" = df %>%
                   group_by(dim_label) %>%
                   summarise(value = sum(`Jobs created`, na.rm=TRUE), .groups="drop") %>%
                   mutate(y_title="Jobs Created", tooltip_suffix="")
    )
    
    # Top N countries
    df <- df %>% 
      arrange(desc(value)) %>% 
      slice_head(n=input$country_top_n) %>%
      mutate(dim_label = factor(dim_label, levels=rev(dim_label)))
    
    df
  }) %>% bindCache(input$country_metric, input$country_project_status,
                   input$country_top_n, input$country_start, input$country_end)
  
  # --- Render chart ---
  output$country_chart <- renderHighchart({
    
    df <- country_data()
    if(nrow(df) == 0) return(NULL)
    
    y_title <- unique(df$y_title)
    tooltip_suffix <- unique(df$tooltip_suffix)
    x_label <- "Source country"
    
    # --- Only line/cumulative charts need time series ---
    if (input$country_chart_type %in% c("line_time", "cum_line")) {
      
      top_countries <- df$dim_label
      
      df_ts <- us_fdi %>%
        mutate(time = as.Date(time),
               month_str = format(time, "%Y-%m"),
               dim_label = `Source country`) %>%
        filter(
          month_str >= format(as.Date(paste0(input$country_start, "-01")), "%Y-%m"),
          month_str <= format(as.Date(paste0(input$country_end, "-01")), "%Y-%m"),
          dim_label %in% top_countries
        ) %>%
        {switch(input$country_project_status,
                "Opened" = filter(., `Project status` == "Opened"),
                "Announced" = filter(., `Project status` == "Announced"),
                "Opened + Announced" = .)}
      
      # --- Aggregate by metric ---
      df_ts <- switch(input$country_metric,
                      "Capital Investment" = df_ts %>% 
                        group_by(month_str, dim_label) %>% 
                        summarise(value = sum(`Capital investment`, na.rm = TRUE), .groups = "drop"),
                      
                      "Number of Projects" = df_ts %>% 
                        group_by(month_str, dim_label) %>% 
                        summarise(value = n(), .groups = "drop"),
                      
                      "Jobs Created" = df_ts %>% 
                        group_by(month_str, dim_label) %>% 
                        summarise(value = sum(`Jobs created`, na.rm = TRUE), .groups = "drop")
      )
      
      # --- Ensure all months and countries are present ---
      all_months <- seq.Date(
        from = as.Date(paste0(input$country_start, "-01")),
        to   = as.Date(paste0(input$country_end, "-01")),
        by   = "month"
      )
      all_months_str <- format(all_months, "%Y-%m")
      
      df_ts <- df_ts %>%
        tidyr::complete(dim_label, month_str = all_months_str, fill = list(value = 0)) %>%
        arrange(dim_label, month_str)
      
      # --- Cumulative if needed ---
      if (input$country_chart_type == "cum_line") {
        df_ts <- df_ts %>%
          group_by(dim_label) %>%
          arrange(month_str) %>%
          mutate(value = cumsum(value)) %>%
          ungroup()
      }
      
      # --- Series for highchart ---
      series_list <- lapply(unique(df_ts$dim_label), function(lbl) {
        df_temp <- df_ts %>% filter(dim_label == lbl)
        list(name = lbl, data = list_parse2(df_temp %>% select(x = month_str, y = value)))
      })
      
      # --- Render Highchart ---
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_add_series_list(series_list) %>%
        hc_colors(my_colors[seq_along(unique(df_ts$dim_label))]) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(
          text = paste0(
            "Source country over time - ", input$country_metric,
            " (", format(as.Date(paste0(input$country_start, "-01")), "%Y-%m"),
            " to ", format(as.Date(paste0(input$country_end, "-01")), "%Y-%m"), ")"
          )
        ) %>%
        hc_xAxis(categories = all_months_str, title = list(text = "Month")) %>%
        hc_yAxis(title = list(text = y_title)) %>%
        hc_tooltip(shared = TRUE) %>%
        hc_plotOptions(line = list(dataLabels = list(enabled = FALSE))) %>%
        hc_exporting(enabled = TRUE)
      
    }else {
      x_label <- "Source Country"  
      if(input$country_chart_type == "bar") {
        # --- Bar chart ---
        hchart(df, "bar", hcaes(x=dim_label, y=value)) %>%
          hc_colors(my_colors) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text=paste0("Top ", input$country_top_n, " Source countries by ", input$country_metric,
                               " - ", input$country_project_status,
                               " (", format(as.Date(paste0(input$country_start, "-01")), "%Y-%m"),
                               " to ", format(as.Date(paste0(input$country_end, "-01")), "%Y-%m"), ")")) %>%
          hc_xAxis(title=list(text=x_label)) %>%
          hc_yAxis(title=list(text=y_title)) %>%
          hc_tooltip(useHTML=TRUE, pointFormatter=JS(paste0("
        function() {
          return '<b>' + this.category + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
        }
      "))) %>%
          hc_plotOptions(series=list(colorByPoint=TRUE, dataLabels=list(enabled=TRUE, format="{point.y:,.0f}"))) %>%
          hc_exporting(enabled=TRUE)
        
      } else if(input$country_chart_type == "pie") {
        # --- Pie chart ---
        hchart(df, "pie", hcaes(name=dim_label, y=value)) %>%
          hc_colors(my_colors) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text=paste0("Top ", input$country_top_n, " Source countries by ", input$country_metric,
                               " - ", input$country_project_status,
                               " (", format(as.Date(paste0(input$country_start, "-01")), "%Y-%m"),
                               " to ", format(as.Date(paste0(input$country_end, "-01")), "%Y-%m"), ")")) %>%
          hc_tooltip(useHTML=TRUE, pointFormatter=JS(paste0("
        function() {
          return '<b>' + this.name + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
        }
      "))) %>%
          hc_plotOptions(pie=list(
            allowPointSelect=TRUE,
            cursor="pointer",
            dataLabels=list(
              enabled=TRUE,
              formatter=JS(paste0("
            function() {
              return '<b>' + this.point.name + '</b>: ' + Highcharts.numberFormat(this.y, 0) + '", tooltip_suffix, "';
            }
          "))
            )
          )) %>%
          hc_exporting(enabled=TRUE)
      }
    }
  })
  
  
} 


#### launch app

shinyApp(ui, server)


