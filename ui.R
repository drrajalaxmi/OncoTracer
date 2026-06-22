#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)

# source("R/sideBarPanel.R")

sidebar_filter <- createSideBarFilterModule()

ui <- navbarPage(id = "multipage",
  "OncoTracer", 
  tags$head(
    tags$style(HTML("
      /* Style for the cancer type select input on first tab */
      #mod1-cancerType + .selectize-control .selectize-input,
      #mod1-cancerType {
        font-size: 18px !important;
        height: 50px !important;
        padding: 10px  !important;
        width: 400px !important;

      }
      
      /* Style for the dropdown items */
      #mod1-cancerType + .selectize-control .selectize-dropdown {
        font-size: 16px !important;
      }
      
      /* Style for the dropdown options */
      #mod1-cancerType + .selectize-control .selectize-dropdown .option {
        padding: 10px 15px !important;
      }
      
      /* Style for the label */
      label[for='mod1-cancerType'] {
        font-size: 20px !important;
        font-weight: bold !important;
        color: #2c3e50 !important;
        margin-bottom: 15px !important;
      }
      
      /* Style for the Go button */
      #go {
        font-size: 18px !important;
        padding: 12px 30px !important;
        border-radius: 8px !important;
      }
    "))
  ),
  tabPanel(value="selection", "Cancer Type",
           
           div(style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 50vh;",
               # div(style = "display: flex; flex-direction: column; align-items: center;",

  cancerTypeInput("mod1"),
  br(),
  actionButton("go", "Go to Data")
))
,
  tabPanel(value="dataPage",
           #title = "Data", # Title of the navigation bar
           textOutput("tab_title", inline = TRUE), 
           fluidPage(
             tags$style(type='text/css', 
                        ".selectize-input, .selectize-dropdown, .checkbox-inline, .checkbox label  {
          font-size: 10px;
          line-height: 10px;
        }label.control-label {
          font-size: 12px; /* Adjust the size as needed */
        } .multicol {
        -webkit-column-count: 2; /* Chrome, Safari, Opera */
        -moz-column-count: 2;    /* Firefox */
        column-count: 2;         /* Standard syntax */
      }.vis-item {font-size: 8pt;
                padding: 0px;}
    .vis-timeline {
  font-size: 10px;
}"
             )
             ,
             sidebarLayout(
               sidebarPanel(
                 width = 3, 
                 actionButton("update", "Update Plot"),
                 
                 ### from module sideBarPanel.R
                 sidebar_filter$sideBarUI("side1")
                 
               ),
               mainPanel(
                 width = 9, 
                 tabsetPanel(id = "outer_tabs",
                   tabPanel("Statistics",
                            br(),
                            br(),
                            tags$div(
                              style = "display: flex; flex-direction: row; gap: 15px; width: 100%;",
                              plotColumn("plot_disease_status", "Disease Status", 300),
                              plotColumn("plot_age", "Age Distribution", 300),
                              plotColumn("plot_path_stage", "Pathological Stage", 300)),
                              br(),
                              br(),
                              tags$div(
                                style = "display: flex; flex-direction: row; gap: 15px; width: 100%;",
                              plotColumn("plot_histo", "Histology", 300),
                              plotColumn("plot_numRegim", "Number of Regimens", 300)
                              # plotColumn("plot_path_stage", "Pathological Stage", 300)
                            )
                   ),
                   tabPanel("Medication",
                            br(),br(),
                            tabsetPanel(id = "inner_meds",
                          tabPanel("Sankey plot",      
                            tags$div(plotly::plotlyOutput("sankey1", height = "600px"))),
                          tabPanel("Medication Combinations",
                                   tags$div(plotOutput("medprint", height = "600px"))),
                          tabPanel("Medication plot",
                            tags$div(plotly::plotlyOutput("sankey2", height = "600px" )),
                   ))
                   ),
                   tabPanel("Tumor Markers",
                            column(width = 4,
                            uiOutput("tumorMarker_category_bl")),
                            column(width = 8,
                            uiOutput("tumorMarker_bl")),
                            br(), br(),
                            tags$div(plotOutput("tumorMarker_print", height = "700px"))
                   ),
                   tabPanel("Time Line",
                            column(width = 4,
                            uiOutput("tumorMarker_timeline_bl")),
                            column(width = 8,
                            uiOutput("timeLineKey_bl")),
                          tabsetPanel(id = "inner_tabs",
                            tabPanel("Timeline plot",
                                     br(),
                                     br(),
                                     tags$div(uiOutput("timelines_bl"))
                            ),
                            tabPanel("Swimlane plot",
                                     br(),
                                     br(),
                                     tags$div(plotlyOutput("swimmer_plot"))
                            )
                            )),
                   tabPanel("Cachexia",
                            uiOutput("cachexiaKey"),
                            uiOutput("cachexiaTest"),
                            br(),
                            tags$div(uiOutput("cachexia")) 

                   )
                 )
               )
    
  # tableOutput("result1"),
  # tableOutput("result")
    ))

)
)


