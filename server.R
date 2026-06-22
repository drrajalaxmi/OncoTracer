#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(easyalluvial)
suppressPackageStartupMessages( require(parcats) )
# library(networkD3)
library(plotly)
library(ComplexHeatmap)
library(timevis)
library(RColorBrewer)
library(stringr)
library(dbscan)



sidebar_filter <- createSideBarFilterModule()

function(input, output, session) {
  hideTab(inputId = "multipage", target = "dataPage")
  # Initialize reactValue
  reactValue <- reactiveValues(
    nodes = NULL,
    links = NULL,
    colors_gg = NULL,
    data_bl_subset = NULL
  )
  
  # Pass data as a reactive
  data_list <- reactive({ dat_list})
  
  ### Call module load_data.R
  output$tab_title <- renderText({
    req(input$`mod1-cancerType`)
    paste(input$`mod1-cancerType`)
  })
  # Observe the go button with validation
  observeEvent(input$go, {
    cancer_selected <- input$`mod1-cancerType`
    
    # Validate selection
    if (is.null(cancer_selected) || cancer_selected == "") {
      showNotification(
        "Please select a cancer type first",
        type = "error",
        duration = 3
      )
      return()
    }
    
    # Show the data page tab
    showTab(inputId = "multipage", target = "dataPage")
    
    # Navigate to data page
    updateNavbarPage(session, "multipage", selected = "dataPage")
  })

  filtered <- diseaseServer("mod1", data_list)
  
  ### from module sideBarPanel.R
  sideServer<- sidebar_filter$sideBarServer("side1", filtered$data_bl)
  
  ### from module ObserverModule.R
  reactValue <- ObserverModule(
    input = input,
    output = output,
    session = session,
    data_bl = filtered$data_bl,  # Your data frame
    filters = reactive({               # Your filter inputs
      list(
        selectAge = input$`side1-age`,
        currentDiseaseStatus = input$`side1-disease_status`,
        medLineRegimen = input$`side1-regimen`,
        selectHistology = input$`side1-histology`,
        selectPathGroupStage = input$`side1-path_stage`,
        selectTarget = input$`side1-med_Targeted Therapy`,
        selectImm = input$`side1-med_Immunotherapy`,
        selectChemo = input$`side1-med_Chemotherapy`,
        selectOther = input$`side1-med_Others`
      )
    }),
    reactValue = reactValue  # Your reactive values object
  )
  
  
### barplots from bar_plots.R
output$plot_disease_status <- renderPlotly({
    req(reactValue$data_bl_subset)
    plot_ly(#height = 400,width = 950,
      data= reactValue$data_bl_subset %>%
        select(Key, CurrentDiseaseStatus) %>% unique() %>%
        mutate(CurrentDiseaseStatus = factor(CurrentDiseaseStatus,levels = c("Active Disease","No Evidence of Disease", "Unknown"))),
      x=~CurrentDiseaseStatus, type = 'histogram', texttemplate = "%{y}",textposition = 'outside',
      color = ~CurrentDiseaseStatus, colors = c("grey" ,'#BF382A','#0C4B8E'))%>%
      layout(legend = list( orientation = "h",   xanchor = "center",  yanchor = "bottom",  y = 1, x = 0.5    ),
             xaxis = list(title ="",tickangle = 25))
  }) %>% bindEvent(input$update)
output$plot_age <- renderPlotly({ quickBarPlot(reactValue$data_bl_subset, "AgeAtDiagnosis", 
                                            title = "Age Distribution") }) %>% bindEvent(input$update)
output$plot_path_stage <- renderPlotly({ quickBarPlot(reactValue$data_bl_subset, "PathGroupStage", 
                                                          title = "Pathological Stage") }) %>% bindEvent(input$update)


output$plot_histo <-renderPlotly({ quickBarPlot(reactValue$data_bl_subset, "Histology", 
                                                    title = "Histology") }) %>% bindEvent(input$update)
output$plot_numRegim <-renderPlotly({ 
  req(reactValue$data_bl_subset)
  plot_ly(data= reactValue$data_bl_subset %>%
                    select(Key, MedLineRegimen, CurrentDiseaseStatus) %>% unique() %>%
                    group_by(Key, CurrentDiseaseStatus) %>%
                    summarise(NumOfRegim= length(MedLineRegimen)) %>%
                    mutate(CurrentDiseaseStatus = factor(CurrentDiseaseStatus,levels = c("Active Disease","No Evidence of Disease", "Unknown"))),
                  x=~NumOfRegim, type = 'histogram', 
                  texttemplate = "%{y}",textposition = 'outside',showlegend = FALSE,
                  color = ~CurrentDiseaseStatus, colors = c("grey" ,'#BF382A','#0C4B8E')
  ) %>%
    layout(title ="Number of Regimens",xaxis = list(title =""))  
}) %>% bindEvent(input$update)
  
  
### chachexia.R

output$cachexiaKey <-renderUI({
     req(cachexia_event())
    key <-  unique(cachexia_event()$AvatarKey)

  selectizeInput("selectCachexiaKey", "Patient ID (Max 6)",
                 choices = key,
                 selected = if(length(key)>5){ key[1:5] } else {key},
                 multiple = TRUE,
                 options = list(maxItems = 10))
}) 

output$cachexiaTest <-renderUI({
  req(cachexia_event())
  key <-  unique(cachexia_event()$AvatarKey)
  test_list <- labs %>% filter(AvatarKey %in% key) 
  test_list <- unique(test_list$LabTest)
  desried <- c("Hemoglobin Level", "Hematocrit %", "Albumin (Serum)", "Protein (Total)" ,"Creatinine (Serum)" , "C Reactive") 
  
    selectizeInput("selectCachexiaTest", "Patient ID (Max 6)",
                 choices = test_list,
                 selected = desried[desried %in% test_list],
                 multiple = TRUE,
                 options = list(maxItems = 7))
}) 

cachexia_event <- reactive({adaptive_peak_analysis(data_list()$physical, reactValue$data_bl_subset,  
                                                   min_peak_distance = 90, threshold = 5.0) })

output$cachexia <- renderUI({
  cachexia_plot(data_list()$physical, data_list()$labs, data_list()$medication, 
                cachexia_event(), input$selectCachexiaKey, input$selectCachexiaTest )
})


### timeline.R 
output$tumorMarker_timeline_bl <- renderUI({
    selectInput("selecttumorMarker_timeline_bl", "Tumor Marker",
                choices = sort(unique(filtered$tumorMarker_bl()$TMarkerTest[filtered$tumorMarker_bl()$AvatarKey %in%  unique(reactValue$data_bl_subset$Key)])),
                # selected = tumorMarker_bl %>% filter(AvatarKey %in%  unique(reactValue$data_bl_subset$Key))%>% 
                # count(TMarkerTest) %>% arrange(desc(n))%>% head(n=10) %>% .$TMarkerTest,
                selected = NULL,
                multiple = TRUE)
  })
  
  output$timeLineKey_bl <- renderUI({
    if(!is.null(input$selecttumorMarker_timeline_bl)) {
      key <-filtered$tumorMarker_bl()$AvatarKey[filtered$tumorMarker_bl()$TMarkerTest %in% input$selecttumorMarker_timeline_bl]
    } else{
      key <- reactValue$data_bl_subset$Key
    }

    selectizeInput("selecttimeLineKey_bl", "Patient ID (Max 6)",
                choices = key,
                selected = if(length(key)>5){ key[1:5] } else {key},
                multiple = TRUE,
                options = list(maxItems = 10))
  }) %>%
    bindEvent(input$selecttumorMarker_timeline_bl, input$update)
  
  
 ##timevis plot 
output$timelines_bl <- renderUI({  
  timeline_plot(data_list(), filtered$diagnosis_bl(), reactValue$data_bl_subset$Key, 
                        input$selecttumorMarker_timeline_bl, input$selecttimeLineKey_bl  )
    
}) %>%
  bindEvent(input$update, input$selecttimeLineKey_bl)

 ## swimline plot
output$swimmer_plot <- renderPlotly({
  
  swimmer_plot( data_list(), filtered, reactValue$data_bl_subset$Key, 
                input$selecttumorMarker_timeline_bl  )
  
}) %>%
  bindEvent(input$update, input$selecttimeLineKey_bl)


## medications.R
output$sankey1 <- renderPlotly({
  
  fig_bl <- plot_ly(
    type = "sankey",
    domain = list(
      x =  c(0,0.9),
      y =  c(0,1)),
    height = 600,
    orientation = "h",
    node = list(
      label = str_wrap(reactValue$nodes$name, 10),
      color = reactValue$nodes$color,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5)),
    link = list(
      source = reactValue$links$source,
      target = reactValue$links$target,
      value = reactValue$links$value,
      color = paste0(reactValue$links$color)))
  
  fig_bl <- fig_bl %>% layout(
    title = paste0(""),
    font = list( size = 10))
  fig_bl <- fig_bl %>% layout(
    font = list(size = 10) ,
    xaxis = list(showgrid = T, zeroline = T),
    yaxis = list(showgrid = F, zeroline = F))
  
  fig_bl
}) %>%
  bindEvent(input$update)

output$sankey2 <- renderPlotly({

    sankey_plot( reactValue )
  
}) %>%
  bindEvent(input$update)

output$medprint <- renderPlot({
  df_oncoplot <- reactValue$data_bl_filters %>% 
    select(Key, Medication, CurrentDiseaseStatus) %>% unique() %>%
    pivot_wider(id_cols = c(Medication), names_from = Key, values_from = CurrentDiseaseStatus,values_fill = NA ) %>%
    column_to_rownames("Medication") %>% as.matrix()
  print_plot(df_oncoplot, reactValue)
  
}) %>%
  bindEvent(input$update)


## tumorMarker.R
output$tumorMarker_category_bl <- renderUI({
  selectInput("selecttumorMarker_category_bl", "Tumor Marker Category",
              choices = sort(unique(filtered$tumorMarker_bl()$Category[filtered$tumorMarker_bl()$AvatarKey %in%  unique(reactValue$data_bl_subset$Key)])),
              selected = NULL,
              multiple = TRUE)
})

output$tumorMarker_bl<- renderUI({
  selectInput("selecttumorMarker_bl", "Tumor Marker",
              choices = sort(unique(filtered$tumorMarker_bl()$TMarkerTest[filtered$tumorMarker_bl()$AvatarKey %in%  unique(reactValue$data_bl_subset$Key)])),
              selected = NULL,
              multiple = TRUE)
})


output$tumorMarker_print <- renderPlot({

  df_marker_oncoplot <- filtered$tumorMarker_bl() %>% 
    filter(AvatarKey %in% unique(reactValue$data_bl_subset$Key)) %>%
    left_join(filtered$outcome_bl(), by = "AvatarKey") %>%
    mutate(CurrentDiseaseStatus = ifelse(is.na(CurrentDiseaseStatus), "Unknown", CurrentDiseaseStatus)) %>%
    select(AvatarKey, TMarkerTest, CurrentDiseaseStatus, Category) %>% unique()

  if(!is.null(input$selecttumorMarker_category_bl)) {
    df_marker_oncoplot <- df_marker_oncoplot %>%
       filter(Category %in% input$selecttumorMarker_category_bl)} 
  
  if(!is.null(input$selecttumorMarker_bl)) {
    df_marker_oncoplot <- df_marker_oncoplot %>%
      filter(TMarkerTest %in% input$selecttumorMarker_bl) } 
  
  df_marker_oncoplot <- df_marker_oncoplot %>%
    pivot_wider(id_cols = c(TMarkerTest), names_from = AvatarKey, values_from = CurrentDiseaseStatus,values_fill = NA ) %>%
    column_to_rownames("TMarkerTest") %>% as.matrix()

    print_plot(df_marker_oncoplot, reactValue)
  
}) %>%
  bindEvent(input$update)


output$blca_sumStat_marker <- renderPlotly({
  
  
  
}) %>%
  bindEvent(input$update)












}