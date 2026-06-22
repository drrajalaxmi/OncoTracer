### sidebar panels and filters

createSideBarFilterModule <- function() {
  
  list(
  sideBarUI = function(id) {
      ns <- NS(id)
      
      tagList(
        selectInput(ns("age"), "Age", choices = NULL, multiple = TRUE),
        selectInput(ns("disease_status"), "Disease Status", choices = NULL, multiple = TRUE),
        selectInput(ns("regimen"), "Regimen", choices = NULL, multiple = TRUE),
        wellPanel(
          tags$div(class = "multicol", 
        checkboxGroupInput(ns("histology"), "Histology", choices = NULL)
      )),
        selectizeInput(ns("path_stage"), "Pathology Stage", choices = NULL, multiple = TRUE),
        hr(),
        uiOutput(ns("med_filters"))
      )
  },
  
    
  sideBarServer = function(id, data) {
      moduleServer(id, function(input, output, session) {
        
        
        # Update basic filters
        observe({
          req(data())
          
          updateSelectInput(session, "age", 
                            choices = rev(levels(data()$AgeAtDiagnosis)),
                            selected = rev(levels(data()$AgeAtDiagnosis)))
          
          updateSelectInput(session, "disease_status",
                            choices = unique(data()$CurrentDiseaseStatus),
                            selected = unique(data()$CurrentDiseaseStatus))
          
          updateSelectInput(session, "regimen",
                            choices = levels(data()$MedLineRegimen),
                            selected = c(head(MedLineRegimenlevels)[1:5], tail(MedLineRegimenlevels)[1:3])
                            )
                            
          updateCheckboxGroupInput(session, "histology",
                                   choices = levels(data()$Histology))
          
          updateSelectizeInput(session, "path_stage",
                               choices = levels(data()$PathGroupStage),
                               selected = levels(data()$PathGroupStage))
        })
    
    
    # Apply basic filters
    basic_filtered <- reactive({
      req(data(), input$age, input$disease_status, input$histology, input$path_stage, input$regimen)
      
      data() %>%
        filter(AgeAtDiagnosis %in% input$age) %>%
        filter(CurrentDiseaseStatus %in% input$disease_status) %>%
        filter(Histology %in% input$histology) %>%
        filter(MedLineRegimen %in% input$regimen)  %>%
        filter(PathGroupStage %in% input$path_stage)
    })
  
    # Generate medication filters
    output$med_filters <- renderUI({
      req(basic_filtered())
      
      ns <- session$ns
      filters <- list()
      
      for (cat_name in sort(unique(basic_filtered()$Category))) {
        meds <- sort(unique(
          basic_filtered()$Medication[ basic_filtered()$Category == cat_name ]
        ))
        
        filters[[cat_name]] <- selectInput(
          ns(paste0("med_", cat_name)),
          paste0(cat_name),
          choices = meds,
          selected = meds,
          multiple = TRUE
        )
      }

      filters
      
    })
    
    # Final filtered data
    filtered_data <- reactive({
      req(basic_filtered())
      
      result <- basic_filtered()
      
      for (cat_name in sort(unique(basic_filtered()$Category))) {
        selected <- input[[paste0("med_", cat_name)]]
        if (!is.null(selected) && length(selected) > 0) {
          result <- result %>% filter(Medication %in% selected)
        }
      }
      
      result
    })
    
    return(filtered_data)
    })
  }
  )
  }
  

    
    