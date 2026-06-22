


physical <- read.csv("data/PhysicalAssessment_V4.csv", header = TRUE)
labs <- read.csv("data/Labs_V4.csv", header = TRUE)
diagnosis <- read.csv("data/Diagnosis_SD.csv", header = TRUE)
medication <- read.csv("data/Medications_SD.csv", header = TRUE)
outcome <- read.csv("data/Outcomes_SD.csv", header = TRUE)
tumorMarker <- read.csv("data/OSU_TumorMarker.csv", header = TRUE)
# flowPanel <- read.csv("data/OSU_TumorMarker.FlowPanel.csv", header = TRUE)
## supplimentory data labels
TMarker_class <- read.csv("data/tumorMarker_class.csv", header = TRUE)
Drug_class <- read.csv("data/drugClass.csv", header  =TRUE)
disease_codes <- read.csv("data/disease_codes.csv", header = TRUE)
MedLineRegimenlevels <- c("Neoadjuvant/First Line","Adjuvant/First Line", "First Line", "Second Line", "Third Line", "Fourth Line" ,"Fifth Line",
                          "Sixth Line", "Seventh Line" ,"Eighth Line" ,"Ninth Line" ,"Tenth Line" , 
                          "Eleventh Line" , "Twelfth Line" , "Thirteenth Line" , 
                          "Maintenance", "Palliative" , "Induction","Unknown", "Unknown/Not Applicable" ,"Unknown/Not Reported"  )

MedLineRegimenlevels <- unique(c(MedLineRegimenlevels, sort(unique(medication$MedLineRegimen))))

dat_list <- list(labs=labs, physical=physical, diagnosis=diagnosis, medication=medication, outcome=outcome, 
                  tumorMarker=tumorMarker, TMarker_class=TMarker_class, Drug_class=Drug_class, 
                 disease_codes=disease_codes, MedLineRegimenlevels=MedLineRegimenlevels)


cancerTypeInput <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("cancerType"), "Select Cancer Type",
                choices = disease_codes$cancer_type,
                selected = NULL
                ),
    # sliderInput(ns("range"), "Range", min = 0, max = 100, value = c(0, 100))
  )
}


diseaseServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
dual_disases_bl  <- reactive({
      req(input$cancerType)
      data_list <- data()

cancer_id <- data_list$disease_codes$cancer_id[data_list$disease_codes$cancer_type== input$cancerType][1]

data_list$diagnosis %>% filter(stringr::str_detect(PrimaryDiagnosisSiteCode, cancer_id)) %>% #as.tibble() %>%
  select(Key, AgeAtDiagnosis, PrimaryDiagnosisSite, Histology) %>% distinct() %>%
  group_by(Key) %>%
  summarise(n = n()) %>% filter(n>1) %>% .$Key

})


med_bl <- reactive({
  req(input$cancerType)
  req(dual_disases_bl())
  data_list <- data()
  cancer_id <- data_list$disease_codes$cancer_id[data_list$disease_codes$cancer_type== input$cancerType][1]
  
  data_list$medication %>% filter(stringr::str_detect(MedPrimaryDiagnosisSiteCode, cancer_id) & ! Key %in% dual_disases_bl() & !Key =="JIW95F5UL6") %>% tibble::as.tibble() %>%
  select(Key) %>% distinct() %>% .$Key
})

diagnosis_bl <- reactive({
  req(input$cancerType)
  req(med_bl())
  data_list <- data()
  cancer_id <- data_list$disease_codes$cancer_id[data_list$disease_codes$cancer_type== input$cancerType][1]

  data_list$diagnosis %>% filter(stringr::str_detect(PrimaryDiagnosisSiteCode, cancer_id)) %>% filter(Key %in% med_bl()) %>%
  select(Key, AgeAtDiagnosis, Histology, PathGroupStage) %>% arrange(Key) %>%
  mutate(Histology = stringr::str_to_title(Histology),
         AgeAtDiagnosis = ifelse(AgeAtDiagnosis=="Age 90 or older", "90", AgeAtDiagnosis),
         PathGroupStage = ifelse(stringr::str_detect(PathGroupStage, "Unknown|applicable"), "Unknown",PathGroupStage))
})



outcome_bl <- reactive({
  req(input$cancerType)
  req(med_bl())
  data_list <- data()
  cancer_id <- data_list$disease_codes$cancer_id[data_list$disease_codes$cancer_type== input$cancerType][1]

  data_list$outcome %>% distinct() %>%
  filter(AvatarKey %in% med_bl() & 
           stringr::str_detect(OutcomesPrimaryDiagnosisSiteCode , cancer_id )  &
           !stringr::str_detect(CurrentDiseaseStatus , "Unknown|Tumor|Evalu")
  ) %>% tibble::as.tibble() %>%
  mutate(AgeAtCurrentDiseaseStatus = as.numeric(AgeAtCurrentDiseaseStatus)) %>%
  filter(!is.na(AgeAtCurrentDiseaseStatus)) %>%
    filter(!AvatarKey %in% c("2MM9D65938", "MKT6K6PWJ5", "RQBSZYBH0H", "VHYH6XU0RQ") ) %>%
  group_by(AvatarKey) %>% filter(AgeAtCurrentDiseaseStatus == max(AgeAtCurrentDiseaseStatus, na.rm = TRUE)) %>%
  ungroup() %>%
  select(AvatarKey, CurrentDiseaseStatus)
})


tumorMarker_all_bl <-reactive({
  req(med_bl())
  data_list <- data()
  

  data_list$tumorMarker %>% filter(AvatarKey %in% med_bl()) %>%
  mutate(TMarkerResult = ifelse(stringr::str_detect(TMarkerTest, "MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Positive", "Intact Nuclear Expression",
                                ifelse(stringr::str_detect(TMarkerTest,"MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Negative", "Loss of Nuclear Expression", TMarkerResult))) %>%
  filter(!stringr::str_detect(TMarkerTest,"Unknown|Other")) %>%
  mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest) )%>%
    left_join(data_list$TMarker_class , by = "TMarkerTest") %>%
    left_join(outcome_bl(), by = "AvatarKey")

})

tumorMarker_bl <-reactive({
  req(med_bl())
  data_list <- data()

data_list$tumorMarker %>% filter(AvatarKey %in% med_bl()) %>%
  mutate(TMarkerResult = ifelse(stringr::str_detect(TMarkerTest, "MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Positive", "Intact Nuclear Expression",
                                ifelse(stringr::str_detect(TMarkerTest,"MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Negative", "Loss of Nuclear Expression", TMarkerResult))) %>%
  filter((!stringr::str_detect(TMarkerResult, "Low Instability|TMB-L|Not|Negative|Unknown|No Amplification|Value|Intact Nuclear|Stable|Indeterminate|Not Available|IgG|Kappa|Lambda") |
            (TMarkerResult=="Value" & TMarkerRangeIndicator == "High")) &
           !stringr::str_detect(TMarkerTest,"Unknown|Other")) %>%
  mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest) ) %>%
  left_join(data_list$TMarker_class , by = "TMarkerTest")

})

data_bl <-reactive({
  req(outcome_bl())
  req(diagnosis_bl())
  req(med_bl())
  
  data_list <- data()
  MedLineRegimenlevels <- data_list$MedLineRegimenlevels
                                                       
  data_list$medication %>%  filter(Key %in% med_bl())  %>%
  left_join(diagnosis_bl()) %>%
  left_join(outcome_bl(), by = c(Key = "AvatarKey")) %>%
  left_join(data_list$Drug_class , by = "Medication") %>%
  # filter(CurrentDiseaseStatus != "No Evidence of Disease") %>%
  select(Key,AgeAtDiagnosis,Histology, PathGroupStage,CurrentDiseaseStatus, MedLineRegimen, Medication, DrugClass, Category) %>%
  mutate(Key = as.factor(Key),
         Histology = stringr::str_to_title(Histology),
         Histology = as.factor(Histology),
         DrugClass = as.factor(DrugClass),
         CurrentDiseaseStatus = ifelse(is.na(CurrentDiseaseStatus), "Unknown",CurrentDiseaseStatus),
         CurrentDiseaseStatus = as.factor(CurrentDiseaseStatus),
         # AgeAtDiagnosis = as.factor(AgeAtDiagnosis),
         PathGroupStage = as.factor(PathGroupStage) ,
         MedLineRegimen = factor(MedLineRegimen, levels = MedLineRegimenlevels),
         # Medication = factor(Medication, levels = unique(Medication)),
         AgeAtDiagnosis = cut(as.numeric(as.character(AgeAtDiagnosis)), breaks = c(0,20, seq(40,80,10), 100), labels = c("0-20", "20-40", "40-50", "50-60","60-70","70-80", "80+")),
         AgeAtDiagnosis = factor(AgeAtDiagnosis, levels = c("0-20", "20-40", "40-50", "50-60","60-70","70-80", "80+"))
  )  %>% droplevels()

})



return(list(
  diagnosis_bl = diagnosis_bl,
  outcome_bl = outcome_bl,
  tumorMarker_all_bl = tumorMarker_all_bl,
  tumorMarker_bl = tumorMarker_bl,
  data_bl = data_bl
))


  })
}
