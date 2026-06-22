



## timelineplot
timeline_plot <- function( data, diagnosis_bl, keys, tumorMarkers, selectedKeys) {
  
  tumorMarker <- data$tumorMarker 
  medication <- data$medication
  outcome <- data$outcome
  
  timeline_marker_bl <- tumorMarker %>% 
    filter(AvatarKey %in% unique(keys)) %>%
    mutate(TMarkerResult = ifelse(str_detect(TMarkerTest, "MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Positive", "Intact Nuclear Expression",
                                  ifelse(str_detect(TMarkerTest,"MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Negative", "Loss of Nuclear Expression", TMarkerResult))) %>%
    filter(!str_detect(TMarkerResult, "Unknown|Not Available"),
           #"Low Instability|TMB-L|Not|Negative|Unknown|No Amplification|Value|Intact Nuclear|Stable|Indeterminate|Not Available|IgG|Kappa|Lambda") |
           #  (TMarkerResult=="Value" & TMarkerRangeIndicator == "High")) &
           !str_detect(TMarkerTest,"Unknown|Other")) %>%
    mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest) )
  
  if(!is.null(tumorMarkers)) {
    df_timeline_marker_bl <- timeline_marker_bl %>%
      filter(TMarkerTest %in% tumorMarkers)
    timeline_marker_bl <- timeline_marker_bl %>%
      filter(AvatarKey %in% unique(df_timeline_marker_bl$AvatarKey) )
  } 
  timeline_bl <- timeline_marker_bl %>%
    group_by(AvatarKey,AgeAtTumorMarkerTest) %>%
    mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest),
           AgeAtTumorMarkerTest = as.numeric(AgeAtTumorMarkerTest) * 365.25,
           start = paste0("00",as.character(ymd("0000-01-01") + AgeAtTumorMarkerTest)),
           end = NA,
           content = paste0(ifelse(TMarkerResult=="Value", paste0(TMarkerTest,": ",TMarkerResultValue, " ", TMarkerValueUOM),
                                   paste0(TMarkerTest,": ", TMarkerResult)), collapse ="<br>") ,
           id = paste0(AvatarKey,"marker",AgeAtTumorMarkerTest),
           Key = AvatarKey,
           group="marker") %>% ungroup() %>%
    select(Key,id, content, start, end, group) %>% unique() %>% arrange(id)
  
  if(!is.null(tumorMarkers)) {
    timeline_diagnosis_bl <- diagnosis_bl %>%
      filter(Key %in% unique(keys) &
               Key %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_diagnosis_bl <- diagnosis_bl %>% 
      filter(Key %in% unique(keys))
  }
  
  timeline_bl <-timeline_diagnosis_bl %>% 
    mutate(
      AgeAtDiagnosis = ifelse(AgeAtDiagnosis =="Age 90 or older", "90",AgeAtDiagnosis),
      AgeAtDiagnosis = as.numeric(AgeAtDiagnosis) * 365.25,
      start = paste0("00",as.character(ymd("0000-01-01") + AgeAtDiagnosis)),
      end = NA,
      content = paste0(Histology,"<br>", "Path Stage ",PathGroupStage) ,
      id = paste0(Key,"dia",1:n()),
      group ="diagnosis") %>% 
    select(Key,id, content, start, end, group) %>% arrange(id) %>%
    rbind(timeline_bl)
  
  if(!is.null(tumorMarkers)) {
    timeline_medication_bl <-  medication %>%  
      filter(Key %in% unique(keys) &
               Key %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_medication_bl <-  medication %>%  
      filter(Key %in% unique(keys))
  }
  timeline_bl <- timeline_medication_bl %>% 
    group_by(Key, AgeAtMedStart,AgeAtMedStop ) %>%
    mutate(
      AgeAtMedStart = as.numeric(ifelse(AgeAtMedStart =="Age 90 or older", "90", AgeAtMedStart)),
      AgeAtMedStop = as.numeric(ifelse(AgeAtMedStop =="Age 90 or older", "90",AgeAtMedStop)),
      start = paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtMedStart) * 365.25) )),
      end = ifelse( AgeAtMedStart == AgeAtMedStop, NA,
                    ifelse(is.na(as.numeric(AgeAtMedStop)), NA, paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtMedStop) * 365.25) )))),
      # end = ifelse(start==end, NA, end) ,
      content = paste0(paste0(unique(MedLineRegimen),collapse = "<br>"), "<br>",paste0(Medication, collapse = "<br>")) ,
      id = paste0(Key,"med",AgeAtMedStart, AgeAtMedStop),
      group ="medication") %>% ungroup() %>%
    select(Key,id, content, start, end, group) %>% unique() %>% arrange(id) %>%
    rbind(timeline_bl) 
  
  if(!is.null(tumorMarkers)) {
    timeline_outcome_bl <- outcome %>% 
      filter(AvatarKey %in% unique(keys) &
               AvatarKey %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_outcome_bl <-  outcome %>% 
      filter(AvatarKey %in% unique(keys))
  }
  timeline_bl <- timeline_outcome_bl %>% 
    mutate(
      AgeAtCurrentDiseaseStatus = ifelse(AgeAtCurrentDiseaseStatus =="Age 90 or older", "90",AgeAtCurrentDiseaseStatus),
      start = paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtCurrentDiseaseStatus) * 365.25) )),
      end = NA,
      content = paste0(CurrentDiseaseStatus) ,
      id = paste0(AvatarKey,"outcome",1:n()),
      group ="outcome",
      Key = AvatarKey) %>% 
    select(Key,id, content, start, end, group) %>% arrange(id) %>%
    rbind(timeline_bl)
  timeline_bl$title <- timeline_bl$content
  groups_nested <- data.frame(
    id = c("diagnosis","medication","outcome", "marker"),
    content = c("Disease","Medication","Outcome", "Tumor Marker")
  )
  

  sapply(selectedKeys , function(i) {
    # print(timeline_bl%>% filter(Key == i) %>%
    #         mutate(start = ifelse(start =="00NA", start[group=="diagnosis"], start)))
    timeline <- timevis(fit = TRUE, showZoom = TRUE, #options = list(width = "1550px"),
                        timeline_bl%>% filter(Key == i) %>%
                          mutate(start = ifelse(start =="00NA", start[group=="diagnosis"], start)),
                        groups = groups_nested) 

    tagList(list(renderText(paste0(i)), renderTimevis(timeline)))
  })
  
  
}



## swimplane_plot
swimmer_plot <- function( data, filtered_data, keys, tumorMarkers) {
  
  tumorMarker <- data$tumorMarker 
  medication <- data$medication
  outcome <- data$outcome
  diagnosis_bl <- filtered_data$diagnosis_bl()
  outcome_bl <- filtered_data$outcome_bl()
  
  timeline_marker_bl <- tumorMarker %>% 
    filter(AvatarKey %in% unique(keys)) %>%
    mutate(TMarkerResult = ifelse(str_detect(TMarkerTest, "MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Positive", "Intact Nuclear Expression",
                                  ifelse(str_detect(TMarkerTest,"MLH1|MSH2|MSH6|PMS2") & TMarkerResult =="Negative", "Loss of Nuclear Expression", TMarkerResult))) %>%
    filter(!str_detect(TMarkerResult, "Unknown|Not Available"),
           #"Low Instability|TMB-L|Not|Negative|Unknown|No Amplification|Value|Intact Nuclear|Stable|Indeterminate|Not Available|IgG|Kappa|Lambda") |
           #  (TMarkerResult=="Value" & TMarkerRangeIndicator == "High")) &
           !str_detect(TMarkerTest,"Unknown|Other")) %>%
    mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest) )
  
  
  
  if(!is.null(tumorMarkers)) {
    df_timeline_marker_bl <- timeline_marker_bl %>%
      filter(TMarkerTest %in% tumorMarkers)
    timeline_marker_bl <- timeline_marker_bl %>%
      filter(AvatarKey %in% unique(df_timeline_marker_bl$AvatarKey) )
  } 
  timeline_bl <- timeline_marker_bl %>%
    group_by(AvatarKey,AgeAtTumorMarkerTest) %>%
    mutate(AgeAtTumorMarkerTest = ifelse( AgeAtTumorMarkerTest =="Age 90 or older", "90",AgeAtTumorMarkerTest),
           AgeAtTumorMarkerTest = as.numeric(AgeAtTumorMarkerTest) * 365.25,
           start = paste0("00",as.character(ymd("0000-01-01") + AgeAtTumorMarkerTest)),
           end = NA,
           content = paste0(ifelse(TMarkerResult=="Value", paste0(TMarkerTest,": ",TMarkerResultValue, " ", TMarkerValueUOM),
                                   paste0(TMarkerTest,": ", TMarkerResult)), collapse ="<br>") ,
           id = paste0(AvatarKey,"marker",AgeAtTumorMarkerTest),
           Key = AvatarKey,
           group="marker") %>% ungroup() %>%
    select(Key,id, content, start, end, group) %>% unique() %>% arrange(id)
  
  if(!is.null(tumorMarkers)) {
    timeline_diagnosis_bl <- diagnosis_bl %>%
      filter(Key %in% unique(keys) &
               Key %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_diagnosis_bl <- diagnosis_bl %>% 
      filter(Key %in% unique(keys))
  }
  
  timeline_bl <-timeline_diagnosis_bl %>% 
    mutate(
      AgeAtDiagnosis = ifelse(AgeAtDiagnosis =="Age 90 or older", "90",AgeAtDiagnosis),
      AgeAtDiagnosis = as.numeric(AgeAtDiagnosis) * 365.25,
      start = paste0("00",as.character(ymd("0000-01-01") + AgeAtDiagnosis)),
      end = NA,
      content = paste0(Histology,"<br>", "Path Stage ",PathGroupStage) ,
      id = paste0(Key,"dia",1:n()),
      group ="diagnosis") %>% 
    select(Key,id, content, start, end, group) %>% arrange(id) %>%
    rbind(timeline_bl)
  
  if(!is.null(tumorMarkers)) {
    timeline_medication_bl <-  medication %>%  
      filter(Key %in% unique(keys) &
               Key %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_medication_bl <-  medication %>%  
      filter(Key %in% unique(keys))
  }
  timeline_bl <- timeline_medication_bl %>% 
    group_by(Key, AgeAtMedStart,AgeAtMedStop ) %>%
    mutate(
      AgeAtMedStart = as.numeric(ifelse(AgeAtMedStart =="Age 90 or older", "90", AgeAtMedStart)),
      AgeAtMedStop = as.numeric(ifelse(AgeAtMedStop =="Age 90 or older", "90",AgeAtMedStop)),
      start = paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtMedStart) * 365.25) )),
      end = ifelse( AgeAtMedStart == AgeAtMedStop, NA,
                    ifelse(is.na(as.numeric(AgeAtMedStop)), NA, paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtMedStop) * 365.25) )))),
      # end = ifelse(start==end, NA, end) ,
      #content = paste0(paste0(unique(MedLineRegimen),collapse = "<br>"),"<br>", paste0(Medication, collapse = "<br>")) ,
      content = Medication,
      id = paste0(Key,"med",AgeAtMedStart, AgeAtMedStop),
      group ="medication") %>% ungroup() %>%
    select(Key,id, content, start, end, group) %>% unique() %>% arrange(id) %>%
    rbind(timeline_bl) 
  
  if(!is.null(tumorMarkers)) {
    timeline_outcome_bl <- outcome %>% 
      filter(AvatarKey %in% unique(keys) &
               AvatarKey %in% unique(df_timeline_marker_bl$AvatarKey) ) 
  } else{
    timeline_outcome_bl <-  outcome %>% 
      filter(AvatarKey %in% unique(keys))
  }
  timeline_bl <- timeline_outcome_bl %>% 
    mutate(
      AgeAtCurrentDiseaseStatus = ifelse(AgeAtCurrentDiseaseStatus =="Age 90 or older", "90",AgeAtCurrentDiseaseStatus),
      start = paste0("00",as.character(ymd("0000-01-01") + (as.numeric(AgeAtCurrentDiseaseStatus) * 365.25) )),
      end = NA,
      content = paste0(CurrentDiseaseStatus) ,
      id = paste0(AvatarKey,"outcome",1:n()),
      group ="outcome",
      Key = AvatarKey) %>% 
    select(Key,id, content, start, end, group) %>% arrange(id) %>%
    rbind(timeline_bl)
  
  swimmer_data <- timeline_bl %>% filter(group %in% c("diagnosis", "medication", "marker")) %>% select(-end) %>%
    group_by(Key) %>%
    mutate(start_age = as.numeric(ymd(start) - ymd("0000-01-01")) / 365.25,
           adj_age = start_age - min(start_age),
           max_point = max(adj_age),
           min_point = min(adj_age[adj_age>0])) %>% ungroup() %>% 
    left_join(outcome_bl, by = c("Key"= "AvatarKey")) %>%
    arrange(content,min_point) %>%
    mutate(CurrentDiseaseStatus = ifelse(is.na(CurrentDiseaseStatus), "Unknown", CurrentDiseaseStatus),
           CurrentDiseaseStatus = factor(CurrentDiseaseStatus,levels = c("Active Disease","No Evidence of Disease", "Unknown")),
           Key = factor(Key, levels = unique(Key))  )
  unique_contents <- unique(swimmer_data$content[swimmer_data$group=="medication"])
  brewer_palette_function <- colorRampPalette(brewer.pal(12, "Set3"))
  content_colors <-   brewer_palette_function(length(unique_contents))
  names(content_colors) <- unique_contents
  
  
  # Generate the 50 colors (as a vector of hexadecimal color codes)
  
  height <- ifelse(length(unique(swimmer_data$Key)) <40, 400, 1000)
  print(paste0("height ",height))
  swimmer_plot <- plot_ly( height = height, swimmer_data, 
                          x=~adj_age, y= ~Key, type = 'scatter',mode = "lines",split=~Key, name = ~CurrentDiseaseStatus,showlegend = FALSE,
                          line = list(width = 4), 
                          # marker = list(size = 0), showlegend = FALSE,
                          #text = ~content , #, texttemplate = "%{y}",textposition = 'outside',
                          color = ~CurrentDiseaseStatus, colors = c("grey" ,'#BF382A','#0C4B8E')
  )%>%
    add_trace(data = swimmer_data %>% filter(group =="medication"), 
              x=~adj_age, y= ~Key, type = 'scatter',mode = "markers", name= ~content, #legendgroup=~content,#legendgrouptitle = list(text = "medications"),
              # line = list(width = 4), 
              marker = list( #color = content_colors,
                symbol = 'square', 
                size = 8,line = list(color = 'white', width = 1)), showlegend = TRUE,
              text = ~content  , texttemplate = "%{y}",textposition = 'outside',hoverinfo = 'text',
              inherit = FALSE,
              split = ~content #, colors = c("grey" ,'#BF382A','#0C4B8E')
    ) %>% 
    add_trace(data = swimmer_data %>% filter(group =="marker") %>% separate(content, sep = ": ", c("test","result"), remove= FALSE), 
              x=~adj_age, y= ~Key, type = 'scatter',mode = "markers", name= ~test, #legendgroup=~test, #legendgrouptitle = list(text = "Tumor Marker"),
              # line = list(width = 4), 
              marker = list( #color = "white",
                symbol = 'triangle-up', 
                size = 14,line = list(color = 'white', width = 1)), showlegend = TRUE, visible = "legendonly",
              text = ~content  , texttemplate = "%{y}",textposition = 'outside',hoverinfo = 'text',
              inherit = FALSE
              #split = ~test #, colors = c("grey" ,'#BF382A','#0C4B8E')
    ) %>% 
    layout(title= "Tumor Marker Vs Treatment plans",
           xaxis = list(rangeslider = list(visible = TRUE) , thickness = "0.05",
                        range = c(0, 3),
                        title ="Period in years"),
           yaxis = list(title = "", showgrid = FALSE))  
  
  swimmer_plot 
  

}
