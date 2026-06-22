


# R/sankeyObserverModule.R

ObserverModule <- function(input, output, session, data_bl, filters, reactValue) {

  
  
  
observeEvent(input$update,{
   
  req(data_bl())
  
  
  
  filter_vals <- filters()
  MedLineRegimenlevels =  levels(data_bl()$MedLineRegimen)
  data_bl_filters <- data_bl() %>% 
    filter(AgeAtDiagnosis %in% filter_vals$selectAge) %>%
    filter(CurrentDiseaseStatus %in% filter_vals$currentDiseaseStatus) %>%
    filter(MedLineRegimen %in% filter_vals$medLineRegimen ) %>%
    filter(Histology %in% filter_vals$selectHistology ) %>%
    filter(PathGroupStage %in% filter_vals$selectPathGroupStage ) %>%
    filter(Medication %in% c(filter_vals$selectTarget,
                             filter_vals$selectImm,
                             filter_vals$selectChemo, filter_vals$selectOther) ) %>%
    droplevels() 
  
  data_bl_subset <-  data_bl_filters %>%
    arrange(Medication) %>%
    select(Key,AgeAtDiagnosis,Histology, PathGroupStage,CurrentDiseaseStatus, MedLineRegimen, Medication) %>%
    
    pivot_wider(id_cols = c(Key,AgeAtDiagnosis,Histology, PathGroupStage,CurrentDiseaseStatus)  , names_from = MedLineRegimen, values_from = Medication, values_fn = list(Medication = ~paste(unique(.), collapse = "+")) ) %>%
    # values_fill = "0") %>%
    pivot_longer(-c(Key,AgeAtDiagnosis,Histology, PathGroupStage,CurrentDiseaseStatus), names_to = "MedLineRegimen", values_to = "Medication" , values_drop_na = TRUE
    )  %>% 
    mutate(
      MedLineRegimen = factor(MedLineRegimen , levels = MedLineRegimenlevels),
      Medication = paste0(substr(MedLineRegimen, 1,3),":",Medication),
      Medication = ifelse(str_detect(Medication, "Pal"), "Pal", Medication)) %>%
    arrange(MedLineRegimen, Medication) %>%
    mutate( Medication = factor(Medication, levels = unique(Medication)),
           CurrentDiseaseStatus = factor(CurrentDiseaseStatus, 
                                                 levels = c("Active Disease", "No Evidence of Disease", "Unknown"))) %>% droplevels()
  

  
  data_bl_pivot <- data_bl_subset %>%
    # arrange(Key) %>%
    # mutate(Medication = as.numeric(Medication) + length(levels(data_bl$CurrentDiseaseStatus)) + length(levels(data_bl$Key)) + length(levels(data_bl$AgeAtDiagnosis)) + length(levels(data_bl$PathGroupStage))) %>%
    mutate(PathGroupStagenum= as.numeric(PathGroupStage),
           Medication = as.numeric(Medication)  + 
             length(levels(data_bl_subset$Key)) + 
             length(levels(data_bl_subset$AgeAtDiagnosis)) +
             length(levels(data_bl_subset$Histology)) +
             length(levels(data_bl_subset$PathGroupStage)),
           CurrentDiseaseStatusnum =as.numeric(CurrentDiseaseStatus) + max(Medication) ) %>%
    pivot_wider(id_cols = c(Key, AgeAtDiagnosis,Histology, PathGroupStagenum,PathGroupStage, CurrentDiseaseStatus, CurrentDiseaseStatusnum), names_from = MedLineRegimen, values_from = Medication ) %>%
    relocate(CurrentDiseaseStatusnum, .after = everything()) 
  
  
  links <- data.frame()
  ncol <- c()
  
  data_bl_num <- data_bl_pivot %>% 
    mutate(Key = as.numeric(Key),
           AgeAtDiagnosis = as.numeric(AgeAtDiagnosis) ,
           # CurrentDiseaseStatusname = CurrentDiseaseStatus,
           # CurrentDiseaseStatus = as.numeric(CurrentDiseaseStatus),
           Histologyname = Histology,
           Histology = as.numeric(Histology)
    ) %>% arrange( desc(CurrentDiseaseStatus))
  
  for(row in 1: nrow(data_bl_num)){
    data_bl_row <- data_bl_num[row,] %>% 
      mutate(PathGroupStagenum= as.numeric(PathGroupStagenum) + 
               max(data_bl_num$AgeAtDiagnosis) + 
               max(data_bl_num$Histology) +
               max(data_bl_num$Key),
             Histology = as.numeric(Histology) +
               max(data_bl_num$AgeAtDiagnosis) + 
               max(data_bl_num$Key),
             # CurrentDiseaseStatusnum = as.numeric(CurrentDiseaseStatus) + # max(as.numeric(data_bl_num$Histology)) + 
             # max(data_bl$Medication) + max(data_bl_num$AgeAtDiagnosis) + max(data_bl_num$Key),
             AgeAtDiagnosis = as.numeric(AgeAtDiagnosis) + max(data_bl_num$Key)
      ) %>%
      select(where(~!all(is.na(.))))
    ncol <- c()
    # for(col in colnames(data_bl_row)){
    source <- data_bl_row[[1]] -1
    label <- as.character(data_bl_row[["PathGroupStage"]])
    status <- as.character(data_bl_row[["CurrentDiseaseStatus"]])
    histo <- as.character(data_bl_row[["Histologyname"]])
    # ncol <- c(col,ncol)
    # ncols <- colnames(data_bl_row)[!colnames(data_bl_row)%in%ncol]
    for(col2 in colnames(data_bl_row)[!colnames(data_bl_row) %in%c("Key","Histologyname","CurrentDiseaseStatusname" ,"CurrentDiseaseStatus", "PathGroupStage")]){
      # if(str_detect(col2, paste0(input$selectNodes, collapse = "|"))){
      #   next
      # }
      if(col2=="CurrentDiseaseStatusnum"){
        last_col <- TRUE
      }else {last_col <- FALSE}
      target <- data_bl_row[[col2]] -1
      links <- rbind( cbind(source, target, label, status,histo,last_col ), links) %>% drop_na()
      
      source <-target
      
    }}
  
  
  links$value <- 1
  # color_function <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
  # links <- links %>% left_join(data.frame(label = unique(links$label), 
  #                                         color = color_function(n = length(unique(links$label)))))
  
  
  nodes <-  data.frame("name" = c(rep("",length(levels(data_bl_subset$Key))), levels(data_bl_subset$AgeAtDiagnosis), 
                                  levels(data_bl_subset$Histology),
                                  levels(data_bl_subset$PathGroupStage), levels(data_bl_subset$Medication),
                                  levels(data_bl_subset$CurrentDiseaseStatus)
  ))
  nodes$color_list <- palette_increase_length(palette_qualitative(), 
                                              n=  max(as.numeric(links$target)+1, na.rm =T))
  links <- links %>% left_join(data.frame(label = unique(links$label), 
                                          color = sample( nodes$color_list,length(unique(links$label)) )))
  
  reactValue$nodes <- nodes %>% left_join(links[,c("label", "color")] %>% unique(), by = c("name"= "label"))  %>%
    mutate(color = ifelse(is.na(color), color_list, color),
           color = ifelse(str_detect(name, "Neo"), color_list[11],
                          ifelse(str_detect(name, "Adj"),color_list[12],
                                 ifelse(str_detect(name, "Sec"),color_list[13],
                                        ifelse(str_detect(name, "Active"),"#D3D3D3",
                                               ifelse(str_detect(name, "Evidence"),'#BF382A',
                                                      ifelse(str_detect(name, "Unknown"),'#0C4B8E',
                                                             ifelse(str_detect(name, "Thi|Fou|Fif"),color_list[15],color))))))),
           nodes = as.character(name),
           name =ifelse(str_detect(name, "Neo|Adj|Sec|Thi|Fou|Fif"), "", name)
    ) %>% select(-color_list)
  
  reactValue$links <- links %>%
    mutate(color =  ifelse(last_col , paste0(color, "20"), paste0(color,"")),
           color = ifelse(status== "Active Disease", "#D3D3D390", color))
  colors_gg <- c("#D3D3D3" ,'#BF382A','#0C4B8E')
  names(colors_gg) <- c("Active Disease","No Evidence of Disease", "Unknown")
  reactValue$colors_gg <- colors_gg

  reactValue$data_bl_subset <- data_bl_subset
  reactValue$data_bl_filters <- data_bl_filters

})
  
  return(reactValue)
}