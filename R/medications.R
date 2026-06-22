

sankey_plot <- function( reactValue) {
  
  data <- reactValue$data_bl_filters

  df_med_levels_subset <- data %>%
    select(Key, Medication) %>% unique() 
  
  df_med_levels <-  df_med_levels_subset %>% 
    count(Medication) %>% arrange(desc(n)) %>% 
    mutate(Medication_levels = (1:nrow(.))-1 + length(unique(df_med_levels_subset$Key)),
           Medication = factor(Medication , levels= Medication))%>% select(-n)
  
  df_links<- data %>%
    select(Key, Medication, CurrentDiseaseStatus) %>% unique() %>% droplevels() %>%
    left_join(df_med_levels)%>%
    mutate(Medication = factor(Medication, levels= levels(df_med_levels$Medication))) %>%
    pivot_wider(id_cols = c(Key, CurrentDiseaseStatus), names_from = Medication, values_from = Medication_levels)
  
  
  links_med <- data.frame()
  for(row in 1:nrow(df_links)){
    source = as.numeric(df_links$Key[row]) -1
    CurrentDiseaseStatus = as.character(df_links$CurrentDiseaseStatus[row])
    Key = as.character(df_links$Key[row])
    for(col in levels(df_med_levels$Medication)){
      if(!is.na(df_links[[col]][row])){
        target = df_links[[col]][row]
        links_med <- rbind( cbind(source, target, Key,CurrentDiseaseStatus ), links_med) %>% drop_na()
        source <- target
        
      } }}  
  links_med$value <-1
 
  links_med$color<-  reactValue$colors_gg[links_med$CurrentDiseaseStatus]
  nodes_med <- data.frame(name_med = c(rep("",length(df_links$Key)),levels(df_med_levels$Medication)))
  nodes_med$color <- palette_increase_length(palette_qualitative(), 
                                             n=  nrow(nodes_med))
  
  reactValue$nodes_med <- nodes_med
  reactValue$links_med <-links_med
  plot_ly(
    type = "sankey",
    domain = list(
      x =  c(0,0.9),
      y =  c(0,1)
    ),
    height = 600,
    orientation = "h",
    node = list(
      label = reactValue$nodes_med$name_med, 
      color = reactValue$nodes_med$color,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = reactValue$links_med$source ,
      target = reactValue$links_med$target,
      value = reactValue$links_med$value,
      color = reactValue$links_med$color
      # hoverinfo= paste0(reactValue$links$source, reactValue$links$source)
    )
  )
  
}

print_plot <- function( df_oncoplot, reactValue){
  
  alter_fun = list(
    background = function(x, y, w, h) {
      # Background rectangle (optional, often grey or white)
      grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = NULL, col = "black", lwd =0.03))
    },
    `Active Disease` = function(x, y, w, h) {
      grid.rect(x, y, w*0.9, h*0.8, gp = gpar(fill =  reactValue$colors_gg["Active Disease"], col = NA))
    },
    `No Evidence of Disease` = function(x, y, w, h) {
      grid.rect(x, y, w*0.9, h*0.8, gp = gpar(fill =  reactValue$colors_gg["No Evidence of Disease"], col = NA))
    },
    Unknown = function(x, y, w, h) {
      grid.rect(x, y, w*0.9, h*0.8, gp = gpar(fill =  reactValue$colors_gg["Unknown"], col = NA))
    }
  )
  
  oncoprint <-oncoPrint(df_oncoplot,
                        alter_fun = alter_fun,
                        col =  reactValue$colors_gg,
                        alter_fun_is_vectorized = TRUE,
                        column_title = "Medications",
                        show_heatmap_legend = FALSE,
                        show_row_names = TRUE, # explicitly set to TRUE if needed
                        show_pct = TRUE        # explicitly set to TRUE if needed
                        
  )
  draw(oncoprint)
  
}