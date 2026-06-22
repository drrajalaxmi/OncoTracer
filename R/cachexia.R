


### detecting the peak of the weight and look for the fall 
adaptive_peak_analysis <- function(physical, data_bl_subset,  min_peak_distance = 90, threshold = 5.0) {
  results <- data.frame()
  # keys <-diagnosis[str_detect(diagnosis$PrimaryDiagnosisSiteCode, "C25"),]$Key
  
  # diagnosis[diagnosis$Key=="3T0L0PL2SS",]
  keys <- unique(data_bl_subset$Key)[unique(data_bl_subset$Key) %in% unique(physical$AvatarKey)]
  # keys <-c("OAS47PHZGH")
  for(key in keys){


    data <- physical %>% filter(AvatarKey==key) %>%
      filter(!is.na(as.numeric(AgeAtPhysicalExam))) %>%
      mutate(dates = ymd("0000-01-01") + (as.numeric(AgeAtPhysicalExam) * 365.25),
             BodyWeight = as.numeric(BodyWeight)
      ) %>%
      filter(!is.na(BodyWeight)) %>% arrange(AvatarKey,dates)
    data <- data[data$BodyWeight >= remove_outliers(data$BodyWeight, 6,6)[[1]] & data$BodyWeight <= remove_outliers(data$BodyWeight, 6,6)[[2]], ]           
    
    if(nrow(data)==0){
      next
    }
    # plot(data$dates, data$BodyWeight, col="gray",
    #      xlab="Date", ylab="Weight (kg)", main="Weight Trajectory")

    # DBSCAN on date_num with epsilon = 90 days
    dates_matrix <- as.matrix(data$dates)
    db <- dbscan(dates_matrix, eps = 180, minPts = 2)
    data$dbscan_cluster <- db$cluster
   table(data$dbscan_cluster)
    # Points in cluster 0 are noise (sparse)

    dense_data <- data %>%
      mutate(dbscan_density = ifelse(dbscan_cluster %in% names(table(data$dbscan_cluster)[table(data$dbscan_cluster)<3]),
                                     "sparse",
                                     ifelse(dbscan_cluster =="0", "sparse", "dense")) ) %>%
      filter(dbscan_density=="dense")

    if(nrow(dense_data)==0){
      next
    } else if(0.15 * nrow(dense_data) <6){
      next
    }
  ## smooth the weights
    analysis <- data.frame(dates =  seq(min(dense_data$dates), max(dense_data$dates), by = "day"))
    loess_fit <- try(loess(BodyWeight ~ as.numeric(dates),
                       data = dense_data,
                       span = 0.15,
                       degree = 1,
                       control = loess.control(surface = "direct")),
                     silent = TRUE)
    if(inherits(loess_fit, "try-error")) {
            next  # Skip to next i
    }

    analysis$smoothed <- predict(loess_fit, analysis$dates)
    # plot(data$dates, data$BodyWeight, col="gray",
    #      xlab="Date", ylab="Weight (kg)", main="Weight Trajectory")
    #
    # lines(analysis$dates, analysis$smoothed, col="blue", lwd=2)
    #

    dates= analysis$dates
    weights= analysis$smoothed

  # Step 1: Identify all local maxima (peaks)
  # A point is a peak if it's higher than its neighbors within a window
  peak_idx <- c()
  n <- length(weights)

  for (i in c(1,(min_peak_distance+1):(n - min_peak_distance))) {
    if (i== 1){
      left_window <- weights[i]
    }

    left_window <- weights[( i-min_peak_distance):(i - 1)]
    # ifelse((i + min_peak_distance)>n ,n, (i + min_peak_distance))
    right_window <- weights[(i + 1): (i + min_peak_distance)]

    # Check if current point is higher than all points in left and right windows
    if (weights[i] >= max(left_window) && weights[i] > max(right_window, na.rm=TRUE)) {
      peak_idx <- c(peak_idx, i)
    }
  }

  # Step 2: For each peak, find the subsequent nadir before the next peak

 if(is.null(peak_idx)) {
   next}
  for (j in 1:length(peak_idx)) {
    current_peak_idx <- peak_idx[j]
    current_peak_date <- dates[current_peak_idx]
    current_peak_weight <- weights[current_peak_idx]

    # Determine end point (next peak or end of data)
    if (j < length(peak_idx)) {
      end_idx <- peak_idx[j+1]
    } else {
      end_idx <- n
    }

    # Segment from this peak to next peak
    segment_weights <- weights[current_peak_idx:end_idx]
    segment_dates <- dates[current_peak_idx:end_idx]

    # Find minimum in this segment (nadir)
    min_idx_in_segment <- which.min(segment_weights)
    nadir_idx <- current_peak_idx + min_idx_in_segment - 1
    nadir_weight <- min(segment_weights)
    nadir_date <- dates[nadir_idx]

    # Calculate decline percentage
    decline_pct <- (current_peak_weight - nadir_weight) / current_peak_weight * 100

    # Calculate rate of decline (kg per day)
    days_to_nadir <- as.numeric(difftime(nadir_date, current_peak_date, units = "days"))
    rate_per_day <- (current_peak_weight - nadir_weight) / days_to_nadir
    bmi_wt_threshold <- ifelse( sum(!is.na(as.numeric(data$BMI)))>0 & quantile(as.numeric(data$BMI), na.rm=TRUE, 0.25) <= 20 , 2,  threshold)
    rate_wt_threshold <- ifelse( sum(!is.na(as.numeric(data$BMI)))>0 & quantile(as.numeric(data$BMI), na.rm=TRUE, 0.25) <= 20 , 2/180,  threshold/180)
    
    results <- rbind(results, data.frame(
      AvatarKey = key,
      idx = j,
      dates = current_peak_date,
      BodyWeight = current_peak_weight,
      days_to_nadir = days_to_nadir,
      decline_percent = decline_pct,
      rate_kg_per_day = rate_per_day,
      cachexia_event = (rate_per_day >= rate_wt_threshold & decline_pct >= bmi_wt_threshold)
    ), 
    data.frame(
      AvatarKey = key,
      idx = paste0(j,"nadir"),
      dates = nadir_date,
      BodyWeight = nadir_weight,
      days_to_nadir = days_to_nadir,
      decline_percent = decline_pct,
      rate_kg_per_day = rate_per_day,
      cachexia_event = (rate_per_day >= rate_wt_threshold & decline_pct >= bmi_wt_threshold)
    ),
    data.frame(
      AvatarKey = key,
      idx = j,
      dates = nadir_date + 1,
      BodyWeight = NA,
      days_to_nadir = days_to_nadir,
      decline_percent = decline_pct,
      rate_kg_per_day = rate_per_day,
      cachexia_event = (rate_per_day >= rate_wt_threshold & decline_pct >= bmi_wt_threshold)
    )
    
    
    )
  }
  # loess_list[[key]] <- analysis
  }

   results <- results %>% filter(cachexia_event)
   # print(results)
  return(results)
}

remove_outliers <- function(x, multiplier_up = 6, multiplier_dn=6) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  
  lower_bound <- Q1 - (multiplier_dn * IQR_val)
  upper_bound <- Q3 + (multiplier_up * IQR_val)
  lower_bound <- ifelse(lower_bound < min(x) , min(x), lower_bound) 
  upper_bound <- ifelse(upper_bound > max(x) , max(x), upper_bound) 
  return(list(lower_bound, upper_bound))
  # x[x >= lower_bound & x <= upper_bound]
}

cachexia_plot <- function(physical, labs, medication, adaptive_peak_analysis, selectCachexiaKey, test_list) {
  
  

  test_list <- test_list #c("Hemoglobin Level", "Hematocrit %", "Albumin (Serum)", "Protein (Total)" ,"Creatinine (Serum)" , "C Reactive") 
  keys <- selectCachexiaKey #unique(adaptive_peak_analysis$AvatarKey) #c("OAS47PHZGH")
  # keys<- c("OAS47PHZGH")


sapply(keys,  function(key) {
  lab_data <- labs %>% #filter(AvatarKey %in% unique(adaptive_peak_analysis$AvatarKey)) %>%
    filter(AvatarKey == key) %>% 
    filter(LabTest %in% test_list) %>%
    mutate(dates = ymd("0000-01-01") + (as.numeric(AgeAtLabResults) * 365.25),
           LabResults = as.numeric(LabResults)) %>%
    filter(!is.na(LabResults)) %>% arrange(AvatarKey,dates)

  
  plot_data <- physical %>% #filter(AvatarKey %in% unique(adaptive_peak_analysis$AvatarKey)) %>%
                  filter(AvatarKey == key) %>% 
                  filter(!is.na(as.numeric(AgeAtPhysicalExam))) %>%
                  mutate(dates = ymd("0000-01-01") + (as.numeric(AgeAtPhysicalExam) * 365.25),
                        BodyWeight = as.numeric(BodyWeight)) %>%
                   filter(!is.na(BodyWeight)) %>% arrange(AvatarKey,dates) 
  
                
  cachexia_event <- adaptive_peak_analysis[adaptive_peak_analysis$AvatarKey==key,]  
  # cachexia_event <- results[results$AvatarKey==key,]
  
  med_data <- medication %>% filter(Key == key) %>% 
    mutate(AgeAtMedStart = ymd("0000-01-01") + (as.numeric(AgeAtMedStart) * 365.25),
           AgeAtMedStop = ymd("0000-01-01") + (as.numeric(AgeAtMedStop) * 365.25),
           Novalue = AgeAtMedStop+1,
           med_value =  as.numeric(as.factor(Medication))) %>% 
    pivot_longer(cols = c("AgeAtMedStart", "AgeAtMedStop", "Novalue"),names_to = "start_end", values_to = "dates")%>%
    mutate(med_value = ifelse(start_end=="Novalue", NA, med_value))
  
  y_ticks <-med_data %>% select(Medication, med_value) %>% 
    filter(!is.na(med_value)) %>% unique() %>%
    arrange(med_value)
    
  
  
  height <- 120*(length(unique(lab_data$LabTest))+3)
  fig <- plot_ly(height = height) %>%
    add_trace(data=med_data,
              x =~ dates,
              y=~med_value, line = list(color="grey", width = 1),
              type = 'scatter',mode = 'markers+lines',symbol = I('square'),
              marker = list(size=4),
              showlegend = FALSE ) %>% 
    add_trace(data=plot_data, 
          x = ~dates, 
          y = ~BodyWeight, 
          type = 'scatter',mode = 'markers',symbol = I('circle-open'),
          marker = list(color = "grey"),
          colors = "grey",  yaxis = "y2",
          showlegend = FALSE) %>%
    add_trace(data= cachexia_event,
              x = ~dates, 
              y = ~BodyWeight,  yaxis = "y2", line = list(color="orange", width = 2),
              marker = list(color = "orange"),
              type = 'scatter', mode = 'lines+markers', showlegend = FALSE,
              text = ~decline_percent)  %>%
    add_annotations(data= cachexia_event[str_detect(cachexia_event$idx, "nadir"),],
                    x = ~dates , 
                    y = ~BodyWeight+2, yref  = "y2", 
                    text = ~paste0( round(decline_percent,2), "% decline\nin ", days_to_nadir, " days"),
                    arrowhead = 2,
                    ax = 20,                # Arrow points left (positive x-offset)
                    ay = -40, 
                    arrowcolor = "orange", showlegend = FALSE,
                    font = list(color = "orange") ) %>%
    add_trace(data= plot_data[!is.na(as.numeric(plot_data$BMI)),],
              x = ~dates, 
              y = ~as.numeric(BMI), yaxis = 'y3',  showlegend = FALSE,
              # marker = list(color = "grey"),
              type = 'scatter', mode = 'markers', marker=list(color= "steelblue", size =6),  #symbol = I('circle-open')
              color = "steelblue"
              # line = list(shape = 'spline', smoothing = 1)
              )  
  
  layout_list <- list(
    title = "",
    xaxis = list(title = "",  anchor = paste0("y3") )
  ) 

  plot_num = 3
  for(test in 1:length(test_list)){

    lab_data_test <- lab_data %>% filter(LabTest ==test_list[test])
 
  if(nrow(lab_data_test)>0){

    unit <- unique(lab_data_test$LabUnits[!str_detect(lab_data_test$LabUnits, "Unknown")])

    fig <- fig %>%
      add_trace(data= lab_data_test,
                x = ~dates, 
                y = ~LabResults, yaxis = paste0('y',test+3), marker = list(color = "grey"),showlegend = FALSE,
                type = 'scatter', mode = 'markers+lines', # line = list(color = toRGB("steelblue")) , 
                symbol = I('circle-open')
                # line = list(shape = 'spline', smoothing = 1)
      )  
    layout_list[[paste0("yaxis",test+3)]] <- list( title = str_wrap(paste0(test_list[test]," ",unit),12) , 
                                                   # domain = c( ((test+1)/(length(test_list)+2)), ((test+2)/(length(test_list)+2))-0.02 ), 
                                                   domain = c( ((test-1)/(length(test_list)+3)) , ((test)/(length(test_list)+3))-0.02 ), 
                                                   anchor = "x" , showline = TRUE,   linecolor = toRGB("steelblue"), linewidth = 2  )
    plot_num = plot_num +1
    layout_list[["xaxis"]] = list(title = "Age",  anchor = paste0("y4"), showline = FALSE )
  } }
    layout_list[["yaxis"]] = list(title = "",  domain = c( ((plot_num-1)/(plot_num))+0.02, 1),  anchor = "x",
                                  # tickfont = list(size = 9),
                                  showline = TRUE,   linecolor = toRGB("steelblue"), linewidth = 2  , zeroline=FALSE,
                                  tickvals = y_ticks$med_value, ticktext = y_ticks$Medication,tickmode = "array" )
    
    
    layout_list[["yaxis2"]] = list(title = "Body Weight\nkg", domain = c( ((plot_num-2)/(plot_num))-0.02, ((plot_num-1)/(plot_num))-0.02 ),  anchor = "x",
                 showline = TRUE,   linecolor = toRGB("steelblue"), linewidth = 2  
                 # range= c(remove_outliers(plot_data$BodyWeight)[[1]], remove_outliers(plot_data$BodyWeight)[[2]] )
                 )
    layout_list[["yaxis3"]] =list( title = "BMI\nkg/m^2", domain = c( ((plot_num-3)/(plot_num))+0.02, ((plot_num-2)/(plot_num))-0.02 ) , anchor = "x" ,
                  showline = TRUE,   linecolor = toRGB("steelblue"), linewidth = 2  )
    

    
    
 fig <- do.call(layout, c(list(p = fig), layout_list))
 fig
 

   tagList(list(renderText(paste0(key)), tags$div(style= paste0("height: ", height, "px;"),renderPlotly(fig)),
                br(), br()))
  })


  
}



