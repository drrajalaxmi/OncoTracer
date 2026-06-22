

# bar plot; simpler version with fixed columns
quickBarPlot <- function(data, x, title = NULL) {
  
  # Always select Key, the x variable, and CurrentDiseaseStatus
  plot_data <- data %>%
    select(Key, !!sym(x), CurrentDiseaseStatus) %>%
    unique() %>%
    mutate(CurrentDiseaseStatus = factor(CurrentDiseaseStatus,levels = c("Active Disease","No Evidence of Disease", "Unknown")))%>%
    count(!!sym(x), CurrentDiseaseStatus)
  
  plot_ly(plot_data, 
          x = ~str_wrap(get(x),25), 
          y = ~n, 
          type = 'bar',
          color = ~CurrentDiseaseStatus,
          colors = c("grey", '#BF382A', '#0C4B8E'),
          texttemplate = "%{y}",
          textposition = "outside",
          showlegend = FALSE) %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           barmode = "group")
  

}


# Helper function to create a plot column
plotColumn <- function(plot_id, title = NULL, height = 400) {
  tags$div(
    style = "flex: 1; min-width: 0; padding: 0 5px;",
    # if (!is.null(title)) h4(title, style = "text-align: center; margin: 0 0 5px 0;"),
    plotlyOutput(plot_id, height = paste0(height, "px"))
  )
}

