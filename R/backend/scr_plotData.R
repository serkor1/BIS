# script: scr_plotData
# date: 2023-06-17
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate a class of functions
# that plots the data
# script start;


# model 1;

.plot_model1 <- function(
    DT,
    x_var,
    y_var,
    group_var,
    plot_color,
    smoothing = 1.3
) {
  
  
  #' function information;
  #' 
  #' This function plots the data 
  #' for model. Returns a plot with
  #' appropriate themes.
  #' 
  #' @param DT a data.table with filtered
  #' content - has to be a off flavored.
  #' TODO: Implement this
  #' 
  #' @param x_var the x-variable along the
  #' x axis. 
  #' 
  #' @param y_var the y-variable along the 
  #' y axis.
  #' 
  #' @param plot_color a character vector of length 
  #' one of valid colors
  
  # TODO: the function
  # has to incorporate custom
  # lables based on the data.
  
  # 1) Create the plot object;
  plot_object <- plot_ly(
    data = DT,
    x    = ~get(x_var),
    y    = ~get(y_var),
    color = ~get(group_var),
    colors = plot_color,
    fill  = 'tozeroy',
    mode  = 'lines+markers',
    type  = 'scatter',
    line  = list(
      shape = 'spline',
      smoothing = smoothing
    )
  )
  
  # 2) Add the theme 
  # of the plot
  layout(
    p = plot_object,
    legend = list(
      orientation = 'h'
    )
  )
  
  
}



plot_data <- function(DT) {
  
  
  
  
  
}


# script end;