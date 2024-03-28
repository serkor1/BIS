# script: Plot functions
# date: 2024-03-28
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create plotfunctions
# script start;

darkModeTheme <- function() {
  list(
    font = list(color = '#e0e0e0'), # Light grey for text
    xaxis = list(
      range=c(-2,5),
      title = 'Tid',
      gridcolor = '#444444', # Slightly lighter grey for grid lines
      tickfont = list(color = '#e0e0e0') # Light grey for tick labels
    ),
    yaxis = list(
      gridcolor = '#444444',
      tickfont = list(color = '#e0e0e0')
    ),
    legend = list(
      orientation = "h",
      bgcolor = '#303030', # Match the plot background
      font = list(color = '#e0e0e0') # Light grey for legend text
    )
  )
}


lightModeTheme <- function() {
  list(
    # plot_bgcolor = '#ffffff', # White background for the plotting area
    # paper_bgcolor = '#f0f0f0', # Light grey for the surrounding paper
    font = list(color = '#333333'), # Dark grey for text, ensuring readability
    xaxis = list(
      range=c(-2,5),
      title = 'Tid',
      gridcolor = '#cccccc', # Light grey for grid lines, subtle but visible
      tickfont = list(color = '#333333') # Dark grey for tick labels
    ),
    yaxis = list(
      gridcolor = '#cccccc',
      tickfont = list(color = '#333333')
    ),
    legend = list(
      orientation = "h",
      bgcolor = '#f0f0f0', # Match the paper background
      font = list(color = '#333333') # Dark grey for legend text
    )
  )
}


# script end;
