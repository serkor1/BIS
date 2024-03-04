# script: scr_buttons
# author: Serkan Korkmaz
# date: 2023-06-12
# objective: Generate low level buttons
# for development
# script start; ####

# animated button;
animated_button <- function(
    inputid,
    label = 'label',
    hidden_content = 'house',
    font_color = NULL,
    button_color = NULL,
    url = NULL
) {
  
  #' animated button
  #' 
  #' @param inputid a character vector of 
  #' length 1.
  #' 
  #' @param visible_content a character vector of
  #' length 1. The label on the button
  #' 
  #' @param hidden_content a chacter vector of length 1.
  #' Has to be a valid icon.
  
  
  # TODO: Set max-width and max-height parameters
  # in the CSS to increase Icon size without increasing
  # the size of the div, ie the buttton.
  
  if (!is.null(url)) {
    
    url <- paste0("window.open(", paste0("'",url,"'"),', ', "'_blank')")
    
  }
  
  
  # Generate outer div;
  tags$div(
    
    # allow for an input
    # id for shiny referencing
    id = inputid,
    
    # set class of the button
    class = 'ui animated button',
    style = paste0(
      'color: ', font_color, '; ',
      'background-color: ',
      button_color, '; ',
      'max-height: 35px; '
    ),
    
    # No clue what this does
    # was a part of the semantic documentation
    tabindex = 0,
    
    # the visible part of the button
    # before moving cursor above the button
    tags$div(
      class = 'visible content',
      label
    ),
    
    # onclick = paste0(
    #   "window.open(", onclick,', ' ,"'_blank')"
    # ),
    onclick = url,
    # the hidden part of the button.
    # revealed once the mouse hovers above it
    tags$div(
      class = 'hidden content',
      hidden_content
    )
  )
  
}


label_button <- function(
    id = 'id',
    icon = 'home',
    button_label = 'button_label',
    label = 'label'
    ) {
  
  
  tags$div(
    id = id,
    class = 'ui labeled button',
    tabindex = 0,
    tags$div(
      class = 'ui button',
      icon(icon),
      button_label
    ),
    tags$a(
      class = 'ui basic left pointing label',
      label
    )
  )
  
  
  
}


# end of script; ####