# script: element_card
# date: 2024-02-29
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Generate a custom Card
# to reduce coding
# script start;

card <- function(
        title  = "title",
        header = NULL,
        footer = NULL,
        body   = NULL) {
    
    bslib::card(
        full_screen = TRUE,
        # Card header
        bslib::card_header(
            shiny::tags$div(
                style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                shiny::tags$div(class = "custom-title", bslib::card_title(title)),
                shiny::tags$div(
                    style = "margin-left: auto;", # This will push all inside elements to the far right
                    
                    header
                    
                    
                )
            )
        ),
        
        # Card Body
        bslib::card_body(
            shiny::tagList(
                body
            )
        ),
        
        bslib::card_footer(shiny::tagList(
            footer
        )
        )
    )
    
}


# script end;