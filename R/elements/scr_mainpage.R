# script: scr_mainpage
# date: 2023-07-04
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create a general page 
# for the semanticPage that is based on grids
# script start;

mainPage <- function(
    title,
    margin,
    header = list(
      left = NULL,
      center = NULL,
      right = NULL
    ),
    body = list(
      parameter = NULL,
      output    = NULL
    ),
    footer =  list(
      left = NULL,
      center = NULL,
      right = NULL
    )
) {
  
  
  # TODO: Add dynamic heights
  # so it is easier to determine
  # heights
  # TODO: Add subheader for menu-choices
  # has include relevant stuff.
  
  # local variables
  header_height <- footer_height <- 50
  
  # NOTE: We have two margins, bottom and top
  fixed_height  <- footer_height + header_height - margin * 2
  
  
  semanticPage(
    # Browser title
    title = title,
    # margins
    margin = paste0(0, 'px'),
    
    # Main grid;
    # 
    # Header; with a subgrid
    # Body; with a subgrid
    # footer; with a subgrid
    
    grid(
      
      # Main grid;
      grid_template = grid_template(
        default = list(
          areas = rbind(
            c('header'),
            c('body'),
            c('footer')
          ),
          
          # The remainder in calc has to include margin
          rows_height = c(
            paste0(header_height, 'px'),
            'auto',
            #paste0('calc(100vh - ', paste0(fixed_height, 'px'), ')'),
            paste0(footer_height, 'px')
          ),
          
          cols_width  = c('100vw')
        )
      ),
      #container_style = "border: 1px solid #f00",
      area_styles = list(
        
        header = generate_styles(
          styles = c(
            'background: red',
            'padding-left: 10px',
            'padding-right: 10px',
            paste0('margin-left:', margin, 'px'),
            paste0('margin-right:', margin, 'px'),
            paste0('margin-top:', margin, 'px')
          )
        ),
        
        body =generate_styles(
          styles = c(
            'background: blue',
            paste0('margin-left:', margin, 'px'),
            paste0('margin-right:', margin, 'px')
          )
        ),
        
        footer = generate_styles(
          styles = c(
            'background: red',
            'padding-left: 10px',
            'padding-right: 10px',
            paste0('margin-left:', margin, 'px'),
            paste0('margin-right:', margin, 'px'),
            paste0('margin-bottom:', margin, 'px')
          )
        )
        
        
        
      ),
      
      header = tags$div(
        # Add grid to the header
        # to avoid futher coding
        # in UI
        grid(
          grid_template = grid_template(
            default = list(
              areas = rbind(
                c('left', 'center', 'right')
              ),
              rows_height = paste0(header_height, 'px'),
              
              # was 2, 1,1 
              cols_width  = c('1fr', '1fr','4fr')
            )
          ),
          left = tags$div(
            # set style
            style = paste(
              collapse = '; ',
              c(
                paste0('height: ', header_height, 'px'),
                'display: flex',
                'justify-content: left',
                'align-items: center',
                'position: relative' # Was relative
              )
            ),
            
            header$left
          ),
          center = tags$div(
            style = paste(
              collapse = '; ',
              c(
                paste0('height: ', header_height, 'px'),
                'display: flex',
                'justify-content: center',
                'align-items: center',
                'position: relative'
              )
            ),
            header$center
          ),
          
          right = tags$div(
            style = paste(
              collapse = '; ',
              c(
                paste0('height: ', header_height, 'px'),
                'display: flex',
                'justify-content: right',
                'align-items: center',
                'position: relative' # was relative
              )
            ),
            
            header$right
          )
        )
        
      ),
  
      body   = grid(
        grid_template = grid_template(list(
          areas = rbind(
            'parameter',
            'output'
          ),
          rows_height = c('auto', 'auto'),
          cols_width  = c('100%')
        )
        ),
        area_styles = list(
          parameter = 'background: yellow;',
          output = 'background: green;'
        ),
        parameter = body$parameter,
        output = body$output
      ),
      
      footer = tags$div(
        grid(
          grid_template = grid_template(
            default = list(
              areas = rbind(
                c('left', 'center', 'right')
              ),
              rows_height = paste0(footer_height, 'px'),
              cols_width  = c('1fr', '1fr','1fr')
            )
          ),
          left = tags$div(
            style = paste(
              collapse = '; ',
              c(
                paste0('height:',footer_height, 'px'),
                'display: flex',
                'position: relative', # was relative
                'justify-content: left',
                'align-items: center'
              )
            ),
            footer$left
            ),
          center = tags$div(
            style = paste(
              collapse = '; ',
              c(
                paste0('height:',footer_height, 'px'),
                'display: flex',
                'position: relative',
                'justify-content: center',
                'align-items: center'
              )
            ),
            footer$center
            ),

          right = tags$div(
            style = paste(
              collapse = '; ',
              c(
                paste0('height:',footer_height, 'px'),
                'display: flex',
                'position: relative',
                'justify-content: right',
                'align-items: center'
              )
            ),
            footer$right
          )
        )
      )
    )
  )
  
  
  
}


# script end;