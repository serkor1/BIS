# script: server_model2
# date: 2023-07-03
# author: Serkan Korkmaz, serkor1@duck.com
# objective: Create a seperated server
# for model 2
# script start;

server_model2 <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # extract DATA
      DT <- reactive(
        {
          
          # 1) generate query string;
          query <- paste(
            'SELECT * FROM model2 WHERE', paste(
              c(
                paste0('service = "', input$assignment_model2, '"'),
                paste0('char_education = "', input$education_model2, '"')
              ),
              
              collapse = ' AND '
            )
          )
          
          # 1.1) verbose the the query;
          # TODO: might be that this should
          # be moved toi the logger.
          cli::cli_inform(
            message = c(
              '!' = 'Query',
              'i' = query
            )
          )
          
          # 2) send the query
          # to the db
          get_results <- dbSendQuery(
            conn = DB_connection,
            statement = query
          )
          
          # 3) Get the results
          # and store as data.table
          DT <- as.data.table(
            dbFetch(
              res = get_results,
              n = -1
            )
          )
          
          # 4) clear results;
          dbClearResult(get_results)
          
          return(
            DT
          )
        }
      )
      
      
      
      output$plot_model2 <- renderPlotly(
        expr = {
          
         try(
            {
              layout(
                p = plot_ly(
                  data = DT(),
                  x = ~service,
                  y = ~qty,
                  type = 'bar',
                  colors = plot_color
                ),
                barmode = 'group'
              )
              
            }
          )
          
          
        }
      )
      
      
      output$table <- DT::renderDataTable(
        {
          semantic_DT(
            {
              DT()
            }
          )
        }
      )
      
      
      
    }
  )
}
# script end;