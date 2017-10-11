library(DT)
library(googleVis)
library(fmsb)
library(corrplot)

shinyServer(function(input, output, session){
  
    observeEvent(input$scat_selectX, {
      updateSelectizeInput(session, 'scat_selectY', 'Y', Scatter_Choices[!Scatter_Choices == input$scat_selectX])
      })
  
    
    # Stats - Histogram
    output$histogram <- renderGvis({
      gvisHistogram(Percentage_Calc(Men, input$s_league_date[1], input$s_league_date[2], 'no')[,input$hist_select],
                    options=list(width="auto",
                                 height=400,
                                 backgroundColor.stroke = 'black'))
    })
    
    observeEvent(input$player1, {
      updateSelectizeInput(session, 'player2', 'Choose an Opponent', Players[!Players == input$player1])
    })
    
    observeEvent(input$player_line, {
      updateSelectizeInput(session, 'player_line2','Choose a player to Compare with', Players[!Players == input$player_line])
    })
    
    output$results <- renderText(wins_against(input$player1, input$player2, Men, input$surface))
    
    # Stats - Scatter Plot
    output$scatter <- renderGvis({
      gvisScatterChart(Percentage_Calc(Men, input$s_league_date[1], input$s_league_date[2],'no')[,c(input$scat_selectX, input$scat_selectY), drop=FALSE],
                    options=list(width="auto",
                                 height=400,
                                 pointSize = 1,
                                 trendlines="{0: {
                                 labelInLegend: 'Trendline',
                                 visibleInLegend: 'true', 
                                 color: 'green',
                                 lineWidth: 1,
                                 opacity: 0.75}}"))
    })
    # Stats - Correlation Plot
    output$correlation <- renderPlot({corrplot(correlationz(Percentage_Calc(Men,
                                                                             input$s_league_date[1],
                                                                             input$s_league_date[2], 'no')),
                                   method = "circle",
                                   outline = TRUE,
                                   order = 'alphabet',
                                   mar = c(2, 0, 1, 0),
                                   col = colorRampPalette(c('darkgreen',
                                                            'white',
                                                            'yellow'))(200))
      })
    
    
    # Player - Age Line Plot
    output$linechart <- renderGvis({
      gvisScatterChart(Line_Charter(Men, input$player_line)[,c('Age',input$stat_line), drop=FALSE],
                       options=list(width="auto",
                                    height=400,
                                    pointSize = 3,
                                    trendlines="{0: {
                                    labelInLegend: 'Trendline',
                                    visibleInLegend: 'true', 
                                    color: 'green',
                                    lineWidth: 2,
                                    opacity: 0.75}}"))
    })
    
    output$linechart2 <- renderGvis({
      gvisScatterChart(Line_Charter(Men, input$player_line2)[,c('Age',input$stat_line2), drop=FALSE],
                       options=list(width="auto",
                                    height=400,
                                    pointSize = 3,
                                    trendlines="{0: {
                                    labelInLegend: 'Trendline',
                                    visibleInLegend: 'true', 
                                    color: 'green',
                                    lineWidth: 2,
                                    opacity: 0.75}}"))
      })
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
      datatable(Percentage_Calc(Men,
                                input$s_league_date[1],
                                input$s_league_date[2],
                                'yes')[,1:8],
                rownames=FALSE, width = 500, height = 600) %>% 
        formatStyle('Name', background="skyblue", fontWeight='bold')
    })
    
    Percentage_Calc(Men,
                    as.character('1990-12-31'),
                    as.character('2017-12-31'),
                    'no')
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(Men[(Men$Winner == input$player1 | Men$Winner == input$player2) &
                        (Men$Loser == input$player1 | Men$Loser == input$player2), c('Tournament','Date','Round', 'Surface', 'Winner', 'Loser', 'Score'), drop = FALSE])
    })
    
    # output$table1 <- DT::renderDataTable({
    #   datatable(inner_join(Men %>% filter(Winner == input$player1 & Loser == input$player2) %>%
    #                                select(Date, Tournament, Round, Surface, Winner, Loser, Score),
    #                        Men %>% filter(Winner == input$player2 & Loser == input$player1) %>%
    #                          select(Date, Tournament, Round, Surface, Winner, Loser, Score)),
    #             rownames=FALSE, 
    #             width = 500, 
    #             height = 600)
    # })
    
    
    # world map tennis data by country
    output$tennismap <- renderGvis({
      gvisGeoChart(Map_Calc(Men, input$map_date_range[1], input$map_date_range[2]),
                   'Country',
                   input$selected_player,
                   options=list(crosshair="{trigger:'both'}",
                                width="auto",
                                height="auto",
                                backgroundColor='#81d4fa',
                                keepAspectRatio = FALSE,
                                explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.50}"))
    })
})
