library(DT)
library(shiny)
library(shinydashboard)
library(googleVis)
library(shinyWidgets)

shinyUI(dashboardPage(skin = "black",
                      
    dashboardHeader(title = "The ATP App"),
    dashboardSidebar(
        
        sidebarUserPanel("Henry Crosby",
                         image = "tennis_ball.png"),

        sidebarMenu(
            menuItem("ATP Stats", tabName = 'stats', icon = icon('signal', lib = "glyphicon")),
            menuItem('Head 2 Head', tabName = 'head2head', icon = icon('line-chart')),
            menuItem('Tennis', tabName = 'tennis', icon = icon('map'))
        )
    ),
    
    dashboardBody(skin = "black",
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # Dashboard Tabs -
            # Tab 1 - Stats: Purpose is to investigate tennis statistics and the relationship between
            #         particular variables.
            #
            # Tab 2 - Having investigated tendencies of league stats we can investigate an individual Player.
            #         Will inlcude functionality of Head 2 Head comparison if desired...
            #
            # Tab 3 - 
            #
            
            # Start of Tab 1: League Stats
            tabItem(tabName = "stats",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("www/styles.css"),
                          includeScript("www/gomap.js"))),
                    # box(h3('Stats Explorer'),
                    #     helpText('In the Stats Explorer one investigated ATP stats based on a choosen date range and the top \"n\" number players for that time period.'),
                    #     fluidRow(column(width = 4,
                    #                     selectizeInput('stat_select', 'Top X of All-Time', Stats)),
                    #              column(width = 4,
                    #                     selectizeInput('stat_select', 'Top X of All-Time', Stats))),
                    #     width = NULL),
                    
                    tabBox(title = 'Stats', width = NULL,
                           
                      # Stats Tab 1 - Histogram     
                      tabPanel('Histogram', 
                                                     shinydashboard::box(htmlOutput("histogram"), width = 16, height = NULL),
                               fluidRow(
                                                     shinydashboard::box(h3('Histogram'),
                                                                         selectizeInput('hist_select','Choose Stats', Stats),
                                                                         width = 3))),

                      # Stats Tab 2 - Scatter Plot
                      tabPanel('Scatter Plot',
                                                        shinydashboard::box(htmlOutput("scatter"),width = 16, height = NULL),
                               fluidRow(
                                                        shinydashboard::box(h3('Scatter Plot'),
                                                                            column(selectizeInput('scat_selectX',
                                                                                                  'X', 
                                                                                                  selected = 'Ace', 
                                                                                                  Scatter_Choices),
                                                                            width = 5),
                                                                            column(selectizeInput('scat_selectY',
                                                                                                  'Y',
                                                                                                  selected = 'WinPercent',
                                                                                                  Scatter_Choices),
                                                                            width = 5)))),

                      # Stats Tab 3 - Correlation Plot
                      tabPanel('Correlation Plot', shinydashboard::box(h2('Correlation Plot'),
                                                                                plotOutput('correlation', width = "100%", height = "700px"), width = NULL)),
                      
                    
                      # Stats Tab 4 - Data Table
                      tabPanel("League Leaders", shinydashboard::box(DT::dataTableOutput("table"), width = 16, height = NULL))
                    ),
                    
                    absolutePanel(id = "controls",
                                  h2("Date Range Filter"),
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 60,
                                  left = "auto",
                                  right = 20,
                                  bottom = "auto",
                                  width = 200,
                                  height = "auto",
                                  HTML('<button data-toggle="collapse" data-target="#demo">Select Date</button>'),
                                  tags$div(id = 'demo',  class="collapse",helpText('NOTE: Match dates range back to only the year 1991 - the year detailed ATP stats began recording'),
                                           dateRangeInput("s_league_date",
                                                          "Date Range",
                                                          format = 'yyyy-mm-dd',
                                                          start = '1990-12-31',
                                                          end = '2017-08-28',
                                                          min = '1990-12-31',
                                                          max = '2017-08-28'))
                                  )
                    ),
            # End of Tab 1: League Stats
            
            # Start of Tab 2: Head 2 Head
            tabItem(tabName = "head2head",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("www/styles.css"),
                          includeScript("www/gomap.js"))),
                    
                    tabBox(title = 'Head 2 Head', width = NULL,
                           
                           # Tab 2 - Head    
                           tabPanel('MatchUp', 
                                    fluidRow(column(width = 4,
                                                    align="center",
                                                    selectizeInput('player1','Choose a player', Players)),
                                             column(width = 4,
                                                    align="center",
                                                    h2('VS.'),
                                                    box(h3('Record'),
                                                        h4(textOutput('results')),
                                                        width = 'auto'),
                                                    selectizeInput('surface','Surface', Surfaces, selected = 'All')),
                                             column(width = 4,
                                                    align="center",
                                                    selectizeInput('player2','Choose an Opponent', Players))),
                                    shinydashboard::box(DT::dataTableOutput("mytable1"), width = 16, height = NULL)),
                           
                    
                           # Tab 3 - Age
                           tabPanel('Age Comparison', 
                                     fluidRow(column(width = 6,
                                                     htmlOutput("linechart"),
                             # shinydashboard::box(htmlOutput("histogram"),width = 16, height = NULL),
                             shinydashboard::box(selectizeInput('player_line','Choose a player', Players),
                                                 selectizeInput('stat_line','Choose a stat', stats_age),
                                                 width = 6)),
                             column(width = 6,
                                     htmlOutput("linechart2"),
                                     shinydashboard::box(selectizeInput('player_line2','Choose a player to Compare with', Players),
                                                         selectizeInput('stat_line2','Choose a stat', stats_age),
                                                         width = 6)))
                             )
                           
                    )
                   
                    ),
            # End of Tab 2: Head 2 Head
                    
            # Filter the date and we can see two tabs... 
            # Tab 1: Map of the dominating countries for a particular stat OR 
            # Tab 2: 
            # Start of Tab 3: Map Comparison
            tabItem(tabName = "tennis",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("www/styles.css"),
                          includeScript("www/gomap.js"))),
                    fluidRow(box(htmlOutput("tennismap"), width = 'auto')),
                    absolutePanel(id = "controls",    h2("Country Explorer"),
                                                      class = "panel panel-default",
                                                      fixed = TRUE,
                                                      draggable = TRUE,
                                                      top = 60,
                                                      left = "auto",
                                                      right = 20,
                                                      bottom = "auto",
                                                      width = 200,
                                                      height = "auto",
                                                      selectizeInput("selected_player",
                                                           "Choose a Stat",
                                                           Map_choice),
                                            helpText('NOTE: Match dates range back to only the year 1991 - the year detailed ATP stats began recording'),
                                            dateRangeInput("map_date_range",
                                                           "Date Range",
                                                           format = 'yyyy-mm-dd',
                                                           start = '1990-12-31',
                                                           end = '2017-08-28',
                                                           min = '1990-12-31',
                                                           max = '2017-08-28')))
            # End of Tab 3: Map Comparison

            
            
            # Examples from states
            # tabItem(tabName = "map",
            #         fluidRow(infoBoxOutput("maxBox"),
            #                  infoBoxOutput("minBox"),
            #                  infoBoxOutput("avgBox")),
            #         fluidRow(box(htmlOutput("map"), height = 300),
            #                  box(htmlOutput("hist"), height = 300))),
            # 
            # tabItem(tabName = "data",
            #         fluidRow(box(DT::dataTableOutput("table"), width = 12))),
        )
    )
))