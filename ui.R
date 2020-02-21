library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyWidgets)
# Common packages I use
library(dplyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(rnaturalearthdata)
library(rgeos)
library(randomForest)

ui <- dashboardPage(title='Template',skin='blue', # This title sets the tab title, skin default is blue, but there are also black, purple, green, red, and yellow. 
                    dashboardHeader(title=tags$a(href='https://www.google.com',tags$script(src = "message-handler.js"),
                                                 tags$img(src='Batman-PNG-Transparent.png', height='45px'))),
                    # The Side Bar puts the menu on the left, and creates the spaces and links for the tabsNames
                    dashboardSidebar(sidebarMenu(
                      menuItem('Data', tabName = "Data", icon = icon("dashboard")),
                      menuItem('Heatmap', tabName = "Heatmap", icon = icon("bar-chart-o")),
                      menuItem('Projections', tabName = "Projections", icon = icon("tablet"))
                    ),width = '150px'),
                    # add some HTML tags, etc next, if desired
                    dashboardBody(tags$head(tags$style(HTML(".btn-styled {
                                                            border: 0;
                                                            line-height: 2.5;
                                                            padding: 0 20px;
                                                            font-size: 1rem;
                                                            text-align: center;
                                                            color: #fff;
                                                            text-shadow: 1px 1px 1px #000;
                                                            border-radius: 10px;
                                                            background-color: #337ab7;
                                                            background-image: linear-gradient(to top left,
                                                            rgba(0, 0, 0, .2),
                                                            rgba(0, 0, 0, .2) 30%,
                                                            rgba(0, 0, 0, 0));
                                                            box-shadow: inset 2px 2px 3px rgba(255, 255, 255, .6),
                                                            inset -2px -2px 3px rgba(0, 0, 0, .6);
                                                            }
                                                            
                                                            .btn-styled:hover {
                                                            background-color: #337aff;
                                                            }
                                                            
                                                            .btn-styled:active {
                                                            box-shadow: inset -2px -2px 3px rgba(255, 255, 255, .6),
                                                            inset 2px 2px 3px rgba(0, 0, 0, .6);
                                                            }
                                                            ")),tags$script(src = "message-handler.js")),
                                  # Define your tab items - in this case, each tab calls out a custom UI rendered in the Server
                                  tabItems(
                                    tabItem(tabName = 'Data', uiOutput('dtable'),width = 12),#end tab 1
                                    tabItem(tabName = 'Projections', uiOutput('continuousPlot'), width=12),#end tab 2ab 3
                                    tabItem(tabName = 'Heatmap',uiOutput('selections'))
                                  )#endtabs
                    ) # end Dashboard body
) # end UI

