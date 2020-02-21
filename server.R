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
library(caret)
library(rgeos)
library(randomForest)
server <- function(input, output, session) { # need session for interactive stuff
  
  v <- reactiveValues(homicides_percapitaMean = NULL, assaults_percapitaMean=NULL, rapes_percapitaMean=NULL, mapData=NULL) # Reactive Values can be read and written to by any function
  v$model<-readRDS('data/trainedmodel.rds')
  v$preproc<-readRDS('data/preprocessobj.rds')
  
  #----------------------------------------------STUFF FOR A DATA TABLE----------
  output$dtable<-renderUI({ # make a data table dynamically
    
    dt<-fread('data/crimereport_processed.csv')
    
    border<-c(-1,1)
    v$df<-dt
    if (!is.null(dt)){
      v$limx<-round(dt[,range(lng)]+border,1)
      v$limy<-round(dt[,range(lat)]+border,1)
    }
    output$data<-DT::renderDataTable(dt,options = list(scrollX = TRUE))
    
    # now create the UI - with "data" output
    fluidPage(
      box(title='Crime in Context',"This data is from Kaggle, lightly processed to include lat/long for mapping.",
          a('The data is available here',href='https://www.kaggle.com/marshallproject/crime-rates'), 'and has been processed lightly to add lat/long coordiates, as described in README',status='info', width=12),
      box(title='From the Site','"The crime data was acquired from the FBI Uniform Crime Reporting program`s Offenses Known and Clearances by Arrest database for the year in question, held at the National Archives of Criminal Justice Data. The data was compiled and analyzed by Gabriel Dance, Tom Meagher, and Emily Hopkins of The Marshall Project; the analysis was published as Crime in Context on 18 August 2016."', status='info',width=12),
      box(title='How to Use this App','Explore the data table below, then use the dashboard tabs on the left to navigate through an interactive map, and a projection using ML to predict future crime rates', status='warning',width=12),
      box( DT::dataTableOutput('data') # add box details
           ,title = NULL, footer = NULL, 
           status = NULL,  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
           solidHeader = FALSE, background = NULL, width = 12, height = NULL,
           collapsible = FALSE, collapsed = FALSE)# end box
    )# end page
    
  }) # end data table UI
  #----------------------------END DATA TABLE STUFF---------------------------
  
  
  
  # ---------------------------STUFF FOR THE CONTINUOUSLY UPDATING PLOT 
  # (Borrowing from Mike Wise's response https://stackoverflow.com/questions/41438725/update-dynamically-created-plot-in-r-shiny?answertab=votes#tab-top )
  
  
  #render the UI with the plot and call the UI comntinuousPlot
  output$continuousPlot<-renderUI({# output the UI generated below into the reactive object "continuousPlot"
    choices<-unique(v$df$agency_jurisdiction)
    
    output$valuePlot <-renderPlot({ # create a second reactive plot
      datacit<-v$df
      setDT(datacit)
      datacit<-datacit[agency_jurisdiction==input$selectedcity]
      datacit[,color:='blue']
      popmod<-glm(population ~ report_year, data=datacit)
      l<-datacit[report_year>2004]
      l$report_year<-2015:2024
      l$agency_code<-as.factor(l$agency_code)
      l$population<-as.numeric(predict(popmod,l))
      predl<-predict(v$preproc, l)
      predl2<-predict(v$preproc,datacit)
      preds<-as.numeric(predict(v$model,predl))
      preds2<-as.numeric(predict(v$model,predl2))
      l$crimes_percapita<-preds
      l$color='red'
      xs2<-datacit[,report_year]
      datacit<-rbind(datacit,l)
      xs<-c(datacit[,report_year], l$report_year)
      ys<-c(datacit[,crimes_percapita], preds)
      
      ys2<-preds2
      
      cs<-c(rep('red',nrow(datacit)), rep('blue',length(preds)))
      title<-paste('Crime Rate',input$selectedcity)
      plot(x=xs,y=ys,col=cs,  main=title, pch=19)
      points(x=xs2,y=ys2,col='blue', pch=19)
      legend('topright',legend=c('Actual','Predicted'), fill=c('red','blue'))
    })
    # The below will create (render) the dynamic UI
    fluidRow( # create two plots in a fluid row
      box(selectInput("selectedcity", "Select city to display",choices = choices),
          box(title='Instructions','Select the city you wish to see. Past values (1975-2014) are shown in red, then future values values (blue) predicted with a Random Forest model. As you change the city, the projection will be recalculated', status='info',width=12),
          plotOutput('valuePlot'), # Set Box Details
          title = NULL, footer = NULL, status = NULL,
          solidHeader = FALSE, background = NULL, width = 12, height = NULL,
          collapsible = FALSE, collapsed = FALSE),
      
    )
  }) # END RENDERUI
  #----------------------------------------END CONTINUOUS PLOT
  
  
  
  
  #----------------------------------------BEGIN MAP--------------
  # Create a section list and output it as a control
  output$selections<-renderUI({
    choices<-unique(v$df$report_year) # dynamically assign the selections
    limx<-round(range(v$df$lng),1)
    limy<-round(range(v$df$lat),1)
    fluidPage(
      box(selectInput("select", "Select year to display",choices = choices),
          selectInput('crimeselect', 'Select Crime Statistic', choices=colnames(v$df)[11:15]),
          title = NULL, footer = NULL, 
          status = 'info',  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
          solidHeader = FALSE, background = NULL, width = 8, height = NULL,
          collapsible = FALSE, collapsed = FALSE,  
          sliderInput('long',"Longitude Range",min = limx[1]-5, max = limx[2]+5, value = (limx)),
          sliderInput('lat',"Lattitude Range",min = limy[1]-5, max = limy[2]+5, value = (limy))),
      box(title = 'Instructions','Use the sliders and selectors above to limit the map and view the mapped data by year and crime problem. Each change of lat/long or crime problem will update the map', status = 'info', width=12),
      uiOutput('heatMap'),
      
      
    )
    
  })
  # render a heatmap based on a selection in the control panel
  output$heatMap<-renderUI({
    limx<-v$limx
    limy<-v$limy
    fluidRow(
      box(uiOutput('filterMap'), width=12)
    )
    
  })
  
  output$filterMap<-renderUI({ # generate the dynamic data table (this is removed from the previous section to keep from resetting the filters each time)
    # find the elements in the data frame between the two ends of the sliders
    dt<-as.data.table(v$df)
    crime<-'crimes_percapita'
    
    if (!is.null(input$select)){ #avoid warnings by making sure the input has been recorded before setting the values of the column
      crime<-input$crimeselect
      dt$selectedcrime<-dt[,crime, with=FALSE]
      maxclr<-max(dt$selectedcrime)
      dt[,clr:=rainbow(maxclr)[selectedcrime], by=selectedcrime]
    }
    
    border<-c(-2,2)
    
    yr<-input$select
    dt<-dt[report_year==yr]
    
    output$map<-renderPlot({
      dt<-dt[lng>=input$long[1] & lng<=input$long[2]]
      dt<-dt[lat>=input$lat[1] & lat<=input$lat[2]]
      v$mapData<-dt
      dt[,nudgey:=lat+runif(1,-.7,.7), by=agency_jurisdiction]
      dt[,nudgex:=lng+runif(1,-.7,.7), by=agency_jurisdiction]
      world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      ggplot(data = world) +
        geom_sf() +
        geom_point(data = dt, aes(x = lng, y = lat, fill = selectedcrime), size = 4, 
                   shape = 23, show.legend = TRUE) +
        scale_color_gradient(low="blue", high="red",guide = 'colorbar', aesthetics = 'fill') +
        geom_text_repel(data = dt, aes(x = lng, y = lat, label = agency_jurisdiction)) +
        coord_sf(xlim = c(input$long[1]-2, input$long[2]+2), ylim = c(input$lat[1]-2, input$lat[2]+2), expand = FALSE) 
    }, height=800)       
    # render the UI
    plotOutput('map', width = '100%', height='900px')
  })
  
  
  #----------------------------------------END MAP----------------
  
  
} # END SERVER
