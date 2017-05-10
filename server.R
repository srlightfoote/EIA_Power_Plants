#### query EIA function ####
queryEIA<-function(plantID,baseID,key){
  eia.xts<-getEIA(ID = str_replace(baseID,'eiaid',as.character(plantID)),key = key)
}

#define EIA key + data channel
key<-'743E7EEB9DD74A8FBD876F83F1196F8D'
baseID<-'ELEC.PLANT.GEN.eiaid-ALL-ALL.M'

#### import EIA data ####
plants.dt<-data.table(read.csv('eia_powerplants_US.csv'))

#### shiny server function ####
shinyServer(function(input, output,session) {
  activeData<-reactiveValues(eia.xts=FALSE,
                             selected_plant.dt=FALSE)
  
  #reactive controls
  basemap<-reactive({input$basemap})
  showLabels<-reactive({input$labels})
  allowEIA<-reactive({input$dataset})
  
  #reactive dataset
  dataset <- reactive({
    plants.dt[is.element(plants.dt$primsource,input$dataset) &
                plants.dt$total_mw>=input$capRange[1] &
                plants.dt$total_mw<=input$capRange[2],]
  })
  
  #### Interactive Map ####
  #initialize map
  output$map<-renderLeaflet({
    leaflet(data = plants.dt) %>%
      fitBounds(~min(longitude,na.rm = TRUE), ~min(latitude,na.rm = TRUE),
                ~max(longitude,na.rm = TRUE), ~max(latitude,na.rm = TRUE)) %>%
      addMeasure(position='topleft',primaryLengthUnit = 'kilometers',secondaryLengthUnit = 'miles')%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 3",
        onClick=JS("function(btn, map){ map.setZoom(3); }")))
  })
  
  #initialize Data Table
  output$Table<-renderDataTable(dataset(),filter="top",options = list(pageLength = 10),selection = 'single')
  
  #Data Controls
  observe({
    pal <- colorFactor(input$colors, dataset()$primsource)
    data.dt<-dataset()
    labels <- sprintf(
      "<strong>%s</strong><br/>Generator: %s<br/>Capacity: %g MW",
      data.dt$plant_name, data.dt$primsource,data.dt$total_mw
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("map", data=data.dt,session=session) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(~longitude,~latitude, weight = 1,radius=~(log(total_mw)*1000),
                 fillColor =~pal(primsource),color = "#777777",
                 stroke = TRUE, fillOpacity = .7,
                 highlightOptions = highlightOptions(color = "black", weight = 3,
                                                     bringToFront = TRUE),
                 label=labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto")
      ) %>% 
      addLegend(position = "bottomleft",
                pal = pal, values = ~primsource,title = "Generator Type")
  })
  
  #change basemap
  observe({
    tile<-basemap()
    data.dt<-dataset()
    leafletProxy("map") %>% clearTiles() %>% addProviderTiles(provider = tile)
  })
  
  #update selected plant if map point is clicked
  observeEvent(eventExpr = input$map_shape_click,{
    selected_plant.dt<-dataset()[round(dataset()$latitude,3)==round(input$map_shape_click$lat,3) & 
                                   round(dataset()$longitude,3)==round(input$map_shape_click$lng,3),]
    activeData$selected_plant.dt<-selected_plant.dt
    
    #update map
    leafletProxy("map",data=selected_plant.dt) %>%
      fitBounds(~min(longitude,na.rm = TRUE)-.1, ~min(latitude,na.rm = TRUE)-.1,
                ~max(longitude,na.rm = TRUE)+.1, ~max(latitude,na.rm = TRUE)+.1)
    
    #update data table
    data.dt<-dataset()
    Table.dt<-data.dt
    selected<-which(Table.dt$plant_code==selected_plant.dt$plant_code)
    output$Table <- DT::renderDataTable({
      DT::datatable(Table.dt,filter="top",options = list(pageLength = 10,displayStart = selected-1),selection = list(mode = "single", target = "row", selected = selected) )
    })
    
    #query EIA
    if(input$queryEIA){
      if(sum(as.numeric(selected_plant.dt$plant_code),na.rm=TRUE)>0){
        #query the EIA Data
        eia.xts<-queryEIA(selected_plant.dt$plant_code,baseID,key)
        activeData$eia.xts<-eia.xts
        #show mini-plot in map
        output$eiaGen<-renderPlot({
          plot(eia.xts,main =selected_plant.dt$plant_name,
               ylab = 'MWh',cex.main=.75,cex.axis=.5,cex.lab=.5)
        })
        output$eiaMonthlyGen<-renderDygraph(
          dygraph(eia.xts,main=selected_plant.dt$plant_name,
                  xlab = 'Date',ylab = 'MWh') %>% 
            dyOptions(axisLabelFontSize = 10)  %>% 
            dyRoller(rollPeriod = 1)
        )
      }
    }
  })
  
  #update selected plant if table row is selected
  observeEvent(eventExpr = input$Table_rows_selected,{
    selected_plant.dt<-dataset()[input$Table_rows_selected,]
    activeData$selected_plant.dt<-selected_plant.dt
    
    #zoom to plant location
    leafletProxy("map",data=selected_plant.dt) %>%
      fitBounds(~min(longitude,na.rm = TRUE)-.1, ~min(latitude,na.rm = TRUE)-.1,
                ~max(longitude,na.rm = TRUE)+.1, ~max(latitude,na.rm = TRUE)+.1)
    
    #update data table
    data.dt<-dataset()
    Table.dt<-data.dt
    selected<-which(Table.dt$plant_code==selected_plant.dt$plant_code)
    output$Table <- DT::renderDataTable({
      DT::datatable(Table.dt,filter="top",options = list(pageLength = 10,displayStart = selected-1),selection = list(mode = "single", target = "row", selected = selected) )
    })
    
    #query EIA
    if(input$queryEIA){
      if(sum(as.numeric(selected_plant.dt$plant_code),na.rm=TRUE)>0){
        #query the EIA Data
        eia.xts<-queryEIA(selected_plant.dt$plant_code,baseID,key)
        activeData$eia.xts<-eia.xts
        #show mini-window in map
        output$eiaGen<-renderPlot({
          plot(eia.xts,main = paste(selected_plant.dt$plant_name,' Net MWh',sep=''),ylab = 'MWh')
        })
        output$eiaMonthlyGen<-renderDygraph(
          dygraph(eia.xts,main=selected_plant.dt$plant_name,
                  xlab = 'Date',ylab = 'MWh') %>% 
            dyOptions(axisLabelFontSize = 10)  %>% 
            dyRoller(rollPeriod = 1)
        )
      }
    }
  })
  
  #export EIA data to CSV
  output$exportEIA <- downloadHandler(
    filename = function() { paste(activeData$selected_plant.dt$plant_name,'_EIA_data.csv',sep='')},
    content = function(con) { write.csv(as.data.table(activeData$eia.xts), con,row.names = FALSE)}
  )
  
})