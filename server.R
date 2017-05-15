#### query EIA function ####
queryEIA<-function(baseID,key,selected_plant.dt){
  eia.xts<-getEIA(ID = str_replace(baseID,'eiaid',as.character(selected_plant.dt$plant_code)),key = key)
  #calculate historical net capacity factor
  eia.dt<-as.data.table(eia.xts)
  names(eia.dt)<-c('Date','mwh')
  eia.dt$mwh[eia.dt$mwh==0]<-NA #remove zero production months from data record
  eia.dt$month<-month(eia.dt$Date)
  eia.dt$daysInMonth<-days_in_month(eia.dt$Date)
  eia.lt<-eia.dt %>% group_by(month) %>% 
    summarise(meanGen=mean(mwh,na.rm=TRUE),meanDays=mean(daysInMonth))
  eia.lt$plant_code<-selected_plant.dt$plant_code
  eia.lt<-merge(eia.lt,plants.dt[,c('plant_code','total_mw'),with=FALSE],all.x=TRUE,all.y=FALSE)
  eia.lt<-mutate(eia.lt,ncf=meanGen/(meanDays*total_mw*24))
  ncf.ann<-sum(eia.lt$meanGen)/(sum(eia.lt$meanDays)*24*mean(eia.lt$total_mw))
  eia.ls<-list(eia.xts=eia.xts,eia.dt=eia.dt,eia.lt=eia.lt,ncf.ann=ncf.ann)
  return(eia.ls)
}

#define EIA key + data channel
key<-'<paste API key here>'
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
    if(input$queryEIA &sum(as.numeric(selected_plant.dt$plant_code),na.rm=TRUE)>0){
      #query the EIA Data
      eia.ls<-queryEIA(baseID,key,selected_plant.dt)
      activeData$eia.dt<-eia.ls$eia.dt
      
      output$ncf<-renderText(paste('NCF = ',round(eia.ls$ncf.ann*1000)/10,'%',sep=''))
      output$eiaMonthlyGen<-renderDygraph(
        dygraph(eia.ls$eia.xts,main=selected_plant.dt$plant_name,
                xlab = 'Date',ylab = 'MWh') %>% 
          dyOptions(axisLabelFontSize = 10)  %>% 
          dyRoller(rollPeriod = 1)
      )
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
    if(input$queryEIA &sum(as.numeric(selected_plant.dt$plant_code),na.rm=TRUE)>0){
      #query the EIA Data
      eia.ls<-queryEIA(baseID,key,selected_plant.dt)
      activeData$eia.dt<-eia.ls$eia.dt

      output$ncf<-renderText(paste('NCF = ',round(eia.ls$ncf.ann*1000)/10,'%',sep=''))
      output$eiaMonthlyGen<-renderDygraph(
        dygraph(eia.ls$eia.xts,main=paste(selected_plant.dt$plant_name,': NCF = ',round(eia.ls$ncf.ann*1000)/10,'%',sep=''),
                xlab = 'Date',ylab = 'MWh') %>% 
          dyOptions(axisLabelFontSize = 10)  %>% 
          dyRoller(rollPeriod = 1)
      )
    }
  })
  
  #export EIA data to CSV
  output$exportEIA <- downloadHandler(
    filename = function() { paste(activeData$selected_plant.dt$plant_name,'_EIA_data.csv',sep='')},
    content = function(con) { write.csv(as.data.table(activeData$eia.dt), con,row.names = FALSE)}
  )
  
})
