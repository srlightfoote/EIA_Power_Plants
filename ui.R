#libraries
library(data.table)
library(DT)
library(dygraphs)
library(EIAdata)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(stringr)

shinyUI(
  navbarPage(
    title = HTML(paste(icon('bolt'),'EIA Power Plant Explorer')), id="nav",
    tabPanel(
      "Map",
      div(class="outer",
          tags$style(type = "text/css", ".outer {font-size:10px; position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
          leafletOutput("map", width = "100%", height = "100%"),
          absolutePanel(fixed = TRUE,draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                        width = 200, height = 550,style ="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFFF; opacity: 0.8;",
                        selectInput('basemap','Basemap',choices = 
                                      c("CartoDB.Positron","CartoDB.DarkMatter", "Esri","Esri.WorldStreetMap","Esri.DeLorme","Esri.WorldTopoMap","Esri.WorldImagery",
                                        "Esri.WorldTerrain","Esri.WorldShadedRelief","Esri.WorldPhysical","Esri.OceanBasemap","Esri.NatGeoWorldMap" )
                                    ,selected = "CartoDB.Positron"),
                        selectInput("dataset",label = "Generator Type(s)",choices = c("hydroelectric",
                                                                                      "natural gas",
                                                                                      "pumped storage",
                                                                                      "petroleum",
                                                                                      "geothermal",
                                                                                      "solar",
                                                                                      "wind",
                                                                                      "coal",
                                                                                      "nuclear",
                                                                                      "biomass",
                                                                                      "other"),selected = c("hydroelectric","solar","wind"),multiple=TRUE),
                        sliderInput(inputId = "capRange","Plant Capacity [MW]",min = 0,max=10000,value = c(0,10000),step=1),
                        selectInput("colors", "Color Scheme",
                                    c('viridis',rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                    selected ='viridis'),
                        checkboxInput('queryEIA','Query EIA Data',value = TRUE),
                        plotOutput("eiaGen",height = 150)
          )
      )
    ),
    tabPanel('Data',
             sidebarLayout(
               sidebarPanel(
                 downloadButton('exportEIA','Export EIA Data'),
                 h4(),
                 dygraphOutput("eiaMonthlyGen",height = 400)
               ),
               mainPanel(
                 div(DT::dataTableOutput("Table"), style = "font-size: 75%; width: 75%")
                 
               )
             )
    )
  )
)