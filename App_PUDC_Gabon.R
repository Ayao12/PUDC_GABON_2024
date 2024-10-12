## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(reactable)
library("RColorBrewer")
library(maptools)
library(sf)
library(sp)
library(rgdal)
library(readxl)
library(sjmisc)


# read in shape data into global environment


shapefile <- sf::st_read("PUDC Gabon/data/gab_admbndp2_1m_salb.shp")
pudc_data_composante <- read_excel("PUDC Gabon/data/pudc_data_composante.xlsx")


shapefile <- sf::st_as_sf(shapefile)  %>%
  mutate(fill_color = case_when(ADM1_NAME == "Moyen-Ogooue" ~ "red",
                                ADM1_NAME == "Nyanga" ~ "yellow",
                                ADM1_NAME == "Haut-Ogooue" ~ "#A9A9A9",
                                ADM1_NAME == "Ogooue-Maritime" ~ "#1C00ff00",
                                ADM1_NAME == "Ngounie" ~brewer.pal(n = 8, name = "RdBu")[2],
                                ADM1_NAME == "Woleu-Ntem" ~brewer.pal(n = 8, name = "RdBu")[3],
                                ADM1_NAME == "Ogooue-Ivindo" ~brewer.pal(n = 8, name = "RdBu")[4],
                                ADM1_NAME == "Estuaire" ~brewer.pal(n = 8, name = "RdBu")[5],
                                ADM1_NAME == "Ogooue-Lolo" ~brewer.pal(n = 8, name = "RdBu")[6]
  )) |>
  dplyr::select(ADM1_NAME, geometry, fill_color)




# define function used in application

region_data <- function(shapefile, markers) {

  removeNotification(id = "region_error", session = getDefaultReactiveDomain())

  dat <- data.frame(Longitude = markers$lon,
                    Latitude = markers$lat,
                    # Province = markers$prov,
                    names = c("Point"))

  dat <- sf::st_as_sf(dat,
                      coords = c("Longitude",
                                 "Latitude"))

  sf::st_crs(dat) <- sf::st_crs(shapefile)

  return(as.data.frame(shapefile)[which(sapply(sf::st_intersects(shapefile,dat), function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ])
}


### Ui
ui <- dashboardPage(
  dashboardHeader(title = "PUDC GABON 2024"),

  ## Sidebar content
  dashboardSidebar(#disable = TRUE
                   ),
  dashboardBody(
    box(width = 6,
        #p("PUDC GABON 2024"),
        leafletOutput(outputId = "map")
    ),
    box(width = 6,
        h2(htmlOutput("text1",
                   style = "text-align:center;color:#CD5C5C")),
        h3(htmlOutput("text2",
                      style = "text-align:center;color:#6495ED")),

        # h3(htmlOutput("text"))
        # ,
        DT::DTOutput("table1"),
        #
        # div(style = "display: inline-block;vertical-align:center;",
        #     actionButton("left", label = "<<")),
        # div(style = "display: inline-block;vertical-align:center;",
        #     sliderInput("obs", "",
        #                 min = 0, max = 1000, value = 500
        #     )),
        # div(style = "display: inline-block;vertical-align:center;",
        #     actionButton("right", label = ">>")),
        #
        h3(htmlOutput("text3",
                      style = "text-align:center;color:#6495ED")),
        DT::DTOutput("table2"),


    )
  ),
  title = "Interactive Maps"
  # reactableOutput("table")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(shapefile) %>%
      setView(lng = 11.609444, lat =  -0.803689, zoom = 6.1) %>% # center the map in Davis, CA
      addPolygons(color = "",
                  # layerId = ~locationname,
                  label = ~ADM1_NAME,
                  fillColor = ~fill_color,
                  fillOpacity = 0.5) %>%
      addMarkers(lng = 11.609444,
                 lat = -0.803689,
                 options = markerOptions(draggable = TRUE)) %>%
      #addProviderTiles('Esri.WorldImagery') %>%
      addTiles()

  })

  current_markers <- reactiveValues(
    lat=-0.803689, lon=11.609444,
    filtered_data = shapefile,
    selected_counties = NULL)

  observeEvent(input$mapfilter_shape_click, { # this is the logic behind the "click" of the map.

    click <- input$mapfilter_shape_click

    ######### map behavior ################
    # If a county is clicked

    if (click$id %in% current_markers$selected_counties) {
      # If selected, remove it
      current_markers$selected_counties <- current_markers$selected_counties[current_markers$selected_counties != click$id]
    } else if(click$id == "selected"){ # when a county is clicked again it is removed

      current_markers$selected_counties <- current_markers$selected_counties[current_markers$selected_counties != tail(current_markers$selected_counties, n = 1)]

    }else {
      # If not selected, add it
      current_markers$selected_counties <- c(current_markers$selected_counties, click$id)
    }

    leafletProxy("mapfilter", session) |>
      addPolygons(data = shapefile,
                  layerId = ~ADM1_NAME,
                  label = ~ADM1_NAME,
                  fillColor = "steelblue", # Change fill color based on selection
                  col = "black",
                  weight = 2,
                  fillOpacity = ifelse(shapefile$ADM1_NAME %in% current_markers$selected_counties, 1, 0.1),
                  highlight = highlightOptions(
                    fillOpacity = 1,
                    bringToFront = TRUE)
      )


  })

  observeEvent(input$map_marker_dragend, {

    rd <- region_data(shapefile = shapefile,
                      markers = data.frame(lat = input$map_marker_dragend$lat, lon = input$map_marker_dragend$lng))

    if(nrow(rd) == 0){
      showNotification("Error: no data for this location - moving point to previous location!", id = "region_error")
    } else {
      current_markers$lat <- input$map_marker_dragend$lat
      current_markers$lon <- input$map_marker_dragend$lng
    }

    # update map after check that the mark is within the defined area
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = current_markers$lat, lng = current_markers$lon),
                 options = markerOptions(draggable = TRUE))

  })

  observeEvent(input$map_shape_click, {

    # update marker location on click
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      addMarkers(data = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng),
                 options = markerOptions(draggable = TRUE))

    current_markers$lat <- input$map_shape_click$lat
    current_markers$lon <- input$map_shape_click$lng
    current_markers$dt_coord <- data.frame(Longitude = current_markers$lon,
                                           Latitude = current_markers$lat,
                                           names = c("Point"))
    current_markers$dt_coord2 <- sf::st_as_sf(current_markers$dt_coord,
                                              coords = c("Longitude",
                                                         "Latitude"))
    sf::st_crs(current_markers$dt_coord2) <- sf::st_crs(shapefile)

    current_markers$dtt <- as.data.frame(shapefile)[which(sapply(sf::st_intersects(shapefile,current_markers$dt_coord2),
                                                                 function(z) if (length(z)==0) NA_integer_ else z[1]) == 1), ] |>
      mutate(Provinces =ADM1_NAME )

    current_markers$prov <- current_markers$dtt[1,"Provinces"]


  })

  # output$text <- renderText({
  #   paste0("Current marker latitide: ", current_markers$lat," <br> ",
  #          "Current marker longitude: ", current_markers$lon, " <br> ",
  #          if_else(!is.na(region_data(shapefile = shapefile, markers = current_markers)$region),
  #                  "The marker is in an agricultural region of California.",
  #                  "The marker is NOT in an agricultural region of California."))
  # })


  output$text1 <- renderText({
    paste("Province ", current_markers$prov, sep = " : ")

  })
  output$text2 <- renderText({
    "Coût des investissements par composante"
   })

  output$table1 <- DT::renderDT({
    pudc_data_composante |>
      dplyr::filter(Provinces == as.character(current_markers$prov)) |>
     dplyr::select(-c(`Total Général`, Provinces)) |>
      sjmisc::rotate_df() |>
      DT::datatable()
  })
    ###########

  output$table2 <- DT::renderDT({
    pudc_data_composante |>
      dplyr::filter(Provinces == as.character(current_markers$prov)) |>
      dplyr::select(-c(`Total Général`, Provinces)) |>
      sjmisc::rotate_df() |>
      DT::datatable() |>
      DT::formatStyle(columns = c(3,6), width='200px')
  })

    # output$distPlot <- renderPlot({
    #   hist(rnorm(input$obs))
    # })
    observeEvent(input$left, {
      updateSliderInput(session, "obs", value = input$obs - 10)
    })
    observeEvent(input$right, {
      updateSliderInput(session, "obs", value = input$obs + 10)
    })

    output$text3 <- renderText({
      "Détails"
    })





  # output$table2 <- DT::renderDT({
  #
  #     DT::datatable()
  #
  #
  # })



  # observe({ # Update table filtering based on selected counties
  #     current_markers$filtered_data <- dtat |>
  #     filter(locationname %in% rv$selected_counties)
  #   } else {
  #     rv$filtered_data <- dtat
  #   }
  # })



}



shinyApp(ui, server)
