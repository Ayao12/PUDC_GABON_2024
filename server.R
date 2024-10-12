#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 # vector_dept <-c( "xxxx","Libreville", "Akanda","Komo-Mondah","Komo","Noya","Komo-Océan")
  list_prov_dep <- list(Estuaire = c( "xxxx","Libreville", "Akanda","Komo-Mondah","Komo","Noya","Komo-Océan"),
                        Haut_Ogooue = c("xxxx", "Franceville","Mpassa", "Lébombi-Leyou", "Lékoko","Ogooué-Létili","Djouori-Agnili",
                          "Plateaux", "Djoué","Lékoni-Lékori", "Sébé-Brikolo", "Lékabi-Léwolo","Bayi-Brikolo"),
                        Moyen_Ogooue = c("xxxx", "Lambaréné", "Ogooué et Lacs", "Abanga-Bigné"),
                        Ngounie = c("xxxx", "Mouila", "Douya-Onoyé","Tsamba-Magotsi", "Ndolou", "Mougalaba", "Dola",
                         "Louétsi-Wano", "Louétsi-Bibaka", "Boumi-Louetsi", "Ogoulou"),
                        Nyanga = c("xxxx","Tchibanga", "Mougoutsi", "Basse-Banio", "Haute-Banio", "Douigni","Doutsila","Mongo"),
                        Ogooue_Ivindo = c("xxxx", "Makokou",  "Ivindo", "Zadié", "Mvoung", "Lopé"),
                        Ogooue_Lolo = c("xxxx", "Koulamoutou", "Lolo-Bouenguidi","Lombo-Bouenguidi","Offoué-Onoyé", "Mouloundou"),
                        Ogooue_Maritime = c("xxxx", "Port-Gentil", "Bendjé","Etimboué","Ndougou"),
                        Woleu_Ntem = c("xxxx", "Oyem",  "Woleu","Ntem", "Haut-Ntem", "Okano", "Haut-Komo")
                        )



  output$map <- renderLeaflet({
    leaflet(shapefile) %>%
      setView(lng = 11.609444, lat =  -0.803689, zoom = 7) %>% # center the map in Davis, CA
      addPolygons(color = "",
                  # layerId = ~locationname,
                  label = ~ADM1_NAME,
                  fillColor = ~fill_color,
                  fillOpacity = 0.4) %>%
      addMarkers(lng = 11.609444,
                 lat = -0.803689,
                 options = markerOptions(draggable = TRUE)) %>%
      #addProviderTiles('Esri.WorldImagery') %>%
      addTiles()

  })

  current_markers <- reactiveValues(
    lat=-0.803689, lon=11.609444,
    filtered_data = shapefile,
    selected_counties = NULL,
    Tab = "xxxx",
    prov = "Estuaire",
    vector_dept = list_prov_dep[[1]])

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
                  weight = 5,
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
    if (current_markers$prov == "Estuaire") {
      current_markers$Tab = list_prov_dep[[1]][1]
      current_markers$vector_dept = list_prov_dep[[1]]}
    else if(current_markers$prov == "Haut-Ogooue"){
      current_markers$Tab = list_prov_dep[[2]][1]
      current_markers$vector_dept = list_prov_dep[[2]]
    }else if(current_markers$prov == "Moyen-Ogooue"){
      current_markers$Tab = list_prov_dep[[3]][1]
      current_markers$vector_dept = list_prov_dep[[3]]
    }else if(current_markers$prov == "Ngounie"){
      current_markers$Tab = list_prov_dep[[4]][1]
      current_markers$vector_dept = list_prov_dep[[4]]
    }else if(current_markers$prov == "Nyanga"){
      current_markers$Tab = list_prov_dep[[5]][1]
      current_markers$vector_dept = list_prov_dep[[5]]
    }else if(current_markers$prov == "Ogooue-Ivindo"){
      current_markers$Tab = list_prov_dep[[6]][1]
      current_markers$vector_dept = list_prov_dep[[6]]
    }else if(current_markers$prov == "Ogooue-Lolo"){
      current_markers$Tab = list_prov_dep[[7]][1]
      current_markers$vector_dept = list_prov_dep[[7]]
    }else if(current_markers$prov == "Ogooue-Maritime"){
      current_markers$Tab = list_prov_dep[[8]][1]
      current_markers$vector_dept = list_prov_dep[[8]]
    }else if(current_markers$prov == "Woleu-Ntem"){
      current_markers$Tab = list_prov_dep[[9]][1]
      current_markers$vector_dept = list_prov_dep[[9]]
    }
  })


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
      DT::datatable(options = list(dom = 't'))
  })
  ###########
  output$text3 <- renderText({
    paste("Département", current_markers$prov, sep = " : ")

  })


  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(current_markers$Tab, current_markers$vector_dept) - 1
      if (tab_id_position == 0) tab_id_position <- length(current_markers$vector_dept)
      current_markers$Tab <- current_markers$vector_dept[tab_id_position]
    }
  )

  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(current_markers$Tab, current_markers$vector_dept) + 1
      if (tab_id_position > length(current_markers$vector_dept)) tab_id_position <- 1
      current_markers$Tab <- current_markers$vector_dept[tab_id_position]
    }
  )


  output$text3 <- renderText({
    paste("Département", current_markers$Tab, sep = " : ")

  })


  output$tab_comp1 <- DT::renderDT({
    pudc_data_details |>
       dplyr::filter(Provinces == as.character(current_markers$prov)) |>
      dplyr::filter(Composante_code == 1) |>
      dplyr::filter(Departements == as.character(current_markers$Tab)) |>
      dplyr::select(-c(Composante,Composante_code, Provinces, Departements, Observations)) |>
      DT::datatable(
        caption = 'Composante 1: Désenclavement des districts et collectivités locales et infrastructures routières',
        options = list(dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#33e3ff', 'color': '#fff'});",
        "}")))

  })


  output$tab_comp2 <- DT::renderDT({
    pudc_data_details |>
      dplyr::filter(Provinces == as.character(current_markers$prov)) |>
      dplyr::filter(Composante_code == 2) |>
      dplyr::filter(Departements == as.character(current_markers$Tab)) |>
      dplyr::select(-c(Composante,Composante_code, Provinces, Departements, Observations)) |>
      DT::datatable(
        caption = 'Composante 2: Amélioration de l’accessibilité des populations périurbaines et rurales aux infrastructures et services socioéconomiques de base',

        options = list(dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#33e3ff', 'color': '#fff'});",
        "}")) )
  })


  output$tab_comp3 <- DT::renderDT({
    pudc_data_details |>
      dplyr::filter(Provinces == as.character(current_markers$prov)) |>
      dplyr::filter(Composante_code == 3) |>
      dplyr::filter(Departements == as.character(current_markers$Tab)) |>
      dplyr::select(-c(Composante,Composante_code, Provinces, Departements, Observations)) |>
      DT::datatable(
        caption = 'Composante 3: Promotion des économies locales, des chaines de valeurs agro-sylvo-pastorales et de l’employabilité des jeunes et des femmes',
        options = list(dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#33e3ff', 'color': '#fff'});",
        "}")) )
  })

  output$tab_comp4 <- DT::renderDT({
    pudc_data_details |>
      dplyr::filter(Provinces == as.character(current_markers$prov)) |>
      dplyr::filter(Composante_code == 4) |>
      dplyr::filter(Departements == as.character(current_markers$Tab)) |>
      dplyr::select(-c(Composante,Composante_code, Provinces, Departements, Observations)) |>
      DT::datatable(
        caption = 'Composante 4: Renforcement des capacités institutionnelles aux niveaux départemental et local',

        options = list(dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#33e3ff', 'color': '#fff'});",
        "}")) )
  })
}

