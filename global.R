source("dependecies.R")


# read in shape data into global environment


shapefile <- sf::st_read("data/gab_admbndp2_1m_salb.shp")

pudc_data_composante <- read_excel("data/pudc_data_composante.xlsx",
                                   sheet = "composantes")

pudc_data_details <- read_excel("data/pudc_data_composante.xlsx",
                                sheet = "details") |>
  dplyr::filter(!is.na(Composante_code))


shapefile <- sf::st_as_sf(shapefile)  %>%
  mutate(fill_color = case_when(ADM1_NAME == "Moyen-Ogooue" ~ "#FF0000",
                                ADM1_NAME == "Nyanga" ~ "#FFFF00",
                                ADM1_NAME == "Haut-Ogooue" ~ "#9FE2BF",
                                ADM1_NAME == "Ogooue-Maritime" ~ "#0000FF",
                                ADM1_NAME == "Ngounie" ~"#FF00FF",
                                ADM1_NAME == "Woleu-Ntem" ~"#008000",
                                ADM1_NAME == "Ogooue-Ivindo" ~ "#800000",
                                ADM1_NAME == "Estuaire" ~"#FF7F50",
                                ADM1_NAME == "Ogooue-Lolo" ~"#34495e"
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
