library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(sf)

# tibble dataset w/ missing coordinates

afribats_df <- readr::read_csv("https://figshare.com/ndownloader/files/50882610") |>
  janitor::clean_names() |>
  dplyr::rename(museum = museum_number) |>
  dplyr::mutate(date = {
    ifelse(!is.na(date),paste0(substr(date, 1,7),year),date) |>
      lubridate::dmy()
  })


afribats_sf <- afribats_df |>
  dplyr::filter(!(is.na(longitude) & is.na(latitude))) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),crs = 4326)

usethis::use_data(afribats_df, overwrite = TRUE)
usethis::use_data(afribats_sf, overwrite = TRUE)





readr::write_csv(afribats_df,file = "inst/extdata/afribats.csv")
sf::write_sf(afribats_sf,"inst/extdata/afribats_sf.gpkg",overwrite=TRUE)

