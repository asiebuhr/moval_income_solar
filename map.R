install.packages("tigris")
install.packages("sf")
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
devtools::install_github("wmurphyrd/colorplaner")
library(colorplaner)
## define map theme; function stolen from Timo Grossebacher (https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/)

ca <- tracts("CA", cb = TRUE)
ca <- st_sf(ca)

riverside_county <- ca %>%
  rename(fips = GEOID) %>%
  filter(COUNTYFP == "065") %>%
  mutate(
    fips = as.numeric(fips)
  )

moval_tracts_variables <- moval %>%
  left_join(riverside_county)

moval_tracts_variables %>%
  rename(Avg_Income = average_household_income) %>%
  rename(PV_count = solar_system_count) %>%
  mutate(
    Avg_Income_thousands = Avg_Income / 1000
  ) %>% 
  ggplot(aes(fill = Avg_Income_thousands, fill2 = PV_count)) +
  geom_sf() +
  scale_fill_colourplane(color_projection = interpolate_projection,
                         zero_color = "#CC79A7",
                         horizontal_color = "#E69F00",
                         vertical_color = "#56b4E9") +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Map of Moreno Valley Census Tracts",
          subtitle = "Color Key: 
          x = Avg income in Thousand Dollars
          y = PV Count Per Thousand Homes")
 
