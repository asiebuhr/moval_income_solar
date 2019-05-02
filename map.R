install.packages("tigris")
install.packages("sf")
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
devtools::install_github("wmurphyrd/colorplaner")

moval_tracts_variables %>%
  rename(Avg_Income = average_household_income) %>%
  rename(PV_count = solar_system_count) %>%
  mutate(
    Avg_Income_thousands = Avg_Income / 1000
  ) %>% 
  ggplot(aes(fill = Avg_Income_thousands, fill2 = PV_count)) +
  geom_sf() +
  scale_fill_colourplane(color_projection = interpolate_projection,
                         zero_color = "darkorange2",
                         horizontal_color = "darkgreen",
                         vertical_color = "magenta") +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Map of Moreno Valley Census Tracts",
          subtitle = "Color Key: 
          x = Avg income in Thousand Dollars
          y = PV Count Per Thousand Homes")
 


