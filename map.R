install.packages("tigris")
install.packages("sf")
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
devtools::install_github("wmurphyrd/colorplaner")

## define map theme; function stolen from Timo Grossebacher (https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/)

# hsv
# Position along diagonal is value
# Distance from the diagonal is saturation
# upper triangle or lower triangle is hue.

col_func <- function(x, y){
  
  x[x == 0] <- 0.000001
  y[y == 0] <- 0.000001
  x[x == 1] <- 0.999999
  y[y == 1] <- 0.999999
  
  # upper or lower triangle?
  u <- y > x
  
  # Change me for different hues.
  hue <- ifelse(u, 0.3, 0.8)
  
  
  # distace from (0,0) to (x,y)
  hyp <- sqrt(x^2 + y^2) 
  
  # Angle between x axis and line to our point
  theta <- asin(y / hyp)
  
  # Angle between 45 degree line and (x,y)
  phi <- ifelse(u, theta - pi/4, pi/4 - theta)
  phi <- ifelse(phi < 0, 0, phi)
  
  # Distance from 45 degree line and (x,y)
  s <- hyp * sin(phi) / sqrt(2)
  
  # Draw line from (x, y) to 45 degree line that is at right angles.
  # How far along 45 degree line, does that line join.
  v <- 1 - hyp * cos(phi) / sqrt(2)
  
  # Get hsv values.
  sapply(seq_along(x), function(i) hsv(hue[i], s[i], v[i]))
  
}


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
 


