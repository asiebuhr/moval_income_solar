library(tidyverse)
library(readxl)
library(ggthemes)
library(broom)
theme_set(theme_minimal())

ce3 <- read_excel("ce3.xlsx")

county <- read_excel("county.xlsx")

moval <- ce3 %>%
  rename(fips = `Census Tract`) %>%
  left_join(county) %>%
  filter(City == "Moreno Valley")

## census tract average household income against total panel area for each census tract
moval %>%
  mutate(
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(average_household_income, total_panel_kw)) +
  geom_point() +
  geom_smooth(color = "black", method = "lm", se = FALSE) +
  labs(x = "Avg Household income for each MoVal Census Tract", 
       y = "Estimated PV System capacity for each MoVal Census Tract in kilowatts") +
  ggtitle("Analysis of Avg Household Income and total amount of Solar for each MoVal Census Tract",
          subtitle = "Data from Cal EnviroScreen 3.0 and Stanford Deepsolar Initiative")

 ## census tract poverty level against total panel area for each census tract
moval %>%
  ggplot(aes(Poverty, total_panel_area)) +
  geom_point() +
  geom_smooth(color = "black", method = "lm") +
  labs(x = "Census Tract Poverty Level", y = "Total Panel Area in squared feet in Census Tract") +
  ggtitle("Analysis of Poverty level and total amount of Solar for each MoVal Census Track", 
          subtitle = "Data from Cal EnviroScreen 3.0 and Stanford Deepsolar Initiative")

## census tract unemployment leve against total panel arrea for each census tract
moval %>%
  ggplot(aes(`Unemployment Pctl`, total_panel_area)) +
  geom_point() +
  scale_y_log10() 

View(moval)

moval <- moval %>%
  select(-county) %>%
  rename(county = `California County`)

moval %>% 
  ggplot(aes(average_household_income, total_panel_area)) + 
  geom_point() + 
  geom_smooth(color = "black", method = "lm") +
  scale_y_log10()


moval %>% 
  filter(total_panel_area < 10000) %>%
  ggplot(aes(average_household_income, total_panel_area)) + 
  geom_point() + 
  geom_smooth(color = "black", method = "lm")

moval %>%
  ggplot(aes(total_panel_area, average_household_income)) +
  geom_point() +
  scale_x_log10()

moval_model <- lm(total_panel_area ~ log10(average_household_income), data = moval)

statistics <- moval_model %>%
  tidy()

intercept <- statistics[1,2][[1]]

average_household_income_estimate <- statistics[2,2][[1]]

moval %>%
  mutate(
    total_panel_area_estimates = intercept + average_household_income_estimate * average_household_income
  ) %>%
  ggplot() +
  geom_point(aes(average_household_income, total_panel_area)) +
  geom_line(aes(average_household_income, total_panel_area_estimates))

  
moval %>%
  mutate(
    total_panel_area_estimates = intercept + average_household_income_estimate * average_household_income
  ) %>%
  ggplot() +
  geom_point(aes(total_panel_area, total_panel_area_estimates))


## census tract average household income against total capacity in log base 10 for each census tract
moval %>%
  mutate(
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(average_household_income, total_panel_kw)) +
  geom_point() +
  geom_smooth(color = "black", method = "lm", se = FALSE) +
  scale_y_log10() +
  labs(x = "Average Household Income for each MoVal census tract",
       y = "Estimated PV System capacity for each MoVal Census Tract in kilowatts in log based 10") +
  ggtitle("Analysis of Avg Household Income and total amount of Solar for each MoVal Census Tract",
          subtitle = "Data from Cal EnviroScreen 3.0 and Stanford Deepsolar Initiative")


## housing burden
moval %>%
  mutate(
    `Housing Burden` = as.numeric(`Housing Burden`),
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(`Housing Burden`, total_panel_kw)) +
  geom_point() +
  scale_y_log10()

# unemployment
moval %>%
  mutate(
    Unemployment = as.numeric(Unemployment),
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(Unemployment, total_panel_kw)) +
  geom_point() +
  geom_smooth(color = "#E69F00", method = "lm", se = FALSE) +
  scale_y_log10()


## linguistic isolation

moval %>%
  mutate(
    `Linguistic Isolation` = as.numeric(`Linguistic Isolation`),
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(`Linguistic Isolation`, total_panel_kw)) +
  geom_point() +
  geom_smooth(color = "#E69F00", method = "lm", se = FALSE) +
  scale_y_log10()

## Education
moval %>%
  mutate(
    Education = as.numeric(Education),
    total_panel_kw = total_panel_area * 144 * (1 / 2535) * .320 ## convert to total panel capacity in kW
  ) %>%
  select(total_panel_kw, everything()) %>%
  ggplot(aes(Education, total_panel_kw)) +
  geom_point() +
  geom_smooth(color = "#E69F00", method = "lm", se = FALSE) +
  scale_y_log10()





