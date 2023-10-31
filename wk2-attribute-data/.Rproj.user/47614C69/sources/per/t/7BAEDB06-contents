# installing packages
install.packages("sf")
install.packages("dplyr")
install.packages("terra")
install.packages("spData")

# load libraries
library(sf)
library(dplyr)
library(terra)
library(spData)

# load data
data(us_states)
data(us_states_df)


# E1
us_states_name <- select(us_states, NAME)
# The class of the new object will be a data frame and it is geographic because it retains the spatial information associated with "us-states"

# E2


us_states_pop <- us_states %>%
  select(contains("total_pop_15"))


us_states_pop <- us_states %>%
  select(matches("total_pop_15"))


us_states_pop_new <- us_states %>%
  select(NAME, total_pop_10, total_pop_15)

# E3

us_states_midwest <- us_states %>%
  filter(REGION == "Midwest")

us_states_midwest

# Belong to the West region, have an area below 250,000 km2and in 2015 a population greater than 5,000,000 residents
west_states <- us_states %>%
  select(NAME, REGION, AREA, total_pop_15) %>%
  filter(REGION == "West", 
         AREA < units::set_units(250000, "km^2"),
         as.numeric(total_pop_15) > 5000000
         )

filter_states
plot(filter_states)

# Belong to the South region, had an area larger than 150,000 km2 or a total population in 2015 larger than 7,000,000 residents.
south_states <- us_states %>%
  select(NAME, REGION, AREA, total_pop_15) %>%
  filter(REGION == "South",
         AREA > units::set_units(150000, "km^2") | as.numeric(total_pop_15) > 7000000
         )

south_states
plot(south_states)

# E4

# Calculate the total population in 2015
total_population_2015 <- us_states %>%
  summarize(total_population_2015 = sum(total_pop_15))

# Calculate the minimum and maximum total population in 2015
min_max_population_2015 <- us_states %>%
  summarise(
    min_population_2015 = min(total_pop_15),
    max_population_2015 = max(total_pop_15)
  )


total_population_2015
min_max_population_2015

# E5

states_per_region <- us_states |>
  group_by(REGION) |>
  summarize(num_states = n())
  
states_per_region

# E6

min_max_pop_region <- us_states |>
  group_by(REGION) |>
  summarize(
    min_pop_region = min(total_pop_15),
    max_pop_region = max (total_pop_15),
    total_pop_region = sum(total_pop_15)
  )

min_max_pop_region

# E7
us_states_stats = us_states |>
  left_join(us_states_df, by = c("NAME" = "state"))

glimpse() # checks more than 10 features
View()

class(us_states_stats)
us_states_stats

# E8
us_states |>
  anti_join(us_states_df, us_states, by = c("NAME" = "state"))

# E9
us_states_pop_density <- us_states |>
  mutate(pop_density_2015 = total_pop_15 / AREA,
         pop_density_2010 = total_pop_10 / AREA)

us_states_pop_density

# E10

pop_density_change <- us_states_pop_density |>
  mutate(change_pop_density = pop_density_2015 - pop_density_2010,
         change_pop_density_percent = (change_pop_density / pop_density_2015) * 100
  )

plot(pop_density_change["change_pop_density_percent"])

# E11

states_columns_lowercase <- us_states %>%
  setNames(tolower(colnames(.)))

# E12

us_states_sel <- us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) |>
  select(median_income_15, geometry) |>
  rename(Income = median_income_15)

us_states_sel

# E13

?us_states_df

us_states_pov <- us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) |>
  mutate(us_states_pov_change = poverty_level_15 - poverty_level_10) |>


us_states_pov

# E14

us_states_pov_other <- us_states_pov %>%
  group_by(REGION) %>%
  summarize(
    min_pop_pov = min(poverty_level_15),
    max_pop_pov = max(poverty_level_15),
    avg_pop_pov = mean(poverty_level_15)
  )


# Classwork 1

states_income_west <- us_states %>%
  left_join(us_states_df, by = c("NAME" = "state")) %>%
  select(NAME, REGION, total_pop_15, median_income_15) %>%
  filter(REGION == "West", total_pop_15 > 150000) %>%
  arrange(desc(total_pop_15))

states_income_west








