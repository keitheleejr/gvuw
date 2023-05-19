library(qualtRics)
library(maps)
library(mapproj)
library(tidyverse)


# load data

data <- read_survey("qualtrics-data.csv")
data <- data |> 
  filter(!is.na(county)) |> 
  mutate(county = tolower(county))

##### MAPS #####

###### All Counties ######

map_data <- data |> 
  mutate(county = tolower(county)) |> 
  drop_na(county) |> 
  group_by(county) |> 
  count()

geo_counties <- map_data("county") |> 
  rename(county = subregion) |>  
  filter(region == "georgia",
         county %in% c("atkinson",
                       "bacon",
                       "berrien",
                       "brooks",
                       "clinch",
                       "coffee",
                       "echols",
                       "jeff davis",
                       "lanier",
                       "lowndes",
                       "ware")) 



map_data <- inner_join(geo_counties, map_data,
                       by = "county")



all_county_map <- ggplot(map_data, aes(x = long, 
                                       y = lat, 
                                       fill = n,
                                       group = n)) +
  geom_polygon(color = "black") + 
  coord_map() +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(name = "Number of\nResponses",
                      low="#fafcff", 
                      high="#1757bf", 
                      na.value = "grey75")

##### County Map Function #####

countymap <- function(countyname){
  map <- map_data |> 
    mutate(fill = ifelse(county == countyname, "blue", "white")) |> 
    ggplot(aes(x = long, 
               y = lat, 
               fill = fill,
               group = group)) +
    geom_polygon(color = "black",
                 show.legend = TRUE) + 
    coord_map() +
    theme_void() +
    scale_fill_identity()
}

##### County Maps #####

atkinson_map <- countymap("atkinson")

bacon_map <- countymap("bacon")

berrien_map <- countymap("berrien")

brooks_map <- countymap("brooks")

clinch_map <- countymap("clinch")

coffee_map <- countymap("coffee")

echols_map <- countymap("echols")

jeffdavis_map <- countymap("jeff davis")

lanier_map <- countymap("lanier")

lowndes_map <- countymap("lowndes")

ware_map <- countymap("ware")
