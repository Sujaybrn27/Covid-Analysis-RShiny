library(tidyverse)
library(vroom)
library(viridis)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)
setwd("~/covid_project/")
download.file(url = 'https://github.com/nychealth/coronavirus-data/archive/master.zip',
              destfile = "corona-data-master.zip")
unzip(zipfile = "corona-data-master.zip")

#read data
percentpos <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")
caserate <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")
testrate <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")

#MODZCTA - Modified zipcode
modzcta <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")
zcta_cov <- vroom("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv", delim = ",")

#data-wrangling
caserates <- caserate %>% select(-c(2:7))
caserates_long <- caserates %>%
                  pivot_longer(2:178,names_to = "modzcta", names_prefix = "CASERATE_", values_to = "caserate")

percentpositive <- percentpos %>% select(-c(2:7))
percentpositive_long <- percentpositive %>%
  pivot_longer(2:178,names_to = "modzcta", names_prefix = "PCTPOS_", values_to = "percentpos")

View(percentpositive_long)

testrates <- testrate %>% select(-c(2:7))
testrates_long <- testrates %>%
  pivot_longer(2:178,names_to = "modzcta", names_prefix = "TESTRATE_", values_to = "testrate")

all <- caserates_long %>%
       left_join(percentpositive_long,by=c('week_ending','modzcta')) %>%
       left_join(testrates_long, by = c('week_ending','modzcta'))

all_modzcta <- geo_join(modzcta, all,
                        'MODZCTA','modzcta',
                        how ='inner')

saveRDS(all_modzcta,"all_modzcta.RDS")

all_modzcta %>%
  ggplot(aes(x=as.numeric(caserate))) +
  geom_histogram(bins = 20, fill = '#69b3a2', color ='white')

labels <- sprintf(
  "<strong>%s</strong><br/>%g cases per 100,000 people",
  all_modzcta$MODZCTA, all_modzcta$caserate) %>%
  lapply(htmltools::HTML)

pal <- colorBin(palette = "viridis", 9, domain = all_modzcta$caserate) #paletter

map_interactive <- all_modzcta %>%
  st_transform(crs = "+init=epsg:4326") %>% #coordinate system
  leaflet() %>% #map
  addProviderTiles(provider = "CartoDB.Positron") %>% #type of map
  addPolygons(label = labels,
              stroke = FALSE,
              smoothFactor = .5,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = ~ pal(caserate),
              highlightOptions = highlightOptions(weight = 5,
                                        fillOpacity = 1,
                                        color = "black",
                                        opacity = 1,
                                        bringToFront = TRUE)) %>%

    addLegend("bottomright",
              pal = pal,
              values = ~ caserate,
              title = "Case per 100,000",
              opacity = 0.7)

saveWidget(map_interactive, "nyc_covid_caserate_map.html")
              
              








