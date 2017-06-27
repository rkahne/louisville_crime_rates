library(tidyverse)
library(zoo)

# Files downloaded from https://data.louisvilleky.gov/dataset/crime-data

crime <- read_csv('D:/Data/Crime_Data_2016_30.csv') %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2015.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2014.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2013.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2012.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2011.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2010.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2009.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2008.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2007.csv')) %>% 
  bind_rows(read_csv('D:/Data/Crime_Data_2006.csv')) %>% 
  mutate(year = as.Date(DATE_OCCURED) %>%  format('%Y') %>% as.numeric())

pop <- tibble(
  population = c(556160, 557789, 566869, 566492, 597269, 600425, 604609, 609863, 612367, 614748, 616261),
  year = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
)

crime_type <- crime$CRIME_TYPE %>% unique()
ct <- crime_type[1]

shiny_crime <- crime %>% select(CRIME_TYPE, year) %>% write_csv('shiny_crime.csv')
