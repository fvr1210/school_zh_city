# in this file i prepare data from different sources for the shiny app

#loading packages ----
library(tidyverse)


# loading data to citizenship and languages
df_cl<- read_delim("raw-data/ZH_Lernende_Volksschule_Regional.csv", delim = "\t", locale = locale(encoding = 'utf-8')) 

# creating dataset with the numbers of students in a specific year, level, schooldistrict and citizenship. Important, not included are  secondary students, students in special classes (special classes) and special schools, and students in public schools
df_cl_c <- df_cl %>%  
  filter(str_detect(Schulgemeinde, 'Zürich')) %>% # use only districts that belong to the city of Zurich
  mutate(Schulgemeinde = str_replace(Schulgemeinde, "Zürich-", "")) %>%  # remove the Zürich Tag
  filter(Schulgemeinde != 'Zürich') %>%    # there are 21 students which are in the district Zürich which is not a district, don't know where they belong
  group_by(Jahr, Schulgemeinde, Land) %>% 
  summarise(country_n = sum(Anzahl)) %>%    # there are students which have the continent as country like afrika, asia or middle and south america further there are stateless students and some with unknown citisienship
  ungroup() %>%
  group_by(Jahr, Schulgemeinde) %>% 
  mutate(per_n = country_n/sum(country_n)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  ungroup() %>% 
  mutate(rank = rank(-country_n, ties.method = "min"))

df_cl_cr <- df_cl_c %>%  #reduce numbers of countries by combining those whit low numbers tho others
  mutate(country_g = if_else(per_n>0.01, Land, "others"))  %>% 
  group_by(Jahr, Schulgemeinde, country_g) %>% 
  summarise(per_g = sum(per_n)) %>% 
  mutate(SG=Schulgemeinde) %>% # is used for graphics
  ungroup()
  
write_delim(df_cl_cr, "processed-data/districts_citizenship.csv", delim = ";")

