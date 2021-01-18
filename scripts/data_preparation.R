# in this file i prepare data from different sources for the shiny app

#loading packages ----
library(tidyverse)

# there are two data sets I can use. One is on available on https://opendata.swiss/de/dataset/lernende-der-oeffentlichen-volksschule-nach-schulgemeinden-im-kanton-zuerich/resource/fd300b24-fa42-44f2-a2cc-a9ff14df3327
# the other one I received form the Bildungsstatistik des Kantons Zürich
# some data is present in both data sets some not. The amount of students are not identical. From 2000 - 2018 there are 1288 less students in the data set from open data which seems to be not a lot
# probably for this reason some calculated data are not exactly the same. I use the data from the Bildungsstatistik as main source and the open data data as supplement

# data received ----

# I got this data from the Bildungsstatistik des Kantons Zürich
# data was sended in two files
#* loading data ----

df_sa_1 <- read_delim("raw-data/daten_klassen_stadt_zuerich_erhalten.csv", delim = ";", locale = locale(encoding = 'utf-8')) #2009-2019
df_sa_2 <- read_delim("raw-data/daten_klassen_stadt_zuerich_erhalten_2.csv", delim = ";", locale = locale(encoding = 'utf-8')) #2000-2008

#joining the files
df_sa <- full_join(df_sa_1, df_sa_2) 

#* data preparation------
df_sa <- df_sa %>% 
  mutate(Schulgemeinde = str_replace(Schulgemeinde, "Zürich-", "")) %>% # get ride of the Zürich- pre word
  mutate(
    Bildungsart_2 = case_when(                                          # create a new, less detailed Bildungsart
      Bildungsart == "Aufnahmeklasse" ~ "Aufnahmklasse",
      Bildungsart == "Aufnahmeklasse KG (Durchgangszentr. Asylsuchende)" ~ "Aufnahmklasse",
      Bildungsart == "Aufnahmeklasse Prim-Sek (Durchgangsz. Asylsuchend)" ~ "Aufnahmklasse",
      Bildungsart == "Kleinklasse B (Kleinklasse für Lernbehinderte)" ~ "Kleinklasse",
      Bildungsart == "Kleinklasse C (Hör- und Sprachbehinderte)" ~ "Kleinklasse",
      Bildungsart == "Kleinklasse D (Verhaltensauffällige)" ~ "Kleinklasse",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung A" ~ "Sekundarstufe A",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung B" ~ "Sekundarstufe B",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung C" ~ "Sekundarstufe C",
      Bildungsart == "Sekundarschule Abteilung A" ~ "Sekundarstufe A",
      Bildungsart == "Sekundarschule Abteilung B" ~ "Sekundarstufe B",
      Bildungsart == "Sekundarschule Abteilung C" ~ "Sekundarstufe C",
      Bildungsart == "Einschulungsklasse" ~ "Einschulungsklasse",
      Bildungsart == "Volksschule, Grundstufe" ~ "Volksschule, Grundstufe",
      Bildungsart == "Regelkindergarten" ~ "Kindergarten",
      Bildungsart == "Stufenkindergarten" ~ "Kindergarten",
      Bildungsart == "Primarschule" ~ "Primarschule",
      TRUE ~ "andere")
  ) %>% 
  mutate(
    Schulart = case_when(                                                  # creat an even less detailed Bildungsart bzw. Schulart
      Bildungsart == "Aufnahmeklasse" ~ "Sonderschulen",
      Bildungsart == "Aufnahmeklasse KG (Durchgangszentr. Asylsuchende)" ~ "Sonderschulen",
      Bildungsart == "Aufnahmeklasse Prim-Sek (Durchgangsz. Asylsuchend)" ~ "Sonderschulen",
      Bildungsart == "Kleinklasse B (Kleinklasse für Lernbehinderte)" ~ "Sonderschulen",
      Bildungsart == "Kleinklasse C (Hör- und Sprachbehinderte)" ~ "Sonderschulen",
      Bildungsart == "Kleinklasse D (Verhaltensauffällige)" ~ "Sonderschulen",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung A" ~ "Sekundarstufe",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung B" ~ "Sekundarstufe",
      Bildungsart == "Dreiteilige Sekundarschule Abteilung C" ~ "Sekundarstufe",
      Bildungsart == "Sekundarschule Abteilung A" ~ "Sekundarstufe",
      Bildungsart == "Sekundarschule Abteilung B" ~ "Sekundarstufe",
      Bildungsart == "Sekundarschule Abteilung C" ~ "Sekundarstufe",
      Bildungsart == "Einschulungsklasse" ~ "Übergangsklassen",
      Bildungsart == "Volksschule, Grundstufe" ~ "Übergangsklassen",
      Bildungsart == "Regelkindergarten" ~ "Kindergarten",
      Bildungsart == "Stufenkindergarten" ~ "Kindergarten",
      Bildungsart == "Primarschule" ~ "Primarschule",
      TRUE ~ "andere")
  )





# open data data ----
#* loading data  ----
# Important, not included are students from higher schools (higher than Sekundarstufe I), students in special classes and special schools, and students in private schools
df_cl<- read_delim("raw-data/ZH_Lernende_Volksschule_Regional.csv", delim = "\t", locale = locale(encoding = 'utf-8')) 


#* data preparation ----
df_cl <- df_cl %>% 
  filter(str_detect(Schulgemeinde, 'Zürich')) %>% # use only districts that belong to the city of Zurich
  mutate(Schulgemeinde = str_replace(Schulgemeinde, "Zürich-", "")) %>%  # remove the Zürich Tag
  filter(Schulgemeinde != 'Zürich') 
  

# All school levels ----
#not using Sonderschulen

#* Amount of students ----
df_sa_t_s <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  summarise(students = sum(alle)) 

#* esd ratio ----
df_sa_t_d <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  summarise(esd = (1- sum(fspr)/sum(alle))) 

#* average class size -----
df_sa_cs <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>% 
  group_by(Jahr, Schulgemeinde, kla_Id) %>% 
  mutate(cs = sum(alle)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  summarise(acs = round(mean(cs)))

# join the data together

df_sa_t <- df_sa_t_s %>%
  left_join(df_sa_t_d) %>%
  left_join(df_sa_cs) 

write_delim(df_sa_t, "processed-data/districts_total_esd_students_acs.csv", delim = ";")

#* citizenship ----
df_cl_c <- df_cl %>%  
  group_by(Jahr, Schulgemeinde, Land) %>% 
  summarise(country_n = sum(Anzahl)) %>%    # there are students which have the continent as country like afrika, asia or middle and south america further there are stateless students and some with unknown citisienship
  ungroup() %>%
  group_by(Jahr, Schulgemeinde) %>% 
  mutate(per_n = country_n/sum(country_n)) %>% 
  ungroup() %>% 
  mutate(rank = rank(-country_n, ties.method = "min"))

# group nations with less than two percent to others 
df_cl_cr <- df_cl_c %>%  #reduce numbers of countries by combining those whit low numbers tho others
  mutate(country_g = if_else(per_n>0.02, Land, "Andere"))  %>% 
  group_by(Jahr, Schulgemeinde, country_g) %>% 
  summarise(per_g = sum(per_n)) %>% 
  mutate(SG=Schulgemeinde) %>% # is used for graphics
  ungroup() %>% 
  mutate(rank = rank(-per_g, ties.method = "min"))

write_delim(df_cl_cr, "processed-data/districts_total_citizenship.csv", delim = ";")

#* different nations ----
df_d_c <- df_cl_c %>%  
  group_by(Jahr, Schulgemeinde) %>%
  mutate(diff_c = length(Land))

write_delim(df_d_c, "processed-data/districts_total_diff_citizenship.csv", delim = ";")  

  

# Kindergarten und Primarschule ----


#* distribution ----
df_sa_kp_di <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Schulart %in% c("Kindergarten", "Übergangsklassen", "Primarschule")) %>% 
  group_by(Jahr, Schulgemeinde, Schulart) %>% 
  summarise(students = sum(alle)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  mutate(per = round((students/sum(students))*100,1))


#* esd ----
df_sa_kp_d <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Schulart %in% c("Kindergarten", "Übergangsklassen", "Primarschule")) %>% 
  group_by(Jahr, Schulgemeinde, Schulart) %>% 
  summarise(esd = (1 - sum(fspr)/sum(alle))) 


#* average class size ----
df_sa_kp_acs <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Schulart %in% c("Kindergarten", "Übergangsklassen", "Primarschule")) %>% 
  group_by(Jahr, Schulgemeinde, Schulart, kla_Id) %>% 
  mutate(cs = sum(alle)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde, Schulart) %>% 
  summarise(acs = round(mean(cs)))



# join the data together
df_sa_kp <- df_sa_kp_di %>%
  left_join(df_sa_kp_d) %>%
  left_join(df_sa_kp_acs) 

write_delim(df_sa_kp, "processed-data/districts_kinder_primar_esd_students_acs.csv", delim = ";")






# Sekundarstufe ----


#* distribution ----
df_sa_sek_di <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Bildungsart_2 %in% c("Sekundarstufe A", "Sekundarstufe B", "Sekundarstufe C")) %>% 
  group_by(Jahr, Schulgemeinde, Bildungsart_2) %>% 
  summarise(students = sum(alle)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde) %>% 
  mutate(per = round((students/sum(students))*100,1))


#* esd ----
df_sa_sek_d <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Bildungsart_2 %in% c("Sekundarstufe A", "Sekundarstufe B", "Sekundarstufe C")) %>% 
  group_by(Jahr, Schulgemeinde, Bildungsart_2) %>% 
  summarise(esd = (1-sum(fspr)/sum(alle))) 


#* average class size ----
df_sa_sek_acs <- df_sa %>% 
  filter(Schulart!="Sonderschulen") %>%
  filter(Bildungsart_2 %in% c("Sekundarstufe A", "Sekundarstufe B", "Sekundarstufe C")) %>% 
  group_by(Jahr, Schulgemeinde, Bildungsart_2, kla_Id) %>% 
  mutate(cs = sum(alle)) %>% 
  ungroup() %>% 
  group_by(Jahr, Schulgemeinde, Bildungsart_2) %>% 
  summarise(acs = round(mean(cs)))



# join the data together
df_sa_sek <- df_sa_sek_di %>%
  left_join(df_sa_sek_d) %>%
  left_join(df_sa_sek_acs) 

write_delim(df_sa_sek, "processed-data/districts_sek_esd_students_acs.csv", delim = ";")

# #* citizenship ----
# df_cl_c_kg <- df_cl %>%  
#   group_by(Jahr, Schulgemeinde, Land) %>% 
#   summarise(country_n = sum(Anzahl)) %>%    # there are students which have the continent as country like afrika, asia or middle and south america further there are stateless students and some with unknown citisienship
#   ungroup() %>%
#   group_by(Jahr, Schulgemeinde) %>% 
#   mutate(per_n = country_n/sum(country_n)) %>% 
#   ungroup() %>% 
#   mutate(rank = rank(-country_n, ties.method = "min"))
# 
# # group nations with less than two percent to others 
# df_cl_cr_kg <- df_cl_c %>%  #reduce numbers of countries by combining those whit low numbers tho others
#   mutate(country_g = if_else(per_n>0.02, Land, "Andere"))  %>% 
#   group_by(Jahr, Schulgemeinde, country_g) %>% 
#   summarise(per_g = sum(per_n)) %>% 
#   mutate(SG=Schulgemeinde) %>% # is used for graphics
#   ungroup() %>% 
#   mutate(rank = rank(-per_g, ties.method = "min"))
# 
# write_delim(df_cl_cr, "processed-data/districts_citizenship.csv", delim = ";")
# 
# #* different nations ----
# df_d_c <- df_cl_c %>%  
#   group_by(Jahr, Schulgemeinde) %>%
#   mutate(diff_c = length(Land))
# 
# write_delim(df_d_c, "processed-data/districts_diff_citizenship.csv", delim = ";")  




# #* languages 
# df_cl_l <- df_cl %>%  
#   filter(str_detect(Schulgemeinde, 'Zürich')) %>% # use only districts that belong to the city of Zurich
#   mutate(Schulgemeinde = str_replace(Schulgemeinde, "Zürich-", "")) %>%  # remove the Zürich Tag
#   filter(Schulgemeinde != 'Zürich') %>%    # there are 21 students which are in the district Zürich which is not a district, don't know where they belong
#   group_by(Jahr, Schulgemeinde, Erstsprache) %>% 
#   summarise(language_n = sum(Anzahl)) %>%    # there are students which have the continent as country like afrika, asia or middle and south america further there are stateless students and some with unknown citisienship
#   ungroup() %>%
#   group_by(Jahr, Schulgemeinde) %>% 
#   mutate(lang_per_n = language_n/sum(language_n)) %>% 
#   ungroup() %>% 
#   group_by(Jahr, Schulgemeinde) %>% 
#   mutate(rank = rank(-language_n, ties.method = "min"))
# 
# #* daf (Deutsch als Zweitsprache) 
# df_daf <- df_cl_l %>% 
#   filter(Erstsprache != "Deutsch") %>% 
#   group_by(Jahr, Schulgemeinde) %>% 
#   summarise(daf = sum(lang_per_n)) %>% 
#   ungroup() 
# 
# write_delim(df_daf, "processed-data/districts_daf.csv", delim = ";")
# 
# 












# calculate ratios and absolut numbers for specific years and school districts
# df_sa <- df_sa %>% 
#   group_by(Jahr, Schulgemeinde) %>% 
#   mutate(students_a = sum(alle)) %>%  # amount of students over all school types
#   mutate(daf_a = sum(fspr)/sum(alle)) %>%  # ratio of daf students over all school types
#   ungroup() %>% 
#   group_by(Jahr, Schulgemeinde, Schulart) %>% 
#   mutate(students_schulart = sum(alle)) %>%   # amount of students over schulart
#   mutate(students_schulart_r = round(students_schulart/students_a, 2)) # ratio of the different Bildungsarte_2 
# 
# 
# 


