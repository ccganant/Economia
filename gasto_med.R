library(data.table)
library(readxl)
library(tidyverse)
library(stringr)

files<- list.files('./viento', pattern = '.csv')

data1 <- files %>% 
  map_df(~read_csv(paste('/home/cristian/CristianC/Econ/data/viento/',., sep = ''), 
                   col_types = cols(.default = "c")) %>%  mutate(m= 'a'))

data <- data %>% 
  mutate(across(c(2:6), as.numeric))

data<- data %>% filter(Velocidad_Prom != -999) 

data<- data %>% select(-c(Velocidad_Max, Direccion_Max))

save(data, file= 'Viento.RData')


#################################################################

load('Viento.RData')

#############################################################

sta_win<- read.csv('Estaciones_Meteorologicas (1).csv', encoding = 'UTF-8')
sta_win$Codigo<- as.character(sta_win$Codigo)

sta<- as.character(unique(as.factor(str_split(files, '_', simplify = TRUE)[,4])))

sta1<- sta_win %>% filter(Codigo %in% sta)

write_csv(sta1, 'sta1.csv')

####################################################################

files<- list.files('./viento')
data<- tibble()
for (i in 1:length(files)){
  files1<- list.files(paste('./viento/', list.files('./viento')[i], sep = ''), pattern = '.csv')
  files1<- paste(sta[i], files1, sep = '/')
  
  data1 <- files1 %>% 
    map_df(~read_csv(paste('/home/cristian/CristianC/Econ/data/viento/',., sep = ''), 
                     col_types = cols(.default = "c")))

  data1 <- data1 %>% 
    mutate(across(c(2:6), as.numeric)) %>% 
    filter(Velocidad_Prom != -999) %>% 
    select(-c(Velocidad_Max, Direccion_Max)) %>% 
    summarise_if(is_numeric, mean) %>% 
    mutate(Long= sta1$Longitud[i], 
           Lat= sta1$Latitud[i])
  
  data<- rbind(data, data1)
}

##################################################################################3

data<- data %>% drop_na()

write_csv(data, 'data.csv')

###################Calidad de aire#############################################

files<- list.files('./pm25/', pattern = '.csv')

data1 <- files %>% 
  map_df(~read_csv(paste('/home/cristian/CristianC/Econ/data/pm25/',., sep = ''), 
                   col_types = cols(.default = "c")))


data2<- data1 %>% select(1:4) %>% 
  mutate(across(3:4, as.numeric), 
         across(2, as.factor)) %>% 
  filter(pm25 > 0, 
         pm25 < 200) %>% 
  group_by(codigoSerial) %>% 
  summarise(mean_pm25= mean(pm25))


sta_air<- read.csv('Estaciones_CalidadAire.csv', encoding = 'UTF-8')

sta_air<- sta_air %>% mutate(Codigo= as.character(Codigo))

cl_air<- sta_air %>% filter(Codigo %in% data2$codigoSerial) %>% 
  mutate(mean_pm25= data2$mean_pm25)

write_csv(cl_air, 'cl_air.csv')



plot(data2$pm25, type = 'l')
         

