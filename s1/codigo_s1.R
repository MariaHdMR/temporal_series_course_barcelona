# Dia 6 de febrero: primer dia del curso de series temporales
#### SESION 1###

# instalar las librerias
library(tidyverse)
library(broom)
library(MetBrewer)
library(cowplot)
library(maps)
library(mapdata)

# set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load the data
bird_pops <- read.csv(file="bird_pops.csv")
# We examine the data.
head(bird_pops)

#contro+shif+m <- hace los pipes de tidyverse

# ahora lo que vams a hacer es cambiar el formato, y pasarlo a pivot_largo, vamos a hacer una columna de ID con
# los años

bird_pops_long <-bird_pops %>% 
  pivot_longer(cols=c(24:68), # Columns we want to pivot over (in this case years)
               names_to ="Year", # Years will be the previous column names
               values_to= "Abundance") # Abundance will contain the values of those columns 
head(bird_pops_long)

#arreglamos un poco los datos
bird_pops_long<- bird_pops_long %>% 
  # First we create a column named Species_name using the function paste applied on genus and species. 
  mutate(Species_name=paste(Genus, Species), 
         # Then, we apply the function parse_number to correct numbers in the column Years
         Year=parse_number(Year)) # esto nos va a quitar las X de los numeros

#We can have a look 

head(bird_pops_long$Species_name)

#filtering the data

falco_data <- bird_pops_long %>% 
  filter(Species_name=="Falco peregrinus", 
         Country.list=="Spain")

# Look at the data 

falco_data

# si a la hora de filtrar queremos seleccionar varios, podemos coger: species_name %in% c("falco peregrinus", "fagus sylvatica)


# task 
anas_data <- bird_pops_long %>% 
  filter(Species_name=="Anas platyrhynchos", 
         Country.list=="United Kingdom")
head(anas_data)
