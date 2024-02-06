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

#solution
anas_data1 <- bird_pops_long %>% 
  filter(Species_name == "Anas platyrhynchos",
         Country.list%in%str_subset(pattern= "United Kingdom", 
                                    Country.list)) # esto lo que hace es incluir los casos en los que aparecen varios paises
#para que coja los casos los que aprezca United kingdom con otros paises. 

bird_pops_long %>%
  # Remove NA from Abundance
  drop_na(Abundance) %>% 
  # Group rows so that each group is one population
  # *** group_by() function from dplyr
  group_by(id) %>%  
  # Make some calculations
  # *** mutate() function from dplyr
  mutate(maxyear = max(Year), 
         minyear = min(Year),
         # Calculate duration
         Duration = maxyear - minyear) %>%
  # Remove any groupings we have created in the pipe
  ungroup() %>% 
  # Remove duplicate time series
  distinct(id, .keep_all=T) %>% 
  # Select the columns Species_name and Duration 
  select(id, Species_name, Duration) %>% 
  # Use arrange to order the data from longer to shorter time series
  arrange(desc(Duration))

# to see the records for a specific order

# Using "group_by()" to calculate a "tally"
# for the number of records per Order

bird_pops_long %>% 
  drop_na(Abundance) %>% 
  group_by(Order) %>% 
  tally() %>%
  arrange(desc(n))


#TASK: 1. Which country has the longest population time series?




bird_pops_long1 <- bird_pops_long %>% 
  mutate(maxyear = max(Year), 
         minyear = min(Year),
         # Calculate duration
         Duration = maxyear - minyear)


bird_pops_long1 %>% 
  drop_na(Duration) %>% 
  group_by(id, Country.list) %>% 
  tally() %>%
  arrange(desc(n)) #Canada


# para hacer estos pasos juntos lo que se sugiere: 

bird_pops_long1 %>% 
  drop_na(Abundance) %>% 
  group_by(id, Country.list) %>% 
  summarise(duration= max(Year)-min(Year))  %>% 
  arrange(desc(duration))
  
  
#2. Which country has the highest number of population time series?


bird_pops_long %>% 
  drop_na(Abundance) %>% 
  group_by(id,Country.list) %>% 
  tally() %>% 
  arrange(desc(n)) #canada


#Merging data with time series

# first load the data
bird_diet <- read.csv("elton_birds.csv")

# Let's have a look

head(bird_diet)

# Select just the species and their diet
bird_diet <- bird_diet %>% 
  dplyr::select(Scientific, `Diet.5Cat`) %>% 
  distinct(Scientific, .keep_all = T) %>% rename(diet = `Diet.5Cat`)

# Join the two data sets
bird_all <- bird_pops_long %>% 
  left_join(bird_diet, by=c("Species_name"="Scientific")) # Notice that we select the two columns here that we want to match 

# We explore
head(bird_all, 2)

#other way to do it

#bird_all <- bird_pops_long %>% 
 # left_join(bird_diet %>% dplyr::select(Scientific, `Diet.5Cat`) %>% 
  #            distinct(Scientific, keep_all=T) %>% 
   #           rename(diet = `Diet.5Cat`, Species_name=Scientific)) 

write.csv(bird_all, "birds_all.csv") 

#ploting
falcon <- bird_all %>% filter(Species_name=="Falco peregrinus")

## make a ggplot object
ggplot(data = falcon, aes(x = Year, y = Abundance))

## make a ggplot object
ggplot(data = falcon, aes(x = Year, y = Abundance)) +
  geom_point()

## make a ggplot object
ggplot(data = falcon, aes(x = Year, y = Abundance, group=id, colour=id)) +
  geom_point()+
  geom_line()

## make a ggplot object
ggplot(data = falcon, aes(x = Year, y = Abundance, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap(.~Country.list)


falcon %>%
  group_by(id) %>% 
  drop_na(Abundance) %>% 
  filter(length(unique(Year))>4) %>%
  ungroup() %>% 
  mutate(logabundance= log(Abundance+1)) %>% 
  ggplot(aes(x = Year, y = logabundance, group=id)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Country.list, scales = "free_y")


# MEtBrewer es un paquete que saca los colores
# de piezas de arte

falcon %>%
  group_by(id) %>% 
  drop_na(Abundance) %>% 
  filter(length(unique(Year))>4) %>%
  ungroup() %>%
  mutate(logabundance= log(Abundance+1)) %>% 
  ggplot(aes(x = Year, y = logabundance, 
             group=id, 
             colour=Country.list)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Country.list)+
  scale_colour_manual(values = met.brewer("Hokusai1", 7))



# si pones esto al principio del codigo, no tiesnes
# que volver a definirlo en el resto de los plots
# que tengas que hacer
theme_set(theme_minimal()+
            theme(axis.title = element_text(size=15),
                  axis.line = element_line(color="black", size = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(color="black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color="black")))


fig1 <- falcon %>%
  group_by(id) %>% 
  drop_na(Abundance) %>% 
  filter(length(unique(Year))>4) %>%
  ungroup() %>%
  mutate(logabundance= log(Abundance+1)) %>% 
  ggplot(aes(x = Year, y = logabundance, 
             group=id, 
             colour=Country.list)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Country.list, scales = "free_x")+
  scale_colour_manual(values = met.brewer("Hokusai1", 7))+
  xlim(min(falcon$Year), max(falcon$Year))+
  labs(y="Abundance", x="Year", title="Example plot time series")+
  theme(legend.position = "none")

ggsave("Figure1.pdf", fig1, width=10, height = 8)
