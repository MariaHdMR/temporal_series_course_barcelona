# sesion 3. Corresponde al dia 7 de febrero
# empezamos con comunidades
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# cargamos librerias

library(tidyverse) # manage data
library(DT) # visualise tables in html
library(viridis) # use color-blind friendly palettes
library(ggrepel) # avoid overlapping texts in the figures
library(codyn) # calculate community dynamics metrics
library(vegan) # calculate community ecology metrics
library(eulerr) # calculate Venn and Euler diagrams

#define the theme for the plots
theme_set(theme_minimal()+
            theme(axis.title = element_text(size = 15),
                  axis.line = element_line(color = "black",
                                           linewidth = 0.5),
                  panel.grid.major = element_blank(),
                  axis.text = element_text(color = "black", size = 12),
                  strip.text.x = element_text(size = 12),
                  axis.ticks = element_line(color = "black")))

# load the data
ants_raw <- read.csv("ants_logged.csv")

glimpse(ants_raw)

ants_raw %>% 
  distinct(block, plot, treatment) # extract the different combinations of block, plot and treatment


#are missing values??
anyNA(ants_raw) # is there any missing value? (Boolean answer)

# how are NAs distributed? And what might NAs exactly mean, in this data set?
# let's see it by year, for example
ants_raw %>% 
  mutate(cell_content = case_when(is.na(abundance) ~ "NA",
                                  abundance == 0 ~ "0",
                                  T ~ ">0")) %>%
  # we create a new variable capturing cell content
  # as we are interested in defining 3 different situations, we use the case_when function
  ggplot(aes(x = year)) +
  geom_histogram(aes(fill = cell_content),
                 binwidth = .5, center = 0) +
  scale_fill_discrete(drop = F) +
  labs(y = "Number of rows")

# vemos que pasa en el año 2007
ants_raw %>% 
  filter(year == 2007) %>% 
  DT::datatable()
# and let's see it by year x species too
ants_raw %>% 
  mutate(cell_content = case_when(is.na(abundance) ~ "NA",
                                  abundance == 0 ~ "0",
                                  T ~ ">0")) %>% 
  ggplot(aes(x = year, y = code, fill = cell_content)) +
  geom_tile(color = "white") +
  # scale_x_continuous(breaks = seq(from = 1970, to = 2015, by = 5)) +
  labs(y = "species code") +
  theme(axis.text.y = element_text(size = 8, face = "italic"))



# DAta cleaning
# Adapt the longer data set to a yearly resolution (not date)
ants_long <- ants_raw %>%
  select(year, genus, species, code, abundance) %>% # the variables we will work with
  group_by(year, genus, species, code) %>% 
  summarise(total_abundance = sum(abundance)) %>% # calculate the total abundance per year and species
  ungroup()

# now we need to have the wide format


ants_wide <-  ants_long %>% 
  pivot_wider(id_cols =year , 
              names_from = c(genus, species),
              names_sep = " ",
              values_from = total_abundance,
              values_fill = list(total_abundance = 0)) %>%
                column_to_rownames(var= "year")
              
#how many species do we have?
dim(ants_wide)
#seems that there is a problem, that there is a new species
ants_long %>% 
  summarise(n_code = n_distinct(code), # count the number of different species codes
            n_sp = n_distinct(paste(genus, species))) # count the number of different species names

# this is to see where is the error           
ants_long %>% 
  group_by(code) %>% 
  mutate(n_sp = n_distinct(paste(genus, species))) %>% 
  filter(n_sp > 1) %>% # keep those rows with more than one species name per code
  distinct(genus, species, code)

# code to solve the problem

ants_long <- ants_long %>% 
  mutate(species = if_else(species == "lognispinosus", # if the specific epithet in that row is wrong
                           "longispinosus", # change it by the correct one
                           species)) # but conserve the specific epithet of the other rows

# volvemos a hacer el formato largo
ants_wide <- ants_long %>%
  pivot_wider(id_cols = year,
              names_from = c(genus, species),
              values_from = total_abundance,
              values_fill = list(total_abundance = 0),
              names_sep = " ") %>% 
  column_to_rownames(var = "year")

dim(ants_wide) # ya esta solucionado el problema

