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



# How do the number of species change across years?
ants_long %>% 
  group_by(year) %>% 
  summarise(n_species = n_distinct(code)) %>% # count the number of different species by year
  ggplot(aes(x = year, y = n_species)) +
  geom_line() +
  geom_point(size = 3) +
  geom_smooth(se = F) +
  #making the plot fancier:
  geom_text(aes(label = n_species), nudge_y = 1) + # adding the number of species as a label
  geom_vline(aes(xintercept = 2005, color = "logging"), linetype = "dashed") + # indicate logging year
  guides(color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  # a sequence taking the limitis of the axis
  labs(y = "richness")

#How does total ant abundance change among species
ants_long %>% 
  group_by(year) %>% 
  summarise(total_abundance = sum(total_abundance)) %>% 
  ggplot(aes(x = year, y = total_abundance)) +
  geom_line() + geom_point(size = 3) +
  geom_smooth(se = F) +
  geom_vline(aes(xintercept = 2005, color = "logging"), linetype = "dashed") + # indicate logging year
  guides(color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  labs(y = "Number of ants\nacross all species")

#How does total ant abundance change across years?
ants_long %>%
  group_by(genus, species) %>% 
  summarise(total_abundance = sum(total_abundance,
                                  na.rm = T)) %>% 
  ggplot(aes(x = reorder(paste(genus, species),
                         -total_abundance), # x axis are complete species names ordered by their abundance
             y = total_abundance)) + 
  geom_col(color = "black", fill = "grey") +
  labs(y = "Number of ants\nacross all years") +
  theme(axis.text.x = element_text(face = "italic",
                                   angle = 90,
                                   vjust = .25,
                                   hjust = 1),
        axis.title.x = element_blank()) 

# Is tis RAD maintained accross years?

# let's have first the order we obtained in the overall RAD
ordered_ab <- ants_long %>%
  group_by(code) %>% 
  summarise(total_abundance = sum(total_abundance, na.rm = T)) %>% 
  arrange(desc(total_abundance))

# and apply this overall order to yearly RADs
ants_long %>% 
  mutate(code = factor(code,levels = ordered_ab$code)) %>%
  split(.$year) %>% 
  map2(names(.), #esto es para iteracionar y guardar los plots con nombres
  #map(
    ~ ggplot(data = .x, aes(x = code, y = total_abundance)) + 
      geom_col(color = "black", fill = "grey") +
      scale_x_discrete(drop = F) +
      ylim(0,130) +
      theme(axis.text.x = element_text(face = "italic", angle = 90, vjust = .25, hjust = 1,
                                       size = 8),
            strip.text = element_text(size = ),
            axis.title.x = element_blank(),
            panel.border = element_rect(fill = NA))
  )
# + labs(title = .y))

#Turnover analysis: did the taxonomic composition of the community change over time?

ants_turnover <- ants_long %>% 
  mutate(total_abundance = 1) %>% # to include 2007 in the turnover analysis
  codyn::turnover(time.var = "year",
                  species.var = "code",
                  abundance.var = "total_abundance")
ants_turnover %>% 
  ggplot(aes(x = year, y = total)) +
  geom_point() + geom_line() +
  geom_vline(aes(xintercept = 2005), linetype = "dashed", color = "black") +
  scale_color_viridis_d(direction = -1) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# vamos a añadir mas datos para entender mejor el grafico
ants_cols <- ants_long %>% 
  mutate(total_abundance = 1) %>% 
  turnover(time.var = "year",
           species.var = "code",
           abundance.var = "total_abundance",
           metric = "appearance") #appereance and isapperance lo tiene en cuenta la 
# funcion, no lo hace 

ants_exts <- ants_long %>% 
  mutate(total_abundance = 1) %>% 
  turnover(time.var = "year",
           species.var = "code",
           abundance.var = "total_abundance",
           metric = "disappearance")


ants_cols %>% 
  left_join(ants_exts) %>% 
  pivot_longer(cols = -year,
               names_to = "metric",
               values_to = "rate") %>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(aes(fill = metric)) +
  geom_point(data = ants_turnover, aes(y = total)) +
  geom_line(data = ants_turnover, aes(y = total)) +
  geom_vline(aes(xintercept = 2005), linetype = "dashed", color = "black") +
  scale_fill_viridis_d(begin = .5) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))





ants_cols %>% 
  left_join(ants_exts) %>% 
  pivot_longer(cols = -year,
               names_to = "metric",
               values_to = "rate") %>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(aes(fill = metric)) +
  geom_point(data = ants_turnover, aes(y = total)) +
  geom_line(data = ants_turnover, aes(y = total)) +
  geom_vline(aes(xintercept = 2005), linetype = "dashed", color = "black") +
  scale_fill_viridis_d(begin = .5) +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) + 
  ylim(0,1) + 
  labs(title = "Community turnover relative to the preceding sample",
       x = "Year",
       y = "Turnover") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))


#euler diagramas
ants_long %>% 
  filter(year %in% c(2004, 2008)) %>% # keep the years of interest
  split(.$year) %>% # create a list with a dataframe per year
  map(select, code) %>% # for each dataframe, select the code variable
  map(unlist) %>% # create a vector from the selected variable
  eulerr::euler() %>% # calculate the parameters for the euler diagram
  plot(quantities = T)  # and plot it


#esto podria ser una opcion, aunque es un poco lioso por los colores
ggplot(ants_long, aes(x= year, fill= total_abundance, color= code))+
  geom_bar()

#The list of taxa that occur consistently through time for a set of samples constitutes the time core taxa. Are the overlapping species identified in the previous exercise time core species of the community?

ants_long %>%  
  group_by(genus, species) %>% 
  summarise(n_year = n_distinct(year)) %>% 
  filter(n_years == nrow(ants_wide))

###ABUNDANCE-BASED ANALYSES
ants_wide %>%  
  na.omit() %>% 
  vegan::vegdist(method = "jaccard", binary = T) %>% # Binary = T: incidence-based calculation
  # we obtain a distance object (i.e. the lower triangle of the distance matrix stored by columns in a vector)
  as.matrix() %>% # from a distance object to a matrix object
  as.data.frame() %>% # from matrix to data.frame
  rownames_to_column(var = "year1") %>% 
  pivot_longer(cols = -year1,
               names_to = "year2",
               values_to = "distance") %>% # we pivot the object for visualisation purposes
  mutate(across(.cols = c(year1, year2), .fns = as.numeric)) %>% 
  ggplot(aes(x = year1, y = year2)) +
  geom_tile(aes(fill = distance), color = "white") +
  geom_text(aes(label = round(distance, 2)), size = 2) + 
  scale_fill_viridis_c(option = "B", limits = c(0, 1)) +
  labs(title = "Jaccard dissimilarity index (incidence-based)",
       subtitle = "i.e. turnover rate") +
  theme(axis.text.x = element_text(angle = 30, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#hen measuring the distance between two points, our geometrically-trained brain tends to calculate it following the Pythagorean theorem (i.e. the Euclidian distance). Use the vegdist function from the vegan package to calculate the Euclidian distance for all sample combinations of the community. What do you think? 
#Do you find this metric appropriate for analysing community dissimilarity? Why?

ants_wide %>%  
  na.omit() %>% 
  vegan::vegdist(method = "euclidean") %>% # Binary = T: incidence-based calculation
  # we obtain a distance object (i.e. the lower triangle of the distance matrix stored by columns in a vector)
  as.matrix() %>% # from a distance object to a matrix object
  as.data.frame() %>% # from matrix to data.frame
  rownames_to_column(var = "year1") %>% 
  pivot_longer(cols = -year1,
               names_to = "year2",
               values_to = "distance") %>% # we pivot the object for visualisation purposes
  mutate(across(.cols = c(year1, year2), .fns = as.numeric)) %>% 
  ggplot(aes(x = year1, y = year2)) +
  geom_tile(aes(fill = distance), color = "white") +
  geom_text(aes(label = round(distance, 2)), size = 2) + 
  scale_fill_viridis_c(option = "B", limits = c(0, 160)) +
  labs(title = "euclidian dissimilarity index (incidence-based)")+
       #subtitle = "i.e. turnover rate") +
  theme(axis.text.x = element_text(angle = 30, hjust = .5, vjust = .5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


hell_vector <- ants_wide %>% 
  na.omit() %>% 
  vegdist(method = "hellinger") %>% 
  as.numeric()

lag_vector <- ants_wide %>% 
  na.omit() %>% 
  rownames() %>% 
  as.numeric() %>% 
  vegdist(method = "euclidean") %>% 
  as.numeric()

lag_df <- data.frame(hell_dist = hell_vector,
                     time_lag = lag_vector)

linear <- lm(hell_dist ~ time_lag, data = lag_df)
summary(linear)

lag_df %>% 
  ggplot(aes(x = time_lag, y = hell_dist)) +
  geom_point(alpha = .5, size = 4) +
  geom_smooth(method = "lm", se = F, size = 1) + # fit a regression line
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  labs(x = "Time lag (years)",
       y = "Hellinger distance")

lag_df %>% 
  ggplot(aes(x = time_lag, y = hell_dist)) +
  geom_point(alpha = .5, size = 4) +
  geom_smooth(method = "gam", se = T, size = 1) + # fit a regression line, different that a linear
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  labs(x = "Time lag (years)",
       y = "Hellinger distance")
