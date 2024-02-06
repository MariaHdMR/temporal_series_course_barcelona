# sesion 2

#leer las librerias
library(tidyverse)
library(broom)
library(patchwork)
library(glmmTMB)
install.packages('TMB', type = 'source')
library(DHARMa)

#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bird_all <- read.csv("birds_all.csv")
# We examine the data.
head(bird_all)


bird_all <- bird_all %>%
  group_by(id) %>%  
  # Keep populations with >4 years worth of data and calculate length of monitoring
  filter(is.finite(Abundance),
         length(unique(Year)) > 4) %>%
  mutate(# Calculate the intrinsic population growth rate
    pop_growth= log(Abundance/lag(Abundance)),
    # Replace the infinite and NULL values (because of the zeros by NA's)
    pop_growth = replace(pop_growth, !is.finite(pop_growth), NA)) %>% 
  # Remove any groupings we have created 
  ungroup() 

# Let's have a look at the distribution of the data

bird_all %>% 
  ggplot(aes(x=pop_growth))+
  geom_histogram()

bird_all %>% 
  summarise(mean=mean(pop_growth, na.rm = T),
            sigma2=var(pop_growth, na.rm = T))

pops_sum <- bird_all %>% 
  group_by(id) %>% 
  summarise(mean=mean(pop_growth, na.rm = T),
            sigma2=var(pop_growth, na.rm = T))

# Explore the first ones

head(pops_sum)


# question
#Why do you think that a population with high variability in its 
#population growth rate could have a higher risk of extinction?

# answer: la que tiene mas variabilidad tiene mas probabilidades de quedar extinta. 


falco_pops <- bird_all %>%
  filter(Species_name=="Falco peregrinus")

#deterministic approach:
# opcion de crear una funcion

# Create a function to project the population

pop_proj<- function(data, tmax){
  n <- tibble(n=0, time=1:tmax)
  n$n[1] <- 100 # aqui pondriamos las abundancias, por ejemplo las abundancias finales de nuestra base de datos. 
  # Calculate mean r
  r <- mean(data$pop_growth, na.rm = T)
  for (t in 2:tmax){	
    # back-transform to get lambda 
    lambda <- exp(r) 
    #project one time step from the current pop size
    n$n[t]=n$n[[(t-1)]]*lambda 
  }
  return(n)
}
  
# Apply the function over the data frame

proj_det <- falco_pops %>%  
  group_by(id) %>% 
  do(pop_proj(., tmax=100)) # la funcion do le dice que aplique esta funcion a todo, y por el punto
# la proyeccio es de 100 años

# We plot it
proj_det %>% 
  ggplot(aes(x=time, y=n, group=id))+
  geom_line(alpha=0.5) +
  ylim(0, 5000) # We limit to make the visualisation easier

# stochastic projection function
pop_proj_stoch<- function(data, tmax){
  n <- tibble(n=0, time=1:tmax)
  n$n[1] <- 100
  for (t in 2:tmax){	
    # Draw r from normal using estimates of mu and sigma2
    r <- rnorm(n = 1, mean = mean(data$pop_growth, na.rm=T), 
               sd = sqrt(var(data$pop_growth, na.rm=T))) # antes era solo la media, ahora le metemos mas variabilidad
    
    # back-transform to get lambda and get pop. size
    lambda <- exp(r) 
    #project one time step from the current pop size
    n$n[t]=n$n[[(t-1)]]*lambda 
  }
  return(n)
}
# Apply the function over the data frame

proj_stoch <- falco_pops %>%  
  group_by(id) %>% 
  do(pop_proj_stoch(., tmax=100))

# We plot it
proj_stoch %>% 
  ggplot(aes(x=time, y=n, group=id))+
  geom_line(alpha=0.5) +
  ylim(0, 5000) # We limit to make the visualisation easier


# ahora lo que vamos a hacer es juntar los dos plots
# Plot deterministic
p1 <- proj_det %>% 
  ggplot(aes(x=time, y=n, group=id))+
  geom_line(alpha=0.5) +
  ylim(0, 5000) # We limit to make the visualisation easier

# Plot deterministic
p2 <- proj_stoch %>% 
  ggplot(aes(x=time, y=n, group=id))+
  geom_line(alpha=0.5) +
  ylim(0, 5000) # We limit to make the visualisation easier

# We join them 

p1+ggtitle("Deterministic")|p2+ggtitle("Stochastic")


#r exinct function

pop_proj_ext<- function(data, threshold, tmax){ #el treshold lo vamos a poner en 20, que son los que sobreviven
  n <- tibble(n=0, time=1:tmax)
  n$n[1] <- 100
  Ne <- n$n[1]*(1-threshold)
  for (t in 2:tmax){	
    # Draw r from normal using estimates of mu and sigma2
    r <- rnorm(n = 1, mean = mean(data$pop_growth, na.rm=T), 
               sd = sqrt(var(data$pop_growth, na.rm=T)))
    # back-transform to get lambda and get population size
    lambda <- exp(r) 
    #project one time step from the current population size
    n$n[t]=n$n[[(t-1)]]*lambda 
    # leave the loop if the population size is <= threshold (when the threshold is crossed)
    ifelse(n$n[t] <= Ne, 
           n$n[t] <- 0, n$n[t] <- n$n[t])
  }
  
  return(n)
}
# Apply the quasi-extinction projection function over the populations

proj_extinct <- falco_pops %>%  
  group_by(id) %>% 
  do(pop_proj_ext(., threshold = 0.8, tmax=100))

#     r estimte the threshold
#Estimate the threshold
Ne <- 100*(1-0.8)

# Which trajectories are above the threshold
prob <- proj_extinct %>% 
  group_by(id, time) %>% 
  mutate(above.qe= n >= Ne,
         alife=cumprod(above.qe)) %>% #cumprod function transforma a 1 y 0
  group_by(time) %>% 
  summarise(prob= 1-(sum(alife, na.rm = T)/length(unique(proj_extinct$id)))) # en 1 menos es la mortalidad, 
# si no es el 1 - es lo que vive

# Plot it
prob %>% 
  ggplot(aes(x=time, y=prob))+
  geom_line()
# si la varianza es muy grande, vamos a tener una probabilidad mas grande para morirnos, por eso al final del tiempo
# la varianza es mayor, por lo que habrá mas variaciones


### TASK: 
# 1. Estimate the quasi-extinction probability at 75 years of the European populations of *Oxyura leucocephala*.
# 2. Make a plot representing which diets are associated with lower population growth rates.

######
#TASK 1: Estimate the quasi-extinction probability at 75 years of the European populations of *Oxyura leucocephala*.

oxyura_pops <- bird_all %>%
  filter(Species_name=="Oxyura leucocephala", Region == "Europe")

proj_extinct_oxy <- oxyura_pops %>%  
  group_by(id) %>% 
  do(pop_proj_ext(., threshold = 0.8, tmax=75))

prob_oxy <- proj_extinct_oxy %>% 
  group_by(id, time) %>% 
  mutate(above.qe= n >= Ne,
         alife=cumprod(above.qe)) %>% #cumprod function transforma a 1 y 0
  group_by(time) %>% 
  summarise(prob= 1-(sum(alife, na.rm = T)/length(unique(proj_extinct$id)))) # esto es para calcular probabilidad muerte

prob_oxy %>% 
  ggplot(aes(x=time, y=prob))+
  geom_line()

prob_oxy %>%  filter(time== 75)

# TASK 2. Make a plot representing which diets are associated with lower population growth rates.
bird_all$pop_growth


ggplot(bird_all, aes(x = pop_growth, y=diet, color= diet))+
  #facet_wrap(~Species_name)+
  geom_boxplot(width= 0.1) #☺para hacerlo debería de haber hecho la media

bird_all %>% # esto no se si funciona bien, comprobar
  mutate(mean.p = mean(pop_growth))

ggplot(bird_all, aes(x = pop_growth, y=diet, color= diet))+
  #facet_wrap(~Species_name)+
  geom_point(width= 0.1)
