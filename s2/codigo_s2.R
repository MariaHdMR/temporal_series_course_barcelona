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



#### dia 7 febrero

falco_pops <- falco_pops %>%
  # Group by id to scale the abundance over each time-series
  group_by(id) %>%
  # Scale the abundance as a proportional change based on the
  # maximum abundance 
  mutate(sabundance = Abundance/(max(Abundance,na.rm = T))) %>%
  # Remove any groupings we have created in the pipe
  ungroup()

# ahora lo que vamos a hcer es hacer un modelo lineal para cada una de las series temporales

falco_models <- falco_pops %>%
  # Nest by the key variables that we want to iterate over note that if we only include e.g. id (the population id), then we only get the id column in the model summary, not e.g. latitude, class...
  nest_by(id, Species_name, Class) %>%
  # Create a linear model for each group
  mutate(mod = list(lm(sabundance ~ Year, data = data))) %>%
  # Extract model coefficients using tidy() and summarise
  summarise(tidy(mod)) %>% 
  # Filter out slopes and remove intercept values
  filter(term == "Year") %>%
  # Get rid of the column term as we don't need it any more
  dplyr::select(-term) %>%
  # Remove of the groupings 
  ungroup()
# el nest by lo que hace es meter una lista en una de las celdas

# Let's have a look at the slopes

ggplot(falco_models, aes(estimate)) +
  geom_histogram()+
  geom_vline(xintercept = 0,linetype="dashed" ) +
  labs(y="Abundance", x="Slope")

# Let's check the mean value of the slope

(mean.slope <- falco_models %>% 
    summarise(mean=mean(estimate)))

#ahora lo que teneos es el plot con cada uno de los puntos reales y las lineas son lo que sale con el modelo
ggplot(falco_pops, aes(x=Year, y=sabundance, group=id))+
  geom_point(aes(fill=id), shape=21)+#shape 21 is filled with colour and black around it, that is why we need to specify the fill=id
  geom_smooth(aes(colour=id), method="lm", se=F) +#we set colour=id here because we want separate coloured lines. We also set se=F to remove the confiedence intervals (this is only for visualisation purposes).
  theme(legend.position = "none")


#models
m <- glmmTMB(Abundance ~ Year+ (1|id), falco_pops, family=poisson())

# Check the model outputs

summary(m)

# Check the residuals with DHARMa

plot(simulateResiduals(fittedModel = m, plot = F))


# Check the autocorrelation of the residuals

acf(resid(m))

# para que no tenga correlacion espacial lo que se haría seria tener las lineas entre la franja azul. Lo mejor
# seria tener casi todas las lienas entre la franja azul. 


# Let's compare the slope with the previous value from the linear models

ggplot(falco_models, aes(estimate)) +
  geom_histogram()+
  geom_vline(xintercept = 0,linetype="dashed" ) +
  geom_vline(xintercept = mean(coefficients(m)$cond$id$Year),
             linetype="dashed", colour="red" ) +
  geom_label(aes(x = mean(coefficients(m)$cond$id$Year),
                 y = 4,label="Mixed model"), nudge_x = -0.01, 
             linetype="dashed", colour="red" ) +
  labs(y="Abundance", x="Slope")


# Model with temporal autocorrelation

m2 <- update(m, ~ Year+ (1|id) + ar1(as.factor(Year)|id))

# Check the model outputs

summary(m2)

#Check the residuals with DHARMa

plot(simulateResiduals(fittedModel = m2, plot = F))

# Check the autocorrelation of the residuals

acf(resid(m2)) # siempre la primera linea se saldrá, ya que es cero

# Let's compare the model fit 

AIC(m2, m)

# We compare again the slope with the previous value from the linear models

ggplot(falco_models, aes(estimate)) +
  geom_histogram()+
  geom_vline(xintercept = 0,linetype="dashed" ) +
  geom_vline(xintercept = mean(coefficients(m)$cond$id$Year),
             linetype="dashed", colour="red" ) +
  geom_label(aes(x = mean(coefficients(m)$cond$id$Year),
                 y = 4,label="Mm without \n autocorrelation"), nudge_x = -0.01, 
             linetype="dashed", colour="red" ) +
  geom_vline(xintercept = mean(coefficients(m2)$cond$id$Year),
             linetype="dashed", colour="blue" ) +
  geom_label(aes(x = mean(coefficients(m2)$cond$id$Year),
                 y = 2,label="Mm with \n autocorrelation"), 
             nudge_x = -0.01, 
             linetype="dashed", colour="blue" ) +
  labs(y="Abundance", x="Slope")
