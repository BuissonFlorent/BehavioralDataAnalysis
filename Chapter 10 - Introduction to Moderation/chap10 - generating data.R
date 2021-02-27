##### Setup #####
library(Rlab)
library(tidyverse)
library(rstudioapi)
### Setting the working directory to the parent folder of this script (Rstudio only)
sourceDir <- rstudioapi::getActiveDocumentContext()$path %>% str_extract("^.+/")
setwd(sourceDir)

set.seed(1234)
options(scipen=10)

### Example with C-Mart data 

# Store-level data
nb_stores <- 50
dat_stores <- tibble(
  id = 1:nb_stores,
  play_area = sample(c(0,0,0,1),nb_stores, replace = TRUE),
  prop_children = rbeta(nb_stores,40,60-play_area),
  avg_visitors = 1000 * rbeta(nb_stores,15+5*play_area,10)
)
#with(dat_stores,boxplot(avg_visitors~play_area))

#day-level data on individual visits
nb_days <- 20


visits_list <- lst()
k <- 1
for(j in 1:nb_days){
  for(i in 1:nb_stores){
    nb_visits <- round(rnorm(1,dat_stores$avg_visitors[i],10),0)
    children <- as.integer((runif(nb_visits,0,1) <= dat_stores$prop_children[i]))
    age <- round(runif(nb_visits,20,80))
    duration <- pmax(3, rnorm(nb_visits, -21, 3) + age * rnorm(nb_visits,0.8,0.2) * (1 + children * rnorm(nb_visits, 0.2, 0.03)) +
                     children * (rnorm(nb_visits,30,5) + 
                                      rnorm(nb_visits,8,4) * dat_stores$play_area[i] + 
                                   rnorm(nb_visits,10,5) * dat_stores$play_area[i] * (60+80-age)/60) +
      (1-children)*(rnorm(nb_visits,20,5) + rnorm(nb_visits,10,2) * dat_stores$prop_children[i] * dat_stores$play_area[i])
      )
    groceries_purchases <- pmax(0,
                            duration * rnorm(nb_visits, 2, 0.5) +
                              duration^2 * rnorm(nb_visits, 0.06^2, 0.01^2))
    
    
    visits_list[[k]] <- tibble(
      day = j,
      store_id = i,
      children = children,
      age = age,
      duration = duration,
      play_area = dat_stores$play_area[i],
      prop_children = dat_stores$prop_children[i],
      groceries_purchases = groceries_purchases
    )
    k <- k + 1
  }
}
visits = bind_rows(visits_list)
summary(visits)

# ggplot(visits, aes(x=age, y=duration)) + geom_point(alpha = 0.5) + 
#   geom_smooth(method = lm, fullrange = TRUE) + xlim(c(0,85))

summary(lm(duration~play_area, data=visits))

summary(lm(groceries_purchases~duration+I(duration^2), data=visits))
ggplot(visits %>% filter(day==2), aes(x=duration, y=groceries_purchases)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method=lm, formula='y~poly(x,2)') + 
  geom_smooth(method=lm, aes(col='red'))




write_csv(visits, "chap12-data.csv")



