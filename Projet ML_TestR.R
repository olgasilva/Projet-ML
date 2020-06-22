# Librairies
library(caret)
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)
library(mice)
library("RColorBrewer")
library(VIM)
library("GGally")
library("ggmap")
library(maps)
library(tmap)
library(mapdata)
library(mapproj)
library(sf)
library(raster)
library(spData)
library(MASS)
library(ROCR)
library(rpart)
library(rpart.plot)
library(InformationValue)
library(randomForest)

learn = read.csv("projet-app-13-learn.csv",header = TRUE,encoding = "UTF-8")
test = read.csv("projet-app-13-test.csv",header = TRUE,encoding = "UTF-8")
#summary(learn)
#summary(test)
View(learn)
#View(test)

cities_gps = read.csv("cities-gps.csv",header = TRUE,encoding = "UTF-8")
cities_population = read.csv("cities-population.csv",header = TRUE,encoding = "UTF-8")
dept = read.csv("departments.csv",header = TRUE,encoding = "UTF-8")
regions = read.csv("regions.csv",header = TRUE,encoding = "UTF-8")
View(cities_gps)
View(cities_population)
View(dept)
View(regions)

# renommage de variable
learn=learn %>% rename(categorie = catégorie)
test=test %>% rename(categorie = catégorie)

#conversion variables en factor
learn$reg = as.factor(learn$reg)
learn$categorie = as.factor(learn$categorie)

test$reg = as.factor(test$reg)
test$categorie = as.factor(test$categorie)

# retraitement variable cible
learn$cible= as.character(learn$cible)
learn$cible[learn$cible == "failure"]=FALSE
learn$cible[learn$cible == "success"]=TRUE
learn$cible=as.logical(learn$cible)
learn$ciblenum[learn$cible == "FALSE"]=-1
learn$ciblenum[learn$cible == "TRUE"]=1
learn$ciblenum=as.numeric(learn$ciblenum)

learn=merge(x=learn,y=cities_gps,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=cities_population,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=regions,by.x = "reg",by.y="id")


department_ciblenum <- learn  %>% group_by(department) %>%
  summarize(ciblenum=mean(ciblenum))%>% inner_join(dept,by=c("department"="id"))

cartefrance =as_tibble(map_data("france"))
cartefrance <- cartefrance %>% mutate(region=str_to_upper(region))
broken_names <- list("VAL-DOISE"="VAL-D'OISE", "COTES-DARMOR"="COTES-D'ARMOR", "COTE-DOR"="COTE-D'OR",
                     "CORSE DU SUD"="CORSE-DU-SUD")
for(bn in names(broken_names)) {
  cartefrance <- cartefrance %>% mutate(region=ifelse(region==bn,broken_names[[bn]],region))
}


department_ciblenum$REGION=as.character(department_ciblenum$name)

cartefrance=cartefrance %>% left_join(department_ciblenum,by=c("region"="REGION"))

france_cible_ggplot= ggplot(cartefrance,aes(x=long,y=lat)) +
  geom_polygon(aes(group=group,fill=ciblenum),color=grey(0.75)) +
  scale_fill_continuous(type="viridis") +
  coord_map() +
  ggtitle("titre")
france_cible_ggplot




learn=merge(x=learn,y=cities_gps,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=cities_population,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=regions,by.x = "reg",by.y="id")

#test=merge(x=test,y=cities_gps,by.x = "insee.code",by.y="id")
#test=merge(x=test,y=cities_population,by.x = "insee.code",by.y="id")
#test=merge(x=test,y=regions,by.x = "reg",by.y="id")

learn=learn %>% rename(REGION = name)
test=test %>% rename(REGION = name)

#View(learn)
#View(test)

#####################################

cartefrance =as_tibble(map_data("france"))
cartefrance <- cartefrance %>% mutate(region=str_to_upper(region))
broken_names <- list("VAL-DOISE"="VAL-D'OISE", "COTES-DARMOR"="COTES-D'ARMOR", "COTE-DOR"="COTE-D'OR",
                     "CORSE DU SUD"="CORSE-DU-SUD")
for(bn in names(broken_names)) {
  cartefrance <- cartefrance %>% mutate(region=ifelse(region==bn,broken_names[[bn]],region))
}
#ggplot(cartefrance,aes(x=long,y=lat)) + geom_polygon(aes(group=group),fill=NA,color=grey(0.5)) +coord_quickmap()
learn$REGION=as.character(learn$REGION)

#department_population <- cities %>% inner_join(cities_population,by="id") %>% group_by(department) %>%
# summarize(population=sum(population)) %>% inner_join(departments,by=c("department"="id"))#%>% select(-region)
#View(department_population)
department_cible= learn  %>% group_by(REGION) %>%
  summarize(ciblenum=sum(ciblenum))
cartefrance=cartefrance %>% left_join(department_cible,by=c("region"="REGION"))

france_cible_ggplot= ggplot(cartefrance,aes(x=long,y=lat)) +
  geom_polygon(aes(group=group,fill=ciblenum),color=grey(0.75)) +
  scale_fill_continuous(type="viridis") +
  #scale_fill_continuous(type="viridis",trans="log10",limits=c(-1,1)) +
  coord_map() +
  ggtitle("titre")
france_cible_ggplot








######################
france <- as_tibble(map_data("france"))
View(france)
cities <- read_csv("cities.csv")
cities_population <- read_csv("cities-population.csv")
departments <- read_csv("departments.csv")
regions <- read_csv("regions.csv")

department_population <- cities %>% inner_join(cities_population,by="id") %>% group_by(department) %>%
  summarize(population=sum(population)) %>% inner_join(departments,by=c("department"="id"))#%>% select(-region)
View(department_population)
france <- france %>% mutate(region=str_to_upper(region))
broken_names <- list("VAL-DOISE"="VAL-D'OISE", "COTES-DARMOR"="COTES-D'ARMOR", "COTE-DOR"="COTE-D'OR",
                     "CORSE DU SUD"="CORSE-DU-SUD")
for(bn in names(broken_names)) {
  france <- france %>% mutate(region=ifelse(region==bn,broken_names[[bn]],region))
}

france <- france %>% left_join(department_population,by=c("region"="name"))
View(france)

france_pop_ggplot <- ggplot(france,aes(x=long,y=lat)) + geom_polygon(aes(group=group,fill=population,color=population),color=grey(0.75)) +
  scale_fill_continuous(type="viridis",trans="log10") +
  coord_map() + ggtitle("French population per department")
france_pop_ggplot

#####################################

cartefrance =as_tibble(map_data("france"))
cartefrance <- cartefrance %>% mutate(region=str_to_upper(region))
broken_names <- list("VAL-DOISE"="VAL-D'OISE", "COTES-DARMOR"="COTES-D'ARMOR", "COTE-DOR"="COTE-D'OR",
                     "CORSE DU SUD"="CORSE-DU-SUD")
for(bn in names(broken_names)) {
  cartefrance <- cartefrance %>% mutate(region=ifelse(region==bn,broken_names[[bn]],region))
}
#ggplot(cartefrance,aes(x=long,y=lat)) + geom_polygon(aes(group=group),fill=NA,color=grey(0.5)) +coord_quickmap()
learn$REGION=as.character(learn$REGION)

#department_population <- cities %>% inner_join(cities_population,by="id") %>% group_by(department) %>%
 # summarize(population=sum(population)) %>% inner_join(departments,by=c("department"="id"))#%>% select(-region)
#View(department_population)
department_cible= learn  %>% group_by(REGION) %>%
  summarize(ciblenum=sum(ciblenum))
cartefrance=cartefrance %>% left_join(department_cible,by=c("region"="REGION"))

france_cible_ggplot= ggplot(cartefrance,aes(x=long,y=lat)) +
  geom_polygon(aes(group=group,fill=ciblenum),color=grey(0.75)) +
  scale_fill_continuous(type="viridis") +
  #scale_fill_continuous(type="viridis",trans="log10",limits=c(-1,1)) +
  coord_map() +
  ggtitle("titre")
france_cible_ggplot
