# Librairies
library(caret)
library(tidyverse)
library(mice)
library("RColorBrewer")
library(VIM)
library("GGally")
library("ggmap")
require(maps)
library(MASS)
library(ROCR)
library(rpart)
library(rpart.plot)
library(InformationValue)



learn = read.csv("projet-app-13-learn.csv",header = TRUE,encoding = "UTF-8")
test = read.csv("projet-app-13-test.csv",header = TRUE,encoding = "UTF-8")
summary(learn)
summary(test)
View(learn)
View(test)

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
learn$cible <- as.logical(learn$cible)


learn=merge(x=learn,y=cities_gps,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=cities_population,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=regions,by.x = "reg",by.y="id")

test=merge(x=test,y=cities_gps,by.x = "insee.code",by.y="id")
test=merge(x=test,y=cities_population,by.x = "insee.code",by.y="id")
test=merge(x=test,y=regions,by.x = "reg",by.y="id")

learn=learn %>% rename(REGION = name)
test=test %>% rename(REGION = name)

View(learn)
View(test)
