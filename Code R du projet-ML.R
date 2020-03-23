library(caret)
library(tidyverse)
library(mice)
library("RColorBrewer")
library(VIM)

library("ggmap")
require(maps)

learn = read.csv("projet-app-13-learn.csv",header = TRUE,encoding = "UTF-8")
test = read.csv("projet-app-13-test.csv",header = TRUE,encoding = "UTF-8")
glimpse(learn)

learn <- learn %>% rename(categorie = catégorie)
test <- test %>% rename(categorie = catégorie)
learn$reg = as.factor(learn$reg)
learn$categorie = as.factor(learn$categorie)
test$reg = as.factor(test$reg)
test$categorie = as.factor(test$categorie)

cities_gps = read.csv("cities-gps.csv",header = TRUE,encoding = "UTF-8")
cities_population = read.csv("cities-population.csv",header = TRUE,encoding = "UTF-8")
dept = read.csv("departments.csv",header = TRUE,encoding = "UTF-8")
regions = read.csv("regions.csv",header = TRUE,encoding = "UTF-8")

learn=merge(x=learn,y=cities_gps,by.x = "insee.code",by.y="id")
learn=merge(x=learn,y=cities_population,by.x = "insee.code",by.y="id")
test=merge(x=test,y=cities_gps,by.x = "insee.code",by.y="id")
test=merge(x=test,y=cities_population,by.x = "insee.code",by.y="id")

colSums(is.na(learn))
colSums(is.na(test))

respattern=md.pattern(learn,rotate.names = TRUE)
matrixplot(learn)

learn2<-select(learn,-c(insee.code,f_name,last.name,commune, department, latitude, longitude,X,Y))
impforest = mice(learn2, method = "rf", m = 30, seed = 600, print=FALSE)
learn_complete=complete(impforest)
learn_complete<-cbind(learn_complete,select(learn,c(insee.code,f_name,last.name,commune, department, latitude, longitude,X,Y)))

test2<-select(test,-c(insee.code,f_name,last.name,commune, department, latitude, longitude,X,Y))
impforest2 = mice(test2, method = "rf", m = 30, seed = 600, print=FALSE)
test_complete=complete(impforest2)
test_complete<-cbind(test_complete,select(test,c(insee.code,f_name,last.name,commune, department, latitude, longitude,X,Y)))

par(mfrow=c(1,2))
densityplot(impforest, main="Forêts aléatoires sur train", layout = c(2, 3))
densityplot(impforest2, main="Forêts aléatoires sur test", layout = c(2, 3))

ggplot(learn_complete, aes(cible))+geom_bar(fill="firebrick")

par(mfrow=c(2,2))
ggplot(learn_complete, aes(cible,age))+geom_boxplot(fill="deepskyblue")
ggplot(learn_complete, aes(cible,population))+geom_boxplot(fill="darkorange")
ggplot(learn_complete, aes(cible,revenue))+geom_boxplot(fill="darkorchid")

ggplot(learn_complete, aes(x = cible, y = age, fill = sex)) + geom_boxplot()+scale_fill_brewer(palette="Set2")
table(learn_complete$sex, learn_complete$cible)

ggplot(learn_complete, aes(reg)) + geom_bar(aes(fill=cible),position = "dodge") + scale_fill_brewer(palette="Set1")
table(learn_complete$reg, learn_complete$cible)

ggplot(learn_complete, aes(categorie))+geom_bar(aes(fill=cible))+scale_fill_brewer(palette="PiYG")
table(learn_complete$categorie, learn_complete$cible)

ggplot(learn_complete, aes(city.type))+geom_bar(aes(fill=cible),position = "dodge") + scale_fill_brewer(palette="Accent")+theme(axis.text.x = element_text(angle = 90))
table(learn_complete$city.type, learn_complete$cible)
ggplot(learn_complete, aes(x = city.type, y = age)) + geom_boxplot(aes(fill=cible))+theme(axis.text.x = element_text(angle = 90))

mapa_mundo <- map_data("world", region = "France")

mapa_mundo %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "grey80",
               color = "white") +
  geom_point(data= learn_complete, 
             aes(x=longitude, y = latitude, color = cible))+
  scale_color_manual(values = c( "darkorange", "purple"), name = " ") + 
  ggtitle( "Carte")


# 2 Modèles prédictives (à suivre)

