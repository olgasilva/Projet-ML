


learn = read.csv("projet-app-13-learn.csv",header = TRUE,encoding = "UTF-8")
test = read.csv("projet-app-13-test.csv",header = TRUE,encoding = "UTF-8")
View(learn)
View(test)
fic.cities = read.csv("cities-gps.csv",header = TRUE,encoding = "UTF-8")
fic.citiespop = read.csv("cities-population.csv",header = TRUE,encoding = "UTF-8")
fic.dep = read.csv("departments.csv",header = TRUE,encoding = "UTF-8")
fic.regions = read.csv("regions.csv",header = TRUE,encoding = "UTF-8")
View(fic.cities)
View(fic.dep)
View(fic.citiespop)
View(fic.regions)


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
