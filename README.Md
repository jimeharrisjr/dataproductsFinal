---
title: "Crime Analysis Project"
author: "James Harris"
date: "2/17/2020"
---


## Crime Data

This data was taken from Kaggle. The data is from the Marshall Project, and can be downloaded here: https://www.kaggle.com/marshallproject/crime-rates

From the website:
`The crime data was acquired from the FBI Uniform Crime Reporting program's "Offenses Known and Clearances by Arrest" database for the year in question, held at the National Archives of Criminal Justice Data. The data was compiled and analyzed by Gabriel Dance, Tom Meagher, and Emily Hopkins of The Marshall Project; the analysis was published as Crime in Context on 18 August 2016.`

The data were processed as seen below:

```{r, eval=FALSE}
library(data.table)
library(maps)
library(ggmap)
dt<-fread('/Users/jamesharris/Documents/R/CrimeAnalysis/data/crimereport.csv')
locations<-dt[,.(agency_jurisdiction)]
for (i in  1:nrow(dt)){
  result <- suppressMessages(geocode(dt$agency_jurisdiction[i], output = "latlona", source = "google"))
  dt$lng[i] <- as.numeric(result[1])
  dt$lat[i] <- as.numeric(result[2])
  dt$geoAddress[i] <- as.character(result[3])
}
dt<-na.omit(dt)
fwrite(dt,'/Users/jamesharris/Documents/R/CrimeAnalysis/data/crimereport_processed.csv')
```

## Training the Model

The model used in the interactive graph was trained as follows:

```{r pressure, eval=FALSE}
library(caret)
inTrain<-createDataPartition(dt$report_year, p=.7, list=FALSE)
training<-dt[inTrain]
testing<-dt[-inTrain]
preproc<-preProcess(training[,.(population,lat,lng)], method = c('center'))
trainPC<-predict(preproc, training)
testPC<-predict(preproc, testing)
library(doParallel)
cl<-makePSOCKcluster(14)
registerDoParallel(cl)
# Fit a Random Forest model
modelFit<-train(crimes_percapita ~ report_year+as.factor(agency_code)+ population+ lng + lat, data=trainPC, method='rf')
stopCluster(cl)

```

The preprocessing object and model were saved into the data directory.
Note: the predicted values in the future projection graph also require a model to predict population increase over time. The testing data is prepared as follows:

```{r, eval=FALSE}
#GLM model of the city's population formed,
popmod<-glm(population ~ report_year, data=datacit)
#get the last 10 years, copy the fields, and then overwrite the population and year
l<-datacit[report_year>2004]
l$report_year<-2015:2024
l$agency_code<-as.factor(l$agency_code)
l$population<-as.numeric(predict(popmod,l))

```


