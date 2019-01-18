library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(readr)
library(rattle)
library(cluster)
library(fpc)
library(dplyr)
library(cor)
library(corrgram)
library(corrplot)

#select(params,school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3)

paramsMat <- read.csv("./resources/student-mat.csv", stringsAsFactors = T)

paramsPor <- read.csv("./resources/student-por.csv", stringsAsFactors = T)
#clean NULL fields just in case
paramsMat <- paramsMat[complete.cases(paramsMat), ]
paramsPor <- paramsPor[complete.cases(paramsPor), ]

#joined data
data <- rbind(paramsMat, paramsPor)
duplicated(data)
#correlation diagram
#corrgram(data, lower.panel=panel.shade, upper.panel=panel.ellipse)
corMatrix <- cor(data)
corrplot(corMatrix, method='circle')

#partition the data (p = ratio between training and the testing data)
inTrain <- createDataPartition(y = data$Dalc,
                               p = 0.3,
                               list = FALSE)
training <- data[ inTrain,]
testing <- data[-inTrain,]

#Walc model; depending on every possible attribute except for Dalc
#weekModel <- lm(factor(Walc) ~ . -Dalc , data=training)

#summary(weekModel)
#plot(weekModel)

# the overall quality of the model can be assessed by examining the R-squared (R2) and Residual Standard Error (RSE)
#RSE: a measure of error of prediction. The lower the RSE, the more accurate the model (on the data in hand).
#sigma(weekModel)/mean(data$Walc) #approx. 0.48, kind of bad

#weekNoG1 <- lm(Walc ~ . -Dalc -G1 , data=training)
#summary(weekNoG1)
#sigma(weekNoG1)/mean(data$Walc) #approx. 0.48, G1 doesn't havey any significance?
#training <- subset(training, select = -c(school))

tree <- rpart(Walc ~ . ,data = training, method = "class")
predicted <- predict(tree, newdata = testing, type = "class")

table(predicted, testing$Walc)
