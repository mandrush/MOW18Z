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
library(ggplot2)
library(plyr)
library(gridExtra)
library(alluvial)
library(extrafont)

#select(params,school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3)

paramsMat <- read.csv("./resources/student-mat.csv",sep=",",header=TRUE)

paramsPor <- read.csv("./resources/student-por.csv",sep=",",header=TRUE)
#clean NULL fields just in case
paramsMat <- paramsMat[complete.cases(paramsMat), ]
paramsPor <- paramsPor[complete.cases(paramsPor), ]

#joined data
data <- rbind(paramsMat, paramsPor)
duplicates <- merge(paramsMat,paramsPor,by=c("school","sex","age","address","famsize","Pstatus",
                             "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                             "guardian","guardian","traveltime","studytime","failures",
                             "schoolsup","famsup","activities","higher","romantic",
                             "famrel","freetime","goout","Dalc","Walc","health","absences"))


# data$ocenymat = rowMeans(cbind(data$G1.x,data$G2.x,data$G3.x))
# data$ocenypor = rowMeans(cbind(data$G1.y,data$G2.y,data$G3.y))

data$Dalc <- as.factor(data$Dalc)      
data$Dalc <- mapvalues(data$Dalc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))

data$Walc <- as.factor(data$Walc)      
data$Walc <- mapvalues(data$Walc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))

# and eliminate the repeats:
data <- data %>% distinct(school,sex,age,address,famsize,Pstatus,
                             Medu,Fedu,Mjob,Fjob,reason,
                             guardian,traveltime,studytime,failures,
                             schoolsup, famsup,activities,nursery,higher,internet,
                             romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
#add a column with average grades (math or Portuguese, whichever is available)
data$avggrades=rowMeans(cbind(data$G1,data$G2,data$G3))
# and drop grades in 3 marking periods.
data<-data[,-(31:33)]



# 1) multiple regression 
lm2<-lm(Walc~., data=data)
summary(lm2)


# 
# #correlation diagram
# #corrgram(data, lower.panel=panel.shade, upper.panel=panel.ellipse)
# corMatrix <- cor(data)
# corrplot(corMatrix, method='circle')
# 
# #partition the data (p = ratio between training and the testing data)
# inTrain <- createDataPartition(y = data$Dalc,
#                                p = 0.3,
#                                list = FALSE)
# training <- data[ inTrain,]
# testing <- data[-inTrain,]
# 
# #Walc model; depending on every possible attribute except for Dalc
# #weekModel <- lm(factor(Walc) ~ . -Dalc , data=training)
# 
# #summary(weekModel)
# #plot(weekModel)
# 
# # the overall quality of the model can be assessed by examining the R-squared (R2) and Residual Standard Error (RSE)
# #RSE: a measure of error of prediction. The lower the RSE, the more accurate the model (on the data in hand).
# #sigma(weekModel)/mean(data$Walc) #approx. 0.48, kind of bad
# 
# #weekNoG1 <- lm(Walc ~ . -Dalc -G1 , data=training)
# #summary(weekNoG1)
# #sigma(weekNoG1)/mean(data$Walc) #approx. 0.48, G1 doesn't havey any significance?
# #training <- subset(training, select = -c(school))
# 
# tree <- lm(Walc ~ . ,data = training, method = "class")
# predicted <- predict(tree, newdata = testing, type = "class")
# 
# table(predicted, testing$Walc)
