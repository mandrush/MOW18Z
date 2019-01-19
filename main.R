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
library(randomForest)

#select(params,school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,nursery,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,absences,G1,G2,G3)

set.seed(20)

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

data$Dalc <- as.numeric(data$Dalc)

data$Walc <- as.numeric(data$Walc)

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

# partition the data (p = ratio between training and the testing data)
inTrain <- createDataPartition(y = data$Dalc,
                               p = 0.1,
                               list = FALSE)
training <- data[ inTrain,]
testing <- data[-inTrain,]



### WALC ####


# 1) multiple regression - all attributes
lm<-glm(Walc~., data=training)
summary(lm)

predicted <- predict(lm, newdata = testing)

predictedNormalized<-cut(predicted, c(0,1,2,3,4,5))

table(predictedNormalized, testing$Walc)

nmse<-mean((predicted-training[,"Walc"])^2)/mean((mean(training$Walc)-data[,"Walc"])^2)
print(nmse) #0.2

# 2) multiple regression - without lowest t values
lm2<-glm(Walc ~  Medu + Fedu + Mjob + goout + freetime + famrel , data=training)
summary(lm2)

predicted2 <- predict(lm2, newdata = testing)

predicted2<-cut(predicted2, c(0,1,2,3,4,5))

table(predicted2, testing$Walc)

# 1) zgadywany multiple regression
lm3<-glm(Walc~ Pstatus + age + freetime
         + goout + absences + studytime 
         + activities + famrel + romantic , data=training)
summary(lm3)

predicted3 <- predict(lm3, newdata = testing)

predicted3<-cut(predicted3, c(0,1,2,3,4,5))

table(predicted3, testing$Walc)


### DALC ####

lm4<-glm(Dalc ~  Medu + Fedu + Mjob + goout + freetime + famrel , data=training)
summary(lm4)

predicted4 <- predict(lm4, newdata = testing)

predicted4<-cut(predicted4, c(0,1,2,3,4,5))

table(predicted4, testing$Dalc)



### CART TREE ####

trainingTree <- subset(training, select = -c(school, Mjob, Fjob, address, famsize, Pstatus, reason, guardian ))

tree <- rpart(Walc ~ . ,data = trainingTree, method = "class")
predictedTree <- predict(tree, newdata = testing, type = "class")
table(predictedTree, testing$Walc)

nmse<-mean((as.numeric(predictedTree)-training[,"Walc"])^2)/mean((mean(training$Walc)-data[,"Walc"])^2)
print(nmse) #0.2

varImpPlot(tree,type=1)


### polynomial multiple ####
polynom <- lm(Walc ~ polym(Dalc, goout, freetime, degree=3, raw=TRUE), data = training)
predictedPolynom <- predict(polynom, newdata = testing)
predictedPolynomNormalized<-cut(predictedPolynom, c(0,1,2,3,4,100))
table(predictedPolynomNormalized, testing$Walc)

nmse<-mean((predictedPolynom-training[,"Walc"])^2)/mean((mean(training$Walc)-data[,"Walc"])^2)
print(nmse) #0.2

### random forest ####

rf<-randomForest(Walc~., data=training, ntree=500, importance=T)
predictedRandomForest<-predict(rf,testing)
predictedRandomForestNormalized<-cut(predictedRandomForest, c(0,1,2,3,4,5))
table(predictedRandomForestNormalized, testing$Walc)

nmse<-mean((predictedRandomForest-training[,"Walc"])^2)/mean((mean(training$Walc)-data[,"Walc"])^2)
print(nmse) #0.2

varImpPlot(rf,type=1)

#1. przygotowanie danych: usuniecie duplikatow, opis machlojek i tego szwindla z usrednieniem G123 dla mat i por i tu wykresy dla ocen, zeby pokazac powiazanie G123
#1. cont. podzial danych na test i training, zmienilismy z 70 na 30% bo overfitting byl 
#2. modele regresji liniowe lm czyli pierwsze 3 i jako pierwsze bedzie corrplot czy tam diagram
#2. cont. pozniej analiza t-value, tabelki z efektami przewidywan
#3. polynomial regression wraz z wynikami (tak samo jak w 2 analiza), wykres rzeczywistych od predicted
#4. tree, narysowac drzewo, tabelka
#5. wnioski xD



