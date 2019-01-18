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
inTrain <- createDataPartition(y = data$school,
                               p = 0.3,
                               list = FALSE)
training <- data[ inTrain,]
testing <- data[-inTrain,]

###### PCA #######

data <- sapply( data, as.numeric )

trainingPCA <- data[ inTrain,]
testingPCA <- data[-inTrain,]

#training <- sapply( training, as.numeric )
#testing <- sapply( testing, as.numeric )

trainingPCA <- subset(trainingPCA, select = -c(Walc))
testingPCA <- subset(testingPCA, select = -c(Walc))

#PCA IMPLEMENTATION
# a) prcomp method
prin_comp <- prcomp(trainingPCA, scale. = T)

# b) compute standard deviation of each principal component
std_dev <- prin_comp$sdev

# c) compute variance
pr_var <- std_dev^2

# d) proportion of variance explained
prop_varex <- pr_var/sum(pr_var)


#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#WE ARE INTERESTED ONLY IN PCAs which have impact of 90% overall  1st step
PCA_impact = 0.7
i=1;
while (i < length(prop_varex)) {
  if(cumsum(prop_varex)[i] >= PCA_impact) break;
  i = i+1;
}
which_component = i;


##### END OF PCA ######


### model ###

#PRINCIPAL COMPONENTS + FRAUDFLAG COLUMN FOR LEARNING ALGORITHM
training <- data.frame(Walc = training$Walc, prin_comp$x)

#WE ARE INTERESTED ONLY IN PCAs which have impact of 90% overall   2nd step
training <- training[,1:(which_component+1)]

#DECISION TREE
tree <- rpart(Walc ~ . , data = training, method = "class")

#TRANSFORM TEST DATA INTO PCA
testingPCA <- predict(prin_comp, newdata = testingPCA)
testingPCA <- as.data.frame(testingPCA)

#WE ARE INTERESTED ONLY IN PCAs which have impact of 90% overall
testingPCA <- testingPCA[,1:(which_component)]

#make prediction on test data
predicted <- predict(tree, testingPCA, type = "class")

table(predicted, testing$Walc)








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

#training <- subset(training, select = -c(school, Mjob, Fjob, address, famsize, Pstatus, reason, guardian ))

#tree <- rpart(Walc ~ . ,data = training, method = "class")
#predicted <- predict(tree, newdata = testing, type = "class")


