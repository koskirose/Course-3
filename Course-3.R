


install.packages("manip")

#Import the data

#Survey contains all the completed data
survey <- read.csv("CompleteResponses.csv")
#Incomplete contains the data that had the model purchased incomplete
incomplete <-read.csv("SurveyIncomplete.csv")

#####
#List the attributes of data
attributes(survey)
#7 columns: names are salary, age, elevl, car, zipcode, credit, brand

#Prints the min, max, mean, median, and quartiles of each attribute
summary(survey)

#Display the structure of the data
str(survey)

#Names your attributes within the data
names(survey)

#Change categorical data to factors
survey$elevel <-as.factor(survey$elevel)
incomplete$elevel <-as.factor(incomplete$elevel)
is.factor(survey$elevel)
survey$car <-as.factor(survey$car)
incomplete$car <-as.factor(incomplete$car)
is.factor(survey$car)
survey$brand <-as.factor(survey$brand)
incomplete$brand <-as.factor(incomplete$brand)
is.factor(survey$brand)
survey$zipcode <-as.factor(survey$zipcode)
incomplete$zipcode <- as.factor(incomplete$zipcode)

summary(incomplete)

#checked the levels to see if they made sense and are in the expected range
levels(survey$elevel)
levels(incomplete$elevel)
levels(survey$car)
levels(incomplete$car)
levels(survey$brand)
levels(incomplete$brand)
levels(survey$zipcode)

#Checked data for missing values by column
which(is.na(survey$salary))
which(is.na(survey$age))
which(is.na(survey$elevel))
which(is.na(survey$car))
which(is.na(survey$zipcode))
which(is.na(survey$credit))
which(is.na(survey$brand))

#Checked data for missing values by dataframe
is.na(survey)
apply(is.na(survey),2,which)
#####

#Looked at how the data is distributed. Education, Car model, zipcode are are very even.  
#Slightly more buy brand 1
par(mfrow=c(2,2))
plot(survey$elevel, main="Education Level")
plot(survey$car, main="Main Car Driven")
plot(survey$zipcode, main='Zip Code')
plot(survey$brand, main="Brand Preferred")
ggplot(data=survey, aes(x=brand)) + geom_bar()


#Value counts of each category

table(survey$elevel)
#0    1    2    3    4 
#2052 1948 1983 1947 1968


table(survey$car)
#1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#492 509 488 479 505 477 495 511 487 500 473 498 473 494 542 470 508 524 489 484

table(survey$zipcode)
#0    1    2    3    4    5    6    7    8 
#1085 1053 1112 1080 1087 1108 1155 1083 1135

table(survey$brand)
#0    1 
#3744 6154 

#Boxplots to look for outliers
par(mfrow=c(1,2))
hist(survey$salary)
boxplot(survey$salary, main='salary', sub=paste("outlier", boxplot.stats(survey$salary)$out))
par(mfrow=c(1,2))
hist(survey$age)
boxplot(survey$age, main='age', sub=paste("outlier", boxplot.stats(survey$age)$out))
par(mfrow=c(1,2))
hist(survey$credit)
boxplot(survey$credit, main='credit', sub=paste("outlier", boxplot.stats(survey$credit)$out))

#Density plots to see how data is spread
par(mfrow=c(1,3))
plot(density(survey$salary), main="density plot salary", ylab="frequency", sub=paste("skewness",round(e1071::skewness(survey$salary),2)))
plot(density(survey$age), main="density plot age", ylab="frequency", sub=paste("skewness",round(e1071::skewness(survey$age),2)))
plot(density(survey$credit), main="density plot credit", ylab="frequency", sub=paste("skewness",round(e1071::skewness(survey$credit),2)))

#####

#load library and set seed
library(caret)
set.seed(123)

#define a 75%/25% train/test split of the data set
inTraining <- createDataPartition(survey$brand, p=.75, list=FALSE)
training <- survey[inTraining,]
testing <- survey[-inTraining,]
head(testing)

#10 fold cross validation
fitControl <-trainControl(method = "repeatedcv", number = 10, repeats = 1)

#####
#Random forest with tuneLength=1  Accuracy is 92.066 Mtry=11  
system.time(
  rfFit1 <-train(brand~., data=training, method = 'rf', trControl=fitControl, tuneLength=1)
)
rfFit1
print(rfFit1)

#Feature importance-car and zipcode do not matter 
importance1 <-varImp(rfFit1, scale=FALSE)
print(importance1)
plot(importance1)

brand.predict<-predict(rfFit1, testing)


postResample(brand.predict, testing$brand) # 0.925

C1<-confusionMatrix(brand.predict, testing$brand)
C1

final <- data.frame(Actual_Brand=testing$brand, Predicted_Brand=brand.predict)
final


#####

#Random forest with tuneLength=2  Accuracy =91.51 mtry=34
rfFit2 <- train(brand~., data=training, method = 'rf', trControl=fitControl, tuneLength=2)
rfFit2

importance2 <-varImp(rfFit2, scale=FALSE)
print(importance2)
plot(importance2)


brand.predict2<-predict(rfFit2, testing)
postResample(brand.predict2, testing$brand)
C2<-confusionMatrix(brand.predict2, testing$brand)
C2
#####
#Random forest with TuneGrid =1,2,3  Accuracy = 92.21464 with mtry=18
rfGrid <- expand.grid(mtry=c(1,2,3))

rfitm1 <- train(brand~., data=training, method='rf', trControl=fitControl, tuneGrid=rfGrid)
rfitm1
brand.predict3<-predict(rfitm1, testing)
postResample(brand.predict3, testing$brand) #72.63
C3 <-confusionMatrix(brand.predict3, testing$brand)
C3

#92.28  Best One
rfFit3 <- train(brand~., data=training, method = 'rf', trControl=fitControl)
rfFit3
brand.predict4<-predict(rfFit3, testing)
postResample(brand.predict4, testing$brand)
C4 <-confusionMatrix(brand.predict4, testing$brand)
C4
#####
#Feature selection  Removed car and zipcode
survey2 <-within(survey, rm("car", "zipcode"))
head(survey2)


#######Use data after feature selection. Got rid of car and zipcode
#
inTraining2 <-createDataPartition(survey2$brand, p=.75, list=FALSE)
training2 <- survey2[inTraining2,]
testing2 <- survey2[-inTraining2,]
fitControl <-trainControl(method = "repeatedcv", number = 10, repeats = 1)


#Accuracy is 96.1 mtry=2
system.time(
  rfFit1.1 <-train(brand~., data=training2, method = 'rf', trControl=fitControl, tuneLength=1)
)
rfFit1.1
brand.predict5<-predict(rfFit1.1, testing)
postResample(brand.predict5, testing$brand)
C5 <-confusionMatrix(brand.predict5, testing$brand)
C5

#Accuracy is 92.13359 with mtry=3
system.time(
  rfFit1.12 <-train(brand~., data=training2, method = 'rf',trControl=fitControl, tuneLength=11)
)

#98
rfFit1.12
brand.predict6<-predict(rfFit1.12, testing)
postResample(brand.predict6, testing$brand)
C6 <-confusionMatrix(brand.predict6, testing$brand)
C6
#####

#Use best model to predict brands for incomplete data.
incomplete.predict<-predict(rfFit1.12, incomplete)
table(predict(rfFit1.12, incomplete))
incomplete$brand.predictedRF <-incomplete.predict
head(incomplete)
str(incomplete)
#write.csv(incomplete, "C:\\Users\\Sherri's Laptop\\Documents\\Course3\\Task2\\incomplete_predicted2.csv",row.names=FALSE)

#C5.0 Classification Models
library(modeldata)
str(survey)
vars <-c("salary","age")

in_train <- round(nrow(survey2)*0.7)
in_test <-nrow(survey2)-in_train

train_data <-survey2[in_train,]
test_data<-survey2[-in_train,]
training_indices<-sample(seq_len(nrow(survey2)),size=in_train)
trainSet <-survey2[training_indices,]
testSet <-survey2[-training_indices,]

install.packages("C50")
library("C50")
install.packages("inum")


str(trainSet[,vars])
tree_mod <-C5.0(x=trainSet[,vars], y=trainSet$brand)
tree_mod
summary(tree_mod)
plot(tree_mod)

predict(tree_mod, testSet[,vars])
table(predict(tree_mod, testSet[,vars]))
#this is on incomplete data

incomplete.predict2 <- predict(tree_mod, incomplete[,vars])
table(predict(tree_mod, incomplete[,vars]))
table(predict(rfFit1, incomplete))

brand.predict7<-predict(tree_mod,testSet[,vars])
C7<-confusionMatrix(brand.predict7, testSet$brand)
C7

incomplete$brand.predictedC50 <-incomplete.predict2
head(incomplete)
str(incomplete)
write.csv(incomplete, "C:\\Users\\Sherri's Laptop\\Documents\\Course3\\Task2\\incomplete_predicted.csv",row.names=FALSE)
head(incomplete)

library(ggplot2)
library(cowplot)

ggplot(incomplete, aes(x=age, 
                       y=salary,
                       color=brand.predictedRF
                       )) + geom_point()
#ggplot(survey, aes(x=age, y=salary, color=brand)) +geom_point()

ggplot(incomplete, aes(x=age)) + geom_bar()




#Scatterplot of predicted Graph A will buy product 0 and product 1

filtered_data <-filter(incomplete, brand.predictedRF == 0)
filtered_data2 <-filter(incomplete, brand.predictedRF ==1)
Acer<-ggplot(filtered_data, aes(x=age, y=salary))+ geom_point()
Sony<-ggplot(filtered_data2, aes(x=age, y=salary))+ geom_point()
plot_grid(Acer,Sony, labels=c("Acer","Sony"))



Agebox<-ggplot(incomplete, aes(x=brand.predictedRF, y=age)) + geom_boxplot() + theme_bw()
Salarybox<-ggplot(incomplete, aes(x=brand.predictedRF, y=salary)) + geom_boxplot() + theme_bw()
plot_grid(Agebox,Salarybox, labels = c("Age", "Salary"))

ggplot(incomplete, aes(x=salary, fill=brand.predictedRF)) + geom_density(alpha=0.7)
summary(incomplete)
summary(survey)
str(survey)
head(incomplete, n=20)
