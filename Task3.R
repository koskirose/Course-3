#Import Libraries

library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(tidyverse)
library(kernlab)

#Import the files
exist <- read_csv("existingproductattributes2017.csv")
new <- read_csv("newproductattributes2017.csv")

#Size of data sets 80X18
dim(exist)
dim(new)

class(exist)


names(exist)


# EDA
View(exist)
glimpse(exist)
head(exist)

#Rename the Columns to easier names
exist <-   exist %>%
                   rename(product = ProductType, 
                   number = ProductNum, 
                    price = Price,
                    x5star = x5StarReviews,
                    x4star = x4StarReviews,
                    x3star = x3StarReviews,
                    x2star = x2StarReviews,
                    x1star = x1StarReviews,
                    pos_serv = PositiveServiceReview,
                    neg_serv = NegativeServiceReview,
                    rec_prod = Recommendproduct,
                    bestsell_rank = BestSellersRank,
                    ship_weight = ShippingWeight,
                    depth = ProductDepth,
                    width = ProductWidth,
                    height = ProductHeight,
                    profit_margin = ProfitMargin,
                    volume = Volume)
new <-   new %>%
  rename(product = ProductType, 
         number = ProductNum, 
         price = Price,
         x5star = x5StarReviews,
         x4star = x4StarReviews,
         x3star = x3StarReviews,
         x2star = x2StarReviews,
         x1star = x1StarReviews,
         pos_serv = PositiveServiceReview,
         neg_serv = NegativeServiceReview,
         rec_prod = Recommendproduct,
         bestsell_rank = BestSellersRank,
         ship_weight = ShippingWeight,
         depth = ProductDepth,
         width = ProductWidth,
         height = ProductHeight,
         profit_margin = ProfitMargin,
         volume = Volume)

#Check for Na's

anyNA(exist)
sapply(exist, {function(x) any(is.na(x))})
na<- filter(exist, is.na(bestsell_rank))
na

#Delete the attributes with the NA which is best seller rank
exist$bestsell_rank <- NULL
new$bestsell_rank <- MNexist <-   exist %>%
  rename(product = ProductType, 
         number = ProductNum, 
         price = Price,
         x5star = x5StarReviews,
         x4star = x4StarReviews,
         x3star = x3StarReviews,
         x2star = x2StarReviews,
         x1star = x1StarReviews,
         pos_serv = PositiveServiceReview,
         neg_serv = NegativeServiceReview,
         rec_prod = Recommendproduct,
         bestsell_rank = BestSellersRank,
         ship_weight = ShippingWeight,
         depth = ProductDepth,
         width = ProductWidth,
         height = ProductHeight,
         profit_margin = ProfitMargin,
         volume = Volume)
head(exist)

#Delete the feature product number.  Do not need
exist$number <- NULL
new$number <- NULL
head(exist) <- NULL

#Check correlation
corrplot(cor(exist[,-1]), method = "square")
chart.Correlation(exist[,-1])

#Combine 3,4,and 5 star reviews together and 1,2 star reviews together and delete those columns
exist <- exist %>%
  mutate(x345star = x5star + x4star + x3star,
         x21star = x2star + x1star) %>%
  select(-x5star,-x4star,-x3star,-x2star,-x1star)
exist

new <- new %>%
  mutate(x345star = x5star + x4star + x3star,
         x21star = x2star + x1star) %>%
  select(-x5star,-x4star,-x3star,-x2star,-x1star)
new
- 
corrplot(cor(exist[,-1]), method = "square")
chart.Correlation(exist[,-1])

#A fucntion to find the mode of the feature
getmode <- function(v) {
  uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
      }

getmode(exist$volume)  #Mode is 1232
summary(exist$volume)  #Median is 200, Mean is 705

#This plot looks at our dependent variable "volume" and compares the mean, median, and mode
#in a histogram

ggplot(data = exist) + 
  geom_histogram(mapping = aes(x=volume), bins=200, boundary=0, fill = "gray", col = "black") + 
  geom_vline(xintercept = mean(exist$volume), col = "blue", size = 1) +
  geom_vline(xintercept = median(exist$volume), col = "red", size = 1)+
  geom_vline(xintercept = getmode(exist$volume), col = "green", size = 1)+
  annotate("text", label = "median = 200", x = 2500, y=20, col = "red", size = 5)+
  annotate("text", label = "mode = 1232", x = 2500, y=15, col = "green", size = 5) +
  annotate("text", label = "mean = 705", x = 2500, y=10, col = "blue", size = 5) + 
  ggtitle("Histogram of Volume") + ylim(0,25)+
  theme_dark()

summary(exist)


#check for colinearity
simple_lm<-lm(volume~.,data = exist[-1])
vif(simple_lm)

#Create DF without the dependent variable "volume",  
exist_no_volume <- select(exist,-volume)
exist_no_volume
summary(exist_no_volume)
boxplot(exist_no_volume[-1], col = "orange", main = "Features Boxplot")
#Examine some of the features closer
par(mfrow = c(2,2))
boxplot(exist_no_volume$pos_serv, col = "red", main = "positive service reviews" )
boxplot(exist_no_volume$neg_serv, col = "orange", main ='negative service reviews')
boxplot(exist_no_volume$rec_prod, col = "yellow", main = "recomended product")
boxplot(exist_no_volume$ship_weight, col = "green", main = 'shipping weight')
par(mfrow = c(2,2))
boxplot(exist_no_volume$depth, col = "blue", main = "depth")
boxplot(exist_no_volume$width, col = "pink", main = 'width')
boxplot(exist_no_volume$height, col = "orchid4", main = 'height')
boxplot(exist_no_volume$x345star, col = 'violet', main = "3,4,5 star")
boxplot(exist_no_volume$x21star, col = "deeppink4", main = "1,2 star")



x345outliers <- which(exist_no_volume$x345star > 700 )
exist_no_volume[x345outliers, "x345star"]

#dummify the data
exist_dummy <-dummyVars(~.,data = exist)
readyData <-as.data.frame(predict(exist_dummy, newdata = exist))
head(readyData)

new_dummy <-dummyVars(~.,data = new)
readyDataNew <-as.data.frame(predict(new_dummy, newdata = new))
head(readyDataNew)



###Ready Data Remove features with low correlations  profit_margin, depth, price
#width, and height
#Start here tomorrow
readyData <- readyData %>%
  select(-profit_margin, -depth, -height, -width, -price, -rec_prod, -ship_weight,)
head(readyData)

readyDataNew <- readyDataNew %>%
  select(-profit_margin, -depth, -height, -width, -price, -rec_prod, -ship_weight,)
head(readyDataNew)

corrplot(cor(readyData[,-1]), method = "square")
chart.Correlation(readyData[,-1])

#Data is now ready to model
set.seed(1201)

#Randomize the data

readyData <- readyData[sample(1:nrow(readyData)),]
dim(readyData)
View(readyData)

inTrain <- createDataPartition(y =readyData$volume , p = .75, list = FALSE)
training <-readyData[inTrain,]
testing <- readyData[-inTrain,]
View(training)
View(testing)

my_control <-trainControl(method = "repeatedcv", number = 10, repeats = 2,
                          savePredictions = "final")
                          #index = createResample(training$volume, 10))
#this looks like it should work and it did for another data set
model_list <-caretList(volume~., data = training,
                       trControl = my_control, 
                       methodList = c( "rf", "svmRadial","xgbTree", 'xgbLinear' ), 
                       continue_on_fail = FALSE)
model_list$rf
model_list$svmRadial
model_list$xgbTree
model_list$xgbLinear


options(digits = 3)

model_results <- data.frame(
  RF = min(model_list$rf$results$RMSE),
  SVM = min(model_list$svmRadial$results$RMSE),
  XGBT = min(model_list$xgbTree$results$RMSE),
  XGBL = min(model_list$xgbLinear$results$RMSE))

print(model_results)


volume.predict<-predict(model_list$xgbTree, testing)
volume.predict

volume.predict2<-predict(model_list$rf, testing)
volume.predict2

postResample(volume.predict, testing$volume)

LMFit1 <- train(volume~., data = readyData, method = "lm", trControl = my_control)       
LMFit1 

print(model_results)

volume.predictlm <- predict(LMFit1, testing)
volume.predictlm
postResample(volume.predictlm, testing$volume)

volume.predictxgb <-predict(model_list$xgbTree, readyData)
volume.predictxgb
volume.predictrf <- predict(model_list$rf, readyData)
volume.predictrf


a <- predict(LMFit1, readyData)
a

postResample(a, readyData$volume)
postResample(volume.predictxgb, readyData$volume)

postResample(volume.predictrf, readyData$volume)
             




readyData <- as.data.frame(readyData %>%
                         mutate(pred_volumexgb = volume.predictxgb,
                                pred_volumerf = volume.predictrf,
                                pred_lm = a))
head(exist) 

volume_only_dataframe <- readyData[c("volume", "pred_volumexgb", 'pred_volumerf', "pred_lm")]
View(volume_only_dataframe)

Predict_Sales_VolumeRF <- predict(model_list$rf, readyDataNew)
Predict_Sales_VolumeLM <- predict(LMFit1, readyDataNew)

readyDataNew <- readyDataNew %>%
  mutate(Predict_Sales_VolumeLM, Predict_Sales_VolumeRF) 

new <- new %>%
  mutate(Predict_Sales_VolumeLM, Predict_Sales_VolumeRF) 



write.csv(new, "C:/Users/Sherri's Laptop/Documents/Course3/Task 3/predicted_sales.csv",row.names=FALSE)


ggplot(data=exist, mapping = aes(x=x345star, y= volume)) +
  geom_point() + 
  geom_smooth(method = lm)+ 
  ggtitle("Number of 3,4 and 5 Star Reviews vs Volume of Units Sold")

ggplot(data=exist, mapping = aes(x=x21star, y= volume)) +
  geom_point() + 
  geom_smooth(method = lm)+ 
  ggtitle("Number of 3,4 and 5 Star Reviews vs Volume of Units Sold")
