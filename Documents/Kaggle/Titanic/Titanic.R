# Load Librery
library('randomForest')
library('dplyr')


# Loading data sets
train<-read.csv("C:/Users/sverma061/Documents/Kaggle/Titanic/train.csv")
test<-read.csv("C:/Users/sverma061/Documents/Kaggle/Titanic/test.csv")

# Understanding the data
View(train)
View(test)
full<-bind_rows(train,test)
str(full)

# Feature Engineering
full$Title<-gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
full$FSize<-full$SibSp+full$Parch+1

# Missing data imputation
sapply(full, function(x) sum(is.na(x)))
full$Age[is.na(full$Age)]<-median(full$Age,na.rm=TRUE)
train <- full[1:891,]
test <- full[892:1309,]

# Building a random forest model
rf_model<-randomForest(factor(Survived)~Pclass+Sex+Age+FSize,data=train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Predicting using the model
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
View(solution)

# Write the solution to file
write.csv(solution, file = 'C:/Users/sverma061/Documents/Kaggle/Titanic/solution.csv', row.names = F)
