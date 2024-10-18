##using libraries
library(dplyr) //data manipulation
library(mice) //data imputation

View(train) //viewing dataset
View(test)

names(train) //names of the datas
names(test)

##combining data
test$SalePrice=NA
dataset<-rbind(train,test)

##taking numeric variables from the dataset
dataset <- filter(Filter(is.numeric, dataset))

##renaming data's
dataset$oneflr=dataset$"1stFlrSF"
dataset$secoundflr=dataset$"2ndFlrSF"
dataset$porch=dataset$"3SsnPorch"

##removing uneccessary elements
data=mice(dataset[, !names(dataset) %in% c("YearBuilt","YearRemodAdd","YrSold","3SsnPorch","1stFlrSF","2ndFlrSF")],where = NULL,methode="pmm")
new_data=complete(data)
sum(is.na(new_data))

##splitting datasets
train <- new_data[1:1460,]
test <- new_data[1461:2919,]
set.seed(1234)

##model creation
m =lm(SalePrice~. ,data = train)
summary(m)


m1=lm(SalePrice~MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea+BsmtFinSF1+BsmtUnfSF+GrLivArea+BsmtFullBath+FullBath+BedroomAbvGr+TotRmsAbvGrd+KitchenAbvGr+GarageYrBlt+GarageCars+WoodDeckSF+ScreenPorch ,data=train)
summary(m1)

##prediction
predict=predict(m,test)

##Saving the prediction result
submission=data.frame(id=test$Id ,SalePrice=predict)
write.csv(submission,file="submission.csv",row.names = F)
getwd()
