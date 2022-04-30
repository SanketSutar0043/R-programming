library(funModeling)#to check the freq of data
library(Hmisc)#describe the data
library(tidyverse)
library(caret)
library(stats)
library(readr)
library(ggplot2)
library(readr)
library(dplyr)

hp=read.csv(file.choose(),header=T)  
dim(hp)
#Get all the character columns
charcol = names(hp)[sapply(hp,is.character)]
charcol
length(charcol)

## get all the numeric columns
numcols = names(hp)[sapply(hp,is.numeric)]
numcols
length(numcol)
anyNA(hp)
View(hp$Property_Sale_Price)
## Find out NULL 
colSums(is.na(hp))             
## Singularity is more than 85% then remove th feature
str(hp)
freq(hp)
hp$Alley = NULL              
hp$PoolQC = NULL 
hp$MiscFeature = NULL
hp$Id = NULL
hp$LandContour = NULL
hp$Road_Type = NULL
hp$PavedDrive = NULL
hp$SaleType = NULL
hp$GarageType = NULL
hp$GarageQual = NULL
hp$Functional = NULL
hp$Electrical = NULL
hp$CentralAir = NULL
hp$Heating = NULL
hp$BsmtFinType2 = NULL
hp$GarageFinish=NULL
hp$BsmtCond = NULL
hp$FireplaceQu=NULL
hp$ExterCond = NULL
hp$RoofMatl = NULL
hp$Condition2 = NULL
hp$Condition1 = NULL
hp$LandSlope = NULL
hp$Utilities = NULL
hp$GarageCond = NULL
hp$BsmtExposure = NULL
dim(hp)
freq(hp)
colSums(is.na(hp))
str(hp)


#NULLS in Numeric and Factor Variables ..

checknulls = function(x)return(any(is.na(x)))

nullnum = numcols[apply(hp[numcols],2,checknulls)]
nullnum
##3 now impute the Value of NULL

# When MasVnrType is NULL then MasVnrArea is also NULL..
table(hp$MasVnrArea)
table(hp$MasVnrType)

# impute the NA in MasVnrType by None.
table(hp$MasVnrType)
hp$MasVnrType[is.na(hp$MasVnrType)] = "None"
table(hp$MasVnrType)


hp$MasVnrArea[is.na(hp$MasVnrArea)] = 0
table(hp$MasVnrArea)


### GARAGE
#CHaracter >- GarageFinish , NUmeric>- GarageYrBlt
table(hp$GarageYrBlt)
hp$GarageYrBlt[is.na(hp$GarageYrBlt)] = 0

####
colSums(is.na(hp))

freq(hp$BsmtQual)
hp$BsmtQual[is.na(hp$BsmtQual)] = 'NR'
freq(hp$BsmtQual)
freq(hp$BsmtFinType1)
hp$BsmtFinType1[is.na(hp$BsmtFinType1)] = "NR"
colSums(is.na(hp))

#merging the unneccessary levels of the factor
freq(hp$HeatingQC)
hp$HeatingQC[hp$HeatingQC=="Po"]="Fa"

freq(hp$SaleCondition)
hp$SaleCondition[hp$SaleCondition=="AdjLand"]="Alloca"

freq(hp$Exterior1st)
hp$Exterior1st[hp$Exterior1st=="CBlock"]="other"
hp$Exterior1st[hp$Exterior1st=="ImStucc"]="other"
hp$Exterior1st[hp$Exterior1st=="BrkComm"]="other"
hp$Exterior1st[hp$Exterior1st=="AsphShn"]="other"
hp$Exterior1st[hp$Exterior1st=="Stone"]="other"

freq(hp$Exterior2nd)
hp$Exterior2nd[hp$Exterior2nd=="CBlock"]="Other"
hp$Exterior2nd[hp$Exterior2nd=="AsphShn"]="Other"
hp$Exterior2nd[hp$Exterior2nd=="Stone"]="Other"
hp$Exterior2nd[hp$Exterior2nd=="Brk Cmn"]="Other"
hp$Exterior2nd[hp$Exterior2nd=="ImStucc"]="Other"
hp$RoofStyle[hp$RoofStyle=="Shed"]="Mansard"

anyNA(hp)
colSums(is.na(hp))

hp$Fence[is.na(hp$Fence)]= "no"

#############################################
#Study about Lot Frontage
table(hp$LotFrontage)
describe(hp$LotFrontage)
library(psych)

describe(hp$LotFrontage)

hp$LotFrontage[is.na(hp$LotFrontage)] = 0
describe(hp$LotFrontage)

#### Find out Outlier with the help of boxplot..
numcol1 = names(hp[sapply(hp,is.numeric)])
length(numcol1)
anyNA(hp)

colSums(is.na(hp))
for(c in numcol1)
{
  boxplot(hp[c],horizontal = T,col="red",main=c)
}
str(hp)

#converting the character column into the factor column
charcol=names(hp)[sapply(hp,is.character)]
hp[charcol] = lapply(hp[charcol], as.factor)
length(charcol)
str(hp)

hp$BsmtFinSF2 = NULL
hp$PoolArea = NULL
hp$ScreenPorch = NULL
hp$SsnPorch = NULL
hp$EnclosedPorch = NULL
hp$KitchenAbvGr = NULL
hp$BsmtHalfBath = NULL
hp$LowQualFinSF = NULL
dim(hp)
     
hp1=hp
#minmax
minmax=function(x)return(x-min(x))/(max(x)-min(x))
numcol1 = names(hp1)[sapply(hp1,is.numeric)]
numcol1=as.data.frame(sapply(hp1[numcol1],minmax))
totalrowss = nrow(hp1)
totalrowss
totalrows = nrow(hp)
#describe(totalrowss)
#dim(totalrowss)

## generate data into train that is 70% 
ss = sample(seq(1,totalrowss),floor(0.7*totalrowss))
length(ss)
ss1 = sample(seq(1,totalrows),floor(0.7*totalrowss))
length(ss1)
#now divide into Train and test

train = hp1[ss,]
test = hp1[-ss,]
train1=hp[ss1,]
test1=hp[-ss1,]
dim(train)
dim(test)

head(train)

m1 = lm(Property_Sale_Price~.,data = train)
m11=lm(Property_Sale_Price~.,data = train1)
summary(m1)

summary(m11)
mean(m1$residuals)
mean(m11$residuals)
plot(m1)


library(lmtest)
library(caret)
library(ggplot2)


#######
## MEan of residual should be zero

#mean(m2$residuals)

# the mean of residuals should be zero but now m1$residual is not zero there for data not homoscidaticity,....


# test for Heteroscedasticity
#install.packages("lmtest",dependencies = T)
#install.packages("car",dependencies = T)
library(lmtest)
library(car)
# H0: homoscedastic
# H1: heteroscedastic

# breusch-pagan test

ret = bptest(m1)
ret

if(ret$p.value<0.05)
  print('model is hetroscedastic') else
    print('model is homoscedastic')

# NCV (non constant variance) test 
ret1=ncvTest(m1)
ret1
if(ret1$p<0.05)
  print('model is heteroscedastic') else
    print('model is homoscedastic')


# Cross-validation
library(DAAG)
library(caret)

freq(hp1$Exterior1st)

# predict the Y on the test data
p1 = predict(m1,test)
p11=predict(m11,test1)
View(p1)
View(p11)
p1[1:10]



# storing the actual result and predicated value 

result1=data.frame('actual' = test$Property_Sale_Price,'predict using minmax' = round(p1,2),'simple predict'=round(p11,2))
head(result1,25)
view(result1)


rmse1_Bas=RMSE(test$Property_Sale_Price,p1)
rmse11=RMSE(test1$Property_Sale_Price,p11)
rmse1_Bas
rmse11


# Create the data frame to store the Predicted Y and actual Y

result = data.frame('actual' = test$Property_Sale_Price , 'predict minmax' = round(p1,2),'predict'=round(p11,2))
head(result)
head(result,25)
View(result)
write.csv(result,'result of basic model.csv')
# since the model (M1) is heteroscedastic, do a BoxCox on the y-variable

library(MASS)
bct = boxcox(Property_Sale_Price~.,data=train)
print(bct)

# to get the optimal lambda value,
# i) find the max Y
# ii) for the max Y, get the corresponding lambda (X)

lamda = bct$x[which(bct$y==max(bct$y))]
print(lamda)

# transform the Y-variable into boxcox transformation
trainY_bct = train$Property_Sale_Price^lamda
testY_bct = test$Property_Sale_Price^lamda

# store the 2 values in the train and test dataframe
train$Y_bct = trainY_bct
test$Y_bct = testY_bct

# all in 1 step
train$Y_bct = train$Property_Sale_Price^lamda
test$Y_bct = test$Property_Sale_Price^lamda


# model 2 
# x: as it is
# y: boxcox transformed value
m2 = lm(trainY_bct~., data=train)
summary(m2)
plot(m2)


# breusch-pagan test
library(lmtest)
ret2 = bptest(m2)
if(ret2$p.value<0.05)
  print('model is heteroscedastic') else
    print('model is homoscedastic')


# predict
p2=predict(m2,test)
View(p2)
head(p2)
head(p2)
# predicted values are in BoxCox format
# need to re-transform this in the original form
p2[1]^(1/lamda)

# create a dataframe to store results of M3
df3=data.frame("actual"=test$Property_Sale_Price,
               "pred"=p2^(1/lamda))
head(df3)               
View(df3)

# RMSE of M2
library(caret)
rmse3=RMSE(df3$actual,df3$pred)
rmse3
cat("M1: RMSE1(minmax)=",rmse1_Bas,"RMSE11=",rmse11," \nM3: RMSE=",rmse3)

