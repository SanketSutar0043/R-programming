install.packages("naivebayes")
library(caret)
library(naivebayes)
gs=read.csv(file.choose(),header = T)

head(gs)
gs$admit=as.factor(gs$admit)
gs$prestige=as.factor(gs$prestige)
table(gs$admit)
prop.table(table(gs$admit))

## split the data into train and test
totalrows=nrow(gs)
print(totalrows)

split = sample(seq(1,totalrows),floor(0.7*totalrows))

train = gs[split,]
test = gs[-split,]
dim(train)
dim(test)

# build the naive bayes model 
#set parameter 'usekernel'=T in case if the numeric  features dont have a normal distribution
m1=naive_bayes(admit~., data = train,usekernel = T)

#prediction
p1=predict(m1,test)

#confusion matrix 
confusionMatrix(test$admit,p1,positive = "1")
