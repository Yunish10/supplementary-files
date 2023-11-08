################# Neural network model ################# 
install.packages("NeuralNetTools")
install.packages("caret")
install.packages("nnet")
install.packages("openxlsx")
install.packages("psych")
install.packages("lattice")
install.packages("ggplot2")
install.packages("tdr")
install.packages("MASS")
install.packages("rio")
library(lattice)
library(ggplot2)
library(caret)
library(nnet)
library(openxlsx)
library(NeuralNetTools)
library(psych)
library(tdr)
library(MASS)
library(rio)
df <- read.xlsx("PCA(WI)_Yield_dataset.xlsx", sheet="result")
#Visualize the data
head(df, 2)
tail(df,2)
# Training and Test Data
training <- df[1:16, ]
testing <- df[17:20, ]
#Fitting nnet model using caret package
nnetGrid <-  expand.grid(size = seq(from = 1, to = 8, by = 1),
                         decay = seq(from = 0.001, to = 0.5, by = 0.01))
trainControl <- trainControl(method="cv", number=3, 
                             savePredictions=TRUE, classProbs=F)
set.seed(7)
fit.nnet <- train(Yield ~ ., data = training,
                  method = "nnet",
                  preProc=c("center","scale"),
                  linout = TRUE,
                  TRACE = FALSE,
                  maxit = 100,
                  tuneGrid = nnetGrid,
                  trControl = trainControl)
fit.nnet
plot(fit.nnet)
varImp(fit.nnet)
plot(varImp(fit.nnet), top = 10)
#Neural network model using nnet package
######normalize all variables 0-1
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))}
df_normal <- as.data.frame(lapply(df[-1], normalize))
df_norm <- data.frame(df$Yield, df_normal)
head(df_norm, 2)
colnames(df_norm)[1] <- "Yield"
# Training and Test Data
dataTrain <- df_norm[1:16, ]
dataTest <- df_norm[17:20, ]
model.nn2 <- nnet(Yield ~ ., data = df_norm, size =7, decay = 0.401, 
                  maxit = 100, linout = T)
nnet2_cal <- predict(model.nn2, dataTrain)
nnet2_cal
nnet2_val <- predict(model.nn2, dataTest)
nnet2_val
#nnet_fc<- predict(model.nn2, dataforecast)
plotnet(model.nn2)
#R2,  RMSE and nRMSE calculation
#For training
eval_Model_cal <- applyStats(nnet2_cal, dataTrain$Yield)
eval_Model_cal
#For testing
eval_Model_val <- applyStats(nnet2_val, dataTest$Yield)
eval_Model_val
##calculation of percentage error
data_cal <- data.frame(Predicted = nnet2_cal, Observed = dataTrain$Yield)
data_val <- data.frame(Predicted = nnet2_val, Observed = dataTest$Yield)
error<-((data_val$Observed-data_val$Predicted)/data_val$Observed)*100
error
export(data_cal, "ann_Yunish(Yield3)cal.xlsx")
export(data_val, "ann_Yunish(Yield3)val.xlsx")
## TO CALCULATE THE VALUE OF EF _ cal
n=sum((data_cal$Observed-data_cal$Predicted)^2)
d=sum((data_cal$Observed-mean(data_cal$Observed))^2)
EF=1-n/d
EF
## TO CALCULATE THE VALUE OF EF _ val
n=sum((data_val$Observed-data_val$Predicted)^2)
d=sum((data_val$Observed-mean(data_val$Observed))^2)
EF=1-n/d
EF
