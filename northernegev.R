#start of the project
install.packages('caret')
install.packages('skimr')
install.packages('RANN')
install.packages('randomForest')
install.packages('fastAdaboost')
install.packages('gbm','xgboost','caretEnsemble','C50','earth')
install.packages('xgboost')
install.packages('gbm')
install.packages('caretEnsemble')
install.packages('C50')
install.packages('earth')

#loading caret
library(caret)
checkpoints_data <- read.csv('C:/Users/Tarik/Desktop/checkpoints_table_FINAL_.csv')
View(checkpoints_data)
str(checkpoints_data)
head(checkpoints_data)[,1:56]

#create the training and test data, 70:30
set.seed(100)
trainRowNumbers<-createDataPartition(checkpoints_data$NDVI_summ_slope, p=0.7, list=FALSE)
trainData <- checkpoints_data[trainRowNumbers, ]
testData <- checkpoints_data[-trainRowNumbers, ]
x = trainData[3:15]
y = trainData$NDVI_summ_slope

library(skimr)
skimmed <- skim(trainData)
skimmed [, c(1:12)]

#one-hot encoding
#creating the dummy variables-converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(" ~ .", data=trainData, fullrank = T)
trainData_mat <- predict(dummies_model, newdata = trainData)
#create a data frame
trainData <- data.frame(trainData_mat)
# See the structure of the new data set
str(trainData)

#preprocess 
preProcess_range_model <- preProcess(trainData, method='bagImpute')
trainData <- predict(preProcess_range_model, newdata = trainData)

# Append the Y variable
trainData$NDVI_summ_slope <- y

apply(trainData[, 3:15], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})


#visual try
featurePlot(x = trainData[3:15],
            y = trainData$NDVI_summ_slope,
            plot = "box",
            strip = strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x=list(relation="free"),
                        y=list(relation="free")))

#feature selection using recursive feature elimination rfe: NDVI
set.seed(100)
options(warn=-1)

subsets <- c(3:15)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 3:15], y=trainData$NDVI_summ_slope,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

#feature selection using recursive feature elimination rfe: TD Index
set.seed(100)
options(warn=-1)

subsets <- c(3:15)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 3:15], y=trainData$TD.index,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile


#exploring models
modelLookup('earth')
modelLookup('rf')
modelLookup('adaboost')
modelLookup('xgbDART')
modelLookup('svmRadial')

#starting with models:

#MARS
set.seed(100)

model_mars = train(NDVI_summ_slope ~ ., data=trainData, method='earth')
fitted <- predict(model_mars)
model_mars
plot(model_mars, main="Model Accuracies with MARS")
