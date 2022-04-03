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
# You already loaded caret above
library(caret)
# Suggest to set a variable to the path
checkpoints_file <- 'C:/Users/Tarik/Desktop/checkpoints_table_FINAL_.csv'
checkpoints_data <- read.csv(checkpoints_file
# This is a small file. Can you just add to git?)
View(checkpoints_data)
str(checkpoints_data)
head(checkpoints_data)[,3:14]

#create the training and test data, 70:30
set.seed(100)
trainRowNumbers<-createDataPartition(checkpoints_data$NDVI_summ_slope, p=0.7, list=FALSE)
trainData <- checkpoints_data[trainRowNumbers,]
testData <- checkpoints_data[-trainRowNumbers,]
x = trainData[, 3:14]
y = trainData$NDVI_summ_slope

# already loaded above
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
# You defined x and y above, just use these variables in the model. No need to redefine
trainData$NDVI_summ_slope <- y

apply(trainData[, 3:14], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})


#visual try
# You defined x and y above, just use these variables in the model. No need to redefine
featurePlot(x = x, y = y,
            plot = "box",
            strip = strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x=list(relation="free"),
                        y=list(relation="free")))

#feature selection using recursive feature elimination rfe: NDVI
set.seed(100)
options(warn=-1)

subsets <- c(3:15)
subsets
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# You defined x and y above, just use these variables in the model. No need to redefine
lmProfile <- rfe(x=trainData[, 3:14], y=trainData$NDVI_summ_slope,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile
ggplot(lmProfile)
#feature selection using recursive feature elimination rfe: TD Index
set.seed(100)
options(warn=-1)

subsets <- c(3:15)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile1 <- rfe(x=trainData[, 3:15], y=trainData$TD.index,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile1
ggplot(lmProfile1)

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
fitted
print(model_mars)
summary(model_mars)
summary(model_mars)%>% .$coefficients %>% head(10)
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor())
hyper_grid

#predict
predicted <- predict(model_mars, testData$NDVI_summ_slope)
head(predicted) 
# Compute the confusion matrix
confusionMatrix(reference = testData$NDVI_summ_slope, data = predicted, mode='everything', positive='MM')

set.seed(123)

# cross validated model
tuned_mars <- train(
  x = subset(trainData, select = -NDVI_summ_slope),
  y = trainData$NDVI_summ_slope,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

tuned_mars$bestTune

#plots mars
ggplot(tuned_mars)  
plot(model_mars, main="Model Accuracies with MARS")

#variable importance
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")

# multiple regression
set.seed(123)
cv_model1 <- train(
  NDVI_summ_slope ~ ., 
  data = trainData, 
  method = "lm",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"))


# principal component regression
set.seed(123)
cv_model2 <- train(
  NDVI_summ_slope ~ ., 
  data = trainData, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20)

# partial least squares regression
set.seed(123)
cv_model3 <- train(
  NDVI_summ_slope ~ ., 
  data = trainData, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20)

# regularized regression
set.seed(123)
cv_model4 <- train(
  NDVI_summ_slope~ ., 
  data = trainData,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 10)

summary(resamples(list(
  Multiple_regression = cv_model1, 
  PCR = cv_model2, 
  PLS = cv_model3,
  Elastic_net = cv_model4,
  MARS = tuned_mars
)))$statistics$RMSE %>%
  kableExtra::kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

install.packages("kableExtra")
install.packages("vip")
library(vip)
p1 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)

