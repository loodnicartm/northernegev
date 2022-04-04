#start of the project

# MS: Here's a nice way to check for installed packages, and install only if necessary
##--------------------------
# Load required packages
##--------------------------
pkg_list = c("caret", "skimr",           
             "RANN",            
             "randomForest", 'ranger',          
             "fastAdaboost", 'gbm', 'xgboost', 
             "C50", "earth", "caretEnsemble",
             "dplyr", "kableExtra", "corrplot")

installed_packages <- pkg_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkg_list[!installed_packages])
}

# Load Packages
lapply(pkg_list, function(p) {require(p,
                                      character.only = TRUE,
                                      quietly=TRUE)})

# install.packages('caret')
# install.packages('skimr')
# install.packages('RANN')
# install.packages('randomForest')
# install.packages('fastAdaboost')
# install.packages('gbm','xgboost','caretEnsemble','C50','earth')
# install.packages('xgboost')
# install.packages('gbm')
# install.packages('caretEnsemble')
# install.packages('C50')
# install.packages('earth')

##--------------------------
# Load data and prepare train/test split
##--------------------------
# Suggest to set a variable to the path
checkpoints_file <- 'checkpoints_table_FINAL_.csv'
checkpoints_data <- read.csv(checkpoints_file)
head(checkpoints_data)

# MS: Define X and Y for all model runs
# MS: I use CAPS to distinguish from the function parameters
X <- checkpoints_data[, 3:14]
Y <- checkpoints_data$NDVI_summ_slope

# MS: Do one-hot encoding right at the beginning
# creating the dummy variables-converting a categorical variable 
# to as many binary variables as here are categories.
dummies_model <- dummyVars("~agro_type", data=X, sep="_")
dummy_mat <- predict(dummies_model, newdata = X)

#create a data frame with newe one-hot encoded columns
X <- cbind(X, as.data.frame(dummy_mat))
# See the structure of the new data set
str(X)
# Now we can drop the agro_type column
X <- select(X, -agro_type)

##--------------------------
# MS: Before continuing. check cross correlations!
##--------------------------
cor_matrix = cor(X)
corrplot::corrplot(cor_matix)
# MS: Ooops, we have two variables that are 100% correlated:
# agro_typeshick and agro_typeliman.
# But that's obvious since every point is either shich or liman
# So we must remove one.
X <- select(X, -agro_typeliman)
# Also DD and rainfall seem highly correlated,
# as well as two and tpi_8m
# TODO: Decide here what to do with these....

# Now create the training and test data, 70:30 split
set.seed(100)
trainRowNumbers<-createDataPartition(Y, p=0.7, list=FALSE)
trainX <- X[trainRowNumbers,]
trainY <- Y[trainRowNumbers]

testX <- X[-trainRowNumbers,]
testY <- Y[-trainRowNumbers]

skim(trainX)
skim(trainY)


#preprocess 
# MS: What do you want to do here? Why do you need bagImpute?
# maybe center and scale??
preProcess_range_model <- preProcess(trainx, method='bagImpute')
trainX <- predict(preProcess_range_model, newdata = trainX)

# Append the Y variable
# You defined x and y above, just use these variables in the model. No need to redefine
# trainX$NDVI_summ_slope <- y

# MS: What do you want to do here? Why is this necessary?
apply(trainX[, 3:14], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})


#visual try
# MS: Don't know what's wrong here
featurePlot(x = trainX, y = trainY,
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
lmProfile <- rfe(x=trainX, y=trainY,
                 sizes = subsets,
                 rfeControl = ctrl)

summary(lmProfile)
ggplot(lmProfile)


#feature selection using recursive feature elimination rfe: TD Index
set.seed(100)
options(warn=-1)
subsets <- c(3:15)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile1 <- rfe(x=trainX, y=trainY,
                 sizes = subsets,
                 rfeControl = ctrl)

summary(lmProfile1)
ggplot(lmProfile1)

##--------------------------
# exploring models
##--------------------------
modelLookup('earth')
modelLookup('rf')
modelLookup('adaboost')
modelLookup('xgbDART')
modelLookup('svmRadial')

#starting with models:
# MS: First a very simple linear model
trainData = cbind(trainY, trainX)
lm1 <- lm(trainY~., data=trainData)
summary(lm1)

glm1 <- glm(trainY~., data=trainData)
summary(glm1)
# MS: maybe some simple scatterplots of trainY vs trainX$... will help
# to visualize which family to use in glm ?? 
scatter.smooth(trainX$year_plant, trainY,
               xlab="Planing Year", ylab="NDVI slope")

#MARS
set.seed(100)

model_mars = train(NDVI_summ_slope ~ ., data=trainX, method='earth')
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
predicted <- predict(model_mars, testData$NDVI_summ_slope, interval='confidence')
head(predicted)
# Compute the confusion matrix
confusionMatrix(reference = testData$NDVI_summ_slope, data = predicted, mode='everything', positive='MM')

set.seed(123)

# cross validated model
tuned_mars <- train(
  x = subset(trainX, select = -NDVI_summ_slope),
  y = trainX$NDVI_summ_slope,
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
  data = trainX, 
  method = "lm",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"))

cv_model1

# principal component regression
set.seed(123)
cv_model2 <- train(
  NDVI_summ_slope ~ ., 
  data = trainX, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20)

cv_model2

# partial least squares regression
set.seed(123)
cv_model3 <- train(
  NDVI_summ_slope ~ ., 
  data = trainX, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20)

cv_model3

# regularized regression
set.seed(123)
cv_model4 <- train(
  NDVI_summ_slope~ ., 
  data = trainX,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 10)

cv_model4

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

# Define the training control
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 

#RF training
set.seed(100)
model_rf = train(NDVI_summ_slope, data=trainX[3:14], method='rf', tuneLength=5, trControl = fitControl)
model_rf


#xgBoost training
set.seed(100)
model_xgbDART = train(NDVI_summ_slope ~ ., data=trainX, method='xgbDART', tuneLength=5, trControl = fitControl, verbose=F)
model_xgbDART


#SVM training
set.seed(100)
model_svmRadial = train(NDVI_summ_slope ~ ., data=trainX, method='svmRadial', tuneLength=15, trControl = fitControl)
model_svmRadial


# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf, XGBDART=model_xgbDART, MARS=model_mars3, SVM=model_svmRadial))

# Summary of the models performances
summary(models_compare)
#summary plots
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)


#ensemble predictions

library(caretEnsemble)

# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')

set.seed(100)
models <- caretList(Purchase ~ ., data=trainX, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)
# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
