data=read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project III Materials//Used_Bikes.csv")
View(data)
head(data)
names(data)
summary(data)
str(data)
library(plyr)
library(dplyr)
library(stringr)
library(mgsub)
library(ggplot2)
library(corrplot)
library(psych)
library(packHV)
library(moments)
library(tidyverse)
library(Metrics)


################################   Pre- processing and Feature Engineering  #######################################

#Removing duplicates
sum(duplicated(data))
data=distinct(data)
nrow(data)
#Replacing "" with NA
data = replace(data, data=='', NA)
colSums(is.na(data))
#Removing records with price=0
data=data[data$price != 0, ] 
data=data[data$kms_driven != 0, ] 
data=data[data$age != 0, ] 
data=data[data$power != 0, ] 
nrow(data)


########################################################################################################
#Create brand_New column
table(data$brand)
brand_New=c()
for(i in 1:length(data$brand)){
  if((data$brand[i]=="Ideal")|(data$brand[i]=="Indian")|
     (data$brand[i]=="LML")|(data$brand[i]=="Rajdoot")|
     (data$brand[i]=="Yezdi")|(data$brand[i]=="MV")|(data$brand[i]=="Jawa")){
    brand_New[i]="Others"
    
  }else{
    brand_New[i]=data$brand[i]
  }
  
}
table(brand_New)
data=cbind(data,brand_New)



#Create the state column
# read in the second file with unique cities and their corresponding state
df= read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project III Materials//cities.csv")
sum(duplicated(df))
df=distinct(df)
table(df$State)
nrow(df)


state=c()
for(i in 1:nrow(df)){
  for(j in 1:nrow(data)){
    if(data$city[j]==df$city[i]){
      state[j]=df$State[i]
    }else{
      next
    }
  }
}

for(i in 1:length(state)){
  if((state[i]=="Arunachal Pradesh")|(state[i]=="Meghalaya")|
     (state[i]=="Sikkim")){
    state[i]="Others"
  }else{
    state[i]=state[i]
  }
}
table(state)
data=cbind(data,state)
table(data$owner)

#Factoring
data$owner= factor(data$owner,level=c("First Owner","Second Owner","Third Owner",
                                      "Fourth Owner Or More"))
data$brand_New = factor(data$brand_New,level=c("Bajaj","Benelli","BMW","Ducati","Harley-Davidson","Hero","Honda",
                                               "Hyosung","Kawasaki","KTM","Mahindra","Royal Enfield"
                                               ,"Suzuki","Triumph","TVS","Yamaha","Others"))
data$state = factor(data$state,level=c("Andhra Pradesh","Assam","Bihar","Chhattisgarh",
                                       "Delhi","Goa","Gujarat","Haryana",
                                       "Himachal Pradesh" ,"Jammu and Kashmir","Jharkhand", "Karnataka",
                                       "Kerala",    "Madhya Pradesh",       "Maharashtra",            "Odisha",
                                       "Puducherry" ,           "Punjab" ,        "Rajasthan",
                                       "Tamil Nadu",         "Telangana",     "Uttar Pradesh",       "Uttarakhand",
                                       "West Bengal" , "Others"))


str(data)
#############################################################################################

y=which(data$kms_driven<10)
data=data %>%  filter(!row_number() %in% as.numeric((y)))
nrow(data)



#Splitting the data in to training and testing sets
set.seed(100)
indexes=sample(1:nrow(data),0.2*nrow(data))
testset=data[indexes,]
trainset=data[-indexes,]
View(trainset)
View(testset)
head(trainset)
head(testset)
nrow(trainset)
nrow(testset)
table(trainset$brand_New)
table(trainset$owner)
table(trainset$state)
table(testset$brand_New)
table(testset$owner)
table(testset$state)
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$price,main="Price distribution",col="#9494b8",xlab="Bike Price(In Indian Rupees)")
boxplot.stats(trainset$price)$stats
x=which(trainset$price %in% boxplot.stats(trainset$price)$out)
length(x)

#################################PLS###################################################
trainset_new=subset(trainset,select=-c(bike_name,city,brand))
testset_new=subset(testset,select=-c(bike_name,city,brand))

library(mdatools)
library(caret)
dummy_coding= dummyVars(" ~ . ", data = trainset_new)
trainset_encoded=predict(dummy_coding, newdata =trainset_new)

View(trainset_encoded)

x=trainset_encoded[,-1]
x[,c(1,6,7)]=scale(x[,c(1,6,7)], center = TRUE, scale = TRUE)
y=as.matrix(trainset_encoded[,1])
set.seed(100)
ModelPLS = pls(x,y,cv=5, info = "Bike Price prediction")
#view summary of model fitting
summary(ModelPLS)

#visualize CV plots
dev.off()
plot(ModelPLS)
plotXScores(ModelPLS,show.label = FALSE)
plotXYLoadings(ModelPLS,show.label = FALSE)
plotVIPScores(ModelPLS,ncomp=19, type = "h",show.label = TRUE)
summary(ModelPLS$coeffs)
plot(ModelPLS$coeffs,show.label = TRUE)
summary(ModelPLS$res$cal)
#Checking Outliers
Model0=setDistanceLimits(ModelPLS,lim.type = ModelPLS$lim.type,alpha=0.05)
plotXYResiduals(Model0,show.labels=FALSE,labels="indices")
# Identify outlier indices
# get row indices for outliers in calibration set
outliers = which(categorize(Model0, ModelPLS$res$cal) == "outlier")
length(outliers)
df1=trainset[c(outliers),]
View(df1)
table(df1$brand_New)
table(df1$owner)
table(df1$state)
trainset_new=trainset_new %>%  filter(!row_number() %in% outliers)
str(trainset_new)
table(trainset_new$owner)
table(testset_new$owner)
table(trainset_new$brand_New)
table(testset_new$brand_New)
table(trainset_new$state)
table(testset_new$state)
nrow(trainset_new)
nrow(testset_new)




#######################################################################################
View(trainset_new)
data1=trainset_new
data1[,c(2,4,5)]=scale(data1[,c(2,4,5)], center = TRUE, scale = TRUE)
data1[,2]=as.numeric(data1[,2])
data1[,4]=as.numeric(data1[,4])
data1[,5]=as.numeric(data1[,5])

View(data1)
df=trainset[trainset$brand_New=="BMW",]
View(df)
table(data1$brand_New)
nrow(trainset_new)
# Create model matrix with categorical variables as dummies and numerical variable as is

model_matrix = model.matrix(price~., data = data1)[,-1]


# Separate the response variable from the predictors
y1 = data1$price
#y1 = df$Min.Delay
is.matrix(model_matrix)
dim(model_matrix)
# Create a sequence of lambda values to test
lambda_seq= 10^seq(-3, 10, length.out = 100)
library(glmnet)
# Use cv.glmnet to perform cross-validation and select the optimal lambda value
#cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq)
set.seed(100)
cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq,nfolds=10,standardize=FALSE)

# View the optimal lambda value
best_lambda1=cv_fit$lambda.min
best_lambda2=cv_fit$lambda.1se
# Plot the cross-validation results
dev.off()
plot(cv_fit)

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 0, lambda = best_lambda)
model_cv1 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix)
y_hat_ridge2 = predict(model_cv2, model_matrix)

library(Metrics)
actual=y1
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
mae_m = mae(y1, y_hat_ridge1)
rmse_m = rmse(y1, y_hat_ridge1)
mae_m
rmse_m
mae_m = mae(y1, y_hat_ridge2)
rmse_m = rmse(y1, y_hat_ridge2)
mae_m
rmse_m
range(trainset_new$price)
mse1=sum((actual -pred1)^2)/nrow(data1)
mse2=sum((actual -pred2)^2)/nrow(data1)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data1))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data1))


c1=coef(model_cv1 ,s=best_lambda1)
c2=coef(model_cv2 ,s=best_lambda2)
#For the test set
data2=testset_new
data2[,c(2,4,5)]=scale(data2[,c(2,4,5)], center = TRUE, scale = TRUE)
data2[,2]=as.numeric(data2[,2])
data2[,4]=as.numeric(data2[,4])
data2[,5]=as.numeric(data2[,5])

View(data2)
nrow(data2)
model_matrix1 = model.matrix(price~., data = data2)[,-1]

# Separate the response variable from the predictors
#y1 = data1$Min.Delay
y2 = data2$price
model_cv1 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix1)
y_hat_ridge2 = predict(model_cv2, model_matrix1)


actual=y2
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
mape_m = mape(y2, pred1)
rmse_m = rmse(y2, pred1)
mape_m
rmse_m
mape_m = mape(y2, pred2)
rmse_m = rmse(y2, pred2)
mape_m
rmse_m
mse1=sum((actual -pred1)^2)/nrow(data2)
mse2=sum((actual -pred2)^2)/nrow(data2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))

# Replace 'c1' with the actual coefficients obtained from your model
c1 = as.matrix(coef(model_cv1, s = best_lambda1))

# Get the column names of the original dataset used in the model
variable_names <- colnames(model_cv1$df)

# Convert the coefficients into a data frame for plotting, with variable names assigned
c1_df <- data.frame(variable = variable_names, coefficient = c1)

# Create the plot using ggplot
ggplot(data = c1_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  labs(title = "Coefficients of Model", x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




#Lasso Regression
# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression
#lasso_cv = cv.glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq, nfolds = 10)
set.seed(100)
lasso_cv = cv.glmnet(x = model_matrix, y=y1, alpha = 1, lambda = lambda_seq, nfolds = 10,standardize=FALSE)
# Plot cross-validation results
dev.off()
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv1 = lasso_cv$lambda.min
lambda_cv2 = lasso_cv$lambda.1se

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv)
model_cv1 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix)
y_hat_lasso2 = predict(model_cv2, model_matrix)

actual=y1
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)
mae_m = mae(y1, pred1)
rmse_m = rmse(y1,pred1)
mae_m
rmse_m
mae_m = mae(y1, pred2)
rmse_m = rmse(y1, pred2)
mae_m
rmse_m
mse1=sum((actual -pred1)^2)/nrow(data1)
mse2=sum((actual -pred2)^2)/nrow(data1)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data1))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data1))

#rsq_lasso_cv = cor(y, y_hat_cv)^2
coef(model_cv1 ,s=lambda_cv1)
coef(model_cv2 ,s=lambda_cv2)

table(trainset_new$brand_New)
# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq,standardize = FALSE)
plot(res, xvar = "lambda")
#legend("bottomright", lwd = 1, col = 1:6, legend = colnames(model_matrix), cex = .7)


#For the test set

model_cv1 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix1)
y_hat_lasso2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)
mape_m = mape(y2, pred1)
rmse_m = rmse(y2,pred1)
mape_m
rmse_m
mape_m = mape(y2, pred2)
rmse_m = rmse(y2, pred2)
mape_m
rmse_m
mse1=sum((actual -pred1)^2)/nrow(data2)
mse2=sum((actual -pred2)^2)/nrow(data2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))
MAPE1=mean(((abs(actual-pred1))/actual)*100)
MAPE2=mean(((abs(actual-pred2))/actual)*100)
#Elastic Net Regression
# Set training control
set.seed(100)

train_control = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             search = "random",
                             verboseIter = TRUE)
View(data1)
elastic_net_model = train(price ~ .,
                          data = data1,
                          method = "glmnet",
                          trControl = train_control)


# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
#For the test set
#elastic_net_model1 = train(log_delay ~ .,data = data2,method = "glmnet",trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))
MAPE=mean(((abs(actual-pred))/actual)*100)















#Random Forest
library(randomForest)
set.seed(100)
indexes=sample(1:nrow(data1),0.1*nrow(data1))
data_new=data1[indexes,]
#Considering a sample of 5000 from the training data
rf_model = randomForest(Min.Delay ~ ., data = data1,importance=TRUE)

rf_model = randomForest(Min.Delay ~ ., data = data1,importance=TRUE,proximity=TRUE,nodesize=100,maxnodes=4)
# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance Plot ")
#Predictions for the training set
y_hat=predict(rf_model,data1[,-2])
actual=y1
pred=c(y_hat)
mse=sum((actual -pred)^2)/nrow(data1)

#Predictions for the test set
y_hat=predict(rf_model,data2[,-2])
actual=y2
pred=c(y_hat)
mse=sum((actual -pred)^2)/nrow(data2)

# Define the tuning grid
tune_grid = expand.grid(
  mtry =c(15,20,25,30),
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  price~ .,
  data = data1,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m

rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set

y_hat_enet = predict(rf_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))               

# get variable importance
# Print the variable importance measures
var_imp=rf_model$finalModel$variable.importance
# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", color = "blue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradientn(colors = c("lightblue", "darkblue"), 
                       guide = "none", aesthetics = "fill") +
  coord_flip() +
  scale_y_log10()



df=var_imp[order(-var_imp$Importance),]
View(df)
df[c(1:33),]

nrow(df)
y=which(data$kms_driven<10)
data=data %>%  filter(!row_number() %in% as.numeric((y)))
trainset_imp=subset(data1,select=-c(bike_name,city,brand))
testset_new=subset(testset,select=-c(bike_name,city,brand))

# Sort variable importances
var_imp <- var_imp[order(var_imp$importance), ]

# Calculate number of variables to be removed

vars_to_remove = df$Variables[33:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)

# Define the tuning grid for random forest hyperparameters
tune_grid = expand.grid(
  mtry =32,
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)

rf_model = train(
  price~ .,
  data = data1_imp,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)

# Make predictions

#For the training set
y_hat_enet = predict(rf_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m

rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Calculate number of variables to be removed

vars_to_remove = df$Variables[33:46]

# Remove least important variables from data1

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)

data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

y_hat_enet = predict(rf_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m










# Perform 5-fold cross-validation
set.seed(100)
rf_model = train(Min.Delay ~ .,
                 data = data_new,
                 method = "rf",
                 
                 trControl = trainControl(method = "cv",
                                          number = 5,
                                          
                                          verboseIter = TRUE),
                 
                 
                 tuneGrid = expand.grid(mtry=seq(3,57,by=3) )
)


plot(rf_model)

#For the training set
y_hat_enet = predict(rf_model, data1[,-2])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set

y_hat_enet = predict(rf_model, data2[,-2])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))               

# get variable importance
var_imp =varImp(rf_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))


























#Xgboost
library(xgboost)
xgb_model = xgboost(data = model_matrix,
                    label = y1,
                    nrounds = 1000,
                    objective = "reg:squarederror",
                    max.depth = 6,
                    eta = 0.1,
                    gamma = 0.1,
                    colsample_bytree = 0.8,
                    subsample = 0.8)
###################################################################################

# convert training data to xgb.DMatrix format
set.seed(100)
dtrain = xgb.DMatrix(data = model_matrix, label = y1)
# set cross-validation parameters
cv_params = list(
  nfold = 5,
  folds = createFolds(data1$Min.Delay, k = 10, list = TRUE, returnTrain = FALSE)
)

# set XGBoost parameters
xgb_params = list(
  objective = "reg:squarederror",
  max_depth = seq(3,10,by=1),
  eta = 0.1,
  gamma = 0.1,
  colsample_bytree = seq(0.5,0.8,by=0.1),
  subsample = seq(0.5,0.8,by=0.1)
)

# run cross-validation to tune the number of rounds
cv_results = xgb.cv(params = xgb_params,
                    data = dtrain,
                    
                    nrounds = 1000,
                    early_stopping_rounds = 10,
                    maximize = FALSE,
                    verbose = TRUE,
                    callback(cb.cv.predict(save_models = TRUE)),
                    folds = cv_params$folds,
                    metrics = "rmse",
                    prediction = TRUE,
                    missing = NA)



# train the final XGBoost model using the best hyperparameters
xgb_params_best = list(
  objective = "reg:squarederror",
  max_depth = best_max_depth,
  eta = best_learning_rate,
  gamma = best_gamma,
  colsample_bytree = best_colsample_bytree,
  subsample = best_subsample
)

xgb_model =xgb.train(params = xgb_params_best, 
                     data = dtrain, 
                     nrounds =94, 
                     verbose = 1)
# Use the trained XGBoost model to make predictions on the test data
train_predictions = predict(xgb_model, model_matrix)

# Calculate the RMSE on the test data
train_rmse = sqrt(mean((train_predictions - y1)^2))
# Use the trained XGBoost model to make predictions on the test data
test_predictions = predict(xgb_model, model_matrix1)

# Calculate the RMSE on the test data
test_rmse = sqrt(mean((test_predictions - y2)^2))



# convert training data to xgb.DMatrix format
set.seed(100)
dtrain = xgb.DMatrix(data = model_matrix, label = y1)
# set cross-validation parameters
cv_params = list(
  nfold = 5,
  folds = createFolds(data1$Min.Delay, k = 10, list = TRUE, returnTrain = FALSE)
)

# set XGBoost parameters
xgb_params = list(
  objective = "reg:squarederror",
  max_depth = seq(3,10,by=1),
  eta = 0.1,
  gamma = 0.1,
  colsample_bytree = seq(0.5,0.8,by=0.1),
  subsample = seq(0.5,0.8,by=0.1)
)

# run cross-validation to tune the number of rounds
cv_results = xgb.cv(params = xgb_params,
                    data = dtrain,
                    
                    nrounds = 1000,
                    early_stopping_rounds = 10,
                    maximize = FALSE,
                    verbose = TRUE,
                    callback(cb.cv.predict(save_models = TRUE)),
                    folds = cv_params$folds,
                    metrics = "rmse",
                    prediction = TRUE,
                    missing = NA)
##############################################################################################
set.seed(100)
xgb_model = train(price ~ .,
                  data = data1,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = c(1,3,5)
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set

y_hat_enet = predict(xgb_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2,pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))


# get variable importance
var_imp =varImp(xgb_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))





df=as.data.frame(var_imp$importance)
nrow(df)
Variables=rownames(df)
df=cbind(df,Variables)

# Calculate number of variables to be removed

vars_to_remove = df$Variables[21:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)


set.seed(100)
xgb_model = train(price ~ .,
                  data = data1_imp,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = c(1,3,5)
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Remove least important variables from data2

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)
str(data2_imp)
data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data2_imp)
y_hat_enet = predict(xgb_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2,pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))




##################################################################################################
nrow(trainset_new)
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset_new$price,main="Price distribution",col="#9494b8",xlab="Price")
boxplot.stats(trainset_new$price)$stats
x=which(trainset_new$price %in% boxplot.stats(trainset_new$price)$out)
length(x)

options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(log(trainset_new$price),main="Log Price distribution",col="#9494b8",xlab="Log price")
boxplot.stats(log(trainset_new$price))$stats
x=which(log(trainset_new$price) %in% boxplot.stats(log(trainset_new$price))$out)
length(x)
# Create a ggplot object
ggplot(trainset_new, aes(x = price)) +
  
  # Add histogram layer
  geom_histogram( aes(y = ..density..), fill = "#9494b8", color = "black") +
  
  # Add density curve layer
  geom_density(color = "red", linetype = "solid", size = 1) +
  
  # Add x-axis label
  xlab("Price ") +
  
  # Add y-axis label
  ylab("Density") +
  
  # Set theme
  theme_minimal()


# Create a ggplot object
ggplot(trainset_new, aes(x = log(price))) +
  
  # Add histogram layer
  geom_histogram( aes(y = ..density..), fill = "#9494b8", color = "black") +
  
  # Add density curve layer
  geom_density(color = "red", linetype = "solid", size = 1) +
  
  # Add x-axis label
  xlab("Log price ") +
  
  # Add y-axis label
  ylab("Density") +
  
  # Set theme
  theme_minimal()



ggplot(data=trainset_new,aes(x=age,y=log(price)))+
  geom_point(color = "brown")+
  
  theme_minimal()+
  
  labs(x="Age",y="Log Bike Price(In Indian Rupees)")

ggplot(data=trainset_new,aes(x=kms_driven,y=log(price)))+
  geom_point(color = "brown")+
  
  theme_minimal()+
  
  labs(x="Kilometers driven",y="Log Bike Price(In Indian Rupees)")


ggplot(data=trainset_new,aes(x=power,y=log(price)))+
  geom_point(color = "black")+
  
  theme_minimal()+
  
  labs(x="Power",y="Log Bike Price(In Indian Rupees)")

tg= trainset_new %>%
  group_by(power) %>%
  summarize(mean_price = mean(log(price)))

ggplot(data = trainset_new, aes(x = power, y = log(price))) +
  geom_point() +
  geom_smooth(data = tg, aes(x = power, y = mean_price), method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "power cc", y = "Log Bike Price (In Indian Rupees)")

#Mean of Delay time
mean(trainset_new$Min.Delay)

#Skewness and kurtosis
skewness(trainset_new$price)#>1 positively skewed
kurtosis(trainset_new$price)
skewness(log(trainset_new$price))#>1 positively skewed
kurtosis(log(trainset_new$price))


log_price=c(log(trainset_new$price))
trainset_new=cbind(log_price,trainset_new)
#trainset=trainset[,-2]
log_price=c(log(testset_new$price))
testset_new=cbind(log_price,testset_new)







#############################################################################################
#Creating the model matrix 
View(trainset_new)
View(testset_new)
data1=trainset_new[,-2]
data2=testset_new[,-2]



data1[,c(2,4,5)]=scale(data1[,c(2,4,5)], center = TRUE, scale = TRUE)
data1[,2]=as.numeric(data1[,2])
data1[,4]=as.numeric(data1[,4])
data1[,5]=as.numeric(data1[,5])

View(data1)


# Create model matrix with categorical variables as dummies and numerical variable as is
#model_matrix = model.matrix(formula, data = data1)
model_matrix = model.matrix(log_price~., data = data1)[,-1]
#model_matrix2 = model.matrix(log_delay~., data = data1)[,-1]

#model_matrix = model.matrix(Min.Delay~., data = df)

# Separate the response variable from the predictors
y1 = data1$log_price
#y1 = df$Min.Delay
is.matrix(model_matrix)
dim(model_matrix)
# Create a sequence of lambda values to test
lambda_seq= 10^seq(-3, 10, length.out = 100)
library(glmnet)
# Use cv.glmnet to perform cross-validation and select the optimal lambda value
#cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq)
set.seed(100)
cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq,nfolds=10,standardize=FALSE)

# View the optimal lambda value
best_lambda1=cv_fit$lambda.min
best_lambda2=cv_fit$lambda.1se
# Plot the cross-validation results
dev.off()
plot(cv_fit)

# Fit final model, get its sum of squared residuals and multiple R-squared
#Predictors are already scaled ---- > No standardizing here
model_cv1 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix)
y_hat_ridge2 = predict(model_cv2, model_matrix)


actual=y1
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
d_actual=trainset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))

rmse_m = rmse(d_actual, d_pred1)
rmse_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data1))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data1))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data1))
mape1=(sum(abs(d_actual-d_pred1)/d_actual))/nrow(data1)*100
mape2=(sum(abs(d_actual-d_pred2)/d_actual))/nrow(data1)*100

coef(model_cv1 ,s=best_lambda1)
coef(model_cv2 ,s=best_lambda2)
max(d_actual-d_pred1)
max(d_actual)
max(d_pred1)

#For the test set


data2[,c(2,4,5)]=scale(data2[,c(2,4,5)], center = TRUE, scale = TRUE)
data2[,2]=as.numeric(data2[,2])
data2[,4]=as.numeric(data2[,4])
data2[,5]=as.numeric(data2[,5])

View(data2)
nrow(data2)
#model_matrix1 = model.matrix(log_delay~., data = data2)
model_matrix1 = model.matrix(log_price~., data = data2)[,-1]

# Separate the response variable from the predictors
#y1 = data1$Min.Delay
y2 = data2$log_price
model_cv1= glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix1)
y_hat_ridge2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))
d_actual=testset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
d_actual=testset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))

rmse_m = rmse(d_actual, d_pred1)
rmse_m
mape_m = mape(d_actual, d_pred1)
mape_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
mape_m = mape(d_actual, d_pred2)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data2))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data2))
mape1=(sum(abs(d_actual-d_pred1)/d_actual))/nrow(data2)*100
mape2=(sum(abs(d_actual-d_pred2)/d_actual))/nrow(data2)*100

class(d_actual)==class(d_pred1)
max(d_actual)
max(d_pred1)
#Lasso Regression
# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression

#lasso_cv = cv.glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq, nfolds = 10)
set.seed(100)
lasso_cv = cv.glmnet(x = model_matrix, y=y1, alpha = 1, lambda = lambda_seq, nfolds = 10,standardize=FALSE)
# Plot cross-validation results
dev.off()
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv1 = lasso_cv$lambda.min
lambda_cv2 = lasso_cv$lambda.1se

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv)
model_cv1 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix)
y_hat_lasso2 = predict(model_cv2, model_matrix)
actual=y1
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data1))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
rmse_m = rmse(d_actual, d_pred1)
rmse_m
mape_m = mape(d_actual, d_pred1)
mape_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
mape_m = mape(d_actual, d_pred2)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data1))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data1))

#rsq_lasso_cv = cor(y, y_hat_cv)^2
coef(model_cv1 ,s=lambda_cv1)
coef(model_cv2 ,s=lambda_cv2)
# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq,standardize = FALSE)
plot(res, xvar = "lambda",label=TRUE,lw=2)
#legend("bottomright", lwd = 1, col = 1:6, legend = colnames(model_matrix), cex = .7)


#For the test set

model_cv1 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix1)
y_hat_lasso2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))
d_actual=testset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
rmse_m = rmse(d_actual, d_pred1)
rmse_m
mape_m = mape(d_actual, d_pred1)
mape_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
mape_m = mape(d_actual, d_pred2)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data2))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data2))

#Elastic Net Regression
# Set training control
set.seed(100)

train_control = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             search = "random",
                             verboseIter = TRUE)
View(data1)
#str(data1)
# Train the model
#elastic_net_model = train(log_delay ~ .,data = data1,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 25,trControl = train_control)
elastic_net_model = train(log_price ~ .,
                          data = data1,
                          method = "glmnet",
                          trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
rmse_m = rmse(d_actual, d_pred)
rmse_m
#For the test set
#elastic_net_model1 = train(log_delay ~ .,data = data2,method = "glmnet",trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))
d_actual=testset_new$price
d_pred=as.integer(exp(pred))
rmse_m = rmse(d_actual, d_pred)
rmse_m
mape_m = mape(d_actual, d_pred)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

#Random Forest

library(randomForest)
set.seed(100)
indexes=sample(1:nrow(data1),0.1*nrow(data1))
data_new=data1[indexes,]
# Perform 5-fold cross-validation
set.seed(100)
rf_model = train(log_price~ .,
                 data = data1,
                 method = "rf",
                 
                 trControl = trainControl(method = "cv",
                                          number = 5,
                                          
                                          verboseIter = TRUE),
                 
                 
                 tuneGrid = expand.grid(mtry=seq(15,30,by=5) )
)


plot(rf_model)

#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(rf_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2)  )             

d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
var_imp =varImp(rf_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))




# Define the tuning grid
tune_grid = expand.grid(
  mtry =c(15,20,25,30),
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  log_price ~ .,
  data = data1,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(rf_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))               
d_actual=testset$Min.Delay
d_pred=as.integer(exp(pred))

d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
# Print the variable importance measures
var_imp=rf_model$finalModel$variable.importance
# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()


df=var_imp[order(-var_imp$Importance),]
View(df)
df[c(1:11),]

nrow(df)


# Calculate number of variables to be removed

vars_to_remove = df$Variables[12:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("log_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)

# Define the tuning grid for random forest hyperparameters
tune_grid = expand.grid(
  mtry =11,
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)

rf_model = train(
  log_price~ .,
  data = data1_imp,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)

# Make predictions

#For the training set
y_hat_enet = predict(rf_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Calculate number of variables to be removed



# Remove least important variables from data1

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)

data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("log_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

y_hat_enet = predict(rf_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m




#Xgboost
set.seed(100)
xgb_model = train(log_price ~ .,
                  data = data1,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(xgb_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))
d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
var_imp =varImp(xgb_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))




df=as.data.frame(var_imp$importance)
nrow(df)
Variables=rownames(df)
df=cbind(df,Variables)

# Calculate number of variables to be removed

vars_to_remove = df$Variables[11:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("log_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)


set.seed(100)
xgb_model = train(log_price ~ .,
                  data = data1_imp,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y1,pred)
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Remove least important variables from data2

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)
str(data2_imp)
data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("log_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))
data2_imp$log_price=as.numeric(data2_imp$log_price)
# Check the data types
str(data2_imp)
y_hat_enet = predict(xgb_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2,pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))


#########################################################################################################
#Skewness and kurtosis
skewness(trainset_new$price)#>1 positively skewed
kurtosis(trainset_new$price)
skewness(sqrt(trainset_new$price))#>1 positively skewed
kurtosis(sqrt(trainset_new$price))

# Create a ggplot object
ggplot(trainset_new, aes(x = sqrt(price))) +
  
  # Add histogram layer
  geom_histogram( aes(y = ..density..), fill = "#9494b8", color = "black") +
  
  # Add density curve layer
  geom_density(color = "red", linetype = "solid", size = 1) +
  
  # Add x-axis label
  xlab("Root price ") +
  
  # Add y-axis label
  ylab("Density") +
  
  # Set theme
  theme_minimal()

ggplot(data=trainset_new,aes(x=age,y=sqrt(price)))+
  geom_point(color = "brown")+
  
  theme_minimal()+
  
  labs(x="Age",y="Root Bike Price(In Indian Rupees)")

ggplot(data=trainset_new,aes(x=kms_driven,y=sqrt(price)))+
  geom_point(color = "brown")+
  
  theme_minimal()+
  
  labs(x="Kilometers driven",y="Root Bike Price(In Indian Rupees)")


ggplot(data=trainset_new,aes(x=power,y=sqrt(price)))+
  geom_point(color = "black")+
  
  theme_minimal()+
  
  labs(x="Power",y="Root Bike Price(In Indian Rupees)")

tg= trainset_new %>%
  group_by(power) %>%
  summarize(mean_price = mean(sqrt(price)))

ggplot(data = trainset_new, aes(x = power, y = sqrt(price))) +
  geom_point() +
  geom_smooth(data = tg, aes(x = power, y = mean_price), method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "power cc", y = "Root Bike Price (In Indian Rupees)")

trainset_new=trainset_new[,-1]
testset_new=testset_new[,-1]

sq_price=c(sqrt(trainset_new$price))
trainset_new=cbind(sq_price,trainset_new)
#trainset=trainset[,-2]
sq_price=c(sqrt(testset_new$price))
testset_new=cbind(sq_price,testset_new)


################################Square root transformation##############################################################
#Creating the model matrix 
View(trainset_new)
View(testset_new)
data1=trainset_new[,-2]
data1[,c(2,4,5)]=scale(data1[,c(2,4,5)], center = TRUE, scale = TRUE)
data1[,2]=as.numeric(data1[,2])
data1[,4]=as.numeric(data1[,4])
data1[,5]=as.numeric(data1[,5])

View(data1)


# Create model matrix with categorical variables as dummies and numerical variable as is
#model_matrix = model.matrix(formula, data = data1)
model_matrix = model.matrix(sq_price~., data = data1)[,-1]
#model_matrix2 = model.matrix(log_delay~., data = data1)[,-1]

#model_matrix = model.matrix(Min.Delay~., data = df)

# Separate the response variable from the predictors
y1 = data1$sq_price
#y1 = df$Min.Delay
is.matrix(model_matrix)
dim(model_matrix)
# Create a sequence of lambda values to test
lambda_seq= 10^seq(-3, 10, length.out = 100)
library(glmnet)
# Use cv.glmnet to perform cross-validation and select the optimal lambda value
#cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq)
set.seed(100)
cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq,nfolds=10,standardize=FALSE)

# View the optimal lambda value
best_lambda1=cv_fit$lambda.min
best_lambda2=cv_fit$lambda.1se
# Plot the cross-validation results
dev.off()
plot(cv_fit)

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 0, lambda = best_lambda)
model_cv1 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix)
y_hat_ridge2 = predict(model_cv2, model_matrix)


actual=y1
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
d_actual=trainset_new$price
d_pred1=as.integer(pred1^2)
d_pred2=as.integer(pred2^2)
library(Metrics)
rmse_m = rmse(d_actual, d_pred1)
rmse_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data1))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data1))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data1))
mape1=(sum(abs(d_actual-d_pred1)/d_actual))/nrow(data1)*100
mape2=(sum(abs(d_actual-d_pred2)/d_actual))/nrow(data1)*100

coef(model_cv1 ,s=best_lambda1)
coef(model_cv2 ,s=best_lambda2)
max(d_actual-d_pred1)
max(d_actual)
max(d_pred1)

#For the test set

data2=testset_new[,-2]
data2[,c(2,4,5)]=scale(data2[,c(2,4,5)], center = TRUE, scale = TRUE)
data2[,2]=as.numeric(data2[,2])
data2[,4]=as.numeric(data2[,4])
data2[,5]=as.numeric(data2[,5])

View(data2)
nrow(data2)
#model_matrix1 = model.matrix(log_delay~., data = data2)
model_matrix1 = model.matrix(sq_price~., data = data2)[,-1]

# Separate the response variable from the predictors
#y1 = data1$Min.Delay
y2 = data2$sq_price
model_cv1= glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda2,standardize = FALSE)

y_hat_ridge1 = predict(model_cv1, model_matrix1)
y_hat_ridge2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))
d_actual=testset_new$price
d_pred1=as.integer(exp(pred1))
d_pred2=as.integer(exp(pred2))
d_actual=testset_new$price
d_pred1=as.integer(pred1^2)
d_pred2=as.integer(pred2^2)

rmse_m = rmse(d_actual, d_pred1)
rmse_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
mape_m = mape(d_actual, d_pred1)
mape_m
mape_m = mape(d_actual, d_pred2)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data2))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data2))
mape1=(sum(abs(d_actual-d_pred1)/d_actual))/nrow(data2)*100
mape2=(sum(abs(d_actual-d_pred2)/d_actual))/nrow(data2)*100

class(d_actual)==class(d_pred1)
max(d_actual)
max(d_pred1)
#Lasso Regression
# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression

#lasso_cv = cv.glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq, nfolds = 10)
set.seed(100)
lasso_cv = cv.glmnet(x = model_matrix, y=y1, alpha = 1, lambda = lambda_seq, nfolds = 10,standardize=FALSE)
# Plot cross-validation results
dev.off()
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv1 = lasso_cv$lambda.min
lambda_cv2 = lasso_cv$lambda.1se

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv)
model_cv1 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix)
y_hat_lasso2 = predict(model_cv2, model_matrix)
actual=y1
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)

d_actual=trainset_new$price
d_pred1=as.integer(pred1^2)
d_pred2=as.integer(pred2^2)
rmse_m = rmse(d_actual, d_pred1)
rmse_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m

d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data1))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data1))

#rsq_lasso_cv = cor(y, y_hat_cv)^2
coef1=coef(model_cv1 ,s=lambda_cv1)
coef2=coef(model_cv2 ,s=lambda_cv2)

# extract coefficients as a vector
coef_vec = as.vector(coef2)


# Convert coefficients to a data frame with variable names
df_coef = data.frame(var = row.names(coef(model_cv2, s=lambda_cv2)),
                     coef = coef(model_cv2, s=lambda_cv2)[,1])

ggplot(data = df_coef, aes(x =var,  y = coef)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "", x = "Variables", y = "Coeffiecients") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq,standardize = FALSE)
plot(res, xvar = "lambda",label=TRUE,lw=2)
#legend("bottomright", lwd = 1, col = 1:6, legend = colnames(model_matrix), cex = .7)


#For the test set

model_cv1 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv1,standardize = FALSE)
model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso1 = predict(model_cv1, model_matrix1)
y_hat_lasso2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_lasso1)
pred2=c(y_hat_lasso2)
rmse1=sqrt(sum((actual -pred1)^2)/nrow(data2))
rmse2=sqrt(sum((actual -pred2)^2)/nrow(data2))
d_actual=testset_new$price
d_pred1=as.integer(pred1^2)
d_pred2=as.integer(pred2^2)

rmse_m = rmse(d_actual, d_pred1)
rmse_m
rmse_m = rmse(d_actual, d_pred2)
rmse_m
mape_m = mape(d_actual, d_pred1)
mape_m
mape_m = mape(d_actual, d_pred2)
mape_m
d_rmse1=sqrt(sum((d_actual -d_pred1)^2)/nrow(data2))
d_rmse2=sqrt(sum((d_actual -d_pred2)^2)/nrow(data2))

#Elastic Net Regression
# Set training control
set.seed(100)

train_control = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             search = "random",
                             verboseIter = TRUE)
View(data1)
#str(data1)
# Train the model
#elastic_net_model = train(log_delay ~ .,data = data1,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 25,trControl = train_control)
elastic_net_model = train(sq_price ~ .,
                          data = data1,
                          method = "glmnet",
                          trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(pred^2)

rmse_m = rmse(d_actual, d_pred)
rmse_m

d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set
#elastic_net_model1 = train(log_delay ~ .,data = data2,method = "glmnet",trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))
d_actual=testset_new$price
d_pred=as.integer(pred^2)

rmse_m = rmse(d_actual, d_pred)
rmse_m
mape_m = mape(d_actual, d_pred)
mape_m
#Random Forest

library(randomForest)
set.seed(100)
indexes=sample(1:nrow(data1),0.1*nrow(data1))
data_new=data1[indexes,]
# Perform 5-fold cross-validation
set.seed(100)
rf_model = train(log_price~ .,
                 data = data1,
                 method = "rf",
                 
                 trControl = trainControl(method = "cv",
                                          number = 5,
                                          
                                          verboseIter = TRUE),
                 
                 
                 tuneGrid = expand.grid(mtry=seq(15,30,by=5) )
)


plot(rf_model)

#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(rf_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2)  )             

d_actual=testset_new$price
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
var_imp =varImp(rf_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))




# Define the tuning grid
tune_grid = expand.grid(
  mtry =c(15,20,25,30),
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  sq_price ~ .,
  data = data1,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$price
d_pred=as.integer(pred^2)
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(exp(pred))
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(rf_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))               
d_actual=testset$Min.Delay
d_pred=as.integer(exp(pred))

d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
# Print the variable importance measures
var_imp=rf_model$finalModel$variable.importance
# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()


df=var_imp[order(-var_imp$Importance),]
View(df)
df[c(1:20),]

nrow(df)


# Calculate number of variables to be removed

vars_to_remove = df$Variables[21:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("sq_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)

# Define the tuning grid for random forest hyperparameters
tune_grid = expand.grid(
  mtry =20,
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)

rf_model = train(
  sq_price~ .,
  data = data1_imp,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)

# Make predictions

#For the training set
y_hat_enet = predict(rf_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
d_actual=trainset_new$price
d_pred=as.integer(pred^2)
mae_m = mae(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mae_m
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Calculate number of variables to be removed



# Remove least important variables from data1

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)

data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("sq_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

y_hat_enet = predict(rf_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m




#Xgboost
set.seed(100)
xgb_model = train(sq_price ~ .,
                  data = data1,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))
d_actual=trainset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data1))
#For the test set

y_hat_enet = predict(xgb_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))
d_actual=testset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
d_rmse1=sqrt(sum((d_actual -d_pred)^2)/nrow(data2))

# get variable importance
var_imp =varImp(xgb_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))





df=as.data.frame(var_imp$importance)
nrow(df)
Variables=rownames(df)
df=cbind(df,Variables)

# Calculate number of variables to be removed

vars_to_remove = df$Variables[13:46]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("sq_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)


set.seed(100)
xgb_model = train(sq_price ~ .,
                  data = data1_imp,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, data1_imp[,-1])
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y1,pred)
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Remove least important variables from data2

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
base_var=c("brand_NewBajaj","ownerFirst Owner","stateAndhra Pradesh" )
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)
str(data2_imp)
data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("sq_price", "kms_driven", "age","power")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data2_imp)
y_hat_enet = predict(xgb_model, data2_imp[,-1])

actual=y2
pred=c(y_hat_enet )
d_actual=testset_new$price
d_pred=as.integer(pred^2)
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2,pred)
mape_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data2))













































































