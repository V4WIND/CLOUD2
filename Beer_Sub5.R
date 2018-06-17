#Loading Packages
library(ggplot2)
library(dplyr)
library(stringr)
library(psych)
library(caret)
library(ModelMetrics)
library(xgboost)
library(Ckmeans.1d.dp)


#Importing Data
train = read.csv("Beer Train Data Set.csv", stringsAsFactors= FALSE)
test = read.csv("Beer Test Data Set.csv", stringsAsFactors = FALSE)


#Combined train + test
all = rbind(train,test)
all$id = 1:nrow(all)

#Checks
str(all)
colSums(is.na(train)|train=="")
colSums(is.na(test)|test=="")
colSums(is.na(all)|all=="")
nrow(distinct(all,ABV))
nrow(distinct(all,Brewing.Company))
nrow(distinct(all,Food.Paring))
nrow(distinct(all,Glassware.Used))
nrow(distinct(all,Beer.Name))
nrow(distinct(all,Ratings))
nrow(distinct(all,Style.Name))
nrow(distinct(all,Cellar.Temperature))
nrow(distinct(all,Serving.Temperature))


#Adjusting "ABV" variable
##Imputation using "Beer Name"
ABV_pre = all[!is.na(all$ABV),c("ABV","Beer.Name")]
ABV_pre = ABV_pre %>% group_by(Beer.Name) %>%
  summarise(med_ABV = median(ABV))

ABV_miss = all[is.na(all$ABV),c("id","Beer.Name")]
ABV_miss = ABV_miss %>% left_join(ABV_pre,by="Beer.Name",copy=TRUE) %>%
  select(id, med_ABV)

all = all %>% left_join(ABV_miss, by = "id")
all$ABV = ifelse(is.na(all$ABV),all$med_ABV,all$ABV)
all$med_ABV = NULL

##Imputation using "Brewing Company"
ABV_pre = all[!is.na(all$ABV),c("ABV","Brewing.Company")]
ABV_pre = ABV_pre %>% group_by(Brewing.Company) %>%
  summarise(med_ABV = median(ABV))

ABV_miss = all[is.na(all$ABV),c("id","Brewing.Company")]
ABV_miss = ABV_miss %>% left_join(ABV_pre,by="Brewing.Company",copy=TRUE) %>%
  select(id, med_ABV)

all = all %>% left_join(ABV_miss, by = "id")
all$ABV = ifelse(is.na(all$ABV),all$med_ABV,all$ABV)
all$med_ABV = NULL

##Imputation using "Style.Name"
ABV_pre = all[!is.na(all$ABV),c("ABV","Style.Name")]
ABV_pre = ABV_pre %>% group_by(Style.Name) %>%
  summarise(med_ABV = median(ABV))

ABV_miss = all[is.na(all$ABV),c("id","Style.Name")]
ABV_miss = ABV_miss %>% left_join(ABV_pre,by="Style.Name",copy=TRUE) %>%
  select(id, med_ABV)

all = all %>% left_join(ABV_miss, by = "id")
all$ABV = ifelse(is.na(all$ABV),all$med_ABV,all$ABV)
all$med_ABV = NULL

#Adjusting "Cellar.Temperature" variable
##Imputation using "Beer Name"
CT_pre = all[!is.na(all$Cellar.Temperature) & all$Cellar.Temperature!="",c("Cellar.Temperature","Beer.Name")]
CT_pre = CT_pre %>% group_by(Beer.Name,Cellar.Temperature) %>%
  tally() %>%
  arrange(Beer.Name,desc(n),Cellar.Temperature) %>%
  rename(CT_Adj = Cellar.Temperature)
CT_pre = CT_pre[!duplicated(CT_pre[c("Beer.Name")]),c("Beer.Name","CT_Adj")]


CT_miss = all[is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",c("id","Beer.Name")]
CT_miss = CT_miss %>% left_join(CT_pre,by="Beer.Name",copy=TRUE) %>%
  select(id, CT_Adj)

all = all %>% left_join(CT_miss, by = "id")
all$Cellar.Temperature = ifelse(is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",
                                all$CT_Adj,all$Cellar.Temperature)
all$CT_Adj = NULL

##Imputation using "Brewing Company"
CT_pre = all[!is.na(all$Cellar.Temperature) & all$Cellar.Temperature!="",c("Cellar.Temperature","Brewing.Company")]
CT_pre = CT_pre %>% group_by(Brewing.Company,Cellar.Temperature) %>%
  tally() %>%
  arrange(Brewing.Company,desc(n),Cellar.Temperature) %>%
  rename(CT_Adj = Cellar.Temperature)
CT_pre = CT_pre[!duplicated(CT_pre[c("Brewing.Company")]),c("Brewing.Company","CT_Adj")]


CT_miss = all[is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",c("id","Brewing.Company")]
CT_miss = CT_miss %>% left_join(CT_pre,by="Brewing.Company",copy=TRUE) %>%
  select(id, CT_Adj)

all = all %>% left_join(CT_miss, by = "id")
all$Cellar.Temperature = ifelse(is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",
                                all$CT_Adj,all$Cellar.Temperature)
all$CT_Adj = NULL

##Imputation using "Style.Name"
CT_pre = all[!is.na(all$Cellar.Temperature) & all$Cellar.Temperature!="",c("Cellar.Temperature","Style.Name")]
CT_pre = CT_pre %>% group_by(Style.Name,Cellar.Temperature) %>%
  tally() %>%
  arrange(Style.Name,desc(n),Cellar.Temperature) %>%
  rename(CT_Adj = Cellar.Temperature)
CT_pre = CT_pre[!duplicated(CT_pre[c("Style.Name")]),c("Style.Name","CT_Adj")]


CT_miss = all[is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",c("id","Style.Name")]
CT_miss = CT_miss %>% left_join(CT_pre,by="Style.Name",copy=TRUE) %>%
  select(id, CT_Adj)

all = all %>% left_join(CT_miss, by = "id")
all$Cellar.Temperature = ifelse(is.na(all$Cellar.Temperature) | all$Cellar.Temperature=="",
                                all$CT_Adj,all$Cellar.Temperature)
all$CT_Adj = NULL

all$Cellar.Temperature = as.factor(all$Cellar.Temperature)


#Adjusting "Serving.Temperature" variable
##Imputation using "Beer Name"
ST_pre = all[!is.na(all$Serving.Temperature) & all$Serving.Temperature!="",c("Serving.Temperature","Beer.Name")]
ST_pre = ST_pre %>% group_by(Beer.Name,Serving.Temperature) %>%
  tally() %>%
  arrange(Beer.Name,desc(n),Serving.Temperature) %>%
  rename(ST_Adj = Serving.Temperature)
ST_pre = ST_pre[!duplicated(ST_pre[c("Beer.Name")]),c("Beer.Name","ST_Adj")]


ST_miss = all[is.na(all$Serving.Temperature) | all$Serving.Temperature=="",c("id","Beer.Name")]
ST_miss = ST_miss %>% left_join(ST_pre,by="Beer.Name",copy=TRUE) %>%
  select(id, ST_Adj)

all = all %>% left_join(ST_miss, by = "id")
all$Serving.Temperature = ifelse(is.na(all$Serving.Temperature) | all$Serving.Temperature=="",
                                 all$ST_Adj,all$Serving.Temperature)
all$ST_Adj = NULL

##Imputation using "Brewing Company"
ST_pre = all[!is.na(all$Serving.Temperature) & all$Serving.Temperature!="",c("Serving.Temperature","Brewing.Company")]
ST_pre = ST_pre %>% group_by(Brewing.Company,Serving.Temperature) %>%
  tally() %>%
  arrange(Brewing.Company,desc(n),Serving.Temperature) %>%
  rename(ST_Adj = Serving.Temperature)
ST_pre = ST_pre[!duplicated(ST_pre[c("Brewing.Company")]),c("Brewing.Company","ST_Adj")]


ST_miss = all[is.na(all$Serving.Temperature) | all$Serving.Temperature=="",c("id","Brewing.Company")]
ST_miss = ST_miss %>% left_join(ST_pre,by="Brewing.Company",copy=TRUE) %>%
  select(id, ST_Adj)

all = all %>% left_join(ST_miss, by = "id")
all$Serving.Temperature = ifelse(is.na(all$Serving.Temperature) | all$Serving.Temperature=="",
                                 all$ST_Adj,all$Serving.Temperature)
all$ST_Adj = NULL

##Imputation using "Style.Name"
ST_pre = all[!is.na(all$Serving.Temperature) & all$Serving.Temperature!="",c("Serving.Temperature","Style.Name")]
ST_pre = ST_pre %>% group_by(Style.Name,Serving.Temperature) %>%
  tally() %>%
  arrange(Style.Name,desc(n),Serving.Temperature) %>%
  rename(ST_Adj = Serving.Temperature)
ST_pre = ST_pre[!duplicated(ST_pre[c("Style.Name")]),c("Style.Name","ST_Adj")]


ST_miss = all[is.na(all$Serving.Temperature) | all$Serving.Temperature=="",c("id","Style.Name")]
ST_miss = ST_miss %>% left_join(ST_pre,by="Style.Name",copy=TRUE) %>%
  select(id, ST_Adj)

all = all %>% left_join(ST_miss, by = "id")
all$Serving.Temperature = ifelse(is.na(all$Serving.Temperature) | all$Serving.Temperature=="",
                                 all$ST_Adj,all$Serving.Temperature)
all$ST_Adj = NULL

all$Serving.Temperature = as.factor(all$Serving.Temperature)

#Adjusting "Ratings" variable
all$Ratings = as.numeric(str_replace(all$Ratings,",",""))

#Adjusting "Brewing.Company" variable
##Feature Egnineering
# BC = all[,c("Ratings","Brewing.Company")]
# BC = BC %>% group_by(Brewing.Company) %>%
#   summarise(count = n(),
#             sum_Ratings = sum(Ratings)) %>%
#   arrange(desc(sum_Ratings))
# BC$max_ratings = max(BC$sum_Ratings)
# BC$share = (BC$sum_Ratings*100)/BC$max_ratings
# 
# temp = data.frame(BC_Feature_Eng = 1:200)
# for (i in 1:66){
#   temp = rbind(temp, data.frame(BC_Feature_Eng = 1:200))
# }
# for (i in 13401:nrow(BC)){
#   temp = rbind(temp,data.frame(BC_Feature_Eng = 201))
# }
# temp = temp %>% arrange(BC_Feature_Eng)
# BC = cbind(BC,temp)
# 
# BC = BC[,c("Brewing.Company","BC_Feature_Eng")]
# 
# all = all %>% left_join(BC, by = "Brewing.Company")
# all$Brewing.Company = all$BC_Feature_Eng
# all$BC_Feature_Eng = NULL

all$Brewing.Company = as.factor(all$Brewing.Company)

#Adjusting "Food.Paring" variable
all$Food.Paring = as.factor(all$Food.Paring)

#Adjusting "Style.Name" variable
all$Style.Name = as.factor(all$Style.Name)

#Adjusting "Glassware.Used" variable
all$Glassware.Used = as.factor(all$Glassware.Used)

#Plotting "Score" Variable distribution -> All
# ggplot(data=all[!is.na(all$Score),], aes(x=Score)) +
# geom_histogram(fill="blue")

#Separate Zero & Non-Zero Ratings
all_ZR = all[all$Ratings == 0,]

all_NZR = all[all$Ratings != 0,]
row.names(all_NZR) = NULL


#Plotting "Score" Variable distribution -> Non-Zero Ratings (Left Skewed) 
# ggplot(data=all_NZR[!is.na(all_NZR$Score),], aes(x=Score)) +
# geom_histogram(fill="blue")

#Dealing Skewness of Response variable
skew(all_NZR$Score)
qqnorm(all_NZR$Score)
qqline(all_NZR$Score)

all_NZR$Score <- all_NZR$Score ^3
skew(all_NZR$Score)
qqnorm(all_NZR$Score)
qqline(all_NZR$Score)

#Pre-Processing predictor variables
DFnumeric <- all_NZR[, c('ABV', 'Ratings')]
DFfactors <- all_NZR[, c('Brewing.Company','Food.Paring',
                         'Glassware.Used','Style.Name',
                         'Cellar.Temperature','Serving.Temperature')]

##Numeric VAR (Reducing Skewness)
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

##Numeric VAR (Normalising)
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
DFnorm <- predict(PreNum, DFnumeric)

##One Hot encoding the categorical VAR
DFdummies <- as.data.frame(model.matrix(~., DFfactors))
dim(DFdummies)

##Removing levels with few or no observations in test
ZerocolTest <- which(colSums(DFdummies[(nrow(all_NZR[!is.na(all_NZR$Score),])+1):nrow(all_NZR),])<10)
DFdummies <- DFdummies[,-ZerocolTest]

combined <- cbind(DFnorm, DFdummies)

#Train & Test Data
train_mod <- combined[!is.na(all_NZR$Score),]
test_mod <- combined[is.na(all_NZR$Score),]

#Modelling
###(XGBoost Model)
my_control <-trainControl(method="cv", number=5)
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

###Hyper Parameter Tuning
# xgb_caret <- train(x=train_mod, y=all_NZR$Score[!is.na(all_NZR$Score)],
#                    method='xgbTree', trControl= my_control,
#                    tuneGrid=xgb_grid)
# xgb_caret$bestTune

label_train <- all_NZR$Score[!is.na(all_NZR$Score)]

dtrain <- xgb.DMatrix(data = as.matrix(train_mod), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test_mod))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.1, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=5, #default=1
  subsample=1,
  colsample_bytree=1
)

##K-fold cross validation
xgbcv <- xgb.cv( params = default_param, data = dtrain, 
                 nrounds = 500, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 40, 
                 early_stopping_rounds = 10, maximize = F)

xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = xgbcv$best_iteration)


mat <- xgb.importance (feature_names = colnames(train_mod),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE)

##Prediction
XGB_train_Pred <- predict(xgb_mod, dtrain)
XGB_train_Pred <- (XGB_train_Pred)^(1/3)

actual = (all_NZR[!is.na(all_NZR$Score),"Score"])^(1/3)

data = data.frame(predict = XGB_train_Pred, actual = actual)
RMSE = rmse(actual, XGB_train_Pred)
final_score = 1- (exp(RMSE)/(1+exp(RMSE)))

XGB_test_Pred <- predict(xgb_mod, dtest)
XGB_test_Pred <- (XGB_test_Pred)^(1/3)

#Combining Zero & Non-Zero Prediction
Predict_NZR <- XGB_test_Pred
Predict_NZR_id = all_NZR[is.na(all_NZR$Score),"id"]
Predict_NZR_df = data.frame(Predict = Predict_NZR, id = Predict_NZR_id)

Predict_ZR <- all_ZR[is.na(all_ZR$Score),"Score"]
Predict_ZR_id = all_ZR[is.na(all_ZR$Score),"id"]
Predict_ZR_df = data.frame(Predict = Predict_ZR, id = Predict_ZR_id)

##Zero Ratings => Zero Score 
Predict_ZR_df[is.na(Predict_ZR_df$Predict),"Predict"]= 0

##Combining
Predict_df = rbind(Predict_NZR_df, Predict_ZR_df) %>% arrange(id)

#Export
test$Score=Predict_df$Predict
write.csv(test,file="Beer_Solutions_XGB.csv")