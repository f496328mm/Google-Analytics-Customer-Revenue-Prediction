
# id : fullVisitorId
# target : bounces, transactionRevenue

install.packages("data.table")
install.packages("xgboost")
install.packages("dplyr")
install.packages("parallel")
install.packages("jsonlite")
install.packages("lubridate")

library(data.table)
library(tidyverse)
library(xgboost)
library(dplyr)
library(parallel)
library(jsonlite)
library(gtools)
library(lubridate)

mc.cores = 32

#setwd('D:\\kaggle')
setwd('/home/linsam/kaggle/input')

train = read_csv('train.csv',n_max = Inf) %>% data.table
test = read_csv('test.csv',n_max = Inf) %>% data.table
#sample_submission = fread('sample_submission.csv')

#colnames( train )
#[1] "channelGrouping"      "date"                 "device"               "fullVisitorId"       
#[5] "geoNetwork"           "sessionId"            "socialEngagementType" "totals"              
#[9] "trafficSource"        "visitId"              "visitNumber"          "visitStartTime"     

train = JsonChange(train)
test = JsonChange(test)
train$log_Revenue = log1p(train$transactionRevenue)
train$campaignCode = NULL
test$transactionRevenue = -1
test$log_Revenue = -1

#data = rbind(train,test)

#------------------------------------------
print('feature engineer')
tem = work_feature(train)

feature = tem[[1]]
by_list = tem[[2]]
#------------------------------------------
data = rbind(train,test)
data$log_Revenue[ is.na( data$log_Revenue ) ] = 0
for (i in c(1:length(by_list))) {
  data = merge(data , feature[i], all.x = TRUE, by = by_list[i] )
}

name = sapply( c(1:length(feature)),function(i){
  return( list( colnames( feature[[i]] ) ) )
} )
name = do.call(c,name)
feature_name = name[ !( name %in% by_list ) ]

data = data %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date)) %>% 
  data.table

#==========================================================
print('train model')

train2 = data %>% 
  filter(log_Revenue != -1) %>% 
  data.table

sel_col = c('visitStartTime',
            'visits','hits','pageviews','newVisits',
            'isMobile','visitNumber',
            'year','month','day')
sel_col = c(sel_col,feature_name)
length(sel_col)
y = train2$log_Revenue

dtrain <- xgb.DMatrix( data = as.matrix(
  subset(train2,select = sel_col )
) ,label = y)
gc()


xgb_params=list( 	
  objective="reg:linear",
  #objective = "binary:logistic",
  booster = "gbtree",
  eta = 0.01, 
  max_depth = 10, 
  colsample_bytree = 0.7,
  subsample = 0.7
  ,tree_method = 'gpu_hist'
)

set.seed(100)
xgb_cv <- xgb.cv(data = dtrain,
                 params = xgb_params,
                 nrounds = 500,
                 maximize = FALSE,
                 prediction = TRUE,
                 nfold = 3,
                 print_every_n = 10,
                 early_stopping_rounds = 10
                 #,nthread=8
                 #,eval_metric = MCC
                 #,eval_metric = "rmse"
)

best_nrounds = xgb_cv$best_iteration

clf <- xgb.train(params = xgb_params,
                 data = dtrain, 
                 nrounds = best_nrounds,
                 watchlist = list(train = dtrain),
                 eval_metric ='rmse'
                 #eval_metric = mcc.evaluation.fun,
                 #feval = mcc.evaluation.fun
)

test2 = data %>% 
  filter(log_Revenue == -1) %>% 
  data.table

dtest <- xgb.DMatrix( data = as.matrix(
  subset(test2,select = sel_col )
) )
pred = predict(clf,dtest)
#test2$fullVisitorId
output = test2 %>% subset(select = c('fullVisitorId'))
output$PredictedLogRevenue = pred

result = output[,.(PredictedLogRevenue = mean(PredictedLogRevenue)),
                by = 'fullVisitorId' ]

fwrite(result,'result.csv',row.names = FALSE)


