
# id : fullVisitorId
# target : bounces, transactionRevenue

#install.packages("data.table")

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

train = del_not_available(train)
test = del_not_available(test)

train$log_Revenue = log1p(train$transactionRevenue)
train$campaignCode = NULL
test$transactionRevenue = -1
test$log_Revenue = -1

data = rbind(train,test)
data = data %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date),
         weekdays = weekdays(date)) %>% 
  data.table
# fwrite(data,'json_data.csv',row.names = FALSE)
# data = fread('json_data.csv')
#------------------------------------------
train = data %>% 
  filter(log_Revenue != -1) %>% 
  data.table
by_list = list('channelGrouping','visitNumber','browser',
               'operatingSystem','deviceCategory','continent',
               'subContinent','country','region',
               'metro','city','networkDomain',
               'source','medium','keyword',
               c('country','subContinent'),
               'referralPath','adContent',
               'adwordsClickInfo.page',
               'adwordsClickInfo.slot',
               'adwordsClickInfo.gclId',
               'adwordsClickInfo.adNetworkType',
               'adwordsClickInfo.isVideoAd',
               c('source','medium')
               )
print('feature engineer')
feature = work_feature(train,by_list)

#------------------------------------------
data$log_Revenue[ is.na( data$log_Revenue ) ] = 0
for (i in c(1:length(by_list))) {
  print(i)
  data = merge(data , feature[[i]], all.x = TRUE, by = by_list[[i]] )
}

name = sapply( c(1:length(feature)),function(i){
  return( list( colnames( feature[[i]] ) ) )
} )
name = do.call(c,name)
feature_name = name[ !( name %in% by_list ) ]
# fwrite(data,'merge_data.csv',row.names = FALSE)
# data = fread('merge_data.csv')
#==========================================================
#n_distinct(train$newVisits)
data$hits = log1p(data$hits)
data$visitNumber = log1p(data$visitNumber)
#==========================================================




#==========================================================
print('train model')
#data$newVisits[is.na(data$newVisits)] = 0

train2 = data %>% 
  filter(log_Revenue != -1) %>% 
  data.table

sel_col = c('visitStartTime',
            #'visits',
            'hits',
            'pageviews'
            ,'newVisits'
            ,'isMobile'
            ,'visitNumber'
            #,'browser_category'
            #,'year'
            #,'month'
            #,'day'
            )

#sel_col = c(sel_col,feature_name[c(1,4,6,7)])
sel_col = c(sel_col,feature_name)
print(sel_col)

tem = build_model(train2,sel_col)
xgb_params = tem[[1]]
best_nrounds = tem[[2]]
dtrain = tem[[3]]


# 
# all 0.066919
# [176]	train-rmse:1.558182+0.003229	test-rmse:1.622127+0.008064
# add feature_subC_country 0.07718767
# [242]	train-rmse:1.541380+0.005972	test-rmse:1.616461+0.015567
#xxx add feature_referralPath 0.07533433
#xxx [214]	train-rmse:1.544096+0.003056	test-rmse:1.617112+0.006664
#xxx add feature_adContent 0.08237
#xxx [247]	train-rmse:1.536317+0.000438	test-rmse:1.616799+0.006252
# all 0.07646267
# [240]	train-rmse:1.521512+0.003444	test-rmse:1.596341+0.005252
# 0.07445367
# [227]	train-rmse:1.522990+0.004012	test-rmse:1.595272+0.009819
# del year month day 0.068411
# [217]	train-rmse:1.528167+0.003533	test-rmse:1.594774+0.009011
# add visitNumber 0.08027
# [272]	train-rmse:1.516221+0.003932	test-rmse:1.594383+0.011284
# 0.09687733
# [325]	train-rmse:1.490414+0.005905	test-rmse:1.587291+0.011823
# 0.1320633
# [164]	train-rmse:1.455116+0.001300	test-rmse:1.587180+0.008467
# 0.097164
# [321]	train-rmse:1.483093+0.006892	test-rmse:1.581753+0.006348 
# 0.09804333
# [319]	train-rmse:1.477606+0.006100	test-rmse:1.575649+0.006353






#[155]	train-rmse:1.539619+0.005814	test-rmse:1.600069+0.009433
#[1] 0.06045

#[132]	train-rmse:1.547473+0.003015	test-rmse:1.604630+0.005393
#[1] 0.05715767







# 
# all 0.1749393
#	[50]	train-rmse:1.463718+0.008715	test-rmse:1.638658+0.007033 

set.seed(100)
clf <- xgb.train(params = xgb_params,
                 data = dtrain, 
                 nrounds = best_nrounds,
                 watchlist = list(train = dtrain),
                 eval_metric ='rmse'
                 ,print_every_n = 100
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

result = output[,.(PredictedLogRevenue = sum(PredictedLogRevenue)),
                by = 'fullVisitorId' ]

fwrite(result,'result.csv',row.names = FALSE)
rm(clf)
gc()



# based : 
#'visitStartTime',
#'visits','hits','pageviews','newVisits',
#'isMobile','visitNumber',
#'year','month','day'
# ---------------------------------------------------------
# eta = 0.01, 
# max_depth = 10, 
# colsample_bytree = 0.7,
# subsample = 0.7
# [334]	train-rmse:1.548918+0.004528	test-rmse:1.720135+0.003180
# kaggle : 1.9655

# ---------------------------------------------------------
# all feature 0.234504
# eta = 0.01, 
# max_depth = 10, 
# colsample_bytree = 0.7,
# subsample = 0.7
# [500]	train-rmse:1.380848+0.005224	test-rmse:1.615352+0.007984 
# kaggle : 1.9134

# ---------------------------------------------------------
# [50]	train-rmse:1.686923+0.003442	test-rmse:1.697125+0.009062 
# [999]	train-rmse:1.671658+0.003650	test-rmse:1.690580+0.009109
# 1.9384

# ---------------------------------------------------------
# all 0.07646267
# [240]	train-rmse:1.521512+0.003444	test-rmse:1.596341+0.005252
# kaggle 1.7102
# colsample_bytree = 0.7, subsample = 1, 0.088771
# [255]	train-rmse:1.504264+0.004784	test-rmse:1.590633+0.006836
# kaggle 1.7097









