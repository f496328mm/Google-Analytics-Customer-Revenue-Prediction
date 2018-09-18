
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
# for (i in c(1:length(by_list))) {
#   print(i)
#   data = merge(data , feature[[i]], all.x = TRUE, by = by_list[[i]] )
# }

name = sapply( c(1:length(feature)),function(i){
  return( list( colnames( feature[[i]] ) ) )
} )
name = do.call(c,name)
feature_name = name[ !( name %in% by_list ) ]
# fwrite(data,'merge_data.csv',row.names = FALSE)
# data = fread('merge_data.csv')
#==========================================================
#n_distinct(train$newVisits)
#data$hits = log1p(data$hits)
#data$visitNumber = log1p(data$visitNumber)
#==========================================================
data2 = data

# mean_hits_day = train[,.(mean_hits_day = mean(hits)),by = 'day']
# sum_hits_day = train[,.(sum_hits_day = sum(hits)),by = 'day']
# max_hits_day = train[,.(max_hits_day = max(hits)),by = 'day']
# min_hits_day = train[,.(min_hits_day = min(hits)),by = 'day']
# var_hits_day = train[,.(var_hits_day = var(hits)),by = 'day']
# 
# 
# data2 = merge(data2 , mean_hits_day, all.x = TRUE, by = 'day' )
# data2 = merge(data2 , sum_hits_day, all.x = TRUE, by = 'day' )
# data2 = merge(data2 , max_hits_day, all.x = TRUE, by = 'day' )
# data2 = merge(data2 , min_hits_day, all.x = TRUE, by = 'day' )
# data2 = merge(data2 , var_hits_day, all.x = TRUE, by = 'day' )
# 
# feature_name2 = c('mean_hits_day','sum_hits_day',
#                   'max_hits_day','min_hits_day',
#                   'var_hits_day')
# 
# feature_name = c(feature_name,feature_name2)

#==========================================================

mean_pageviews_networkDomain = train[,.(mean_pageviews_networkDomain = mean(pageviews)),by = 'networkDomain']
sum_pageviews_networkDomain = train[,.(sum_pageviews_networkDomain = sum(pageviews)),by = 'networkDomain']
max_pageviews_networkDomain = train[,.(max_pageviews_networkDomain = max(pageviews)),by = 'networkDomain']
min_pageviews_networkDomain = train[,.(min_pageviews_networkDomain = min(pageviews)),by = 'networkDomain']
var_pageviews_networkDomain = train[,.(var_pageviews_networkDomain = var(pageviews)),by = 'networkDomain']


data2 = merge(data2 , mean_pageviews_networkDomain, all.x = TRUE, by = 'networkDomain' )
data2 = merge(data2 , sum_pageviews_networkDomain, all.x = TRUE, by = 'networkDomain' )
data2 = merge(data2 , max_pageviews_networkDomain, all.x = TRUE, by = 'networkDomain' )
data2 = merge(data2 , min_pageviews_networkDomain, all.x = TRUE, by = 'networkDomain' )
data2 = merge(data2 , var_pageviews_networkDomain, all.x = TRUE, by = 'networkDomain' )

feature_name3 = c('mean_pageviews_networkDomain','sum_pageviews_networkDomain',
                  'max_pageviews_networkDomain','min_pageviews_networkDomain',
                  'var_pageviews_networkDomain')

feature_name = c(feature_name,feature_name3)



#==========================================================
#==========================================================

test = function(by_list2,data2,feature_name){
  
  print('feature engineer')
  feature2 = work_feature(train,by_list2)
  
  for (i in c(1:length(by_list2))) {
    print(i)
    data2 = merge(data2 , feature2[[i]], all.x = TRUE, by = by_list2[[i]] )
  }
  
  name2 = sapply( c(1:length(feature2)),function(i){
    return( list( colnames( feature2[[i]] ) ) )
  } )
  name2 = do.call(c,name2)
  feature_name4 = name2[ !( name2 %in% by_list ) ] 
  feature_name = c(feature_name,feature_name4)  
  
  return( list(data2,feature_name) )
}


by_list2 = list(c('browser','deviceCategory'),
                c('browser','operatingSystem'))

tem = test(by_list2,data2,feature_name)
data2 = tem[[1]]
feature_name = tem[[2]]
print(feature_name)




#==========================================================
print('train model')
#data$newVisits[is.na(data$newVisits)] = 0


#n_distinct(train$visits)



train2 = data2 %>% 
  filter(log_Revenue != -1) %>% 
  data.table

sel_col = c('visitStartTime',
            #'visits',
            'hits',
            'pageviews'
            ,'newVisits'
            ,'isMobile'
            ,'visitNumber'
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

# [321]	train-rmse:1.483093+0.006892	test-rmse:1.581753+0.006348 
# 0.09804333
# [319]	train-rmse:1.477606+0.006100	test-rmse:1.575649+0.006353

# [248]	train-rmse:1.512077+0.004390	test-rmse:1.585583+0.009832
# [1] 0.073506

# [229]	train-rmse:1.516353+0.005211	test-rmse:1.585985+0.015172

# [434]	train-rmse:1.528111+0.006927	test-rmse:1.582860+0.014678
# [1] 0.054749
# 1.7000
# [496]	train-rmse:1.521538+0.006741	test-rmse:1.582090+0.014915
# [1] 0.060552
# alpha = 0
# [197]	train-rmse:1.553005+0.005076	test-rmse:1.590428+0.016481

# [1] 0.03742367








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

test2 = data2 %>% 
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
# 1.7295

#--------------------------------------------------------
# [434]	train-rmse:1.528111+0.006927	test-rmse:1.582860+0.014678
# [1] 0.054749
# 1.7000

# [197]	train-rmse:1.553004+0.005078	test-rmse:1.590425+0.016485
# [1] 0.03742067
# 1.6910


