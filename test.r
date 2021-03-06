
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
#fwrite(data,'json_data.csv',row.names = FALSE)
# data = fread('json_data.csv')
#------------------------------------------
#data = fread('merge_data.csv')
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

#==========================================================
#n_distinct(train$newVisits)
#data$hits = log1p(data$hits)
#data$visitNumber = log1p(data$visitNumber)
#==========================================================
data2 = data

# mean_hits_day = train[,.(mean_hits_day = mean(hits,na.rm = TRUE)),by = 'day']
# sum_hits_day = train[,.(sum_hits_day = sum(hits,na.rm = TRUE)),by = 'day']
# max_hits_day = train[,.(max_hits_day = max(hits,na.rm = TRUE)),by = 'day']
# min_hits_day = train[,.(min_hits_day = min(hits,na.rm = TRUE)),by = 'day']
# var_hits_day = train[,.(var_hits_day = var(hits,na.rm = TRUE)),by = 'day']
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
#print(feature_name)

#feature_name = c(feature_name,'deviceCategory','operatingSystem')


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
#print(sel_col)

tem = build_model(train2,sel_col,lambda = 120)
xgb_params = tem[[1]]
best_nrounds = tem[[2]]
dtrain = tem[[3]]
v = tem[[4]]

# lambda = 120
# [403]	train-rmse:1.537031+0.004888	test-rmse:1.580607+0.015449
# [1] 0.04357533
# 1.6915
# lambda = 200
# [374]	train-rmse:1.548075+0.004795	test-rmse:1.584406+0.016228
# [1] 0.03633033
# 1.6958
# nfold = 5, early = 5, lambda = 120
# [429]	train-rmse:1.534650+0.002011	test-rmse:1.577384+0.008542
# [1] 0.0427346
# pred<0 = 0
# 1.6840

# lambda = 100
# [388]	train-rmse:1.535366+0.002595	test-rmse:1.577132+0.008634
# [1] 0.041766
# lamda = 50
# [418]	train-rmse:1.523184+0.003717	test-rmse:1.574496+0.007551
# [1] 0.0513118



#sum( train2$log_Revenue == 0 )/nrow(train2)



# alpha = 0
# [197]	train-rmse:1.553004+0.005078	test-rmse:1.590425+0.016485
# [1] 0.03742067
# 1.6910
# alpha = 1
# [310]	train-rmse:1.537928+0.005213	test-rmse:1.585629+0.016280
# [1] 0.04770067
# 1.6946
# cs, ss = 0.8
# [357]	train-rmse:1.525758+0.002900	test-rmse:1.580888+0.017612
# [1] 0.05513
# cs, ss = 0.5
# [361]	train-rmse:1.547201+0.006121	test-rmse:1.593169+0.014507
# [1] 0.04596867
# max_depth = 3
# [614]	train-rmse:1.574561+0.006980	test-rmse:1.601359+0.014820
# [1] 0.02679767

# [383]	train-rmse:1.541689+0.004001	test-rmse:1.584438+0.016526
# [1] 0.042749
# 1.6954

# [440]	train-rmse:1.533990+0.002190	test-rmse:1.577195+0.008308
# [1] 0.0432052







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

# per = sum( train2$log_Revenue == 0 )/nrow(train2)
# filter_rank = as.integer( per*length(pred) )
# filter_value = sort(pred)[filter_rank]


#test2$fullVisitorId
output = test2 %>% subset(select = c('fullVisitorId'))
output$PredictedLogRevenue = pred

result = output[,.(PredictedLogRevenue = sum(PredictedLogRevenue)),
                by = 'fullVisitorId' ]
result$PredictedLogRevenue[ result$PredictedLogRevenue<0 ] = 0
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


