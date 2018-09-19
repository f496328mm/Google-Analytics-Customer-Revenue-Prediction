
del_not_available = function(data){
  
  data$browserSize = NULL
  data$browserVersion = NULL
  data$operatingSystemVersion  = NULL
  data$mobileDeviceBranding = NULL
  data$mobileDeviceModel = NULL
  data$mobileInputSelector = NULL
  data$mobileDeviceInfo = NULL
  data$mobileDeviceMarketingName = NULL
  data$flashVersion = NULL
  data$language = NULL
  data$screenColors = NULL
  data$screenResolution = NULL
  data$socialEngagementType = NULL
  data$adwordsClickInfo.criteriaParameters = NULL
  data$longitude = NULL
  data$networkLocation = NULL
  data$latitude = NULL
  data$cityId = NULL
  
  return(data)
}


factor2numeric = function(train2,by,var_name){
  # var_name = c('country','subContinent')
  # paste('feature_',var_name,sep = '')
  if( length(var_name) > 1 ){
    var_name = var_name %>% paste(collapse = "_")
    var_name = paste('feature_',var_name,sep = '')
  }else{
    var_name = paste('feature_',var_name,sep = '')
  }
    
  value = train2[, .(
    value = mean(log_Revenue)), 
    by = by]
  
  colnames( value ) = c(colnames( value )[1:ncol(value)-1],var_name)
  return(list(value))
}

work_feature = function(train,by_list){

  feature = sapply(by_list, function(text){
    value = factor2numeric(
      train,by = text,var_name = text)
    return(value)
  })

  return(feature)
}


JsonChange = function(data){
  
  print('json change to data table')
  device = data$device %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  
  geoNetwork = data$geoNetwork %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  
  totals = data$totals %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  totals <- totals[, lapply(.SD, as.numeric)]
  
  trafficSource = data$trafficSource %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table

  data$device = NULL
  data$geoNetwork = NULL
  data$totals = NULL
  data$trafficSource = NULL
  
  data = Reduce(function(x,y) cbind(x,y), 
                list(	data,
                      device,
                      geoNetwork,
                      totals,
                      trafficSource
                ))
  data$fullVisitorId = as.character(data$fullVisitorId)
  
  return(data)
}


build_model = function(train2,sel_col,lambda){

  y = train2$log_Revenue
  dtrain <- xgb.DMatrix( data = as.matrix(
    subset(train2,select = sel_col )
  ) ,label = y)
  gc()
  
  xgb_params=list( 	
    objective="reg:linear",
    #objective = "binary:logistic",
    booster = "gbtree",
    #eval_metric = 'rmse',
    eta = 0.1, 
    max_depth = 5,
    colsample_bytree = 0.7,
    subsample = 0.7
    ,tree_method = 'gpu_hist'
    ,seed = 0
    ,lambda = lambda
    ,alpha = 0
  )
  
  set.seed(100)
  xgb_cv <- xgb.cv(data = dtrain,
                   params = xgb_params,
                   nrounds = 1000,
                   maximize = FALSE,
                   prediction = TRUE,
                   nfold = 5,
                   print_every_n = 100
                   ,early_stopping_rounds = 5
                   #,nthread = 8
                   #,eval_metric = MCC
                   #,eval_metric = "rmse"
  )
  
  best_nrounds = xgb_cv$best_iteration
  
  value = xgb_cv$evaluation_log$train_rmse_mean[best_nrounds] - 
    xgb_cv$evaluation_log$test_rmse_mean[best_nrounds]
  v = abs(value)
  print(v)

  rm(xgb_cv);gc()
  
  return(list(xgb_params,best_nrounds,dtrain,v))
}





