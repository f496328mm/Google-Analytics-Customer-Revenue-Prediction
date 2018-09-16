

factor2numeric = function(train2,by,var_name){
  #colnames(train2)
  value = train2[, .(
    value = mean(log_Revenue,na.rm = TRUE)), 
    by = by]

  colnames( value ) = c(colnames( value )[1],var_name)
  return(value)
}

work_feature = function(train2){
  
  feature_channelG = factor2numeric(
    train2,by = 'channelGrouping',var_name = 'feature_channelG')
  
  feature_visitN = factor2numeric(
    train2,by = 'visitNumber',var_name = 'feature_visitN')
  
  feature_browser = factor2numeric(
    train2,by = 'browser',var_name = 'feature_browser')
  
  feature_operatingS = factor2numeric(
    train2,by = 'operatingSystem',var_name = 'feature_operatingS')
  
  feature_deviceC = factor2numeric(
    train2,by = 'deviceCategory',var_name = 'feature_deviceC')
  
  feature_continent = factor2numeric(
    train2,by = 'continent',var_name = 'feature_continent')
  
  feature_subC = factor2numeric(
    train2,by = 'subContinent',var_name = 'feature_subC')
  
  feature_country = factor2numeric(
    train2,by = 'country',var_name = 'feature_country')
  
  feature_region = factor2numeric(
    train2,by = 'region',var_name = 'feature_region')
  
  feature_metro = factor2numeric(
    train2,by = 'metro',var_name = 'feature_metro')
  
  feature_city = factor2numeric(
    train2,by = 'city',var_name = 'feature_city')
  
  feature_networkD = factor2numeric(
    train2,by = 'networkDomain',var_name = 'feature_networkD')
  
  feature_source = factor2numeric(
    train2,by = 'source',var_name = 'feature_source')
  
  feature_medium = factor2numeric(
    train2,by = 'medium',var_name = 'feature_medium')
  
  feature_keyword = factor2numeric(
    train2,by = 'keyword',var_name = 'feature_keyword')
  
  feature = list(feature_channelG,
                 feature_visitN,
                 feature_browser,
                 feature_operatingS,
                 feature_deviceC,
                 feature_continent,
                 feature_subC,
                 feature_country,
                 feature_region,
                 feature_metro,
                 feature_city,
                 feature_networkD,
                 feature_source,
                 feature_medium,
                 feature_keyword)
  by_list = c('channelGrouping','visitNumber','browser',
              'operatingSystem','deviceCategory','continent',
              'subContinent','country','region',
              'metro','city','networkDomain',
              'source','medium','keyword')
  return(list(feature,by_list))
}


JsonChange = function(data){
  
  print('json change to data table')
  #s = Sys.time()
  device = data$device %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  
  geoNetwork = data$geoNetwork %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  
  totals = data$totals %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  totals <- totals[, lapply(.SD, as.numeric)]
  
  trafficSource = data$trafficSource %>% paste(., collapse = ",") %>% paste("[", ., "]") %>% 
    fromJSON(flatten = T) %>% data.table
  
  #e = Sys.time() - s
  
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













