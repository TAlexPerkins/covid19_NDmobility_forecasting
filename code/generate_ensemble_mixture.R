library(tidyverse)
##library(covdata)

## read in states that have been processed
state_pops = read.csv('../data/state_pops.csv',header=F)
f = list.files('../output/')
f = f[!grepl("probs",f)]
f = f[!grepl("training",f)]
f = f[!grepl("weights",f)]
state_list = unique(substr(f,17,18))
load("../data/cov.RData")

# loop through states and compile their results in the right format
for(STATE in state_list){
  # get state name for writing to file
  STATE_NAME = as.character(unlist(
      state_pops[state_pops[,1]==STATE,2:3][
          which(!is.na(state_pops[state_pops[,1]==STATE,2:3]))][1]))

  # get state populaltion
  pop = state_pops[state_pops[,1]==STATE,4]

  # read in first model fit for that state to know how to set up storage
  f = list.files('../output/')
  f = f[substr(f,0,3)=='mod']
  f = f[grep(STATE,f)]
  f = f[!grepl("probs",f)]
  f = f[!grepl("weights",f)]
  f = f[!grepl("training",f)]
  load(paste('../output/',f[1],sep=''))
  load(paste0("../output/weights_",STATE,".RData"))
  weights <- weights[weights > 0.0]
  if (length(weights) != length(f)) print("ERROR in weight!!!")
  # samples.list = list()
  # npi.mat = matrix(NA,nrow(df),length(f))
  deathPreds.array = array(NA,c(length(f),nrow(deathPreds),ncol(deathPreds)))
  # deviance = numeric()
  
  # read in all model fits for that state and store results
  for(ii in 1:length(f)){
    load(paste('../output/',f[ii],sep=''))
    # samples.list[[ii]] = samples[,1:12]
    # print(median(samples[,14]))
    # npi.mat[,ii] = df$mobility
    if (nrow(deathPreds)==nrow(deathPreds.array[ii,,])){
        deathPreds.array[ii,,] = ifelse(is.na(deathPreds),0,deathPreds)
    } else {
        row.n <- nrow(deathPreds.array[ii,,])
        deathPreds.array[ii,,] = ifelse(is.na(deathPreds[1:row.n,]),0,deathPreds[1:row.n,])
    }
    # deviance[ii] = mean(-2*samples[,'Lposterior'])
  }

  # time frame of analysis
  t = df$doy

  # calculate weights for models  
  # wts = exp(-(deviance-min(deviance))) / sum(exp(-(deviance-min(deviance))))

  # date processing
  forecast.date = Sys.Date()
  forecast.row = nrow(df) + as.numeric(forecast.date - max(df$date))
  forecast.day = as.POSIXlt(forecast.date)$wday

  # get deaths in NYT data
  state_data = covus %>%
      filter(state == STATE,measure == "death")
  state_deaths = max(state_data$count)
  state_date = max(state_data$date)
    
  # take random draws from mean to obtain posterior predictions
  deathPreds.array = array(
    rpois(prod(dim(deathPreds.array)),deathPreds.array),
    dim(deathPreds.array))
  deathPredsCum.array = deathPreds.array
  for(ii in 1:dim(deathPredsCum.array)[1]){
    for(jj in 1:dim(deathPredsCum.array)[3]){
      deathPredsCum.array[ii,,jj] = cumsum(deathPredsCum.array[ii,,jj]) - sum(deathPredsCum.array[ii,1:(nrow(df) - as.numeric(max(df$date)-state_date)),jj]) + state_deaths
    }
  }
  
  # quantiles that need to be computed
  quantiles = c(
    0.01,0.025,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
    0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.975,0.99)
   
  ## # prepare quantiles for daily incidence
  ## daily.quantiles = as.numeric(apply(deathPreds.array,2,function(x)
  ##   quantile(x,quantiles))[,forecast.row+(1:42)])
  ## df.forecast.deathInc.daily.quantile = data.frame(
  ##   forecast_date = forecast.date,
  ##   target = rep(paste(1:42,' day ahead inc death',sep=''),each=length(quantiles)),
  ##   location = NA,
  ##   location_name = STATE_NAME,
  ##   target_end_date = rep(forecast.date+(1:42),each=length(quantiles)),
  ##   type = 'quantile',
  ##   quantile = quantiles,
  ##   value = daily.quantiles)
  
  ## # prepare point estimates for daily incidence
  ## daily.points = as.numeric(apply(deathPreds.array,2,mean)[forecast.row+(1:42)])
  ## df.forecast.deathInc.daily.point = data.frame(
  ##   forecast_date = forecast.date,
  ##   target = paste(1:42,' day ahead inc death',sep=''),
  ##   location = NA,
  ##   location_name = STATE_NAME,
  ##   target_end_date = forecast.date+(1:42),
  ##   type = 'point',
  ##   quantile = NA,
  ##   value = daily.points)
  
  ## # prepare quantiles for daily cumulative
  ## daily.quantiles = as.numeric(apply(deathPredsCum.array,2,function(x)
  ##   quantile(x,quantiles))[,forecast.row+(1:42)])
  ## df.forecast.deathCum.daily.quantile = data.frame(
  ##   forecast_date = forecast.date,
  ##   target = rep(paste(1:42,' day ahead cum death',sep=''),each=length(quantiles)),
  ##   location = NA,
  ##   location_name = STATE_NAME,
  ##   target_end_date = rep(forecast.date+(1:42),each=length(quantiles)),
  ##   type = 'quantile',
  ##   quantile = quantiles,
  ##   value = daily.quantiles)
  
  ## # prepare point estimates for daily cumulative
  ## daily.points = as.numeric(apply(deathPredsCum.array,2,mean)[forecast.row+(1:42)])
  ## df.forecast.deathCum.daily.point = data.frame(
  ##   forecast_date = forecast.date,
  ##   target = paste(1:42,' day ahead cum death',sep=''),
  ##   location = NA,
  ##   location_name = STATE_NAME,
  ##   target_end_date = forecast.date+(1:42),
  ##   type = 'point',
  ##   quantile = NA,
  ##   value = daily.points)
  
  # generate weekly forecast from daily forecast
  sample.number <- dim(deathPreds.array)[3]
  sample.models <- sample(length(weights), sample.number,replace=TRUE,prob=weights)
  if (sample.number != dim(deathPreds.array)[3]) {
      sample.runs <- sample(dim(deathPreds.array)[3], sample.number,replace=TRUE)
  } else {
      sample.runs <- 1:sample.number
  }
  deathPreds.array.weekly.temp = apply(deathPreds.array,c(1,3),function(x)
    aggregate(x[forecast.row+(1:42)-(forecast.day+1)%%7],by=list(rep(1:6,each=7)),FUN=sum)[,2])
  deathPreds.array.weekly <- tapply(deathPreds.array.weekly.temp,1,
                                     function(x) diag(x[sample.models,sample.runs])))
  deathPredsCum.array.weekly.temp = apply(deathPredsCum.array,c(1,3),function(x)
    x[forecast.row+7*(1:6)-(forecast.day+1)%%7])
  deathPredsCum.array.weekly <- t(apply(deathPredsCum.array.weekly.temp,1,
                                        function(x) diag(x[sample.models,sample.runs])))
  rm(deathPreds.array.weekly.temp,deathPredsCum.array.weekly.temp)
    
  # prepare quantiles for weekly incidence
  weekly.quantiles = as.numeric(apply(deathPreds.array.weekly,1,function(x)
    quantile(x,quantiles)))
  df.forecast.deathInc.weekly.quantile = data.frame(
    forecast_date = forecast.date,
    target = rep(paste(1:6,' wk ahead inc death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(forecast.date+7*(1:6)-(forecast.day+1)%%7,each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = weekly.quantiles)
  
  # prepare point estimates for weekly incidence
  weekly.points = as.numeric(apply(deathPreds.array.weekly,1,mean))
  df.forecast.deathInc.weekly.point = data.frame(
    forecast_date = forecast.date,
    target = paste(1:6,' wk ahead inc death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = forecast.date+7*(1:6)-(forecast.day+1)%%7,
    type = 'point',
    quantile = NA,
    value = weekly.points)
  
  # prepare quantiles for weekly cumulative
  weekly.quantiles = as.numeric(apply(deathPredsCum.array.weekly,1,function(x)
    quantile(x,quantiles)))
  df.forecast.deathCum.weekly.quantile = data.frame(
    forecast_date = forecast.date,
    target = rep(paste(1:6,' wk ahead cum death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(forecast.date+7*(1:6)-(forecast.day+1)%%7,each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = weekly.quantiles)
  
  # prepare point estimates for weekly cumulative
  weekly.points = as.numeric(apply(deathPredsCum.array.weekly,1,mean))
  df.forecast.deathCum.weekly.point = data.frame(
    forecast_date = forecast.date,
    target = paste(1:6,' wk ahead cum death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = forecast.date+7*(1:6)-(forecast.day+1)%%7,
    type = 'point',
    quantile = NA,
    value = weekly.points)
  
  # combine all targets into a single data frame for this state
  eval(parse(text=paste('
    df.forecast_',STATE,' = rbind(',
      paste(ls()[substr(ls(),0,12)=='df.forecast.'],collapse=','),')',sep='')))
  
  print(STATE)
}

# combine forecast info across all states
eval(parse(text=paste('
    df.forecast = rbind(',paste(ls()[substr(ls(),0,12)=='df.forecast_'],collapse=','),')',sep='')))

# add FIPS codes for each state
library(tidyverse)
df.forecast = dplyr::select(df.forecast, -location) %>%
  mutate(location_name = as.character(location_name))
fips_codes = read_csv('../data/fips_codes.csv')
fips = fips_codes %>%
  dplyr::select( state_code, state_name) %>%
  rename(location = state_code, location_name = state_name) %>%
  unique()
df.forecast = left_join(df.forecast, fips, by = "location_name") %>%
  dplyr::select(forecast_date, target, location, #location_name,
                target_end_date, type, quantile, value)


# save forecast for all states
save(df.forecast,file=paste0('../forecasts/forecast_states_',forecast.date,'.RData'))
# write_csv(df.forecast,path=paste0('../forecasts/',forecast.date,'-NotreDame-mobility.csv'))
