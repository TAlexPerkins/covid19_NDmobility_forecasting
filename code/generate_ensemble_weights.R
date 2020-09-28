library(tidyverse)
library(pomp)
##library(covdata)

##training length
training.length <- 4*7

## read in states that have been processed
state_pops = read.csv('../data/state_pops.csv',header=F)
f = list.files('../output/')
f = f[!grepl("probs",f)]
f = f[grepl("training",f)]
f = f[!grepl("weights",f)]
state_list = unique(substr(f,26,27))
load("../data/cov.RData")

## Function to estimate loglikelihood for a given weighting and forecast
weighted.loglikelihood <- function(weights,means,sds,data){
    weighted.means <- colSums(as.numeric(weights)*t(means))
    weighted.sds <- sqrt(colSums((as.numeric(weights)*t(sds))^2))
    nll <- -sum(dnorm(data,weighted.means,weighted.sds,log=TRUE))
    return(nll)
}

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
  f = f[grepl("training",f)]
  f = f[!grepl("weights",f)]
  load(paste('../output/',f[1],sep=''))
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
  forecast.date = Sys.Date() - training.length
  forecast.row = nrow(df) + as.numeric(forecast.date - max(df$date))
  forecast.day = as.POSIXlt(forecast.date)$wday

  # get deaths in NYT data
  state_data.full = covus %>%
      filter(state == STATE,measure == "death")
  state_data = state_data.full %>%
      filter(date <= forecast.date)
  state_deaths = max(state_data$count)
  state_date = max(state_data$date)
    


  # take random draws from mean to obtain posterior predictions
  deathPreds.array = array(
    rpois(prod(dim(deathPreds.array)),deathPreds.array),
    dim(deathPreds.array))
  
  # generate weekly forecast from daily forecast
  deathPreds.array.weekly = apply(deathPreds.array,c(1,3),function(x)
    aggregate(x[forecast.row+(1:training.length)-(forecast.day+1)%%7],
              by=list(rep(1:(training.length/7),each=7)),FUN=sum)[,2])
  
  ## get mean and sd for each metric/week
  weekly.sd = apply(deathPreds.array.weekly,c(1,2),sd)
  weekly.mean = apply(deathPreds.array.weekly,c(1,2),mean)
  target.dates <- forecast.date + 7*0:4 - (forecast.day+1)%%7
  target.data <- diff((state_data.full %>% filter(date %in% target.dates))$count)
  ## LL <- array(dnorm(rep(target.data, ncol(weekly.mean)),
  ##                   as.numeric(weekly.mean),as.numeric(weekly.sd),log=T),
  ##             dim=dim(weekly.sd))

  ## if (ncol(weekly.mean)>1){
  ##     length.sweep <- 5
  ##     weight.sweep <- expand.grid(as.data.frame(matrix(rep(0:length.sweep,
  ##                                                          ncol(weekly.mean)),
  ##                                                      ncol=ncol(weekly.mean))))
  ##     weight.sweep <- weight.sweep[rowSums(weight.sweep) == length.sweep,]
  ##     weight.sweep <- weight.sweep/length.sweep
  ##     NLLs <- vector(mode="numeric",length=nrow(weight.sweep))
  ##     for (ii in 1:nrow(weight.sweep)) {
  ##         NLLs[ii] <- weighted.loglikelihood(weight.sweep[ii,],weekly.mean,weekly.sd,target.data)
  ##     }
  ##     weights <- weight.sweep[which.min(NLLs),]
  ## } else if (ncol(weekly.mean) == 1) {
  ##     weights <- 1
  ## } else {
  ##     print("ERROR! Wrong length")
  ## }

  if (ncol(weekly.mean)>1){
      weights.init <- rep(1,ncol(weekly.mean))/ncol(weekly.mean)
      weights.init <- weights.init/sum(weights.init)
      weights.out <- optim(par=weights.init,
                           fn=function(par) {
                               x=par/sum(par)
                               min(1e6,weighted.loglikelihood(x,weekly.mean,weekly.sd,target.data))
                           },
                           method="L-BFGS-B",lower=0)
      weights <- weights.out$par/sum(weights.out$par)
  } else {
      weights <- 1
  }

  save(weights,file=paste0("../output/weights_",STATE,".RData"))
  print(STATE)
}
