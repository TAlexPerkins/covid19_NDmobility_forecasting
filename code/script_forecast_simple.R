# load libraries
# install.packages('BayesianTools')
library(BayesianTools)
# remotes::install_github("kjhealy/covdata")
library(covdata)
# install.packages('foreach')
library(foreach)
# install.packages('mgcv')
library(mgcv)



# load state population data <- Where is this file supposed to run from? code or data?
state_pops = read.csv('../data/state_pops.csv',header=F,  stringsAsFactors = F)
state_pops = state_pops[which(rowSums(is.na(state_pops[,2:3]))<2),]
state_pops[which(state_pops$V2 == 'New Jersy'),]$V2 = 'New Jersey'
state_pops[which(state_pops$V3 == 'New Jersy'),]$V3 = 'New Jersey'

# models that need to be run
models = expand.grid(
  STATE = state_pops[,1],
  MOBILITY = 1:9)

# select which model
args = commandArgs(trailingOnly=TRUE)
STATE = models[as.numeric(args[1]),'STATE']
MOBILITY = models[as.numeric(args[1]),'MOBILITY']

mobility_types = c('driving', 'transit', 'walking', 'grocery', 'parks', 
                   'residential', 'retail', 'transit', 'workplaces')


print(MOBILITY)

# subset to the desired state
d = subset(covus,state==STATE)

if(MOBILITY%in%(1:3)){
  a = subset(
    apple_mobility,
    sub_region==as.character(state_pops[which(state_pops[,1]==STATE),2]))
}
if(MOBILITY%in%(4:9)){
  g = subset(
    google_mobility,
    sub_region_1==as.character(state_pops[which(state_pops[,1]==STATE),3]))
}

if(MOBILITY %in% 1:3){
  if(sum(which(a$transportation_type == mobility_types[MOBILITY])) > 1){
    GO = T
  }else{
    GO = F
  }
}

if(MOBILITY %in% 4:9){
  if(sum(which(g$type == mobility_types[MOBILITY])) > 1){
    GO = T
  }else{
    GO = F
  }
}

if(GO){
  pop = state_pops[state_pops[,1]==STATE,4]
  
  ## Format data to get incidence data
  d = subset(d, !(measure %in% c("deaths_increase", "positive_increase", "negative_increase", "total_test_results_increase")))
  
  ## Deaths incidence
  deaths_df = subset(d, measure == "death")
  deaths_df = deaths_df[order(deaths_df$date),]
  deaths_df$count = c(deaths_df$count[1], diff(deaths_df$count))
  deaths_df$measure = "death_increase"
  deaths_df$measure_label = "Death_Increase"
  
  ## Positive incidence
  pos_df = subset(d, measure == "positive")
  pos_df = pos_df[order(pos_df$date),]
  pos_df$count = c(pos_df$count[1], diff(pos_df$count))
  pos_df$measure = "positive_increase"
  pos_df$measure_label = "Positive_Increase"
  
  
  ## Positive incidence
  neg_df = subset(d, measure ==  "negative")
  neg_df = neg_df[order(neg_df$date),]
  neg_df$count = c(neg_df$count[1], diff(neg_df$count))
  neg_df$measure = "negative_increase"
  neg_df$measure_label = "Negative_Increase"
  
  total_df = pos_df
  total_df$count = pos_df$count + neg_df$count
  total_df$measure = "total_test_results_increase"
  total_df$measure_label = "Total_Tests_Increase"
  
  d = rbind(d, deaths_df, pos_df, total_df)
  
  
  # load and format data
  df = data.frame(
    date = seq(as.Date('20200101','%Y%m%d'),max(covus$date),1),
    doy = 1:(max(covus$date)-as.Date('20191231','%Y%m%d')))
  df$deaths = rep(NA, nrow(df))
  df$tpos = rep(NA, nrow(df))
  df$ttot = rep(NA, nrow(df))
  for(dd in unique(d$date)){
    ddd = which(df$date==dd)
    df$deaths[ddd] = pmax(0,subset(d, measure == 'death_increase' & date == dd)$count)
    df$tpos[ddd] = pmax(0,subset(d, measure == 'positive_increase' & date == dd)$count)
    df$ttot[ddd] = pmax(0,subset(d, measure=='total_test_results_increase'&date==dd)$count)
    if(MOBILITY==1){
      df$mobility[ddd] =
        mean(subset(a,transportation_type=='driving'&date==dd)$index,na.rm=T)
    } else if(MOBILITY==2){
      df$mobility[ddd] =
        mean(subset(a,transportation_type=='transit'&date==dd)$index,na.rm=T)
    } else if(MOBILITY==3){
      df$mobility[ddd] =
        mean(subset(a,transportation_type=='walking'&date==dd)$index,na.rm=T)
    } else if(MOBILITY==4){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='grocery'&date==dd)$pct_diff,na.rm=T)
    } else if(MOBILITY==5){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='parks'&date==dd)$pct_diff,na.rm=T)
    } else if(MOBILITY==6){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='residential'&date==dd)$pct_diff,na.rm=T)
    } else if(MOBILITY==7){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='retail'&date==dd)$pct_diff,na.rm=T)
    } else if(MOBILITY==8){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='transit'&date==dd)$pct_diff,na.rm=T)
    } else if(MOBILITY==9){
      df$mobility[ddd] = 100 +
        mean(subset(g,type=='workplaces'&date==dd)$pct_diff,na.rm=T)
    }
  }
  
  # add a safeguard against positive tests > total tests
  for(dd in which(!is.na(df$tpos))){
    if(!is.na(df$tpos[dd] > df$ttot[dd]) & df$tpos[dd] > df$ttot[dd]){
      df$ttot[dd] = NA
      df$tpos[dd] = NA
    }else if(is.na(df$tpos[dd] > df$ttot[dd])){
      df$ttot[dd] = df$tpos[dd]
    }
  }
  
  df$deaths[is.na(df$deaths)] = 0
  df$mobility[
    is.na(df$mobility) &
      df$date < df$date[min(which(!is.na(df$mobility)))]] = 
    df$mobility[min(which(!is.na(df$mobility)))]
  
  
  
  # replace mobility data with GAM fits
  gam.fit = gam(mobility ~ s(doy), data = df)
  df$mobility = predict(gam.fit,newdata=data.frame(doy=1:(max(df$doy))))
  
  
  
  # initial values of free parameters
  imp.mag = log(10^c(-3,-1,1,3))
  imp.mean = seq(1,101,20)
  imp.sd = log(c(7,30,90))
  R0.mag = log(c(1.5,2.5,3.5))
  npi.mag = log(c(0.1,1.1,2))
  dead.mag = log(0.015 / (1 - 0.015))
  dead.tim = log(17.7)
  ratio.int = log(c(0.001,0.01,0.1))
  R0.tim = log(c(4,5.8,8))
  symp.mag = log(0.6 / (1 - 0.6))
  symp.tim = log(c(4,5.2,8))
  test.tim = log(c(2,6.05,10))
  
  # set up parameter combinations
  pars = expand.grid(
    imp.mag=imp.mag,imp.mean=imp.mean,imp.sd=imp.sd,R0.mag=R0.mag,
    npi.mag=npi.mag,dead.mag=dead.mag,dead.tim=dead.tim,ratio.int=ratio.int,
    R0.tim=R0.tim,symp.mag=symp.mag,symp.tim=symp.tim,test.tim=test.tim)
  pars$LL = NA
  
  
  
  # log likelihood function
  LL = function(par, browse = F){
    if(browse) browser()
    imp.mag = exp(par[1])
    imp.mean = par[2]
    imp.sd = exp(par[3])
    R0.mag = exp(par[4])
    npi.mag = exp(par[5])
    dead.mag = exp(par[6]) / (exp(par[6]) + 1)
    dead.tim = exp(par[7])
    ratio.int = par[8]
    R0.tim = exp(par[9])
    symp.mag = exp(par[10]) / (exp(par[10]) + 1)
    symp.tim = exp(par[11])
    test.tim = exp(par[12])
    
    # adjust mobility predictor
    npi = pmin(1, pmax(0, 1 - npi.mag * (1 - npi.ref)))
    
    # specify secondary infection distribution
    R0 = R0.mag * dpois(1:21,R0.tim) / sum(dpois(1:21,R0.tim))
    
    # simulate infections
    inf = imp.mag * dnorm(t,mean=imp.mean,sd=imp.sd)
    for(tt in 1:(length(t)-1)){
      inf[tt+(1:min(length(R0),max(t)-tt))] =
        inf[tt+(1:min(length(R0),max(t)-tt))] +
        inf[tt] *
        R0[1:min(length(R0),max(t)-tt)] *
        npi[tt+(1:min(length(R0),max(t)-tt))] *
        (pop - sum(inf[1:(tt+min(length(R0),max(t)-tt))])) / pop
    }
    
    # specify delay between infection and symptom onset
    prop.inf.symp = symp.mag * dpois(1:28,symp.tim) / sum(dpois(1:28,symp.tim))
    
    # simulate symptomatic infections
    symp = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp[tt+(1:min(length(prop.inf.symp),max(t)-tt))] =
        symp[tt+(1:min(length(prop.inf.symp),max(t)-tt))] +
        inf[tt] *
        prop.inf.symp[1:min(length(prop.inf.symp),max(t)-tt)]
    }
    
    # specify delay between symptom onset and seeking testing
    prop.symp.test = dpois(1:28,test.tim) / sum(dpois(1:28,test.tim))
    
    # simulate symptomatic infections that seek testing
    symp.test = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp.test[tt+(1:min(length(prop.symp.test),max(t)-tt))] =
        symp.test[tt+(1:min(length(prop.symp.test),max(t)-tt))] +
        symp[tt] *
        prop.symp.test[1:min(length(prop.symp.test),max(t)-tt)]
    }
    
    # specify delay between symptom onset and death
    prop.symp.dead = dead.mag * dpois(1:42,dead.tim) / sum(dpois(1:42,dead.tim))
    
    # simulate symptomatic infections that result in death
    symp.dead = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp.dead[tt+(1:min(length(prop.symp.dead),max(t)-tt))] =
        symp.dead[tt+(1:min(length(prop.symp.dead),max(t)-tt))] +
        symp[tt] *
        prop.symp.dead[1:min(length(prop.symp.dead),max(t)-tt)]
    }
    
    # likelihood contribution from testing
    ratio = exp(ratio.int) # + ratio.lin * t + ratio.qua * (t^2))
    prob = pmax(1e-10,pmin(1-1e-10,(symp.test / pop) /
                             ((symp.test / pop) + ratio * (1 - (symp.test/pop)))))
    prob = ifelse(is.nan(prob),1e-10,prob)
    LL = sum(dbinom(df$tpos, df$ttot, prob, log=T),na.rm=T)
    
    # likelihood contribution from deaths
    LL = LL + sum(dpois(df$deaths,pmax(1e-10,symp.dead),log=T),na.rm=T)
    
    # return log likelihood
    return(LL)
  }
  
  
  
  # time frame
  t = df$doy
  
  # incorporate mobility data
  npi.ref = df$mobility
  npi.ref = ifelse(
    predict(gam.fit,newdata=data.frame(doy=t)) < max(df$mobility,na.rm=T),
    predict(gam.fit,newdata=data.frame(doy=t)),
    max(df$mobility,na.rm=T))
  npi.ref = npi.ref / npi.ref[1]
  
  
  
  # calculate LL at all initial value combinations
  for(ii in 1:nrow(pars)){
    pars$LL[ii] = LL(as.numeric(pars[ii,1:12]), browse = F)
  }
  par = as.numeric(pars[which.max(pars$LL),1:12])
  
  # specify parameter ranges as factor of two around MLE
  lower = ifelse(par > 0, 1/2 * par, 2 * par)
  upper = ifelse(par > 0, 2 * par, 1/2 * par)
  
  
  
  # sample posterior using MCMC
  bayesianSetup = createBayesianSetup(LL,lower=lower,upper=upper)
  settings = list(iterations = 3e5, message = F)
  out = runMCMC(bayesianSetup, settings = settings)
  samples = getSample(out,start=5e4,thin=5e1,parametersOnly=F)
  
  
  
  # posterior preds
  postPred = function(par,t,toReturn){
    imp.mag = exp(par[1])
    imp.mean = par[2]
    imp.sd = exp(par[3])
    R0.mag = exp(par[4])
    npi.mag = exp(par[5])
    dead.mag = exp(par[6]) / (exp(par[6]) + 1)
    dead.tim = exp(par[7])
    ratio.int = par[8]
    R0.tim = exp(par[9])
    symp.mag = exp(par[10]) / (exp(par[10]) + 1)
    symp.tim = exp(par[11])
    test.tim = exp(par[12])
    
    # adjust mobility predictor
    npi = pmin(1, pmax(0, 1 - npi.mag * (1 - npi.ref)))
    
    # specify secondary infection distribution
    R0 = R0.mag * dpois(1:21,R0.tim) / sum(dpois(1:21,R0.tim))
    
    # simulate infections
    inf = imp.mag * dnorm(t,mean=imp.mean,sd=imp.sd)
    for(tt in 1:(length(t)-1)){
      inf[tt+(1:min(length(R0),max(t)-tt))] =
        inf[tt+(1:min(length(R0),max(t)-tt))] +
        inf[tt] *
        R0[1:min(length(R0),max(t)-tt)] *
        npi[tt+(1:min(length(R0),max(t)-tt))] *
        (pop - sum(inf[1:(tt+min(length(R0),max(t)-tt))])) / pop
    }
    
    # specify delay between infection and symptom onset
    prop.inf.symp = symp.mag * dpois(1:28,symp.tim) / sum(dpois(1:28,symp.tim))
    
    # simulate symptomatic infections
    symp = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp[tt+(1:min(length(prop.inf.symp),max(t)-tt))] =
        symp[tt+(1:min(length(prop.inf.symp),max(t)-tt))] +
        inf[tt] *
        prop.inf.symp[1:min(length(prop.inf.symp),max(t)-tt)]
    }
    
    # specify delay between symptom onset and seeking testing
    prop.symp.test = dpois(1:28,test.tim) / sum(dpois(1:28,test.tim))
    
    # simulate symptomatic infections that seek testing
    symp.test = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp.test[tt+(1:min(length(prop.symp.test),max(t)-tt))] =
        symp.test[tt+(1:min(length(prop.symp.test),max(t)-tt))] +
        symp[tt] *
        prop.symp.test[1:min(length(prop.symp.test),max(t)-tt)]
    }
    
    # specify delay between symptom onset and death
    prop.symp.dead = dead.mag * dpois(1:42,dead.tim) / sum(dpois(1:42,dead.tim))
    
    # simulate symptomatic infections that seek testing
    symp.dead = rep(0,length(t))
    for(tt in 1:(length(t)-1)){
      symp.dead[tt+(1:min(length(prop.symp.dead),max(t)-tt))] =
        symp.dead[tt+(1:min(length(prop.symp.dead),max(t)-tt))] +
        symp[tt] *
        prop.symp.dead[1:min(length(prop.symp.dead),max(t)-tt)]
    }
    symp.dead = pmax(1e-10,symp.dead)
    
    # likelihood contribution from testing
    ratio = exp(ratio.int) # + ratio.lin * t + ratio.qua * (t^2))
    prob = pmax(1e-10,pmin(1-1e-10,(symp.test / pop) /
                             ((symp.test / pop) + ratio * (1 - (symp.test/pop)))))
    prob = ifelse(is.nan(prob),1e-10,prob)
    
    # return output
    if(toReturn=='deaths'){
      return(symp.dead)
    } else {
      return(prob)
    }
  }
  
  
  
  # time frame
  t = c(df$doy,max(df$doy)+(1:60))
  
  # incorporate mobility data
  npi.ref = ifelse(
    predict(gam.fit,newdata=data.frame(doy=t)) < max(df$mobility,na.rm=T),
    predict(gam.fit,newdata=data.frame(doy=t)),
    max(df$mobility,na.rm=T))
  npi.ref = npi.ref / npi.ref[1]
  
  # get model predictions
  deathPreds = foreach(ii = 1:nrow(samples),.combine='cbind') %do% {
    postPred(samples[ii,1:12],t,'deaths')
  }
  
  
  
  # save output
  save(par,samples,t,deathPreds,df,LL,
       file=paste('../output/model_posterior_',STATE,'_',MOBILITY,'.RData',sep=''))
  
  
  # save(par,samples,t,deathPreds,df,LL,
  #      file=paste('../test/model_posterior_',STATE,'_',MOBILITY,'.RData',sep=''))
  
  rm(GO)
}




# matplot(deathPreds,type='l',lty=1,col=rgb(0,0,0,0.01),
#         # ylim=range(df$deaths),
#         ylab='Deaths',las=1, ylim = c(0, max(deathPreds, df$deaths)))
# lines(df$deaths,col=2)
# positivityPreds = foreach(ii = 1:nrow(samples),.combine='cbind') %do% {
#   postPred(samples[ii,1:8],t,'positivity')
# }
# matplot(positivityPreds,type='l',lty=1,col=rgb(0,0,0,0.01),
#         ylim=c(0,1),ylab='Test positivity',las=1)
# lines(df$tpos/df$ttot,col=2)

