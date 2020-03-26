library(tibble)
####################################################################
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
####################################################################
Modes <- function(x) {
  ux <- unique(x[!is.na(x)])
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]}
###################################################################
setwd("C:/Users/NPourkasraei/Documents/R/Working Folder/Risk")
color<- c("darkviolet","blue4","darkorange","darkorchid","gray15","deeppink4","turquoise4","tomato4","red4","brown","orchid4","maroon4","chocolate","yellow")
############
risk.all<- read.csv(file = "Risk.csv", header = TRUE) 
############
population<- as_tibble(as.data.frame(read.csv(file="Population.csv",header = TRUE)))
age<- as_tibble(as.data.frame(read.csv(file="AssetAge.csv",header = TRUE)))
############
AC<- sort(unique(risk.all$Asset))
WLCM <- list() 

####################################################################
#   Looping through data to create Risk table per Asset Class      #
####################################################################
i<- 1
# i<- i+1
for (i in 1:length(AC)) {
  x<- seq(0.001,1, by = 0.001) # Vecotr of probability
  xx<- seq(0,1, by = 0.01)
  
  counter<- 1000
  Failure<- as.data.frame(x)
  Impact<- as.data.frame(x)
  print(paste("i", i, sep = "-"))
  
  asset<- AC[i] # 
  print(asset)
  #************************************************************# 
  risk<- as.data.frame(filter(risk.all, Asset == asset, Type == 1))
  popul <- as.data.frame(filter(population, Asset.Class == asset))
  
  Asset.count<- length(unique(popul$Equipment))
  risk<- risk[, c(1,2,6:11)]
  
  
  
  risk[, "PF.Low"]<- as.numeric(as.character(risk[ ,"PF.Low"]))
  risk[, "PF.High"]<- as.numeric(as.character(risk[ ,"PF.High"]))
  risk[, "PI.Low"]<- as.numeric(as.character(risk[ ,"PI.Low"]))
  risk[, "PI.High"]<- as.numeric(as.character(risk[ ,"PI.High"]))
  #************************************************************# 
  
  #************************************************************# 
  
  Asset<- rep(AC[i],times = length(unique(risk$Risk)))
  # PF.Low  <-summaryBy(PF.Low~Risk, data = risk, FUN = mean)                #$$$$$$$$$$$$$$$$$$$$  mean  $$$$$$$$$$$$$$$$$$$$
  # PF.High<- summaryBy(PF.High~Risk, data = risk, FUN = mean)[,2]           #$$$$$$$$$$$$$$$$$$$$  mean  $$$$$$$$$$$$$$$$$$$$
  # PI.Low <- summaryBy(PI.Low~Risk,  data = risk, FUN = mean)[,2]           #$$$$$$$$$$$$$$$$$$$$  mean  $$$$$$$$$$$$$$$$$$$$
  # PI.High<- summaryBy(PI.High~Risk, data = risk, FUN = mean)[,2]           #$$$$$$$$$$$$$$$$$$$$  mean  $$$$$$$$$$$$$$$$$$$$
  #************************************************************# 
  Risk<- as.data.frame(Asset)
 # Risk2<- Risk
Risk[,c(2,3)]<- ((risk %>%
    group_by(Risk) %>%
    summarise(PF.Low_mean= (mean(PF.Low)/100))))

Risk[4]<-(risk %>%
  group_by(Risk) %>%
  summarise( PF.High_mean= (mean(PF.High)/100)))[2]   

Risk[5]<-(risk %>%
            group_by(Risk) %>%
            summarise( PI.Low_mean= (mean(PI.Low))))[2]   

Risk[6]<-(risk %>%
            group_by(Risk) %>%
            summarise( PI.High_mean= (mean(PI.High))))[2]   

  
  # Risk$Risk<- (summaryBy(PF.Low~Risk, data = risk, FUN = mean))[,1]
  # Risk$PF.Low_mean  <- ((summaryBy(PF.Low~Risk, data = risk, FUN = mean) )[,2]/100)
  # Risk$PF.High_mean <- ((summaryBy(PF.High~Risk, data = risk, FUN = mean)[,2] )/100)
  # Risk$PI.Low_mean  <- ifelse((summaryBy(PI.Low~Risk,  data = risk, FUN = mean)[,2]  ) ==0, 1,(summaryBy(PI.Low~Risk,  data = risk, FUN = mean)[,2]  ))
  # Risk$PI.High_mean <- summaryBy(PI.High~Risk, data = risk, FUN = mean)[,2] 
  cname<- paste0(i,"_Risk_",AC[i])
  #************************************************************#  
  #************************************************************# 
  Risk2<- as.data.frame(Asset)
  Risk2[,c(2,3)]<- ((risk %>%
                      group_by(Risk) %>%
                      summarise(PF.Low_mean= (max(PF.Low)/100))))
  
  Risk2[4]<-(risk %>%
              group_by(Risk) %>%
              summarise( PF.High_mean= (max(PF.High)/100)))[2]   
  
  Risk2[5]<-(risk %>%
              group_by(Risk) %>%
              summarise( PI.Low_mean= (max(PI.Low))))[2]   
  
  Risk2[6]<-(risk %>%
              group_by(Risk) %>%
              summarise( PI.High_mean= (max(PI.High))))[2]   
  
  # Risk2$Risk<- (summaryBy(PF.Low~Risk, data = risk, FUN = max))[,1]
  # Risk2$PF.Low_max  <- ((summaryBy(PF.Low~Risk, data = risk, FUN = max) )[,2]/100)
  # Risk2$PF.High_max <- ((summaryBy(PF.High~Risk, data = risk, FUN = max)[,2] )/100)
  # Risk2$PI.Low_max  <- ifelse((summaryBy(PI.Low~Risk,  data = risk, FUN = max)[,2]  ) ==0, 1,(summaryBy(PI.Low~Risk,  data = risk, FUN = mean)[,2]  ))
  # Risk2$PI.High_max <- summaryBy(PI.High~Risk, data = risk, FUN = max)[,2] 
  cname2<- paste0(i,"_Risk_",AC[i])
  #************************************************************#  
  
  df<-as.data.frame(x)
  
  
  RandAge<- data.frame(x=1:10)
  Randnum<- data.frame(x=1:10)
  
  x2<- 1:length(Risk$Risk)
  df2<- data.frame(x2)     
  df2$risk<- Risk$Risk 
  
  par(mfrow=c(2,2))
  set.seed(123)
  
  
  impact_Cost_ru_mean<- as.data.frame(xx)
  impact_Cost_ru_max<- as.data.frame(xx)
  impact_Cost_ru_mean_count<- as.data.frame(xx)
  impact_Cost_ru_max_count<- as.data.frame(xx)
  impact_Cost_rn_mean<- as.data.frame(xx)
  impact_Cost_rn_max<- as.data.frame(xx)
  impact_Cost_rn_mean_count<- as.data.frame(xx)
  impact_Cost_rn_max_count<- as.data.frame(xx)
  j<- 1
  #j<- 4
  for (j in 1:length(Risk$Risk)) {
    
    x3<- seq(1:10)
    Eta<- 100
    Beta<- 3
    xplot1<-0.3
    xplot2<- 0.4
    
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************       MEAN UNIFORM  DISTRIBUTION           *****************************# 
    #***************************************** ***************************************** ***************************************** 
    ru <- runif(counter, min = Risk[j,"PF.Low_mean"] , max =  Risk[j,"PF.High_mean"] )
    ##hist(ru,probability=TRUE,col= color[j], main= paste0("PF.Low = ",Risk[j,"PF.Low_mean"], "PF.High = ",Risk[j,"PF.High_mean"]), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    q<- quantile(ru)
    mu<-  mean(ru)
    var<-((q["25%"] + q["100%"])/2) #sd(ru)
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"] ,Risk[j,"PI.High_mean"],(((Risk[j,"PI.Low_mean"] )+(Risk[j,"PI.High_mean"]))/2))
    
    #plot(density(ti))
    #sort(ti)
    Risk[j,"Impact_ru_mean"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]
    
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail) 
    
    
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    
    
    
    
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    
    
    impact_Cost_ru_mean[j+1]<- impact_Cost
    n<- paste0( Risk$Risk[j],"_ru_mean")
    names(impact_Cost_ru_mean)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************           MEAN  NORMAL DISTRIBUTION         *****************************# 
    #***************************************** ***************************************** ***************************************** 
    rn<- rnorm(counter,mean= (Risk[j,"PF.Low_mean"] + Risk[j,"PF.High_mean"] ) /2, sd=  Risk[j,"PF.High_mean"]- (Risk[j,"PF.Low_mean"] + Risk[j,"PF.High_mean"] ) /2)
    # hist(rn)
    q<- quantile(rn)
    mu<-  mean(rn)
    var<- if(sd(ru)==0){0.00001} else{var}
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"] ,Risk[j,"PI.High_mean"],(((Risk[j,"PI.Low_mean"] )+(Risk[j,"PI.High_mean"]))/2))
    #plot(density(ti))
    Risk[j,"Impact_rn_mean"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_rn_mean[j+1]<- impact_Cost
    n<- paste0( Risk$Risk[j],"_rn_mean")
    names(impact_Cost_rn_mean)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    #***************************************** ***************************************** ***************************************** 
    #          ***************************************    MEAN  UNIFORM DISTRIBUTION/ ASSET COUNT   *****************************# 
    #***************************************** ***************************************** ***************************************** 
    ru <- runif(counter, min = (Risk[j,"PF.Low_mean"])/Asset.count , max =  Risk[j,"PF.High_mean"]/Asset.count )
    ##hist(ru,probability=TRUE,col= color[j], main= paste0("PF.Low = ",Risk[j,"PF.Low_mean"], "PF.High = ",Risk[j,"PF.High_mean"]), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    q<- quantile(ru)
    mu<-  mean(ru)
    var<-((q["25%"] + q["100%"])/2) #sd(ru)
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"]/Asset.count ,Risk[j,"PI.High_mean"]/Asset.count,(((Risk[j,"PI.Low_mean"]/Asset.count )+(Risk[j,"PI.High_mean"]/Asset.count))/2))
    #plot(density(ti))
    Risk[j,"Impact_ru_mean_count"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    
    impact_Cost<- mean(ti)* Failure.Probability
    
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_ru_mean_count[j+1]<- impact_Cost
    n<- paste0( Risk$Risk[j],"_ru_mean_count")
    names(impact_Cost_ru_mean_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          ***************************************    MEAN  NORMAL DISTRIBUTION/ ASSET COUNT   *****************************# 
    #***************************************** ***************************************** *****************************************
    rn<- rnorm(counter,mean= (Risk[j,"PF.Low_mean"]/Asset.count + Risk[j,"PF.High_mean"]/Asset.count ) /2, sd=  Risk[j,"PF.High_mean"]/Asset.count- (Risk[j,"PF.Low_mean"]/Asset.count + Risk[j,"PF.High_mean"]/Asset.count ) /2)
    # hist(rn)
    q<- quantile(rn)
    mu<-  mean(rn)
    var<- if(sd(ru)==0){0.00001} else{var}
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"]/Asset.count ,Risk[j,"PI.High_mean"]/Asset.count,(((Risk[j,"PI.Low_mean"]/Asset.count )+(Risk[j,"PI.High_mean"]/Asset.count))/2))
    #plot(density(ti))
    Risk[j,"Impact_rn_mean_count"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_rn_mean_count[j+1]<- impact_Cost
    n<- paste0( Risk$Risk[j],"_rn_mean_count")
    names(impact_Cost_rn_mean_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************       MAX UNIFORM  DISTRIBUTION            *****************************# 
    #***************************************** ***************************************** ***************************************** 
    ru <- runif(counter, min = Risk2[j,"PF.Low_max"] , max =  Risk2[j,"PF.High_max"] )
    #hist(ru,probability=TRUE,col= color[j], main= paste0("PF.Low = ",Risk2[j,"PF.Low_max"], "PF.High = ",Risk2[j,"PF.High_max"]), xlab = paste0(Risk2[j,"Asset"], "-", Risk2[j,"Risk"]))
    q<- quantile(ru)
    mu<-  mean(ru)
    var<-((q["25%"] + q["100%"])/2) #sd(ru) if(sd(ru)==0){0.00001} else{var}
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    
    
    
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk2[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"] ,Risk2[j,"PI.High_max"],(((Risk2[j,"PI.Low_max"] )+(Risk2[j,"PI.High_max"]))/2))
    #plot(density(ti))
    Risk2[j,"Impact_ru_max"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_ru_max[j+1]<- impact_Cost
    n<- paste0( Risk2$Risk[j],"_ru_max") 
    names(impact_Cost_ru_max)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************           MAX  NORMAL  DISTRIBUTION         *****************************# 
    #***************************************** ***************************************** ***************************************** 
    rn<- rnorm(counter,mean= (Risk2[j,"PF.Low_max"] + Risk2[j,"PF.High_max"] ) /2, sd=  Risk2[j,"PF.High_max"]- (Risk2[j,"PF.Low_max"] + Risk2[j,"PF.High_max"] ) /2)
    # hist(rn)
    q<- quantile(rn)
    mu<-  mean(rn)
    var<- if(sd(ru)==0){0.00001} else{var}
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk2[j,"Asset"], "-", Risk[j,"Risk"]))
    
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"] ,Risk2[j,"PI.High_max"],(((Risk2[j,"PI.Low_max"] )+(Risk2[j,"PI.High_max"]))/2))
    #plot(density(ti))
    Risk2[j,"Impact_rn_max"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_rn_max[j+1]<- impact_Cost
    n<- paste0( Risk2$Risk[j],"_rn_max") 
    names(impact_Cost_rn_max)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    #***************************************** ***************************************** ***************************************** 
    #          *************************************       MAX UNIFORM  DISTRIBUTION /ASSET COUNT           *********************# 
    #***************************************** ***************************************** ***************************************** 
    ru <- runif(counter, min = (Risk2[j,"PF.Low_max"])/Asset.count , max =  Risk2[j,"PF.High_max"]/Asset.count )
    #hist(ru,probability=TRUE,col= color[j], main= paste0("PF.Low = ",Risk2[j,"PF.Low_max"], "PF.High = ",Risk2[j,"PF.High_max"]), xlab = paste0(Risk2[j,"Asset"], "-", Risk2[j,"Risk"]))
    q<- quantile(ru)
    mu<-  mean(ru)
    var<-((q["25%"] + q["100%"])/2) #sd(ru)
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"]/Asset.count ,Risk2[j,"PI.High_max"]/Asset.count,(((Risk2[j,"PI.Low_max"]/Asset.count )+(Risk2[j,"PI.High_max"]/Asset.count))/2))
    #plot(density(ti))
    Risk2[j,"Impact_ru_max_count"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_ru_max_count[j+1]<- impact_Cost
    n<- paste0( Risk2$Risk[j],"_ru_max_count")
    names(impact_Cost_ru_max_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          *************************************       MAX NORMAL  DISTRIBUTION /ASSET COUNT           *********************# 
    #***************************************** ***************************************** ***************************************** 
    rn<- rnorm(counter,mean= (Risk2[j,"PF.Low_max"]/Asset.count + Risk2[j,"PF.High_max"]/Asset.count ) /2, sd=  Risk2[j,"PF.High_max"]/Asset.count- (Risk2[j,"PF.Low_max"]/Asset.count + Risk2[j,"PF.High_max"]/Asset.count ) /2)
    # hist(rn)
    q<- quantile(rn)
    mu<-  mean(rn)
    var<- if(sd(ru)==0){0.00001} else{var}
    es<- estBetaParams(mu, var)
    a<- abs(as.numeric(es[1])) #alpha
    b<- abs(as.numeric(es[2])) #beta
    rpbeta<-pbeta(x, shape1= a, shape2=b , ncp = 0, lower.tail = TRUE, log.p = FALSE) 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk2[j,"Asset"], "-", Risk2[j,"Risk"]))
    
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"]/Asset.count ,Risk2[j,"PI.High_max"]/Asset.count,(((Risk2[j,"PI.Low_max"]/Asset.count )+(Risk2[j,"PI.High_max"]/Asset.count))/2))
    #plot(density(ti))
    Risk2[j,"Impact_rn_max_count"] <- round(mean(ti))
    
    randAge<- 0
    ruall<- 0
    while(length(randAge)< 11){
      for (ii in 1:10000) {  run<- runif(1,0,1) 
      if (  run < mu  & (length(randAge)<11)  ) {
        randAge<- c(randAge,ii)
        ruall<- c( ruall,run)   }   
      ii<- ii+1    }   
    }
    
    
    df3<- data.frame( x3)
    df3["rand_num"]<- ruall[-1]  
    df3["AgeToFail"]<- Eta *(log (1/ (1-df3["rand_num"]))) ^ (1/Beta)
    df3["RankedAgeToFailure"]<- sort(df3$AgeToFail)  
    df3["PlotPosition"]<-     (x3-xplot1)/((10)+xplot2)
    df3["lnMedRank"]<- log(1/(1-df3["PlotPosition"])) #***** is abs ok?????????********
    df3["lnAgetoFailure"]<- log(df3["RankedAgeToFailure"])  
    df3["cumTime"]<- cumsum(df3["AgeToFail"])
    df3["failureAtEachTime"]<- rep(1,length(x3))
    df3["y"]<- x3
    df3["lny"]<-log(df3["y"])
    df3["lnx"]<-log(df3["cumTime"])    
    lm1<-    lm(log(df3$y) ~ log(df3$cumTime)  ) 
    s1<- summary(lm1)
    df2[j,"Rank_Reg_R2"]<- s1$r.squared# R^2 (Growth Plot)
    df2[j,"Rank_Reg_beta_slope"]<- s1$coefficients[2]
    df2[j,"Rank_Reg_Lambda_intercept"]<- exp(s1$coefficients[1])
    lm2<- lm(df3$lnAgetoFailure~ log  (df3$lnMedRank)) # deleted log for df$lnMedRank?????????
    s2<- summary(lm2)
    df2[j,"R2"]<- s2$r.squared# R^2 (Growth Plot)    
    df2[j,"BETA"]<- 1/s2$coefficients[2] #slope           #############   BETA    ##############
    lm3<- lm(log(df3$RankedAgeToFailure)~ log(df3$lnMedRank))
    s3<- summary(lm3)
    df2[j,"ETA"] <-  exp(s3$coefficients[1] )#intercept   #############   ETA    ###############
    
    Failure.Probability <- (pweibull( 1:101, shape = df2[j,"BETA"] , scale= df2[j,"ETA"],lower = TRUE))
    impact_Cost<- mean(ti)* Failure.Probability 
    
    
    
    impact_Cost<- mean(ti)* Failure.Probability 
    
    impact_Cost_rn_max_count[(j+1)]<- impact_Cost
    n<- paste0( Risk2$Risk[j],"_rn_max_count")
    names(impact_Cost_rn_max_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    j<- j+1
    
  }
  
  
  
  IR_impact_Cost_ru_mean<- as.data.frame(xx)
  IR_impact_Cost_ru_max<- as.data.frame(xx)
  IR_impact_Cost_ru_mean_count<- as.data.frame(xx)
  IR_impact_Cost_ru_max_count<- as.data.frame(xx)
  IR_impact_Cost_rn_mean<- as.data.frame(xx)
  IR_impact_Cost_rn_max<- as.data.frame(xx)
  IR_impact_Cost_rn_mean_count<- as.data.frame(xx)
  IR_impact_Cost_rn_max_count<- as.data.frame(xx)
  
  # i<- i+1
  
  j<- 1
  #j<- 4
  for (j in 1:length(Risk$Risk)) {
    
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************       MEAN UNIFORM  DISTRIBUTION           *****************************# 
    #***************************************** ***************************************** ***************************************** 
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"] ,Risk[j,"PI.High_mean"],(((Risk[j,"PI.Low_mean"] )+(Risk[j,"PI.High_mean"]))/2))
    sort(ti)
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_ru_mean[j+1]<- impact_Cost
    n<- paste0( "IR_", Risk$Risk[j],"_ru_mean")
    names(IR_impact_Cost_ru_mean)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************           MEAN  NORMAL DISTRIBUTION         *****************************# 
    
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"] ,Risk[j,"PI.High_mean"],(((Risk[j,"PI.Low_mean"] )+(Risk[j,"PI.High_mean"]))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_rn_mean[j+1]<- impact_Cost
    n<- paste0( "IR_", Risk$Risk[j],"_rn_mean")
    names(IR_impact_Cost_rn_mean)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    #***************************************** ***************************************** ***************************************** 
    #          ***************************************    MEAN  UNIFORM DISTRIBUTION/ ASSET COUNT   *****************************# 
    #***************************************** ***************************************** ***************************************** 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"]/Asset.count ,Risk[j,"PI.High_mean"]/Asset.count,(((Risk[j,"PI.Low_mean"]/Asset.count )+(Risk[j,"PI.High_mean"]/Asset.count))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_ru_mean_count[j+1]<- impact_Cost
    n<- paste0("IR_",  Risk$Risk[j],"_ru_mean_count")
    names(IR_impact_Cost_ru_mean_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          ***************************************    MEAN  NORMAL DISTRIBUTION/ ASSET COUNT   *****************************# 
    #***************************************** ***************************************** *****************************************
    
    ti<- triangle::rltriangle(101,Risk[j,"PI.Low_mean"]/Asset.count ,Risk[j,"PI.High_mean"]/Asset.count,(((Risk[j,"PI.Low_mean"]/Asset.count )+(Risk[j,"PI.High_mean"]/Asset.count))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_rn_mean_count[j+1]<- impact_Cost
    n<- paste0("IR_",  Risk$Risk[j],"_rn_mean_count")
    names(IR_impact_Cost_rn_mean_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************       MAX UNIFORM  DISTRIBUTION            *****************************# 
    #***************************************** ***************************************** ***************************************** 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk2[j,"Asset"], "-", Risk[j,"Risk"]))
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"] ,Risk2[j,"PI.High_max"],(((Risk2[j,"PI.Low_max"] )+(Risk2[j,"PI.High_max"]))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_ru_max[j+1]<- impact_Cost
    n<- paste0("IR_",  Risk2$Risk[j],"_ru_max") 
    names(IR_impact_Cost_ru_max)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          *****************************************           MAX  NORMAL  DISTRIBUTION         *****************************# 
    #***************************************** ***************************************** ***************************************** 
    
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"] ,Risk2[j,"PI.High_max"],(((Risk2[j,"PI.Low_max"] )+(Risk2[j,"PI.High_max"]))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    IR_impact_Cost_rn_max[j+1]<- impact_Cost
    n<- paste0( Risk2$Risk[j],"_rn_max") 
    names(IR_impact_Cost_rn_max)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    
    #***************************************** ***************************************** ***************************************** 
    #          *************************************       MAX UNIFORM  DISTRIBUTION /ASSET COUNT           *********************# 
    #***************************************** ***************************************** ***************************************** 
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"]/Asset.count ,Risk2[j,"PI.High_max"]/Asset.count,(((Risk2[j,"PI.Low_max"]/Asset.count )+(Risk2[j,"PI.High_max"]/Asset.count))/2))
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_ru_max_count[j+1]<- impact_Cost
    n<- paste0("IR_",  Risk2$Risk[j],"_ru_max_count")
    names(IR_impact_Cost_ru_max_count)[j+1]<- n
    
    plot(mean(ti)* Failure.Probability ,col=color[i],main = paste0(AC[i],"\n", Risk$Risk[j], "\nMean Impact: ",round(mean(ti),2),  "  BETA: ", round(df2[j,"BETA"],2),  "  ETA: ", round(df2[j,"ETA"],2) ),
         xlab = n)
    #***************************************** ***************************************** ***************************************** 
    #          *************************************       MAX NORMAL  DISTRIBUTION /ASSET COUNT           *********************# 
    #***************************************** ***************************************** ***************************************** 
    #hist(rpbeta,probability=TRUE,col= color[j]   ,main= paste0("Shape = ",round(a,2), " ,Scale = ",round(b,2)), xlab = paste0(Risk2[j,"Asset"], "-", Risk2[j,"Risk"]))
    
    ti<- triangle::rltriangle(101,Risk2[j,"PI.Low_max"]/Asset.count ,Risk2[j,"PI.High_max"]/Asset.count,(((Risk2[j,"PI.Low_max"]/Asset.count )+(Risk2[j,"PI.High_max"]/Asset.count))/2))
    
    if (AC[i] == "Circuit Breaker") {
      Failure.Probability <-   pweibull(1:101 , shape = 3.6 , scale= 31 )
    } else if ( AC[i] == "Generator Winding, Multi-turn Coil, Stator" ) {
      Failure.Probability <-  pweibull(1:101 , shape = 3.2 , scale= 40 )
    } else if (AC[i] == "Substation Transformer") {
      Failure.Probability <-   pweibull(1:101, shape = 3.3 , scale= 66 )
    }   else if (AC[i] == "Turbine, Runner" ) {
      Failure.Probability <-   pweibull(1:101 , shape = 3 , scale= 102 )
    }  else {
      print("5")  #df$Failure.Probability <-    pweibull(x , shape = 1 , scale= 1 )
    }
    
    impact_Cost<- mean(ti)* Failure.Probability 
    IR_impact_Cost_rn_max_count[j +1]<- impact_Cost
    n<- paste0("IR_", Risk2$Risk[j],"_rn_max_count")
    names(IR_impact_Cost_rn_max_count)[j +1]<- n
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    
    j<- j+1
    
    print(j)
    
  }
  impact_Cost_ru_mean_count$age<- xx*100
  
  Risk$Asset<- AC[i]
  WLCM[[i]]<- Risk
  cname<- paste0(i,"_Risk_",AC[i])
  names(WLCM)[i] <- cname
  
  
  impact_Cost_ru_mean$Total_Impact_ru_mean <-rowSums (impact_Cost_ru_mean[-1], na.rm = FALSE, dims = 1)
  impact_Cost_ru_mean$Mean_Impact_ru_mean <- rowMeans( impact_Cost_ru_mean[-c(1,(  length(impact_Cost_ru_mean)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_ru_mean$Asset<- AC[i]
  impact_Cost_ru_mean$Prob<- "ru_mean_Impact_Cost"
  impact_Cost_ru_mean$x<- xx
  impact_Cost_ru_mean$age<- xx*100
  WLCM[[length(AC)+i]]<- impact_Cost_ru_mean  
  cname<- paste0(i, "_ru_mean_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)+i] <- cname
  
  impact_Cost_rn_mean$Total_Impact_rn_mean <- rowSums (impact_Cost_rn_mean[-1], na.rm = FALSE, dims = 1)
  impact_Cost_rn_mean$Mean_Impact_rn_mean <- rowMeans(impact_Cost_rn_mean[-c(1,(  length(impact_Cost_rn_mean)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_rn_mean$Asset<- AC[i]
  impact_Cost_rn_mean$Prob<- "rn_mean_Impact_Cost"
  impact_Cost_rn_mean$x<- xx
  impact_Cost_rn_mean$age<- xx*100
  WLCM[[length(AC)*2+i]]<- impact_Cost_rn_mean  
  cname<- paste0(i, "_rn_mean_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*2+i] <- cname
  
  impact_Cost_ru_mean_count$Total_Impact_ru_mean_count <-rowSums (impact_Cost_ru_mean_count[-1], na.rm = FALSE, dims = 1)
  impact_Cost_ru_mean_count$Mean_Impact_ru_mean_count <-rowMeans(impact_Cost_ru_mean_count[-c(1,(  length(impact_Cost_ru_mean_count)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_ru_mean_count$Asset<- AC[i]
  impact_Cost_ru_mean_count$Prob<- "ru_mean_count_Impact_Cost"
  impact_Cost_ru_mean_count$x<- xx
  impact_Cost_ru_mean_count$age<- xx*100
  WLCM[[length(AC)*3+i]]<- impact_Cost_ru_mean_count  
  cname<- paste0(i, "_ru_mean_count_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*3+i] <- cname
  
  impact_Cost_rn_mean_count$Total_Impact_rn_mean_count <-rowSums (impact_Cost_rn_mean_count[-1], na.rm = FALSE, dims = 1)
  impact_Cost_rn_mean_count$Mean_Impact_rn_mean_count <-rowMeans (impact_Cost_rn_mean_count[-c(1,(  length(impact_Cost_rn_mean_count)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_rn_mean_count$Asset<- AC[i]
  impact_Cost_rn_mean_count$Prob<- "rn_mean_count_Impact_Cost"
  impact_Cost_rn_mean_count$x<- xx
  impact_Cost_rn_mean_count$age<- xx*100
  WLCM[[length(AC)*4+i]]<- impact_Cost_rn_mean_count  
  cname<- paste0(i, "_rn_mean_count_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*4+i] <- cname
  
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  impact_Cost_ru_max$Total_Impact_ru_max <-rowSums (impact_Cost_ru_max[-1], na.rm = FALSE, dims = 1)
  impact_Cost_ru_max$Mean_Impact_ru_max <-rowMeans (impact_Cost_ru_max[-c(1,(  length(impact_Cost_ru_max)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_ru_max$Asset<- AC[i]
  impact_Cost_ru_max$Prob<- "ru_max_Impact_Cost"
  impact_Cost_ru_max$x<- xx
  impact_Cost_ru_max$age<- xx*100
  WLCM[[length(AC)*5+i]]<- impact_Cost_ru_max  
  cname<- paste0(i, "_ru_max_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*5+i] <- cname
  
  impact_Cost_rn_max$Total_Impact_rn_max <-rowSums (impact_Cost_rn_max[-1], na.rm = FALSE, dims = 1)
  impact_Cost_rn_max$Mean_Impact_rn_max <-rowMeans (impact_Cost_rn_max[-c(1,(  length(impact_Cost_rn_max)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_rn_max$Asset<- AC[i]
  impact_Cost_rn_max$Prob<- "rn_max_Impact_Cost"
  impact_Cost_rn_max$x<- xx
  impact_Cost_rn_max$age<- xx*100
  WLCM[[length(AC)*6+i]]<- impact_Cost_rn_max  
  cname<- paste0(i, "_rn_max_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*6+i] <- cname
  
  impact_Cost_ru_max_count$Total_Impact_ru_max_count <-rowSums (impact_Cost_ru_max_count[-1], na.rm = FALSE, dims = 1)
  impact_Cost_ru_max_count$Mean_Impact_ru_max_count <-rowMeans (impact_Cost_ru_max_count[-c(1,(  length(impact_Cost_ru_max_count)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_ru_max_count$Asset<- AC[i]
  impact_Cost_ru_max_count$Prob<- "ru_max_count_Impact_Cost"
  impact_Cost_ru_max_count$x<- xx
  impact_Cost_ru_max_count$age<- xx*100
  WLCM[[length(AC)*7+i]]<- impact_Cost_ru_max_count  
  cname<- paste0(i, "_ru_max_count_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*7+i] <- cname
  
  impact_Cost_rn_max_count$Total_Impact_rn_max_count <-rowSums (impact_Cost_rn_max_count[-1], na.rm = FALSE, dims = 1)
  impact_Cost_rn_max_count$Mean_Impact_rn_max_count <-rowMeans (impact_Cost_rn_max_count[-c(1,(  length(impact_Cost_rn_max_count)))], na.rm = FALSE, dims = 1)
  
  impact_Cost_rn_max_count$Asset<- AC[i]
  impact_Cost_rn_max_count$Prob<- "rn_max_count_Impact_Cost"
  impact_Cost_rn_max_count$x<- xx
  impact_Cost_rn_max_count$age<- xx*100
  WLCM[[length(AC)*8+i]]<- impact_Cost_rn_max_count  
  cname<- paste0(i, "_rn_max_count_Impact_Cost_", AC[i])
  names(WLCM)[length(AC)*8+i] <- cname
  
  
  IR_impact_Cost_ru_mean$IR_Total_Impact_ru_mean <-rowSums (IR_impact_Cost_ru_mean[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_ru_mean$IR_Mean_Impact_ru_mean <- rowMeans( IR_impact_Cost_ru_mean[-c(1,(  length(IR_impact_Cost_ru_mean)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_ru_mean$Asset<- AC[i]
  IR_impact_Cost_ru_mean$Prob<- "ru_IR_Mean_Impact_Cost"
  IR_impact_Cost_ru_mean$x<- xx
  IR_impact_Cost_ru_mean$age<- xx*100
  WLCM[[length(AC)*9+i]]<- IR_impact_Cost_ru_mean  
  cname<- paste0(i, "_ru_mean_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*9+i] <- cname
  
  IR_impact_Cost_rn_mean$IR_Total_Impact_rn_mean <- rowSums (IR_impact_Cost_rn_mean[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_rn_mean$IR_Mean_Impact_rn_mean <- rowMeans(IR_impact_Cost_rn_mean[-c(1,(  length(IR_impact_Cost_rn_mean)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_rn_mean$Asset<- AC[i]
  IR_impact_Cost_rn_mean$Prob<- "rn_IR_Mean_Impact_Cost"
  IR_impact_Cost_rn_mean$x<- xx
  IR_impact_Cost_rn_mean$age<- xx*100
  WLCM[[length(AC)*10+i]]<- IR_impact_Cost_rn_mean  
  cname<- paste0(i, "_rn_mean_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*10+i] <- cname
  
  IR_impact_Cost_ru_mean_count$IR_Total_Impact_ru_mean_count <-rowSums (IR_impact_Cost_ru_mean_count[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_ru_mean_count$IR_Mean_Impact_ru_mean_count <-rowMeans(IR_impact_Cost_ru_mean_count[-c(1,(  length(IR_impact_Cost_ru_mean_count)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_ru_mean_count$Asset<- AC[i]
  IR_impact_Cost_ru_mean_count$Prob<- "ru_IR_mean_count_Impact_Cost"
  IR_impact_Cost_ru_mean_count$x<- xx
  IR_impact_Cost_ru_mean_count$age<- xx*100
  WLCM[[length(AC)*11+i]]<- IR_impact_Cost_ru_mean_count  
  cname<- paste0(i, "_ru_mean_count_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*11+i] <- cname
  
  IR_impact_Cost_rn_mean_count$IR_Total_Impact_rn_mean_count <-rowSums (IR_impact_Cost_rn_mean_count[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_rn_mean_count$IR_Mean_Impact_rn_mean_count <-rowMeans (IR_impact_Cost_rn_mean_count[-c(1,(  length(IR_impact_Cost_rn_mean_count)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_rn_mean_count$Asset<- AC[i]
  IR_impact_Cost_rn_mean_count$Prob<- "rn_IR_mean_count_Impact_Cost"
  IR_impact_Cost_rn_mean_count$x<- xx
  IR_impact_Cost_rn_mean_count$age<- xx*100
  WLCM[[length(AC)*12+i]]<- IR_impact_Cost_rn_mean_count  
  cname<- paste0(i, "_rn_mean_count_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*12+i] <- cname
  
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  IR_impact_Cost_ru_max$IR_Total_Impact_ru_max <-rowSums (IR_impact_Cost_ru_max[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_ru_max$IR_Mean_Impact_ru_max <-rowMeans (IR_impact_Cost_ru_max[-c(1,(  length(IR_impact_Cost_ru_max)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_ru_max$Asset<- AC[i]
  IR_impact_Cost_ru_max$Prob<- "ru_IR_max_Impact_Cost"
  IR_impact_Cost_ru_max$x<- xx
  IR_impact_Cost_ru_max$age<- xx*100
  WLCM[[length(AC)*13+i]]<- IR_impact_Cost_ru_max  
  cname<- paste0(i, "_ru_max_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*13+i] <- cname
  
  IR_impact_Cost_rn_max$IR_Total_Impact_rn_max <-rowSums (IR_impact_Cost_rn_max[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_rn_max$IR_Mean_Impact_rn_max <-rowMeans (IR_impact_Cost_rn_max[-c(1,(  length(IR_impact_Cost_rn_max)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_rn_max$Asset<- AC[i]
  IR_impact_Cost_rn_max$Prob<- "rn_IR_max_Impact_Cost"
  IR_impact_Cost_rn_max$x<- xx
  IR_impact_Cost_rn_max$age<- xx*100
  WLCM[[length(AC)*14+i]]<- IR_impact_Cost_rn_max  
  cname<- paste0(i, "_rn_max_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*14+i] <- cname
  
  IR_impact_Cost_ru_max_count$IR_Total_Impact_ru_max_count <-rowSums (IR_impact_Cost_ru_max_count[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_ru_max_count$IR_Mean_Impact_ru_max_count <-rowMeans (IR_impact_Cost_ru_max_count[-c(1,(  length(IR_impact_Cost_ru_max_count)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_ru_max_count$Asset<- AC[i]
  IR_impact_Cost_ru_max_count$Prob<- "ru_IR_max_count_Impact_Cost"
  IR_impact_Cost_ru_max_count$x<- xx
  IR_impact_Cost_ru_max_count$age<- xx*100
  WLCM[[length(AC)*15+i]]<- IR_impact_Cost_ru_max_count  
  cname<- paste0(i, "_ru_max_count_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*15+i] <- cname
  
  IR_impact_Cost_rn_max_count$IR_Total_Impact_rn_max_count <-rowSums (IR_impact_Cost_rn_max_count[-1], na.rm = FALSE, dims = 1)
  IR_impact_Cost_rn_max_count$IR_Mean_Impact_rn_max_count <-rowMeans (IR_impact_Cost_rn_max_count[-c(1,(  length(IR_impact_Cost_rn_max_count)))], na.rm = FALSE, dims = 1)
  
  IR_impact_Cost_rn_max_count$Asset<- AC[i]
  IR_impact_Cost_rn_max_count$Prob<- "rn_IR_max_count_Impact_Cost"
  IR_impact_Cost_rn_max_count$x<- xx
  IR_impact_Cost_rn_max_count$age<- xx*100
  WLCM[[length(AC)*16+i]]<- IR_impact_Cost_rn_max_count  
  cname<- paste0(i, "_rn_max_count_IR_impact_Cost_", AC[i])
  names(WLCM)[length(AC)*16+i] <- cname
  
  
  i<- i+1
}


#melt(data = x, id.vars = c("id","id2"), measure.vars = c("blue",", red"))




# 
# measure<- colnames(WLCM[[36]][!names(WLCM[[36]])%in% c("x","Asset","prob")])
# melt(data = WLCM[[36]], id.vars = id, measure.vars = measure)
# 




allrisks<- data.frame()
id<- c("x","Asset","Prob","age","xx")
i<- 5
for (i in 5:(length(AC)*17)) {
  
  measure<- colnames(WLCM[[i]][!names(WLCM[[i]])%in% c("x","Asset","Prob","age","xx")])
  allr<-  melt(data = WLCM[[i]], id.vars = id, measure.vars = measure)
  allrisks<- rbind(allr, allrisks)
  print(i)
}

write.csv(allrisks,"All_Risks_Test.csv")

