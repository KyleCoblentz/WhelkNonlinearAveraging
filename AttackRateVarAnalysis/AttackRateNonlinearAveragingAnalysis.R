###################################################################################################
### whelk nonlinear averaging -- attack rate nonlinear averaging
###################################################################################################

### load packages 

library(dplyr); library(tidyr); library(rjags); library(ggplot2); library(cowplot); 

### set seed so exact numbers in manuscript can be repeated

set.seed(seed = 666)

##########################################################################################################
### Estimating Individual attack rates and nonlinear averaging
##########################################################################################################

### load feeding observations data

FeedData <- read.csv('Feeding_Data_Wide_AllInd.csv')

### manipulate data -- drop Ct, LotP, and START columns, add total number of feeding observations and total overall observations

FeedData <- FeedData %>% select(-c(Ct, LotP, START))  %>% mutate(TotalFeed = Bg + Mt, Total = Bg + Mt + NF)

### drop individuals with no feeding observations or less than 30 total observations

LessThan30 <- which(FeedData$Total < 30)

FeedData <- FeedData[-LessThan30, ]

# 550 of 719 individuals remain

### load resource density data 

Density <- read.csv('ResourceDensityData.csv')

### filter to weighted average densities

ResourceDensity <- Density %>% filter(Date == 'WtAverage')

### load individual handling time data 

IndHandling <- read.csv('Handling_data_AllInd.csv')

### drop individuals whose feeding events were dropped

IndHandling <- IndHandling[-LessThan30, ]

IndHandling <- select(IndHandling, -c(Ct, LotP))

### set up other data needed for the model

Number <- FeedData %>% group_by(Cage) %>% summarise(Number = n())

ResourceInd <- ResourceDensity[rep(row.names(ResourceDensity), times = Number$Number),]

### start and end indices for each cage

CageStart <- cumsum(Number$Number) - Number$Number + 1

CageEnd <- cumsum(Number$Number)

### Cage Handling

CageHandling <- IndHandling %>% group_by(Cage) %>% select(-c(X, WhelkID)) %>% summarise_all(.funs = mean)


### write JAGS model to estimate individual feeding and attack rates on barnacles and mussels

sink('feedingrateind.txt')
cat('
    model{
    # Bayesian model to estimate proportions of feeding and nonfeeding events
    for (i in 1:N){
    yik[i, 1:J] ~ dmulti(pik[i, 1:J], nik[i]) # individual observations used to estimate pik 
    }
    for (i in 1:N){
    pik[i, 1:J] ~ ddirch(alphak[cage[i], 1:J]) # individual proportions are nested in cage proportions pk
    }
    for (k in 1:K) {
    for (j in 1:J) {
    alphak[k, j] <- pk[k, j]
    }
    }
    for(k in 1:K){
    pk[k, 1:J] ~ ddirch(alpha[])
    }
    for (j in 1:J) {
    alpha[j] <- 1
    }
    
    # calculate individual attack rates
    
    for(i in 1:N){
    for(s in 1:S){
    aiks[i,s] <- pik[i,s]/(pik[i,3] * h[i,s] * R[i,s])
    }
    }
    
    # calculate the overall mean attack rates -- for estimating experiment level nonlinear averaging
    
    for(s in 1:S){
    meanaiks[s] <-  mean(aiks[,s])
    } 
    
    # estimate individual level feeding rates
    
    for(i in 1:N){
    for(s in 1:S){
    fiks[i,s] <- aiks[i,s]*R[i,s]/(1 + aiks[i,1] * R[i,1] * h[i,1] + aiks[i,2] * R[i,2] * h[i,2]) 
    }
    }
    
    # cage mean attack rates
    
    for(k in 1:K){
    for(s in 1:S){
    aks[k,s] <- mean(aiks[CageStart[k]:CageEnd[k],s])
    }
    }
    
    # cage mean feeding rates from individual feeding rates 
    
    for(k in 1:K){
    for(s in 1:S){
    fks[k,s] <- mean(fiks[CageStart[k]:CageEnd[k],s])
    }
    }
    
    # cage mean feeding rates using mean attack rate for each cage
    
    for(k in 1:K){
    for(s in 1:S){
    meanfks[k,s] <- aks[k,s] * CageR[k,s]/ (1 + aks[k,1] * Cageh[k,1] * CageR[k,1] + aks[k,2] * Cageh[k,2] * CageR[k,2])
    }
    }
    
    # differences in feeding rate estimates
    
    for(k in 1:K){
    for(s in 1:S){
    Diff[k,s] <- meanfks[k,s] - fks[k,s]
    }
    }
    
    # estimates of experiment-level mean feeding rate with: 1) variation in attack rates and resource densities
    # 2) attack rate variation and mean resource densities
    # 3) mean attack rate and variation in resource densities
    # 4) mean attack rates and mean resource densities
    # 5) cage mean attack rate and variation in resource densities
    
    # 1)
    for(s in 1:S){
    meanf[s] <- mean(fiks[,s])
    }
    
    # 2)
    for(i in 1:N){
    for (s in 1:S){
    fimeanR[i,s] <- aiks[i,s]*meanR[s]/(1 + aiks[i,1] * meanR[1] * h[i,1] + aiks[i,2] * meanR[2] * h[i,2])
    }
    }
    for(s in 1:S){
    fmeanR[s] <- mean(fimeanR[,s])
    }
    
    # 3)
    for (i in 1:N){
    for(s in 1:S){
    aiNoaVar[i,s] <- meanaiks[s]
    }
    }
    for(i in 1:N){
    for(s in 1:S){
    fiNoaVar[i,s] <- aiNoaVar[i,s] * R[i,s]/(1 + aiNoaVar[i,1] * R[i,1] * h[i,1] + aiNoaVar[i,2] * R[i,2] * h[i,2])
    }
    }
    for(s in 1:S){
    meanfNoaVar[s] <- mean(fiNoaVar[,s])
    }
    # 4)
    for(s in 1:S){
    meanfNoVar[s] <- meanaiks[s] * meanR[s]/(1 + meanaiks[1] * meanR[1] * h[1,1] + meanaiks[2] * meanR[2] * h[1,2])
    }
    }', fill = TRUE)
sink()

### define data list for model

jags_indfeedrate_data <- list(yik = data.matrix(FeedData[,4:6]), nik = FeedData[,8], h = data.matrix(IndHandling[4:5]), cage = as.numeric(as.factor(FeedData[,3])), 
                              R = data.matrix(ResourceInd[,3:4]), J = 3, S = 2, N = length(FeedData[,1]), K = 20,
                              meanR = c(mean(ResourceDensity$BGland), mean(ResourceDensity$MytTross)), 
                              CageStart = CageStart, CageEnd = CageEnd, Cageh = data.matrix(CageHandling[,2:3]),
                              CageR = data.matrix(ResourceDensity[,3:4]))

### burn in 
                              
jags_indfeedrate <- jags.model("feedingrateind.txt", data = jags_indfeedrate_data,
                                                             n.chains = 3, n.adapt = 100000)
                              
### parameters to sample

params_indfeedrate <- c('pk', 'pik',  'fiks', 'aiks', 'meanaiks', 'meanf', 'fmeanR', 'meanfNoaVar', 'meanfNoVar', 'aks', 'fks',
                        'meanfks', 'Diff')

### sample parameters

samples_indfeedrate <- jags.samples(jags_indfeedrate, variable.names = params_indfeedrate, n.iter = 2000, 
                                    n.thin = 100)

### extract estimates from posterior samples 

IndAttackData <- data.frame(BAttack = apply(samples_indfeedrate$aiks, c(1,2), mean)[,1], 
                            MAttack = apply(samples_indfeedrate$aiks, c(1,2), mean)[,2],
                            BFeed = apply(samples_indfeedrate$fiks, c(1,2), mean)[,1],
                            MFeed = apply(samples_indfeedrate$fiks, c(1,2), mean)[,2], 
                            Cage = as.factor(FeedData$Cage))

MeanAttackData <- data.frame(BAttack = apply(samples_indfeedrate$aks, c(1,2), mean)[,1],
                             MAttack = apply(samples_indfeedrate$aks, c(1,2), mean)[,2],
                             BFeed = apply(samples_indfeedrate$fks, c(1,2), mean)[,1],
                             MFeed = apply(samples_indfeedrate$fks, c(1,2), mean)[,2],
                             EstFeedB = apply(samples_indfeedrate$meanfks, c(1,2), mean)[,1],
                             EstFeedM = apply(samples_indfeedrate$meanfks, c(1,2), mean)[,2],
                             DiffB = apply(samples_indfeedrate$Diff, c(1,2), mean)[,1],
                             DiffM = apply(samples_indfeedrate$Diff, c(1,2), mean)[,2],
                             Cage = as.factor(ResourceDensity$Cage))


