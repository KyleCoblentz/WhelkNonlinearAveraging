#################################################################################
### Handling Time variation analysis

### use variation in handling times from the lab experiment to examine how much
### mean feeding rates would increase in the cages if whelks varied in their 
### handling times as much as in the lab experiment where we purposefully 
### maximized variation
#################################################################################

### load libraries

library(dplyr); library(ggplot2); library(cowplot); 

### load data from handling time experiment

HandlingData <- read.csv('NucellaHandlingTimes.csv')

### manipulate data

### clean up column names 

colnames(HandlingData) <- c('CameraTankID', 'RepeatObs', 'Temp', 'PredSp', 'PredSize', 'PreySp', 'PreySize',
                        'StartDateTime', 'EndDateTime', 'HandleType', 'FleshRemaining', 'Recorder', 'Comments')

### get standard deviations of handling times observed for barnacles and mussels

### put date and time into a standard format

HandlingData$StartDateTime <- strptime(HandlingData$StartDateTime, format='%m/%d/%y %H:%M')

HandlingData$EndDateTime <- strptime(HandlingData$EndDateTime, format='%m/%d/%y %H:%M')

### get time spent handling

HandlingData$HandlingTime <- HandlingData$EndDateTime-HandlingData$StartDateTime

### can drop Start and End DateTimes

HandlingData <- HandlingData %>% select(c(-StartDateTime, -EndDateTime))

### convert time to days?

HandlingData$HandlingTime <- as.numeric(HandlingData$HandlingTime)/60/24

### filter data to only include Nucella ostrina as predator

HandlingData <- HandlingData %>% filter(PredSp == 'No')

### get data set of only Balanus glandula as prey

HandlingDataBg <- HandlingData %>% filter(PreySp == 'B. glandula')

### same for Mytilus trossulus with the removal of prey that weren't mostly

HandlingDataMt <- HandlingData %>% filter(PreySp == 'M. trossulus' & FleshRemaining <= 20)

### get standard deviations of handling times for both prey

SDBG <- sd(HandlingDataBg$HandlingTime)

SDMT <- sd(HandlingDataMt$HandlingTime)

### set up simulations for estimating the effects of handling time variation on 
### mean feeding rates

### source function to perform the simulations -- R file for function must be in
### the same working directory

source('HandlingTimeSimFunction.R')

### this sources the function handlevarsim which requires arguments: 
### nsim, nind, prey1dens, prey1attack, prey2dens, prey2attack, prey1meanhandling, prey2meanhandling, sdeviationprey1, sdeviationprey2

### load the results from the attack rate variation analysis

Data <- read.csv('HandlingAnalysisData.csv')

### Now we can use the handlevarsim function to see what the effects of handling time
### variation would be on feeding rates if it was the same magnitude as the variation 
### in handling times observed in the lab handling time experiment

### Cage 1

HandleSim1 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[1], prey1dens = Data$BGland[1], 
             prey1attack = Data$BAttack[1], prey2dens = Data$MytTross[1], 
             prey2attack = Data$MAttack[1], prey1meanhandling = Data$BarnacleHandle[1],
             prey2meanhandling = Data$MusselHandle[1], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 2

HandleSim2 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[2], prey1dens = Data$BGland[2], 
                                 prey1attack = Data$BAttack[2], prey2dens = Data$MytTross[2], 
                                 prey2attack = Data$MAttack[2], prey1meanhandling = Data$BarnacleHandle[2],
                                 prey2meanhandling = Data$MusselHandle[2], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 3

HandleSim3 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[3], prey1dens = Data$BGland[3], 
                                 prey1attack = Data$BAttack[3], prey2dens = Data$MytTross[3], 
                                 prey2attack = Data$MAttack[3], prey1meanhandling = Data$BarnacleHandle[3],
                                 prey2meanhandling = Data$MusselHandle[3], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 4

HandleSim4 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[4], prey1dens = Data$BGland[4], 
                                 prey1attack = Data$BAttack[4], prey2dens = Data$MytTross[4], 
                                 prey2attack = Data$MAttack[4], prey1meanhandling = Data$BarnacleHandle[4],
                                 prey2meanhandling = Data$MusselHandle[4], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 5

HandleSim5 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[5], prey1dens = Data$BGland[5], 
                                 prey1attack = Data$BAttack[5], prey2dens = Data$MytTross[5], 
                                 prey2attack = Data$MAttack[5], prey1meanhandling = Data$BarnacleHandle[5],
                                 prey2meanhandling = Data$MusselHandle[5], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 6

HandleSim6 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[6], prey1dens = Data$BGland[6], 
                                 prey1attack = Data$BAttack[6], prey2dens = Data$MytTross[6], 
                                 prey2attack = Data$MAttack[6], prey1meanhandling = Data$BarnacleHandle[6],
                                 prey2meanhandling = Data$MusselHandle[6], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 7

HandleSim7 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[7], prey1dens = Data$BGland[7], 
                                 prey1attack = Data$BAttack[7], prey2dens = Data$MytTross[7], 
                                 prey2attack = Data$MAttack[7], prey1meanhandling = Data$BarnacleHandle[7],
                                 prey2meanhandling = Data$MusselHandle[7], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 8

HandleSim8 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[8], prey1dens = Data$BGland[8], 
                                 prey1attack = Data$BAttack[8], prey2dens = Data$MytTross[8], 
                                 prey2attack = Data$MAttack[8], prey1meanhandling = Data$BarnacleHandle[8],
                                 prey2meanhandling = Data$MusselHandle[8], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 9

HandleSim9 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[9], prey1dens = Data$BGland[9], 
                                 prey1attack = Data$BAttack[9], prey2dens = Data$MytTross[9], 
                                 prey2attack = Data$MAttack[9], prey1meanhandling = Data$BarnacleHandle[9],
                                 prey2meanhandling = Data$MusselHandle[9], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 10

HandleSim10 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[10], prey1dens = Data$BGland[10], 
                                 prey1attack = Data$BAttack[10], prey2dens = Data$MytTross[10], 
                                 prey2attack = Data$MAttack[10], prey1meanhandling = Data$BarnacleHandle[10],
                                 prey2meanhandling = Data$MusselHandle[10], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 11

HandleSim11 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[11], prey1dens = Data$BGland[11], 
                                 prey1attack = Data$BAttack[11], prey2dens = Data$MytTross[11], 
                                 prey2attack = Data$MAttack[11], prey1meanhandling = Data$BarnacleHandle[11],
                                 prey2meanhandling = Data$MusselHandle[11], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 12

HandleSim12 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[12], prey1dens = Data$BGland[12], 
                                 prey1attack = Data$BAttack[12], prey2dens = Data$MytTross[12], 
                                 prey2attack = Data$MAttack[12], prey1meanhandling = Data$BarnacleHandle[12],
                                 prey2meanhandling = Data$MusselHandle[12], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 13

HandleSim13 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[13], prey1dens = Data$BGland[13], 
                                 prey1attack = Data$BAttack[13], prey2dens = Data$MytTross[13], 
                                 prey2attack = Data$MAttack[13], prey1meanhandling = Data$BarnacleHandle[13],
                                 prey2meanhandling = Data$MusselHandle[13], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 14

HandleSim14 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[14], prey1dens = Data$BGland[14], 
                                 prey1attack = Data$BAttack[14], prey2dens = Data$MytTross[14], 
                                 prey2attack = Data$MAttack[14], prey1meanhandling = Data$BarnacleHandle[14],
                                 prey2meanhandling = Data$MusselHandle[14], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 15

HandleSim15 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[15], prey1dens = Data$BGland[15], 
                                 prey1attack = Data$BAttack[15], prey2dens = Data$MytTross[15], 
                                 prey2attack = Data$MAttack[15], prey1meanhandling = Data$BarnacleHandle[15],
                                 prey2meanhandling = Data$MusselHandle[15], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 16

HandleSim16 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[16], prey1dens = Data$BGland[16], 
                                 prey1attack = Data$BAttack[16], prey2dens = Data$MytTross[16], 
                                 prey2attack = Data$MAttack[16], prey1meanhandling = Data$BarnacleHandle[16],
                                 prey2meanhandling = Data$MusselHandle[16], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 17

HandleSim17 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[17], prey1dens = Data$BGland[17], 
                                 prey1attack = Data$BAttack[17], prey2dens = Data$MytTross[17], 
                                 prey2attack = Data$MAttack[17], prey1meanhandling = Data$BarnacleHandle[17],
                                 prey2meanhandling = Data$MusselHandle[17], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 18

HandleSim18 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[18], prey1dens = Data$BGland[18], 
                                 prey1attack = Data$BAttack[18], prey2dens = Data$MytTross[18], 
                                 prey2attack = Data$MAttack[18], prey1meanhandling = Data$BarnacleHandle[18],
                                 prey2meanhandling = Data$MusselHandle[18], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 19

HandleSim19 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[19], prey1dens = Data$BGland[19], 
                                 prey1attack = Data$BAttack[19], prey2dens = Data$MytTross[19], 
                                 prey2attack = Data$MAttack[19], prey1meanhandling = Data$BarnacleHandle[19],
                                 prey2meanhandling = Data$MusselHandle[19], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### Cage 20

HandleSim20 <- apply(handlevarsim(nsim = 10000, nind = Data$Number[20], prey1dens = Data$BGland[20], 
                                 prey1attack = Data$BAttack[20], prey2dens = Data$MytTross[20], 
                                 prey2attack = Data$MAttack[20], prey1meanhandling = Data$BarnacleHandle[20],
                                 prey2meanhandling = Data$MusselHandle[20], sdeviationprey1 = SDBG, sdeviationprey2 = SDMT), 2, mean)

### collate results add to Data

NamesHandleSim <- paste0('HandleSim', 1:20)

HandleSimAll <- rbind(HandleSim1,  HandleSim2,  HandleSim3,  HandleSim4,  HandleSim5,  HandleSim6,  HandleSim7,  HandleSim8,  HandleSim9, 
                      HandleSim10, HandleSim11, HandleSim12, HandleSim13, HandleSim14, HandleSim15, HandleSim16, HandleSim17, HandleSim18,
                      HandleSim19, HandleSim20)

Data <- cbind(Data, HandleSimAll)

Data$NetFRDiffB <- -Data$DiffB + Data$Prey1FRDiffs

Data$NetFRDiffM <- -Data$DiffM + Data$Prey2FRDiffs





