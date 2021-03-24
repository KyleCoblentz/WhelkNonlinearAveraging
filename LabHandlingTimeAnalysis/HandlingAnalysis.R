### handling time analyses for whelk data

### load libraries

library(rstan); library(rstanarm); library(dplyr); library(ggplot2)

### load data

data_all <- read.csv('NucellaHandlingTimes.csv')

str(data_all)

### clean up column names 

colnames(data_all) <- c('CameraTankID', 'RepeatObs', 'Temp', 'PredSp', 'PredSize', 'PreySp', 'PreySize',
                        'StartDateTime', 'EndDateTime', 'HandleType', 'FleshRemaining', 'Recorder', 'Comments')

str(data_all)

### put date and time into a standard format

data_all$StartDateTime <- strptime(data_all$StartDateTime, format='%m/%d/%y %H:%M')

data_all$EndDateTime <- strptime(data_all$EndDateTime, format='%m/%d/%y %H:%M')

### get time spent handling

data_all$HandlingTime <- data_all$EndDateTime-data_all$StartDateTime

### can drop Start and End DateTimes

data_all <- data_all %>% select(c(-StartDateTime, -EndDateTime))


### convert time to days?

data_all$HandlingTime <- as.numeric(data_all$HandlingTime)/60/24


hist(data_all$HandlingTime)

### Nucella ostrina -- Balanus glandula

ostrina_data <- data_all %>% filter(PredSp == 'No')

ostrina_balanus_data <- ostrina_data %>% filter(PreySp == 'B. glandula')

### plot some relationships

qplot(data = ostrina_balanus_data, x = FleshRemaining, y = HandlingTime)

### want to drop all of the data where individuals ate less than 
### 90% of the barnacle

ostrina_balanus_data <- ostrina_balanus_data %>% filter(FleshRemaining <= 10)

### look at data to see if any transformations are necessary

hist(log(ostrina_balanus_data$HandlingTime))

### handling time needs log transformed

hist(ostrina_balanus_data$Temp)

hist(log(ostrina_balanus_data$PredSize))

hist(log(ostrina_balanus_data$PreySize))


### write linear mixed effects model

Balanus_model <- stan_lmer(log(HandlingTime) ~ log(PredSize) + log(PreySize) + HandleType + (1|CameraTankID), data = ostrina_balanus_data)

hist(Balanus_model$residuals)

plot(Balanus_model, 'trace')

summary(Balanus_model)

### model mussel handling time

ostrina_mytilus_data <- ostrina_data %>% filter(PreySp == 'M. trossulus')

### take a look at the data

qplot(data = ostrina_mytilus_data, x = FleshRemaining, y = HandlingTime)

### remove all of the observations where more than 25% of the flesh was left

ostrina_mytilus_data <- ostrina_mytilus_data %>% filter(FleshRemaining <= 25)

### not enough repeat observations to run mixed effects model

### take a look at variables to see if transformations are necessary

qplot(data = ostrina_mytilus_data, x = log(HandlingTime))

qplot(data = ostrina_mytilus_data, x = log(PreySize))
  
qplot(data = ostrina_mytilus_data, x = log(PredSize))

qplot(data = ostrina_mytilus_data, x = Temp)

### fit multiple regression to data

Mytilus_model <- stan_lm(log(HandlingTime) ~ log(PreySize) + log(PredSize) + HandleType, data = ostrina_mytilus_data, prior = R2(location = 0.7))

hist(Mytilus_model$residuals)

summary(Mytilus_model)

### how many observations do we have for the other prey species?

ostrina_Ctham_data <- ostrina_data %>% filter(PreySp == 'C. dalli')

ostrina_Lottia_data <- ostrina_data %>% filter(PreySp == 'L. asmi')

ostrina_Semi_data <- ostrina_data %>% filter(PreySp == 'S. cariosus')

### probably have enough data for Cthamalus and Semibalanus

### let's check out the Cthamalus data

qplot(data = ostrina_Ctham_data, x = log(HandlingTime))

qplot(data = ostrina_Ctham_data, x = Temp)

qplot(data = ostrina_Ctham_data, x = log(PreySize))

qplot(data = ostrina_Ctham_data, x = log(PredSize))

### look at relationships

qplot(data = ostrina_Ctham_data, y = log(HandlingTime), x = Temp)

qplot(data = ostrina_Ctham_data, y = log(HandlingTime), x = log(PreySize))

qplot(data = ostrina_Ctham_data, y = log(HandlingTime), x = log(PredSize))

### only one barnacle was drilled, not sure if we'll be able to estimate an effect ... 

### specify model

Ctham_Fit <- stan_lmer(log(HandlingTime) ~ log(PreySize) + log(PredSize) + HandleType + (1|CameraTankID), data = ostrina_Ctham_data)


hist(Ctham_Fit$residuals)
 
summary(Ctham_Fit)

