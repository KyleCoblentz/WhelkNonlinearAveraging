### Generate handling time data

### load packages

library('tidyr'); library('dplyr');

### load feeding data

Handling_data <- read.csv('FeedingData_Long_Clean.csv', stringsAsFactors = TRUE) 

### functions to come up with handling times for each observation

Bal_hand <- function(PredSize, PreySize, HandleType) {
  exp(0.326 - 0.18* log(PredSize) + .61*log(PreySize) - 1.08 * HandleType)
}

Mytilus_hand <- function(PredSize, PreySize, HandleType) {
  exp(1.27 - 1.3*log(PredSize) + 1.05*log(PreySize) - 0.5 * HandleType)
}

Ctham_hand <- function(PredSize, PreySize, HandleType) {
  exp(2.26 - 1.09*log(PredSize) + 0.64*log(PreySize) - 0.92 * HandleType)
}

Limp_hand <- function(PredSize, PreySize, HandleType) {
  exp(-1.04 - 0.61*log(PredSize) + 1.23*log(PreySize) - 0.3 * HandleType)
}

Handling_data$HandleType <- ifelse(Handling_data$DrillSide == 'Y', 0, 1)

Handling_data$HandlingTime <- NA





for(i in 1:length(Handling_data$X)) {
  if (Handling_data$Prey[i] == 'Bg') {
    Handling_data$HandlingTime[i] <- Bal_hand(PredSize = Handling_data$Size[i], PreySize = Handling_data$PreySize[i], HandleType = Handling_data$HandleType[i])
  } else {
    if (Handling_data$Prey[i] == 'Mt') {
      Handling_data$HandlingTime[i] <- Mytilus_hand(PredSize = Handling_data$Size[i], PreySize = Handling_data$PreySize[i], HandleType = Handling_data$HandleType[i])
    } else {
      if (Handling_data$Prey[i] == 'Ct') {
        Handling_data$HandlingTime[i] <- Ctham_hand(PredSize = Handling_data$Size[i], PreySize = Handling_data$PreySize[i], HandleType = Handling_data$HandleType[i])
      } else {
        Handling_data$HandlingTime[i] <- Limp_hand(PredSize = Handling_data$Size[i], PreySize = Handling_data$PreySize[i], HandleType = Handling_data$HandleType[i])
      }
    }
  }
}

### Average across handling times per individual

Handling_data <- Handling_data %>% group_by(WhelkID, Cage, Prey) %>% summarise(HandlingTime = mean(HandlingTime))

### Switch to wide format

Handling_data <- Handling_data %>% spread(Prey, HandlingTime, fill = NA) 


mean_barnacle <- mean(Handling_data$Bg, na.rm = TRUE)

mean_trossulus <- mean(Handling_data$Mt, na.rm = TRUE)
### Need to fill in NA's for prey

### For barnacles

bal_zeros <- function(df) {
  bal_zeros_removed <- df[-which(is.na(df$Bg)),] 
  bal_mean_h <- mean(bal_zeros_removed$Bg)
  df$Bg[which(is.na(df$Bg))] <- bal_mean_h
  df
}

Handling_data <- do.call(rbind, by(data = Handling_data, Handling_data[,'Cage'], bal_zeros))

### Tross handling times

tross_zeros <- function(df) {
  tross_zeros_removed <- df[-which(is.na(df$Mt)),] 
  tross_mean_h <- mean(tross_zeros_removed$Mt)
  df$Mt[which(is.na(df$Mt))] <- tross_mean_h
  df
}

Handling_data <- do.call(rbind, by(data = Handling_data, Handling_data[,'Cage'], tross_zeros))

### for rarer prey, will use overall average across cages 

### Cthamalus handling times

Ctham_zeros_removed <- Handling_data[-which(is.na(Handling_data$Ct)),]

Ctham_mean_h <- mean(Ctham_zeros_removed$Ct)

Handling_data$Ct[which(is.na(Handling_data$Ct))] <- Ctham_mean_h

### Limpets

LotP_zeros_removed <- Handling_data[-which(is.na(Handling_data$LotP)),]

LotP_mean_h <- mean(LotP_zeros_removed$LotP)

Handling_data$LotP[which(is.na(Handling_data$LotP))] <- LotP_mean_h

### Make all handling times averages

Handling_data$Bg <- mean_barnacle

Handling_data$Mt <- mean_trossulus

Handling_data$Ct <- Ctham_mean_h

Handling_data$LotP <- LotP_mean_h

Handling_data_AllInd <- Handling_data %>% select(-c(NF, START))

# write.csv(Handling_data_AllInd, file = 'Handling_data_AllInd.csv')

