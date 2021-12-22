# Script for analyzing TreeAge output in COVID OB ARDS project
# run the function at the bottom first 

#"BaseCase"
#"Sensitivity_RR0.8"
#"Sensitivity_RR1.25"
#"Sensitivity_GA28"
#"Sensitivity_GA30"
#"Sensitivity_GA34"
#"Sensitivity_FL0.02"
#"Sensitivity_FL0.1"

# run one at a time
 name <- "BaseCase"

 nMom <- 10000
 analyze_treeagecsv(name, nMom)

# or do all of them at once with the following two commands:
#nMom <- 10000
#names <- list("BaseCase",
#               "Sensitivity_RR0.7",
#               "Sensitivity_RR1.4",
#               "Sensitivity_GA28",
#               "Sensitivity_GA30",
#               "Sensitivity_GA34",
#               "Sensitivity_FL0.01",
#               "Sensitivity_FL0.1"
# )
# lapply(names, analyze_treeagecsv, nMom)

analyze_treeagecsv <- function(filename, nMom){

library(tidyverse)
  # adjust for your appropriate working drive
setwd("C:/Users/chris/Dropbox/Coursework/Decision Analysis/HAD5304 Project")

df <- read.table(paste0(filename,".txt"), header = T)

# 0 = Csection, 1 = continue pregnancy
id0 <- c(1, seq(from = 2, to = 79, by = 2))
id1 <- c(1, seq(from = 3, to = 79, by = 2))

df0mom <- df[1:nMom,id0]
df1mom <- df[1:nMom,id1]

df0baby <- df[(nMom+1):(2*nMom),id0]
df1baby <- df[(nMom+1):(2*nMom),id1]

# read in and combine with global matrices 

momdf3 <- as.data.frame(read.table(paste0(filename, "GlobalN3.txt"),
                     skip = 4, 
                     col.names = c("BabyID","BabyOutcomeDetermined",
                                   "GA","fetalLoss")))


df0mom <- bind_cols(df0mom, momdf3[,c(1,4)])

momdf7 <- as.data.frame(read.table(paste0(filename, "GlobalN7.txt"),
                                skip = 4, 
                                col.names = c("BabyID","BabyOutcomeDetermined",
                                              "GA","fetalLoss")))

df1mom <- bind_cols(df1mom, momdf7[,c(1,4)])

babydf4 <- as.data.frame(read.table(paste0(filename, "GlobalN4.txt"),
                                   skip = 4, 
                                   col.names = c("MomID","MomAge","GA",
                                                 "fetalLoss", "HospitalMort",
                                                 "LongtermComplication")))


df0baby <- bind_cols(df0baby, babydf4[(nMom+1):(2*nMom),c(1,3,4)])

babydf8 <- as.data.frame(read.table(paste0(filename, "GlobalN8.txt"),
                                   skip = 4,  
                                   col.names = c("MomID","MomAge","GA",
                                                 "fetalLoss", "HospitalMort",
                                                 "LongtermComplication")))

df1baby <- bind_cols(df1baby, babydf8[(nMom+1):(2*nMom),c(1,3,4)])



outcomes.mom <- function(df){

  n_outcomes = 9
  temp <- data.frame(center = rep(NA, n_outcomes),
                     lower = rep(NA, n_outcomes),
                     upper = rep(NA, n_outcomes),
                     string = rep(NA, n_outcomes))
  
  row.names(temp) <- c("HospitalLOS",
                       "VentDuration",
                       "LongtermComp",
                       "HospMortality",
                       "FetalLoss",
                       "GA",
                       "PropBirthWhileIMV",
                       "LifeYears",
                       "QALYs"
  )
  
  temp[1,1:3] <- quantile(df[,26], prob = c(0.5,0.25,0.75))
  temp[1,4] <- paste0(temp[1,1], " (", temp[1,2], " to ", temp[1,3], ")")
  temp[2,1:3] <- quantile(df[,28], prob = c(0.5,0.25,0.75))
  temp[2,4] <- paste0(temp[2,1], " (", temp[2,2], " to ", temp[2,3], ")")  
  temp[3,1] <- mean(df[,32])
  temp[3,4] <- round(temp[3,1],3)
  temp[4,1] <- mean(df[,6])
  temp[4,4] <- round(temp[4,1],3)
  temp[5,1] <- mean(df[,21])
  temp[5,4] <- round(temp[5,1],3)
  temp[6,1:3] <- quantile(df[,23], prob = c(0.5,0.25,0.75))
  temp[6,4] <- paste0(temp[6,1], " (", temp[6,2], " to ", temp[6,3], ")")  
  temp[7,1] <- mean(df[,18]<=df[,28], prob = c(0.5,0.25,0.75))
  temp[7,4] <- round(temp[7,1],3)
  temp[8,4] <- round(c(mean(df[,5])),2)
  temp[9,4] <- round(mean(df[,3]),2)

  temp[,4]
  
}
outcomes.baby <- function(df){
  
  n_outcomes = 8
  temp <- data.frame(center = rep(NA, n_outcomes),
                     lower = rep(NA, n_outcomes),
                     upper = rep(NA, n_outcomes),
                     string = rep(NA, n_outcomes))
  
  row.names(temp) <- c("GAatbirth",
                       "NICUadmit",
                       "NICUlos",
                       "NeonatalCOVID",
                       "LongtermComp",
                       "FetalNNmortality",
                       "LifeYears",
                       "QALYs")
  
  temp[1,1:3] <- quantile(df[,24], prob = c(0.5,0.25,0.75))
  temp[1,4] <- paste0(temp[1,1], " (", temp[1,2], " to ", temp[1,3], ")")
  temp[2,1] <- mean(df[,33]>0)
  temp[2,4] <- round(temp[2,1],3)
  temp[3,1:3] <- quantile(df[(df[,33]>0),33], prob = c(0.5,0.25,0.75))
  temp[3,4] <- paste0(temp[3,1], " (", temp[3,2], " to ", temp[3,3], ")")  
  temp[4,1] <- mean(df[,34])
  temp[4,4] <- round(temp[4,1],3)
  temp[5,1] <- mean(df[,32])
  temp[5,4] <- round(temp[5,1],3)
  temp[6,1] <- mean(df[,6])
  temp[6,4] <- round(temp[6,1],3)
  
  temp[7,4] <- round(c(mean(df[,5])),2)
  
  temp[8,4] <- round(c(mean(df[,4])),2)

  temp[,4]
  
  }

join_mombaby <- function(mom, baby){
  mom <- mom[, c(1,41,3,5,6, 32, 42)]
  baby <- baby[, c(1, 41, 4, 5, 6, 32, 33)]
  names(mom)[1] <- "MomID"
  names(baby)[1] <-"BabyID"
  mombaby <- left_join(mom, baby, by = "MomID",
                       suffix = c(".mom",".baby"))
  names(mombaby) <- c("MomID",
                      "BabyID",
                      "QALYsMom",
                      "LifeYearsMom",
                      "HospDeathMom",
                      "LongtermCompMom",
                      "FetalLossMom",
                      "BabyID2",
                      "QALYsBaby",
                      "LifeYearsBaby",
                      "FetalNNMortality",
                      "LongtermCompBaby",
                      "NICULOS")
  
  mombaby
}

outcomes.mombaby <- function(df){
  n_outcomes = 12
  temp <- data.frame(center = rep(NA, n_outcomes),
                     string = rep(NA, n_outcomes))
  
  row.names(temp) <- c("d_d",
                       "d_s",
                       "s_d",
                       "s_s",
                       "ltc_ltcp",
                       "ltc_ncp",
                       "ltc_ltct",
                       "ltc_nct",
                       "nc_ltcp",
                       "nc_ncp",
                       "nc_ltct",
                       "nc_nct")
  
  temp[1,1] <- mean(df[,5] == 1 & df[,11] == 1)
  temp[1,2] <- round(temp[1,1],3)
  temp[2,1] <- mean(df[,5] == 1 & (df[,11] == 0))
  temp[2,2] <- round(temp[2,1],3)
  temp[3,1] <- mean(df[,5] == 0 & (df[,11] == 1))
  temp[3,2] <- round(temp[3,1],3)
  temp[4,1] <- mean(df[,5] == 0 & (df[,11] == 0))
  temp[4,2] <- round(temp[4,1],3)
  
  temp[5,1] <- mean(df[,6] == 1 & df[,12] == 1 & df[,13] > 0)
  temp[5,2] <- round(temp[5,1],3)
  temp[6,1] <- mean(df[,6] == 1 & df[,12] == 0 & df[,13] > 0)
  temp[6,2] <- round(temp[6,1],3)
  temp[7,1] <- mean(df[,6] == 1 & df[,12] == 1 & (df[,13] == 0 & df[,11] == 0))
  temp[7,2] <- round(temp[7,1],3)
  temp[8,1] <- mean(df[,6] == 1 & df[,12] == 0 & df[,13] == 0 & df[,11] == 0)
  temp[8,2] <- round(temp[8,1],3)
  temp[9,1] <- mean(df[,6] == 0 & df[,12] == 1 & df[,13] > 0)
  temp[9,2] <- round(temp[9,1],3)
  temp[10,1] <- mean(df[,6] == 0 & df[,12] == 0 & df[,13] > 0)
  temp[10,2] <- round(temp[10,1],3)
  temp[11,1] <- mean(df[,6] == 0 & df[,12] == 1 & df[,13] == 0 & (df[,11] == 0))
  temp[11,2] <- round(temp[11,1],3)
  temp[12,1] <- mean(df[,6] == 0 & df[,12] == 0 & df[,13] == 0 & (df[,11] == 0))
  temp[12,2] <- round(temp[12,1],3)
  
  temp[,2]
}


outcomes.diff <- function(mom0,mom1, baby0, baby1, mombaby0, mombaby1){
  
  n_outcomes = 29
  temp <- data.frame(value = rep(NA, n_outcomes))
  
  row.names(temp) <- c("HospitalLOS",
                       "VentDuration",
                       "MatLongtermComp",
                       "HospMortality",
                       "FetalLoss",
                       "MatGA",
                       "PropBirthWhileIMV",
                       "MatLifeYears",
                       "MatQALYs",
                       "NNGAatbirth",
                       "NICUadmit",
                       "NICUlos",
                       "NeonatalCOVID",
                       "NNLongtermComp",
                       "FetalNNmortality",
                       "NNLifeYears",
                       "NNQALYs",
                       "d_d",
                       "d_s",
                       "s_d",
                       "s_s",
                       "ltc_ltcp",
                       "ltc_ncp",
                       "ltc_ltct",
                       "ltc_nct",
                       "nc_ltcp",
                       "nc_ncp",
                       "nc_ltct",
                       "nc_nct")

  temp[1,1] <- median(mom0[,26] - mom1[,26])
  temp[2,1] <- median(mom0[,28]-mom1[,28])
  temp[3,1] <- mean(mom0[,32]-mom1[,32])
  temp[4,1] <- mean(mom0[,6]-mom1[,6])
  temp[5,1] <- mean(mom0[,21]-mom1[,21])
  temp[6,1] <- median(mom0[,23]-mom1[,23])
  temp[7,1] <- mean(mom0[,18]<=mom0[,28])/mean(mom1[,18]<=mom1[,28])
  temp[8,1] <- round(c(mean(mom0[,5]-mom1[,5])),2)
  temp[9,1] <- round(c(mean(mom0[,3]-mom1[,3])),2)
  
  temp[10,1] <- median(baby0[,24]-baby1[,24])
  temp[11,1] <- mean(baby0[,33]>0)/mean(baby1[,33]>0)
  temp[12,1] <- median(baby0[(baby0[,33]>0),33])-median(baby1[(baby1[,33]>0),33])
  temp[13,1] <- mean(baby0[,34] - baby1[,34])
  temp[14,1] <- mean(baby0[,32] - baby1[,32])
  temp[15,1] <- mean(baby0[,6]-baby1[,6])
  temp[16,1] <- round(c(mean(baby0[,5]-baby1[,5])),2)
  temp[17,1] <- round(c(mean(baby0[,4]-baby1[,5])),2)
  
  temp[18,1] <- mean(mombaby0[,5] == 1 & mombaby0[,11] == 1)- mean(mombaby1[,5] == 1 & mombaby1[,11] == 1)
  temp[19,1] <- mean(mombaby0[,5] == 1 & (mombaby0[,11] == 0))- mean(mombaby1[,5] == 1 & (mombaby1[,11] == 0))
  temp[20,1] <- mean(mombaby0[,5] == 0 & (mombaby0[,11] == 1))- mean(mombaby1[,5] == 0 & (mombaby1[,11] == 1))
  temp[21,1] <- mean(mombaby0[,5] == 0 & (mombaby0[,11] == 0)) - mean(mombaby1[,5] == 0 & (mombaby1[,11] == 0))
  temp[22,1] <- mean(mombaby0[,6] == 1 & mombaby0[,12] == 1 & mombaby0[,13] > 0) - 
    mean(mombaby1[,6] == 1 & mombaby1[,12] == 1 & mombaby1[,13] > 0)
  temp[23,1] <- mean(mombaby0[,6] == 1 & mombaby0[,12] == 0 & mombaby0[,13] > 0) -
    mean(mombaby1[,6] == 1 & mombaby1[,12] == 0 & mombaby1[,13] > 0)
  temp[24,1] <- mean(mombaby0[,6] == 1 & mombaby0[,12] == 1 & (mombaby0[,13] == 0 & mombaby0[,11] == 0)) - 
    mean(mombaby1[,6] == 1 & mombaby1[,12] == 1 & (mombaby1[,13] == 0 & mombaby1[,11] == 0))
  temp[25,1] <- mean(mombaby0[,6] == 1 & mombaby0[,12] == 0 & mombaby0[,13] == 0 & mombaby0[,11] == 0) - 
    mean(mombaby1[,6] == 1 & mombaby1[,12] == 0 & mombaby1[,13] == 0 & mombaby1[,11] == 0)
  temp[26,1] <- mean(mombaby0[,6] == 0 & mombaby0[,12] == 1 & mombaby0[,13] > 0) - 
    mean(mombaby1[,6] == 0 & mombaby1[,12] == 1 & mombaby1[,13] > 0)
  temp[27,1] <- mean(mombaby0[,6] == 0 & mombaby0[,12] == 0 & mombaby0[,13] > 0) - 
    mean(mombaby1[,6] == 0 & mombaby1[,12] == 0 & mombaby1[,13] > 0)
  temp[28,1] <- mean(mombaby0[,6] == 0 & mombaby0[,12] == 1 & mombaby0[,13] == 0 & (mombaby0[,11] == 0)) - 
    mean(mombaby1[,6] == 0 & mombaby1[,12] == 1 & mombaby1[,13] == 0 & (mombaby1[,11] == 0))
  temp[29,1] <- mean(mombaby0[,6] == 0 & mombaby0[,12] == 0 & mombaby0[,13] == 0 & (mombaby0[,11] == 0)) - 
    mean(mombaby1[,6] == 0 & mombaby1[,12] == 0 & mombaby1[,13] == 0 & (mombaby1[,11] == 0))
  
  temp
  
}

mombaby0 <- join_mombaby(df0mom, df0baby)
mombaby1 <- join_mombaby(df1mom, df1baby)


outcomes <- data.frame(Tx0 = c(outcomes.mom(df0mom),
                               outcomes.baby(df0baby),
                               outcomes.mombaby(mombaby0)),
                       Tx1 = c(outcomes.mom(df1mom),
                               outcomes.baby(df1baby),
                               outcomes.mombaby(mombaby1)),
                       Diff = outcomes.diff(df0mom,df1mom,
                                            df0baby,df1baby,
                                            mombaby0, mombaby1))

write.csv(outcomes, file = paste0(filename, "output.csv"))

temp0 <- df0mom[,c(1,2)]
temp1 <- df1mom[,c(1,2)]
names(temp0) <- c("patient_id", "QALYs")
names(temp1) <- c("patient_id", "QALYs")
momjoin <- bind_rows(temp0,temp1, .id = "strategy_id")

temp0 <- df0baby[,c(1,4)]
temp1 <- df1baby[,c(1,4)]
names(temp0) <- c("patient_id", "QALYs")
names(temp1) <- c("patient_id", "QALYs")
babyjoin <- bind_rows(temp0,temp1, .id = "strategy_id")

dfplot <- bind_rows(momjoin, babyjoin, .id = "mombaby_id") %>%
  select(-patient_id) %>%
  mutate(mombaby_id = factor(mombaby_id, labels = c("Mother","Baby")),
         strategy_id = factor(strategy_id, labels = c("Elective delivery","Expectant management")))

plot <- ggplot(data = dfplot,
       aes(x = QALYs, fill = strategy_id)) +
  geom_density(alpha = 0.5) +
  facet_wrap(mombaby_id~., nrow = 2) +
  theme_minimal() +
  labs(x = "Quality-adjusted life years") +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        strip.text = element_blank()) +  
  scale_fill_manual(values = c("black","white"))

ggsave(plot, filename = paste0(filename, ".svg"))

plot2 <- ggplot(data = filter(dfplot, mombaby_id == "Mother"),
                aes(x = QALYs, fill = strategy_id)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Quality-adjusted life years") +  
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        strip.text = element_blank()) +
  scale_fill_manual(values = c("black","white"))

ggsave(plot2, filename = paste0(filename,"_onlyMom.svg"))

plot3 <- ggplot(data = filter(dfplot,mombaby_id == "Baby"),
                aes(x = QALYs, fill = strategy_id)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Quality-adjusted life years") +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        strip.text = element_blank()) +
  scale_fill_manual(values = c("black","white"))

ggsave(plot3, filename = paste0(filename,"_onlyBaby.svg"))

print

}



