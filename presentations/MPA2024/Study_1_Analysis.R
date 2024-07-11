library(tidyverse)
library(JSmediation)

data <- read.csv("Agnostic_Perception_Study1_Raw.csv", na.strings = c("", "NA"))
data <- data[-1:-2,-1:-17]

# Remove Not Necessary Columns & Rename Condition DO
data <- data %>% select(-D4_8_TEXT,-CN_DO,-Atheist_Threat_DO,
                        -Percept_ConvAcc_Ath_DO,-Agnostic_Threat_DO,-Percept_ConvAcc_Agn_DO,
                        -SDO_DO,-SNBS_DO) %>% 
  rename("Condition_Order" = "FL_24_DO")

# Recode Attention Checks
data <- data %>% mutate(
  Att_1 = if_else(Att_1 == "A",1,0, missing = 0),
  Att_2 = if_else(Att_2 == "E",1,0, missing = 0),
  Att_3 = if_else(Att_3 == "C",1,0, missing = 0),
  Att_4 = if_else(Att_4 == "D",1,0, missing = 0),
  Att_5 = if_else(Att_5 == "B",1,0, missing = 0),
  Att_Comp = Att_1 + Att_2 + Att_3 + Att_4 + Att_5
)

# Remove Inattentive Participants
data <- data %>% filter(Att_Comp >= 4)

# Remove Explicitly Non-Christians

data <- data %>%
  filter(!D5 %in% c('Agnostic', 'Atheist'))


# Big 5 Recode

big5 <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Disagree Strongly" = 1,
                      "Disagree A Little" = 2,
                      "Neither Agree nor Disagree" = 3,
                      "Agree A Little" = 4,
                      "Agree Strongly" = 5)
}

big5_R <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Disagree Strongly" = 5,
                      "Disagree A Little" = 4,
                      "Neither Agree nor Disagree" = 3,
                      "Agree A Little" = 2,
                      "Agree Strongly" = 1)
}

data <- data %>% 
  mutate(across(.cols = c(Big5_1:Big5_5),.fns = big5))

data <- data %>% 
  mutate(across(.cols = c(Big5_6:Big5_10),.fns = big5_R))

# CN Recode

cn <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Strongly Disagree\n\n(1)\n" = 1,
                      '2' = 2,
                      "Neither Disagree Nor Agree\n(3)\n" = 3,
                      '4' = 4,
                      "Strongly Agree\n(5)\n" = 5)
}

data <- data %>% 
  mutate(across(.cols = c(CN_1:CN_6),.fns = cn))

data$CN_Mean <- rowMeans(data[,c("CN_1","CN_2","CN_3","CN_4","CN_5","CN_6")], na.rm = FALSE)

### !!! START HERE !!! ###

# Atheist & Agnostic Attitude Recode

ath_agn_att <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Dislike them a great deal" = 1,
                      "Very Bad" = 1,
                      "Very Unfavorable" = 1,
                      "Very harmful" = 1,
                      "Very Unnecessary" = 1,
                      "Dislike them" = 2,
                      "Bad" = 2,
                      "Unfavorable" = 2,
                      "Harmful" = 2,
                      "Unnecessary" = 2,
                      "Dislike them somewhat" = 3,
                      "Somewhat bad" = 3,
                      "Somewhat unfavorable" = 3,
                      "Somewhat harmful" = 3,
                      "Somewhat unnecessary" = 3,
                      "Neither dislike nor like them" = 4,
                      "Neither bad nor good" = 4,
                      "Neither unfavorable nor favorable" = 4,
                      "Neither harmful nor beneficial" = 4,
                      "Neither unnecessary nor necessary" = 4,
                      "Like them somewhat" = 5,
                      "Somewhat good" = 5,
                      "Somewhat favorable" = 5,
                      "Somewhat beneficial" = 5,
                      "Somewhat necessary" = 5,
                      "Like them" = 6,
                      "Good" = 6,
                      "Favorable" = 6,
                      "Beneficial" = 6,
                      "Necessary" = 6,
                      "Like them a great deal" = 7,
                      "Very Good" = 7,
                      "Very favorable" = 7,
                      "Very beneficial" = 7,
                      "Very Necessary" = 7)
}

data <- data %>% 
  mutate(across(.cols = c(Ath_Att_1_1:Ath_Att_5_1,Agn_Att_1_1:Agn_Att_5_1),.fns = ath_agn_att))

data$Agn_Att_Mean <- rowMeans(data[,c("Agn_Att_1_1","Agn_Att_2_1","Agn_Att_3_1","Agn_Att_4_1","Agn_Att_5_1")],na.rm = FALSE)

data$Ath_Att_Mean <- rowMeans(data[,c("Ath_Att_1_1","Ath_Att_2_1","Ath_Att_3_1","Ath_Att_4_1","Ath_Att_5_1")],na.rm = FALSE)

# Atheist & Agnostic Threat Recode

ath_agn_threat <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Strongly Disagree" = 1,
                      "Moderately Disagree" = 2,
                      "Slightly Disagree" = 3,
                      "Neither Agree Nor Disagree" = 4,
                      "Slightly Agree" = 5,
                      "Moderately Agree" = 6,
                      "Strongly Agree" = 7)
}

data <- data %>% 
  mutate(across(.cols = c(Atheist_Threat_1:Atheist_Threat_8,Agnostic_Threat_1:Agnostic_Threat_8),.fns = ath_agn_threat))

data$Atheist_TThreat <- rowMeans(data[,c("Atheist_Threat_1","Atheist_Threat_2","Atheist_Threat_3","Atheist_Threat_4",
                                         "Atheist_Threat_5","Atheist_Threat_6","Atheist_Threat_7","Atheist_Threat_8")])

data$Atheist_Threat_Symb_Mean <- rowMeans(data[,c("Atheist_Threat_1","Atheist_Threat_2","Atheist_Threat_3","Atheist_Threat_4")])

data$Atheist_Threat_Real_Mean <- rowMeans(data[,c("Atheist_Threat_5","Atheist_Threat_6","Atheist_Threat_7","Atheist_Threat_8")])

data$Agnostic_TThreat <- rowMeans(data[,c("Agnostic_Threat_1","Agnostic_Threat_2","Agnostic_Threat_3","Agnostic_Threat_4",
                                          "Agnostic_Threat_5","Agnostic_Threat_6","Agnostic_Threat_7","Agnostic_Threat_8")])

data$Agnostic_Threat_Symb_Mean <- rowMeans(data[,c("Agnostic_Threat_1","Agnostic_Threat_2","Agnostic_Threat_3","Agnostic_Threat_4")])

data$Agnostic_Threat_Real_Mean <- rowMeans(data[,c("Agnostic_Threat_5","Agnostic_Threat_6","Agnostic_Threat_7","Agnostic_Threat_8")])

# Atheist & Agnostic Convert Perception

ath_agn_convert <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Not At All Likely" = 1,
                      "Slightly Likely" = 2,
                      "Somewhat Likely" = 3,
                      "Moderately Likely" = 4,
                      "Very Likely" = 5)
}

data <- data %>% 
  mutate(across(.cols = c(Percept_ConvAcc_Ath_1:Percept_ConvAcc_Ath_6,Percept_ConvAcc_Agn_1:Percept_ConvAcc_Agn_6),.fns = ath_agn_convert))

data$Atheist_Covert_Mean <- rowMeans(data[,c("Percept_ConvAcc_Ath_1","Percept_ConvAcc_Ath_2","Percept_ConvAcc_Ath_3")])

data$Atheist_Acceptance_Mean <- rowMeans(data[,c("Percept_ConvAcc_Ath_4","Percept_ConvAcc_Ath_5","Percept_ConvAcc_Ath_6")])

data$Agnostic_Convert_Mean <- rowMeans(data[,c("Percept_ConvAcc_Agn_1","Percept_ConvAcc_Agn_2","Percept_ConvAcc_Agn_3")])

data$Agnostic_Acceptance_Mean <- rowMeans(data[,c("Percept_ConvAcc_Agn_4","Percept_ConvAcc_Agn_5","Percept_ConvAcc_Agn_6")])

# SDO Recode

sdo <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Strongly Oppose" = 1,
                      "Moderately Oppose" = 2,
                      "Slightly Oppose" = 3,
                      "Neutral" = 4,
                      "Slightly Favor" = 5,
                      "Moderately Favor" = 6,
                      "Strongly Favor" = 7)
}

data <- data %>% 
  mutate(across(.cols = c(SDO_1:SDO_4,SDO_9:SDO_12),.fns = sdo))

sdo_R <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Strongly Oppose" = 7,
                      "Moderately Oppose" = 6,
                      "Slightly Oppose" = 5,
                      "Neutral" = 4,
                      "Slightly Favor" = 3,
                      "Moderately Favor" = 2,
                      "Strongly Favor" = 1)
}

data <- data %>% 
  mutate(across(.cols = c(SDO_5:SDO_8,SDO_13:SDO_16),.fns = sdo_R))

data$SDO_Mean <- rowMeans(data[,c("SDO_1","SDO_2","SDO_3","SDO_4",
                                  "SDO_5","SDO_6","SDO_7","SDO_8",
                                  "SDO_9","SDO_10","SDO_11","SDO_12",
                                  "SDO_13","SDO_14","SDO_15","SDO_16")])

# Political Affiliation Recode

Pol_R <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Extreme Liberal" = 1,
                      "Strong Liberal" = 2,
                      "Moderate Liberal" = 3,
                      "Slight Liberal" = 4,
                      "Neutral" = 5,
                      "Slight Conservative" = 6,
                      "Moderate Conservative" = 7,
                      "Strong Consservative" = 8,
                      "Extreme Conservative" = 9)
}

data <- data %>% 
  mutate(across(.cols = c(D6_1),.fns = Pol_R))

# RFS Recode

rfs <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Very Strongly Disagree" = 1,
                      "Strongly Disagree" = 2,
                      "Moderately Disagree" = 3,
                      "Slightly Disagree" = 4,
                      "Neither Disagree Nor Agree" = 5,
                      "Slightly Agree" = 6,
                      "Moderately Agree" = 7,
                      "Strongly Agree" = 8,
                      "Very Strongly Agree" = 9)
}

rfs_R <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Very Strongly Disagree" = 9,
                      "Strongly Disagree" = 8,
                      "Moderately Disagree" = 7,
                      "Slightly Disagree" = 6,
                      "Neither Disagree Nor Agree" = 5,
                      "Slightly Agree" = 4,
                      "Moderately Agree" = 3,
                      "Strongly Agree" = 2,
                      "Very Strongly Agree" = 1)
}

data <- data %>% 
  mutate(across(.cols = c(RFS_1,RFS_3,RFS_5,RFS_6,RFS_8,RFS_11),.fns = rfs))

data <- data %>% 
  mutate(across(.cols = c(RFS_2,RFS_4,RFS_7,RFS_9,RFS_10,RFS_12),.fns = rfs_R))

data$RFS_Mean <- rowMeans(data[c("RFS_1","RFS_3","RFS_5","RFS_6","RFS_8","RFS_11",
                                 "RFS_2","RFS_4","RFS_7","RFS_9","RFS_10","RFS_12")])
# SNBS Recode

snbs <- function(df)
{
  df <- dplyr::recode({{df}},
                      "Strongly disagree" = 1,
                      "Disagree" = 2,
                      "Somewhat disagree" = 3,
                      "Slightly disagree" = 4,
                      "Neither disagree nor agree" = 5,
                      "Slightly agree" = 6,
                      "Somewhat agree" = 7,
                      "Agree" = 8,
                      "Strongly agree" = 9)
}

data <- data %>% 
  mutate(across(.cols = c(SNBS_1:SNBS_10),.fns = snbs))

data$SNBS_Mean <- rowMeans(data[,c("SNBS_1","SNBS_2","SNBS_3","SNBS_4","SNBS_5",
                                   "SNBS_6","SNBS_7","SNBS_8","SNBS_9","SNBS_10")])

key_data <- data %>% select("D1","D2","D3","D4","D5","D6_1",
                            "Condition_Order","Att_Comp","CN_Mean",
                            "SDO_Mean","RFS_Mean","SNBS_Mean","Ath_Att_Mean","Agn_Att_Mean",
                            "Atheist_Threat_Symb_Mean","Atheist_Threat_Real_Mean","Agnostic_Threat_Symb_Mean",
                            "Agnostic_Threat_Real_Mean","Atheist_Covert_Mean","Atheist_Acceptance_Mean",
                            "Agnostic_Convert_Mean","Agnostic_Acceptance_Mean","Atheist_TThreat","Agnostic_TThreat") %>% 
  rename("AthAtt" = "Ath_Att_Mean",
         "AgnAtt" = "Agn_Att_Mean",
         "AthSyT" = "Atheist_Threat_Symb_Mean",
         "AthReT" = "Atheist_Threat_Real_Mean",
         "AgnSyT" = "Agnostic_Threat_Symb_Mean",
         "AgnReT" = "Agnostic_Threat_Real_Mean",
         "AthConv" = "Atheist_Covert_Mean",
         "AthAcc" = "Atheist_Acceptance_Mean",
         "AgnConv" = "Agnostic_Convert_Mean",
         "AgnAcc" = "Agnostic_Acceptance_Mean",
         "AthTotT" = "Atheist_TThreat",
         "AgnTotT" = "Agnostic_TThreat")

key_data <- key_data %>% filter(!D5 %in% c("Other (Please Specify)",
                               "Prefer Not To Answer",
                               "Spiritual"))

key_data <- key_data[-5,]

write.csv(key_data,file = "Cleaned_Data.csv")
haven::write_sav(key_data,"Cleaned_Data.sav")

## T.Test (Paired Realistic Threat) - No Difference

t.test(x = key_data$AgnReT,
       y = key_data$AthReT,
       paired = TRUE,
       alternative = c("less"))

mean_diff <- mean(key_data$AgnReT - key_data$AthReT)
sd_diff <- sd(key_data$AgnReT - key_data$AthReT)

mean_diff / sd_diff

## T.Test (Paired Symbolic Threat) - Yes Different Among Atheists/Agnostics

t.test(x = key_data$AgnSyT,
       y = key_data$AthSyT,
       paired = TRUE,
       alternative = c("less"))

mean_diff <- mean(key_data$AgnSyT - key_data$AthSyT)
sd_diff <- sd(key_data$AgnSyT - key_data$AthSyT)

mean_diff / sd_diff

## T.Test (Paired Convert) - Yes Different Among Atheists/Agnostics

t.test(x = key_data$AgnConv,
       y = key_data$AthConv,
       paired = TRUE,
       alternative = c("greater"))

mean_diff <- mean(key_data$AgnConv - key_data$AthConv)
sd_diff <- sd(key_data$AgnConv - key_data$AthConv)

mean_diff / sd_diff

## T.Test (Paired Acceptance) - Yes Different Among Atheists/Agnostics

t.test(x = key_data$AgnAcc,
       y = key_data$AthAcc,
       paired = TRUE,
       alternative = c("greater"))

mean_diff <- mean(key_data$AgnAcc - key_data$AthAcc)
sd_diff <- sd(key_data$AgnAcc - key_data$AthAcc)

mean_diff / sd_diff

## Mediation (Real & Convert)

model1 <- JSmediation::mdt_within_wide(data=key_data,
                             DV_A = AgnReT,
                             DV_B = AthReT,
                             M_A = AgnConv,
                             M_B = AthConv)

model1i <- JSmediation::add_index(model1)

## Mediation (Symb & Convert)

model2 <- JSmediation::mdt_within_wide(data=key_data,
                                       DV_A = AgnSyT,
                                       DV_B = AthSyT,
                                       M_A = AgnConv,
                                       M_B = AthConv)

model2i <- JSmediation::add_index(model2)

# Mediation (Real & Accept)

model3 <- JSmediation::mdt_within_wide(data=key_data,
                                       DV_A = AgnReT,
                                       DV_B = AthReT,
                                       M_A = AgnAcc,
                                       M_B = AthAcc)

model3i <- JSmediation::add_index(model3)

## Mediation (Symb & Accept)**

model4 <- JSmediation::mdt_within_wide(data=key_data,
                                       DV_A = AgnSyT,
                                       DV_B = AthSyT,
                                       M_A = AgnAcc,
                                       M_B = AthAcc)

model4i <- JSmediation::add_index(model4)

## Correlation (Agn Convert Real Threat)

cor.test(x = key_data$AgnConv,
    y = key_data$AgnReT,
    method = c("pearson"))

## Correlation (Agn Convert Symb Threat)**

cor.test(x = key_data$AgnConv,
         y = key_data$AgnSyT,
         method = c("pearson"))

## Correlation (Ath Convert Real Threat)

cor.test(x = key_data$AthConv,
         y = key_data$AthReT,
         method = c("pearson"))

## Correlation (Ath Convert Symb Threat)

cor.test(x = key_data$AthConv,
         y = key_data$AthSyT,
         method = c("pearson"))



## Correlation (Agn Accept Real Threat)

cor.test(x = key_data$AgnAcc,
         y = key_data$AgnReT,
         method = c("pearson"))

## Correlation (Agn Accept Symb Threat)

cor.test(x = key_data$AgnAcc,
         y = key_data$AgnSyT,
         method = c("pearson"))

## Correlation (Ath Accept Real Threat)

cor.test(x = key_data$AthAcc,
         y = key_data$AthReT,
         method = c("pearson"))


## Correlation (Ath Accept Symb Threat)

cor.test(x = key_data$AthAcc,
         y = key_data$AthSyT,
         method = c("pearson"))
