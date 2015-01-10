#Datamining Exercise Insurance Customer
setwd("P:/R/Datamining/Exercises/CaravanMarketing/")

#library(dplyr)
columns <- read.csv("./Data/columns.csv" , sep=";", header=FALSE, strip.white=TRUE, na.strings="")
columns <- as.character(na.omit(columns[,2]))


data           <- read.table("./Data/ticdata2000-training.txt" , sep="\t", header=FALSE)
colnames(data) <- columns

summary(data)
cor(data[1:85],data[86], method="spearman") 


#Creating data-frame 
data_df <- tbl_df(data)

#Religious variables are excluded now as their correlation coefficient is very small
data_df_mR <- select(data_df, -contains("MGOD"))


#Converting variables as factors for logit regression
data_df_mR$MOSTYPE <- as.factor(data_df_mR$MOSTYPE)
data_df_mR$MGEMLEEF <- as.factor(data_df_mR$MGEMLEEF)
data_df_mR$PWAPART <- as.factor(data_df_mR$PWAPART)
data_df_mR$MOSHOOFD <- as.factor(data_df_mR$MOSHOOFD)

library(Hmisc)
cust.logit <- glm(CARAVAN~., data=data_df_mR)
summary(cust.logit)
