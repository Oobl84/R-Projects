# Loan Risk Assessment Project

######################## 
# Load Required Packages
########################
 
 # Note: this process could take a couple of minutes
 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(ggthemes)
library(ggplot2)
library(GGally)
 
memory.limit(size=35000)
 
# original data source from Kaggle
# 'https://www.kaggle.com/mirbektoktogaraev/should-this-loan-be-approved-or-denied/download'
# copy of data stored in github
# https://github.com/Oobl84/R-Projects/raw/main/edx_final_project/loan_data.zip

dl <- tempfile()
download.file("https://github.com/Oobl84/R-Projects/raw/main/edx_final_project/loan_data.zip",dl)


df <- read.csv(unzip(dl), header=TRUE, stringsAsFactors = FALSE)

dl2 <- tempfile()
download.file("https://github.com/Oobl84/R-Projects/raw/main/edx_final_project/naic_codes.csv",dl)

sectors <- read.csv(dl2, header=TRUE, stringsAsFactors = TRUE)
head(df)

remove(dl)

# get NAICS data file for industry names

# check for null values
sapply(df, function(x) sum(is.na(x)))


# check target column
df %>% group_by(MIS_Status) %>% summarise(n())

df["NAIC_2"] <- substr(df["NAICS"],1,2)

df <- df

# drop any rows that don't have a target status. 1997 rows

df2 <- df[(df$MIS_Status=="CHGOFF" | df$MIS_Status=="P I F"),]

df3 <- df[(df$MIS_Status != "CHGOFF") & (df$MIS_Status != "P I F"),]

# recheck for null values
sapply(df2, function(x) sum(is.na(x)))


################
# Data Cleaning
################

str(df2)

df2 %>% group_by(City) %>% summarise(num=n()) %>% arrange()

df2$City <- str_replace_all(df2$City,c("\\[" = "", "\\:" = "", "&" = ""))

# columns need to be converted to factors before splitting data into train and validation sets

factor_cols <- c("State", "Zip", "BankState", "NAICS",
          "UrbanRural", "City", "MIS_Status")

logic_cols_yn <- c("RevLineCr", "LowDoc")

logic_cols_other <- c("MIS_Status", "NewExist")

num_cols <-c("DisbursementGross", "BalanceGross", "ChgOffPrinGr", "GrAppv", "SBA_Appv", "ApprovalFY")

date_cols <- c("DisbursementDate", "ApprovalDate")

df2[factor_cols] <- lapply(df2[factor_cols],as.factor)

df2[num_cols] <- lapply(df2[num_cols], parse_number)

df2[date_cols] <- lapply(df2[date_cols], parse_date_time2, orders=c("dby"))

df2["is_lowdoc"] <- as.numeric(df2$LowDoc == "Y")

df2["defaulted"] <- as.numeric(df2$MIS_Status == "CHGOFF")

# Aim to predict whether a loan should have been issued or not based on characteristics given.
# following columns would give away answer
df2 <- df2 %>% select(-c("ChgOffPrinGr", "ChgOffDate","BalanceGross"))

# Validation set will be 10% of SBA data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = df2$defaulted, times = 1, p = 0.2, list = FALSE)
train <- df2[-test_index,]


tmp <- df2[test_index,]

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_split <- createDataPartition(y=tmp$defaulted, times=1, p=0.5, list = FALSE)
test <- tmp[-test_split,]
validation <- tmp[test_split,]

rm(df, df2, tmp, num_cols, date_cols, factor_cols, drop_cols, logic_cols_yn, logic_cols_other)

##################
# Data Exploration
##################


# target variable distribution

train %>% ggplot(aes(MIS_Status)) + geom_histogram(stat="count") + theme_economist()

# ~18% of loans have defaulted.
sum(train$MIS_Status == "CHGOFF")/nrow(train)

state_defaults <- train %>% group_by(State) %>% summarise(state_default_ratio = sum(MIS_Status == "CHGOFF")/n())


# default rate by state
state_defaults %>% ggplot() + 
        geom_point(aes(reorder(State,state_default_ratio),state_default_ratio)) +
        xlab("State") + theme(axis.text.x = element_text(angle = 90))


# default_rate by Low doc status

train %>% group_by(is_LowDoc) %>% 
        summarize(default_rate = sum(defaulted)/n())

## lowdoc status companies are half as likely to default as non-lowdoc companies

# defaults by year
train %>% count(ApprovalFY, MIS_Status) %>% ggplot(aes(ApprovalFY, n )) + 
        geom_col(aes(fill=MIS_Status)) + xlab("Approval Year") + ylab("Number of Loans")

#default rate by year
train %>% group_by(ApprovalFY) %>% 
        summarise(default_rate = sum(defaulted)*100/n()) %>% 
        ggplot(aes(ApprovalFY,default_rate)) + geom_line() + 
        xlab("Approval Year") + ylab("Default Rate (%)")

# number of loans by year
train %>% ggplot(aes(ApprovalFY)) + 
        geom_histogram(col='black', fill='gray', binwidth = 2) +
        facet_grid(defaulted ~ .) + ylab("Number of Loans") + xlab("Approval Year")





