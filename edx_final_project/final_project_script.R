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
download.file("https://github.com/Oobl84/R-Projects/raw/main/edx_final_project/naic_codes.csv",dl2)

sectors <- read.csv(dl2, header=TRUE, stringsAsFactors = TRUE)
head(df)

remove(dl,dl2)


# check for null values
sapply(df, function(x) sum(is.na(x)))

# checking sectors
df %>% group_by(NAICS) %>% summarise(num=n()) %>% arrange(desc(num))

# 22% of rows don't have industry defined. Will keep for now but may discard later
df %>% summarise(undefined = sum(NAICS == 0)/n())

# add undefined to sector table
sectors <- sectors %>% add_row(Sector = 0, Definition = 'UNDEFINED', .before = 1)

# get first two nums from NAIC col to join with sectors
df$NAIC_2 <- as.numeric(substr(df$NAICS,1,2))

df %>% group_by(NAIC_2) %>% summarise(n())

# join
df <- merge(df,sectors,by.x = "NAIC_2", by.y = "Sector", all.x = TRUE)


################
# Data Cleaning
################

str(df)

df %>% group_by(City) %>% summarise(num=n()) %>% arrange(desc(num))

# cleaning city names
df$City <- str_replace_all(df$City, "[[:punct:]]", "")

df$City <- str_replace_all(df$City,"\\`", "")

# cleaning revolving line of credit column

df %>% group_by(RevLineCr) %>% summarise(num = n())

# Lots of extraneous codes outside of the key. Reasonable to assume that 0 is equivalent to N and 1 to Y, but drop all others
df$RevLineCr[df$RevLineCr=="0"] <- "N"
df$RevLineCr[df$RevLineCr=="1" | df$RevLineCr == "T"] <- "Y"

# Rather than guess what the values might equate to it's best to just drop the ones that don't fit
df <- df %>% filter(RevLineCr == "Y" | RevLineCr == "N")

# cleaning revolving line of credit column

df %>% group_by(RevLineCr) %>% summarise(num = n())

# check franchise code
df %>% group_by(FranchiseCode) %>% summarise(num = n())

# columns need to be converted to factors before splitting data into train and validation sets

factor_cols <- c("State", "Zip", "BankState", "NAICS_2",
          "UrbanRural", "City", "MIS_Status")

logic_cols_yn <- c("RevLineCr", "LowDoc")

logic_cols_other <- c("MIS_Status", "NewExist")

num_cols <-c("DisbursementGross", "BalanceGross", "ChgOffPrinGr", "GrAppv", "SBA_Appv", "ApprovalFY")

date_cols <- c("DisbursementDate", "ApprovalDate")

df[factor_cols] <- lapply(df[factor_cols],as.factor)

df[num_cols] <- lapply(df[num_cols], parse_number)

df[date_cols] <- lapply(df[date_cols], parse_date_time2, orders=c("dby"))

df["is_lowdoc"] <- as.numeric(df$LowDoc == "Y")

df["defaulted"] <- as.numeric(df$MIS_Status == "CHGOFF")

#add new column for franchise
df$is_franchise <- as.numeric(df$FranchiseCode == 0 | df$FranchiseCode == 1)


# new business
df %>% group_by(NewExist) %>% summarise(n())

# drop any rows that don't conform to key
df <- df[(df$NewExist == 1 | df$NewExist == 2),]

df['is_new'] <- as.numeric(df$NewExist == 2)

#check whether bank state is same as borrower state
df %>% summarise(out_of_state = sum(df$State != df$BankState)/n())

df %>% filter(State != BankState) %>% head(30)
# nearly half of loans come from out of state banks
df['is_out_of_state_loan'] <- as.numeric(df$State != df$BankState)

# check urban rural col

df %>% group_by(UrbanRural) %>% summarise(num=n())

# drop columns that would give away the loan status
df <- df %>% select(-c("ChgOffPrinGr", "ChgOffDate","BalanceGross"))

# drop any rows that don't have a target status. 1997 rows

df %>% group_by(MIS_Status) %>% summarise(n())

df2 <- df[(df$MIS_Status=="CHGOFF" | df$MIS_Status=="P I F"),]

df3 <- df[(df$MIS_Status != "CHGOFF") & (df$MIS_Status != "P I F"),]



# recheck for null values
sapply(df2, function(x) sum(is.na(x)))


# Aim to predict whether a loan should have been issued or not based on characteristics given.

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

train %>% group_by(is_lowdoc) %>% 
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

# number of loans by year # large increase in 2006-07
train %>% ggplot(aes(ApprovalFY)) + 
        geom_histogram(col='black', fill='gray', binwidth = 1) +
        facet_grid(defaulted ~ .) + ylab("Number of Loans") + xlab("Approval Year")


# default rate by sector
train %>% group_by(Definition) %>% summarise(default_rate = sum(defaulted)*100/n()) %>%
        ggplot(aes(Definition, default_rate)) + geom_line() + xlab("Industry") +
        ylab("Default Rate (%)")

# default rate by revolving line of credit
train %>% group_by(RevLineCr) %>% summarize(num = n())

train %>% group_by(RevLineCr) %>% filter(RevLineCr == "Y" | RevLineCr == "N") %>%
        summarise(default_rate = sum(defaulted)*100/n())



