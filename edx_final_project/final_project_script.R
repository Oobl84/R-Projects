# Loan Risk Assessment Project

######################## 
# Load Required Packages
########################
 
 # Note: this process could take a couple of minutes
 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(GGally)
library(fastDummies)
library(RColorBrewer)
library(corrplot)
 
memory.limit(size=350000)
 
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

#drop na rows
df <- df %>% drop_na()

# checking sectors
df %>% group_by(NAICS) %>% summarise(num=n()) %>% arrange(desc(num))

# 22% of rows don't have industry defined. Will keep for now but may discard later
df %>% summarise(undefined = sum(NAICS == 0)/n())

# add undefined to sector table
sectors <- sectors %>% add_row(Sector = 0, Definition = 'UNDEFINED', .before = 1)

sectors

# name too long so will shorten
sectors[sectors == "Agriculture, Forestry, Fishing and Hunting"] <- "Agg, Forest, Fish, Hunt"
sectors[sectors == "Mining, Quarrying, and Oil and Gas Extraction"] <- "Mine, Quarry, Oil & Gas"
sectors[sectors == "Administrative and Support and Waste Management and Remediation Services"] <- "Admin & Support, Waste, Remediation"

# get first two nums from NAIC col to join with sectors
df$NAIC_2 <- as.factor(as.numeric(substr(df$NAICS,1,2)))

df %>% group_by(NAIC_2) %>% summarise(n())

# get first 3 digits of zip
df$zip_2 <- as.factor(substr(as.character(df$Zip),1,3))

ex_zips <- df %>% group_by(zip_2) %>% summarise(num = n()) %>% filter(num < 30) 

df <- df %>% subset(!(zip_2 %in% ex_zips$zip_2))

# join
df <- merge(df,sectors,by.x = "NAIC_2", by.y = "Sector", all.x = TRUE)

df <- df %>% rename(Sector=Definition)
################
# Data Cleaning
################
# Setting columns to match the data key

str(df)

df %>% group_by(City) %>% summarise(num=n()) %>% arrange(num)

# using zips for region so wil drop city, state

#check new or existing business
df %>% group_by(NewExist) %>% summarise(num = n())

# remove those without status

df <- df %>% filter(NewExist != 0)

# convert to binary
df['is_newBiz'] <- as.factor(as.numeric(df$NewExist == 2))

# check franchise code
df %>% group_by(FranchiseCode) %>% summarise(num = n())

#add new column for franchise
df$is_franchise <- as.factor(as.numeric(df$FranchiseCode != 0 & df$FranchiseCode != 1))


df %>% group_by(is_franchise) %>% summarise(num = n())

# checking urban rural
df %>% group_by(UrbanRural) %>% summarise(num = n())

# cleaning revolving line of credit column

df %>% group_by(RevLineCr) %>% summarise(num = n())

# Lots of extraneous codes outside of the key. Reasonable to assume that 0 is equivalent to N and 1 to Y, but drop all others
df$RevLineCr[df$RevLineCr=="0"] <- "N"
df$RevLineCr[df$RevLineCr=="1"] <- "Y"

# Rather than guess what the values might equate to it's best to just drop the ones that don't fit
df <- df %>% filter(RevLineCr == "Y" | RevLineCr == "N")

# cleaning revolving line of credit column
df$RevLineCr <- as.factor(df$RevLineCr)

df %>% group_by(RevLineCr) %>% summarise(num = n())

# check low doc column
df %>% group_by(LowDoc) %>% summarise(num = n())

# remove extraneous values
df <- df %>% filter(LowDoc %in% c("Y", "N", "0", "1"))

# convert 0 1 to N and Y respectively
df$LowDoc[df$LowDoc == "0"] <- "N"
df$LowDoc[df$LowDoc == "1"] <- "Y"

df$LowDoc <- as.factor(df$LowDoc)

# drop any rows that don't have a target status. 1997 rows

df %>% group_by(MIS_Status) %>% summarise(n())

# keep only rows with a completed status
df <- df[(df$MIS_Status=="CHGOFF" | df$MIS_Status=="P I F"),]

# create a numerical column for default status. This will be the target variable
df["defaulted"] <- as.factor(as.numeric(df$MIS_Status == "CHGOFF"))

# columns I definitely won't be using

# drop columns that would give away the loan status
df <- df %>% select(-c("ChgOffPrinGr", "ChgOffDate","BalanceGross"))

# drop city and name and anything I've created alternative columns for
df <- df %>% select(-c("Name", "City", "NAICS","Bank", "MIS_Status","Zip","FranchiseCode", "LoanNr_ChkDgt", "NewExist"))


# columns need to be converted to factors before splitting data into train and validation sets

factor_cols <- c("State", "zip_2", "BankState", "NAIC_2",
          "UrbanRural", "Sector")

num_cols <-c("DisbursementGross", "GrAppv", "SBA_Appv", "ApprovalFY")

date_cols <- c("DisbursementDate", "ApprovalDate")

df[factor_cols] <- lapply(df[factor_cols],as.factor)

df[num_cols] <- lapply(df[num_cols], parse_number)

df[date_cols] <- lapply(df[date_cols], parse_date_time2, orders=c("dby"))



# Feature engineering

#check whether bank state is same as borrower state
df %>% summarise(out_of_state = sum(as.character(df$State) != as.character(df$BankState))/n())

# nearly half of loans come from out of state banks
df['is_out_of_state_loan'] <- as.factor(as.numeric(as.character(df$State) != as.character(df$BankState)))

# check whether the guarantee is for the same amount as the loan
df["guarantee_covers_loan"] <- as.factor(as.numeric(df$SBA_Appv >= df$GrAppv))

df["disbursement_less_than_approved"] <- as.factor(as.numeric(df$DisbursementGross < df$GrAppv))


# check correlation of all pairs
df.cor <- cor(df[sapply(df, is.numeric)],method="spearman")

corrplot(df.cor)

# Aim to predict whether a loan should have been issued or not based on characteristics given.

# Validation set will be 10% of SBA data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = df$defaulted, times = 1, p = 0.2, list = FALSE)
train <- df[-test_index,]


tmp <- df[test_index,]

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_split <- createDataPartition(y=tmp$defaulted, times=1, p=0.5, list = FALSE)
test <- tmp[-test_split,]
validation <- tmp[test_split,]

rm(df, df2, tmp, num_cols, date_cols, factor_cols, drop_cols, logic_cols_yn, logic_cols_other, test_index, test_split)

########
# Model
########


# target variable distribution

train %>% ggplot(aes(defaulted)) + 
        geom_histogram(stat="count") + 
        theme_economist()

# ~18% of loans have defaulted.
avg_default <- sum((train$defaulted == 1))*100/nrow(train)

state_defaults <- train %>% group_by(State) %>% 
        summarise(default_percent = sum(defaulted == 1)*100/n())


# default rate by state
state_defaults %>% ggplot() + 
        geom_point(aes(reorder(State,default_percent),default_percent)) +
        ylab("default percent") +
        xlab("State") + 
        theme(axis.text.x = element_text(angle = 90))

# default rate by bank_state

bank_state_defaults <- train %>% group_by(BankState) %>% 
        summarise(bs_default_rate = sum(defaulted ==1)*100/n())

bank_state_defaults %>%
        ggplot() + 
        geom_point(aes(reorder(BankState,bs_default_rate),bs_default_rate, colour="Bank State")) + 
        geom_point(aes(State,default_percent, colour="Business State"),data=state_defaults, shape=17) + 
        xlab("State/Bank State") + theme(axis.text.x = element_text(angle = 90))

# default_rate by Low doc status

train %>% group_by(LowDoc) %>% 
        summarize(default_rate = sum(defaulted == 1)*100/n())

## lowdoc status companies are half as likely to default as non-lowdoc companies

# defaults by year
train %>% count(ApprovalFY, defaulted) %>% ggplot(aes(ApprovalFY, n )) + 
        geom_col(aes(fill=defaulted)) + xlab("Approval Year") + ylab("Number of Loans") + theme(axis.text.x = element_text(angle = 90))

#default rate by year
train %>% group_by(ApprovalFY) %>% 
        summarise(default_rate = sum(defaulted)*100/n(), num = n()) %>% 
        ggplot(aes(ApprovalFY,default_rate, size=num)) + geom_line(alpha=0.5) + geom_point(colour ="red", shape=17, line="black") +
        xlab("Approval Year") + ylab("Default Rate (%)") + theme_economist()


# default rate by sector
train %>% group_by(NAIC_2,Sector) %>% 
        summarise(default_rate = sum(defaulted ==1)*100/n(), num = n()) %>%
        ggplot(aes(NAIC_2, default_rate,colour=Sector, shape=Sector)) + 
        geom_point(size=4) + 
        xlab("Industry") + ylab("Default Rate (%)") + 
        theme_economist() + 
        theme(legend.text = element_text(size=7)) + 
        scale_shape_manual(values=rep(c(15,16,17,18,19),each=5)) +
        scale_colour_manual(values=rep(brewer.pal(5,"Set1"),times=5)) +
        geom_hline(colour="blue",size=1, yintercept = avg_default)

# default rate by revolving line of credit
train %>% ggplot(aes(RevLineCr)) + 
        geom_bar() + 
        facet_wrap(vars(as.factor(defaulted)),labeller = as_labeller(c("0" = "Paid in Full","1" = "Defaulted"))) +
        xlab("Revolving Line of Credit") + scale_y_continuous(labels=scales::comma) + theme_economist()

# default rate by new business
train %>% ggplot(aes(as.factor(is_newBiz))) + 
        geom_bar() + 
        facet_wrap(vars(as.factor(defaulted)),labeller = as_labeller(c("0" = "Paid in Full","1" = "Defaulted"))) +
        xlab("Is New Business") + scale_y_continuous(labels=scales::comma) + theme_economist()

# default rate by whether the loan is fully guaranteed by scheme
train %>% ggplot(aes(as.factor(guarantee_covers_loan))) + 
        geom_bar() + 
        facet_wrap(vars(as.factor(defaulted)),labeller = as_labeller(c("0" = "Paid in Full","1" = "Defaulted"))) +
        xlab("Fully Guaranteed Loan") + scale_y_continuous(labels=scales::comma) + theme_economist()


# default rate by jobs created
train %>% ggplot(aes(CreateJob, colour=as.factor(defaulted), fill = as.factor(defaulted))) + 
        geom_histogram() + 
        scale_y_log10(labels = scales::comma) + 
        theme_economist() + xlab("Jobs Created") + ylab("Number of Loans")


# building model

glm(formula = defaulted ~ NoEmp + GrAppv + SBA_Appv + LowDoc + Term + RetainedJob + CreateJob, data=train)
