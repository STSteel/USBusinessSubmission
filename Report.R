# This reports is based on a study of business and industry trends in the United States 
# using the US Census Bureau data.
# The inspiration for this study is the hypothesis that US National level of value generation,
# as an indication of economic optimism, drives the antecipation of new property demand. 
# The analysis looks for regional variations in the US and attempts to predict the regional 
# growth of new home construction and sales.

# The source of data is https://www.kaggle.com/census/business-and-industry-reports

# Understanding the data
# The original dataset has many economic factors that will not be used in this study.
# The time series that are relevant to the subject of this report are the 
# macroeconomic indicators (Financial Reports) and the new housing indicators 
# (New Home Sales, New Residential Construction)
# As all dates indicate the begining of the analysis period, they have been used to align 
# the time series into quarterly periods.
#



# install.packages("tidytext")
# install.packages("textdata")

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(caret)
library(data.table)
library(rpart)
library(randomForest)

options(digits = 3)    # 3 significant digits

getwd()
filename <- "CapstoneProject_data.csv"
metadata <- "CapstoneProject_metadata.csv"

# Loading time series data
temp <- read_file(filename)
data <-read_csv(temp, guess_max = 1000000)
df <- as.data.frame(data)
df <- df %>% mutate(value = as.numeric(value))


# Loading metadata to enable selection of Housing indicators and overall Financial results
temp <- read_file(metadata)
md <-read_csv(temp)
md_df <- as.data.frame(md)

# What time series are used for new home sales and housing under construction indicators? 
housing_cat <- md_df %>% 
  filter((report=="New Residential Construction" | report=="New Home Sales"), 
         dt_code=="TOTAL",
         is_adj==0)
str(housing_cat)

# What time series are used for value creation in the financial reports (sales, revenue)? 
finance_cat <- md_df %>% 
  filter(report=="Quarterly Financial Report", 
         dt_code=="101",
         is_adj==0)
str(finance_cat)



# Loading the relevant time series for analysis

# All activity related to new dwellings, as a proxy for expected regional demand for housing
housing_dat <- df %>% filter(time_series_code %in% housing_cat$time_series_code)
head(housing_dat)

# All sales and revenue at national level, keeping all the aggregated numbers per industry (category_level=0)
finance_cat <- finance_cat %>% filter(category_level==0)
finance_dat <- df %>% filter(time_series_code %in% finance_cat$time_series_code)
# Separate industry category as a column
finance_dat <- finance_dat %>% mutate(industry = str_trunc(time_series_code, 3, side="right", ellipsis=""))
finance_dat <- finance_dat %>% mutate(industry = as.factor(industry))
head(finance_dat)

# Trimming the housing time series according to available financial data
housing_dat <- housing_dat %>% filter(date>=min(finance_dat$date) & date<=max(finance_dat$date))
min(housing_dat$date) == min(finance_dat$date)
max(housing_dat$date) == max(finance_dat$date)


# Aggregate quarterly for analysis alongside financial data
housing_dat <- housing_dat %>% 
  mutate(quarter = as_date(ifelse(month(date)==2 | month(date)==3,
                                               as_date(paste(as.character(year(date)),'01',as.character(day(date)), sep="-")),
                           ifelse(month(date)==5 | month(date)==6,
                                               as_date(paste(as.character(year(date)),'04',as.character(day(date)), sep="-")),
                           ifelse(month(date)==8 | month(date)==9,
                                               as_date(paste(as.character(year(date)),'07',as.character(day(date)), sep="-")),
                           ifelse(month(date)==11 | month(date)==12,
                                               as_date(paste(as.character(year(date)),'10',as.character(day(date)), sep="-")),
                                               date))))))

housing_dat %>% group_by(quarter) %>% summarise(n())

# Override date column for easier manipulation later
housing_dat <- housing_dat %>% mutate(date=quarter)

# Separate state and national levels
housing_dat <- housing_dat %>% mutate(region = str_trunc(time_series_code, 2, side="left", ellipsis=""))
housing_dat <- housing_dat %>% mutate(region = as.factor(region))
# Aggregating all new house activity
housing_dat <- housing_dat %>% group_by(date, region) %>% summarise(value=sum(value))
housing_dat_total <- data.frame(housing_dat)
str(housing_dat_total)

# Visualising the effects of National value generation on the antecipation of housing demand (i.e. new build) regionally.

# Firstly, the 2008 housing market shock is clearly visible in both the financial and housing data
graph_fin <- finance_dat %>% group_by(date) %>% summarise(value_generated=sum(value)) %>% 
  ggplot(aes(x=date, y=value_generated)) + 
  ylab("Sales and Revenue in USD Millions") +
  ggtitle("US total from Q4/2000 to Q1/2017") +
  geom_line() + geom_vline(xintercept=date("2008-09-15"), colour="red") +
  annotation_custom(grid::linesGrob())
graph_fin

avg <- housing_dat %>% ungroup() %>% filter(region=="US") %>% select(v=value) %>% summarise(mean(v))
graph_h_us <- housing_dat_total %>% filter(region=="US") %>% 
   group_by(region, date) %>% summarise(new_builds=sum(value)) %>% 
   ggplot(aes(x=date, y=new_builds)) + 
   ylab("Thousands of new housing units") +
   ggtitle("US total from Q4/2000 to Q1/2017") +
   geom_point() + geom_hline(yintercept=as.numeric(avg), colour="blue") +
   geom_vline(xintercept=date("2008-09-15"), colour="red")
graph_h_us

# Looking closer at each region, the volatility in the South is far greater than the other regions.
# The Northeast is stable relatively to the national income fluctuations.
graph_h_reg <- housing_dat_total %>% filter(region!="US") %>% 
  group_by(region, date) %>% summarise(new_builds=sum(value)) %>% 
  ggplot(aes(x=date, y=new_builds, colour=region)) + 
  ylab("Thousands of new housing units") +
  ggtitle("Total per region from Q4/2000 to Q1/2017") +
  geom_smooth() #geom_point() #+ facet_grid(region ~ .)
graph_h_reg


# Adding a lag of 1 year based on sentiment of growth (sales, revenue) and new house activity
housing_dat_total <- housing_dat_total %>% mutate(previous_date = as_date(paste(as.character(year(date)-1),
                                                   as.character(month(date)),
                                                   as.character(day(date)), sep="-")))
housing_dat_total <- housing_dat_total %>% mutate(date=previous_date)
head(housing_dat_total)

# Matrix for prediction, aligning all values by quarter
dataset <- inner_join(finance_dat, housing_dat_total, by = "date")
dataset <- dataset %>% mutate(new_houses_K = value.y, 
                              national_income_M = value.x) %>%
                       dplyr::select(date, region, industry, new_houses_K, national_income_M)

head(dataset)

# Removing US totals
dataset <- dataset %>% filter(!is.na(national_income_M), region!="US")

# Pre 2008 (darker shades), construction was higher in the South and West regions.
# But over time, it seems negatively influenced by income, especially in the Mining industry
dataset %>% 
  ggplot(aes(x=log10(national_income_M), y=new_houses_K, colour=year(date))) + 
  xlab("Sales and Revenue in USD Millions (log10)") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)

# This requires centreing the analysis on regional means

housing_avg <- dataset %>% group_by(region) %>% summarise(avg = mean(new_houses_K))
dataset <- inner_join(dataset, housing_avg, by="region")

# Trend = 1 meaning positive, if more new houses than average are predicted
# Trend = 0 meaning negative, if fewer new houses than average are predicted

dataset <- dataset %>% mutate(trend=as.factor(ifelse(new_houses_K>=avg,1,0)))
head(dataset)

# Splitting training and test datasets

# Validation set will be 50% of Housing data
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = dataset$new_houses_K, times = 1, p = 0.5, list = FALSE)
train_ds <- dataset[-test_index[,1],]
test_ds <- dataset[test_index[,1],]



# Training 

# Selecting predictors (Training Set)
x <- train_ds %>% dplyr::select (date, region, industry, national_income_M, avg)
y <- train_ds$trend


# ============= GLM ==============
fit <- train(x, y, method = "glm")
fit$results
# Testing
y_hat_GLM <- predict(fit, test_ds) 
y_hat <- y_hat_GLM
acc <- confusionMatrix(data = y_hat, reference = test_ds$trend)$overall["Accuracy"]
res <- data.frame(model="GLM", acc=acc)


# ========= Random Forest ========
fit <- randomForest(y ~ ., data = x, nodesize = 50, maxnodes = 25)
fit$importance
# Testing
y_hat_RF <- predict(fit, test_ds) 
y_hat <- y_hat_RF
acc <- confusionMatrix(data = y_hat, reference = test_ds$trend)$overall["Accuracy"]
res <- rbind(res, data.frame(model="RF", acc=acc))

# ========= KNN ============

ks <- seq(1, 31, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(y ~ ., data = x, k = k)
  y_hat <- predict(fit, test_ds, type = "class") %>% 
    factor(levels = levels(test_ds$trend))
  F_meas(data = y_hat, reference = test_ds$trend)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]
fit <- knn3(y ~ ., data = x, k = ks[which.max(F_1)])
y_hat_KNN <- predict(fit, test_ds, type = "class")
y_hat <- y_hat_KNN
acc <- confusionMatrix(data = y_hat, reference = test_ds$trend)$overall["Accuracy"]
res <- rbind(res, data.frame(model="KNN", acc=acc))


# ======= Ensemble ===========
# ENSEMBLE #
y_set <- data.frame(y_hat_GLM = as.integer(ifelse(y_hat_GLM==1, 1, 0)),
                    y_hat_KNN10 = as.integer(ifelse(y_hat_KNN10==1, 1, 0)),
                    y_hat_RF = as.integer(ifelse(y_hat_RF==1, 1, 0)))

y_set <- y_set %>% mutate(ens_y_hat = round((y_hat_GLM + y_hat_KNN10 + y_hat_RF)/3))
                                               
y_set <- y_set %>% mutate(y_hat = as.factor(ens_y_hat))
levels(y_set$y_hat) <- levels(test_ds$trend)
head(y_set)
acc <- confusionMatrix(y_set$y_hat, test_ds$trend)$overall["Accuracy"]
res <- rbind(res, data.frame(model="Ensemble", acc=acc))
res %>% knitr::kable()
