# This reports is based on a study of business and industry trends in the United States 
# using the US Census Bureau data.
# The inspiration for this study is the hypothesis that US National level of value generation,
# as an indication of economic optimism, drives the anticipation of new property demand. 
# The analysis looks for regional variations in the US and attempts to predict the regional 
# growth of new home construction and sales.

# The source of data is https://www.kaggle.com/census/business-and-industry-reports

##########################
# 
# Preparing the environment:packages, libraries and data files
#
##########################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
#if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
#if(!require(textdata)) install.packages("textdata", repos = "http://cran.us.r-project.org")
#if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(stringr)
library(tidyverse)
library(dplyr)
library(caret)
library(tinytex)
library(lubridate)
library(tidytext)
library(data.table)

library(rpart)
library(ggplot2)
library(randomForest)

options(digits = 3)    # 3 significant digits

# Loading time series data
temp <- read_file(unzip("CapstoneProject_data.csv.zip", "CapstoneProject_data.csv"))
data <-read_csv(temp, guess_max = 1000000)
df <- as.data.frame(data)
df <- df %>% mutate(value = as.numeric(value))

# Loading metadata to enable selection of Housing indicators and overall Financial results
temp <- read_file(unzip("CapstoneProject_metadata.csv.zip", "CapstoneProject_metadata.csv"))
md <-read_csv(temp)
md_df <- as.data.frame(md)

# Remove temporary variables
rm(md, data, temp)

##########################
# 
# Understanding and preparing the data:
# The original dataset has many economic factors that will not be used in this study.
# The time series that are relevant to the subject of this report are the 
# macroeconomic indicators (Financial Reports) and the new housing indicators 
# (New Home Sales, New Residential Construction)
# As all dates indicate the beginning of the analysis period, they have been used to align 
# the time series into quarterly periods.
#
##########################

# What time series are used for new home sales and housing under construction indicators? 
housing_cat <- md_df %>% 
  filter((report=="New Residential Construction" | report=="New Home Sales"), 
         dt_code=="TOTAL",
         is_adj==0)

housing_cat %>% group_by(report) %>% summarise (number_of_timeseries=n()) %>% knitr::kable() 

# What time series are used for value creation in the financial reports (sales, revenue)? 
finance_cat <- md_df %>% 
  filter(report=="Quarterly Financial Report", 
         dt_code=="101",
         is_adj==0)

finance_cat %>% group_by(report) %>% summarise (number_of_timeseries=n()) %>% knitr::kable() 

# All activity related to new dwellings, as a proxy for expected regional demand for housing
housing_dat <- df %>% filter(time_series_code %in% housing_cat$time_series_code)

print("First 10 rows of relevant Housing Data")
head(housing_dat, 10) %>% knitr::kable()
print(c("Total rows for Housing Data: ", nrow(housing_dat)))

# All sales and revenue at national level, keeping all the aggregated numbers per industry (category_level=0)
finance_cat <- finance_cat %>% filter(category_level==0)
finance_dat <- df %>% filter(time_series_code %in% finance_cat$time_series_code)
# Separate industry category as a column
finance_dat <- finance_dat %>% mutate(industry = str_trunc(time_series_code, 3, side="right", ellipsis=""))
finance_dat <- finance_dat %>% mutate(industry = as.factor(industry))

print("First 10 rows of relevant Financial Data")
head(finance_dat, 10) %>% knitr::kable()
print(c("Total rows for Financial Data: ", nrow(finance_dat)))

# Trimming the housing time series according to available financial data
housing_dat <- housing_dat %>% filter(date>=min(finance_dat$date) & date<=max(finance_dat$date))
min(housing_dat$date) == min(finance_dat$date)
max(housing_dat$date) == max(finance_dat$date)

rm(df, md_df)

# Aggregate housing data quarterly in order to align with the granularity of financial data
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

# Extract 2 letters representing the State from the time series code so that we can separate regional and national levels later
housing_dat <- housing_dat %>% mutate(region = str_trunc(time_series_code, 2, side="left", ellipsis=""))
housing_dat <- housing_dat %>% mutate(region = as.factor(region))

# Aggregating all new house activity
housing_dat <- housing_dat %>% group_by(date, region) %>% summarise(value=sum(value))
housing_dat_total <- data.frame(housing_dat)
str(housing_dat_total)

# Visualising the effects of National value generation on the anticipation of housing demand (i.e. new build) regionally.

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

# Looking closer at each region, the house offer variability in the South is far greater than the other regions.
# The Northeast is stable relatively to the national income fluctuations.
graph_h_reg <- housing_dat_total %>% filter(region!="US") %>% 
  group_by(region, date) %>% summarise(new_builds=sum(value)) %>% 
  ggplot(aes(x=date, y=new_builds, colour=region)) + 
  ylab("Thousands of new housing units") +
  ggtitle("Total per region from Q4/2000 to Q1/2017") +
  geom_line()
graph_h_reg


# Industry codes and description
print("Industry codes and description")
finance_cat %>% select(Code=cat_code, Description=cat_desc) %>% knitr::kable()

# Adding a time lag in years (1, 2, 3, 5, 8, 13) between sentiment of growth (sales, revenue) and new house activity

finance_dat <- finance_dat %>% mutate(lag_1y = as_date(paste(as.character(year(date)+1), as.character(month(date)),as.character(day(date)), sep="-")),
                                      lag_2y = as_date(paste(as.character(year(date)+2), as.character(month(date)),as.character(day(date)), sep="-")),
                                      lag_3y = as_date(paste(as.character(year(date)+3), as.character(month(date)),as.character(day(date)), sep="-")),
                                      lag_5y = as_date(paste(as.character(year(date)+5), as.character(month(date)),as.character(day(date)), sep="-")),
                                      lag_8y = as_date(paste(as.character(year(date)+8), as.character(month(date)),as.character(day(date)), sep="-")),
                                      lag_13y = as_date(paste(as.character(year(date)+13), as.character(month(date)),as.character(day(date)), sep="-")))


print("First 10 rows of relevant Financial Data, with extra columns representing time lag")
head(finance_dat, 10)

# Matrix for prediction, aligning all values by quarter: 
# anchoring the year of housing offer and bringing in financial results according to time lag.

temp <- left_join(housing_dat_total, finance_dat, by= c("date"="lag_1y"))
temp <- temp %>% mutate(new_houses_K = value.x, nat_income_M_1y = value.y) %>% 
  dplyr::select(date, region, industry, new_houses_K, nat_income_M_1y)

temp <- left_join(temp, finance_dat, by= c("date"="lag_2y", "industry"="industry"))
temp <- temp %>% mutate(nat_income_M_2y = value) %>% 
  dplyr::select(date, region, industry, new_houses_K, 
                nat_income_M_1y, nat_income_M_2y)

temp <- left_join(temp, finance_dat, by= c("date"="lag_3y", "industry"="industry"))
temp <- temp %>% mutate(nat_income_M_3y = value) %>% 
  dplyr::select(date, region, industry, new_houses_K,
                nat_income_M_1y, nat_income_M_2y, nat_income_M_3y)

temp <- left_join(temp, finance_dat, by= c("date"="lag_5y", "industry"="industry"))
temp <- temp %>% mutate(nat_income_M_5y = value) %>% 
  dplyr::select(date, region, industry, new_houses_K, 
                nat_income_M_1y, nat_income_M_2y, nat_income_M_3y, nat_income_M_5y)


temp <- left_join(temp, finance_dat, by= c("date"="lag_8y", "industry"="industry"))
temp <- temp %>% mutate(nat_income_M_8y = value) %>% 
  dplyr::select(date, region, industry, new_houses_K,  
                nat_income_M_1y, nat_income_M_2y, nat_income_M_3y, nat_income_M_5y, nat_income_M_8y)

temp <- left_join(temp, finance_dat, by= c("date"="lag_13y", "industry"="industry"))
temp <- temp %>% mutate(nat_income_M_13y = value) %>% 
  dplyr::select(date, region, industry, new_houses_K,  
                nat_income_M_1y, nat_income_M_2y, nat_income_M_3y, nat_income_M_5y, nat_income_M_8y, nat_income_M_13y)

# Removing rows that have no industry-level information
temp <- temp %>% filter(!is.na(industry))

print("Number of data points available when the different time lags were introduced")
temp %>% group_by(industry) %>% summarise(sum1y = sum(!is.na(nat_income_M_1y)),
                                          sum2y = sum(!is.na(nat_income_M_2y)),
                                          sum3y = sum(!is.na(nat_income_M_3y)),
                                          sum5y = sum(!is.na(nat_income_M_5y)),
                                          sum8y = sum(!is.na(nat_income_M_8y)),
                                          sum13y = sum(!is.na(nat_income_M_13y))) %>% knitr::kable()


# Removing all US housing totals because the analysis is regional only
dataset <- temp %>% filter(region!="US")
rm(temp)

# This shows that there are no data points for 8- and 13-year lag for Information and Professional and Technical services.
# Therefore, 8- and 13-year lag will not be considered in the building of predictive models.
# Pre 2008 (darker shades), construction was higher in the South and West regions.
# But over time, it seems negatively influenced by income, especially in the Mining industry

dataset %>% 
  ggplot(aes(x=log10(nat_income_M_1y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 1 year income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)

dataset %>% 
  ggplot(aes(x=log10(nat_income_M_2y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 2 years income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)

dataset %>% 
  ggplot(aes(x=log10(nat_income_M_3y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 3 years income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)



dataset %>% 
  ggplot(aes(x=log10(nat_income_M_5y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 5 years income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)


dataset %>% 
  ggplot(aes(x=log10(nat_income_M_8y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 8 years income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)

dataset %>% 
  ggplot(aes(x=log10(nat_income_M_13y), y=new_houses_K, colour=year(date))) + 
  theme(axis.text = element_text(size = 8))  +
  theme(axis.title = element_text(size = 10))  +
  theme(text = element_text(size = 8))  +
  xlab("Sales and Revenue in USD Millions (log10), with 13 years income lag") +
  ylab("New Housing in Thousands of Units") +
  ggtitle("Revenue and New Housing, per region and industry") +
  geom_point()  + 
  facet_grid(industry ~ region)

dataset <- dataset %>% dplyr::select(date, region, industry, new_houses_K,  
                                     nat_income_M_1y, nat_income_M_2y, nat_income_M_3y, nat_income_M_5y)


###################
# Considerable differences in construction output per region will require the analysis to be centred on regional means
###################

housing_avg <- dataset %>% group_by(region) %>% summarise(avg = mean(new_houses_K))
dataset <- inner_join(dataset, housing_avg, by="region")

# Trend = 1 meaning positive, if more new houses than average are predicted
# Trend = 0 meaning negative, if fewer new houses than average are predicted

dataset <- dataset %>% mutate(trend=as.factor(ifelse(new_houses_K>=avg,1,0)))

print("First 10 rows showing the addition of Trend (0 or 1) to the dataset for each region")
head(dataset)

# Splitting training and test datasets

# The dataset will be split into Training (0.9) and Validation (0.1) sets for fitting the model and testing, respectively.

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = dataset$new_houses_K, times = 1, p = 0.1, list = FALSE)
train_ds <- dataset[-test_index[,1],]
test_ds <- dataset[test_index[,1],]

# Training 
lag <- c(1,2,3,5)   # Time lag expressed as reduced Fibonacci sequence

res <- sapply(lag, function(i) {

  train_ds_clean <- train_ds %>% dplyr::select(trend, region, industry,
                                 nat_income=c(paste("nat_income_M_",i, "y", sep=""))) %>%
                                 filter(!is.na(nat_income)) # Removing NA in the lagged income
  
  x <- train_ds_clean %>% dplyr::select(region, industry, nat_income)
    
  y <- train_ds_clean$trend # Outcome used to train the model
  
  # Align names of columns between Training and Testing 
  # (rename columns to match the name of the national income with time lag in the loop)
  # (ensure there are no NAs in predictors)
  test_ds_clean <- test_ds %>% dplyr::select(trend, region, industry, nat_income=c(paste("nat_income_M_",i,"y", sep=""))) %>%
                               filter(!is.na(nat_income)) # Removing NA in the lagged income
  
  # ============= GLM ==============
  fit <- train(x, y, method = "glm")
  fit$results
  # Testing 
  y_hat_GLM <- predict(fit, test_ds_clean) 
  y_hat <- y_hat_GLM
  acc <- confusionMatrix(data = y_hat, reference = test_ds_clean$trend)$overall["Accuracy"]
  
  res <- data.frame() # Initiate as empty data frame of results 
  res <- rbind(res, data.frame(Model=paste("GLM",i, "year_lag", sep="_"), Accuracy=acc))

  # ========= Random Forest ========
  fit <- randomForest(y ~ ., data = x, nodesize = 50, maxnodes = 25)
  fit$importance
  # Testing
  y_hat_RF <- predict(fit, test_ds_clean) 
  y_hat <- y_hat_RF
  acc <- confusionMatrix(data = y_hat, reference = test_ds_clean$trend)$overall["Accuracy"]
  res <- rbind(res, data.frame(Model=paste("RandomForest",i, "year_lag", sep="_"), Accuracy=acc))
  
  # ========= KNN ============
  
  ks <- seq(1, 51, 3)
  F_1 <- sapply(ks, function(k){
    fit <- knn3(y ~ ., data = x, k = k)
    y_hat <- predict(fit, test_ds_clean, type = "class") %>% 
      factor(levels = levels(test_ds_clean$trend))
    F_meas(data = y_hat, reference = test_ds_clean$trend)
  })
  max(F_1)
  k_max <- ks[which.max(F_1)]
  print(paste("For time lag =", i, "year(s), the optimal k parameter for KNN =",k_max, sep=" "))
  plot(ks, F_1)
  fit <- knn3(y ~ ., data = x, k= k_max)
  y_hat_KNN <- predict(fit, test_ds_clean, type = "class")
  y_hat <- y_hat_KNN
  acc <- confusionMatrix(data = y_hat, reference = test_ds_clean$trend)$overall["Accuracy"]
  res <- rbind(res, data.frame(Model=paste("KNN", k_max,i, "year_lag", sep="_"), Accuracy=acc))
  
  
  # ======= Ensemble ===========
  # ENSEMBLE of all models, i.e. looking at all results and selecting the predominant one#
  y_set <- data.frame(y_hat_GLM = as.integer(ifelse(y_hat_GLM==1, 1, 0)),
                      y_hat_KNN = as.integer(ifelse(y_hat_KNN==1, 1, 0)),
                      y_hat_RF = as.integer(ifelse(y_hat_RF==1, 1, 0)))
  
  y_set <- y_set %>% mutate(ens_y_hat = round((y_hat_GLM + y_hat_KNN + y_hat_RF)/3))
  
  y_set <- y_set %>% mutate(y_hat = as.factor(ens_y_hat))
  levels(y_set$y_hat) <- levels(test_ds_clean$trend)
  acc <- confusionMatrix(y_set$y_hat, test_ds_clean$trend)$overall["Accuracy"]
  res <- rbind(res, data.frame(Model=paste("Ensemble",i, "year_lag", sep="_"), Accuracy=acc))
  
  # Table with combined results of models for all time lags  
  # res %>% knitr::kable()
  pull(res)
}) 

colnames(res) <- c("1_year_lag","2_year_lag","3_year_lag","5_year_lag")
rownames(res) <- c("GLM", "RandomForest","KNN", "Ensemble")

print("Accuracy results of predictive models built and tested in this report")
res %>% knitr::kable()

graph_results <- as.data.frame(res) %>% 
                 rownames_to_column(var="Model") %>%
                 gather(key="Lag", value="Accuracy", -Model) %>%
                 ggplot(aes(x=Lag, y=Accuracy, colour=Model)) + 
                 geom_point() 
graph_results


