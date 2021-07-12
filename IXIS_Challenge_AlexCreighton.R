#IXIS Code Challenge
#Online Retailer Performance Analysis
#Alex Creighton
#July 2021


#set working directory, import datasets and call necessary libraries

install.packages('xlsx')
install.packages("janitor")

library(dplyr)
library(ggplot2)
library(readxl)
library(waffle)
library(stringr)
library(tidyverse)
library(zoo)
library(janitor)
require(openxlsx)

setwd("/Users/alexcreighton/Desktop/Personal Projects/IXIS")
sessionCounts <- read.csv("sessionCounts.csv")
addsToCart <- read.csv("addsToCart.csv")



############ PART 1
### sheet 1

#calculate and set ECR variable

sessionCounts$ECR <- sessionCounts$transactions/sessionCounts$sessions


#convert dim_date to Date format, grab month number from dim_date and convert to month
#abbreviation, set month variable

sessionCounts$dim_date <- as.Date(sessionCounts$dim_date, "%m/%d/%y") #as.Date converts var to Date
month_num <- format(sessionCounts$dim_date, "%m") #select just month from date, set month_num variable
sessionCounts$month_num <- as.numeric(month_num) #add month_num variable as number value to sessionCounts
sessionCounts$month <- month.abb[sessionCounts$month_num] #set month variable as abbreviated month name


#rename variable for Device

sessionCounts<- rename(sessionCounts, device = dim_deviceCategory)


#create sheet of Month * Device aggregation of data

month_by_device <- data.frame(sessionCounts$month, sessionCounts$device, sessionCounts$sessions, 
                     sessionCounts$transactions, sessionCounts$QTY, sessionCounts$ECR) #add necessary columns to new dataset for sheet 1 

newnames = c('Month', 'Device', 'Sessions', 'Transactions', 'QTY', 'ECR') #set newnames to rename the columns in month_by_device
colnames(month_by_device) <- newnames #set column names in month_by_device as newnames



############ PART 2
### sheet 2

#rename dim_month variable, merge sessionCounts and addsToCart on month_num variable

addsToCart <- rename(addsToCart, month_num = dim_month) #rename dim_month to be able to merge datasets on same variable

merged <- merge(sessionCounts, addsToCart, by='month_num') #merge datasets on month_num variable

merged <- merged[rev(order(as.Date(merged$dim_date))),] #reorder dataset based on most recent date


#condense data into total/average per specific month statistics for each metric being evaluated

merged <- merged %>% 
  mutate(
    YearMonth = format(dim_date, "%Y-%m") 
  ) #create new YearMonth variable that displays just the year and month 

condensed <- merged %>% #create new dataframe 'condensed' for per month statistics
  group_by(YearMonth) %>% #group data by same year and month to get specific monthly data
  summarize(
    sessions_total = sum(sessions),
    transactions_total = sum(transactions),
    QTY_total = sum(QTY),
    ECR_total = sum(ECR, na.rm = TRUE), #'_total' variables get the sum of the variables being measured for each month
    sessions_avg = mean(sessions),
    transactions_avg = mean(transactions),
    QTY_avg = mean(QTY),
    ECR_avg = mean(ECR, na.rm = TRUE) #'_avg' variables get average of variables measured for each month
  ) %>%
  arrange(YearMonth) #arrange the order of the dataset to be set by month


condensed$addsToCart <- addsToCart$addsToCart #add 'addsToCart' variable to condensed dataset
##have to make sure that the order of months in addsToCart and condensed datasets are 
##the same, or else the addsToCart variable values will be placed with the wrong months


#select only two most recent months from data, create new dataset to evaluate

month_over_month <- condensed[11:12,] #indexes last two months of condensed dataset and creates new dataset with those values

#month_over_month$YearMonth <- NULL

month_over_month <- as.data.frame(t(month_over_month)) #transpose the data to be display the months as columns and metrics as rows
##rather than changing dataframe, could also use lag() function to calculate month over month statistics 
##with the previous dataframe format


#rename column variables to YearMonth values, rename those to more convenient variables

month_over_month  <- row_to_names(month_over_month, row_number = 1) #sets top row of dataset as column names
##only run once, or else it will rename the columns again to the new top row of the data

month_over_month <- rename(month_over_month, month_first = `2013-05`) 
month_over_month <- rename(month_over_month, month_second = `2013-06`) #rename columns to be convenient for reference

month_over_month$month_first <- as.numeric(month_over_month$month_first)
month_over_month$month_second <- as.numeric(month_over_month$month_second) #set variables as numeric

options(scipen=999) #removes scientific notation


#create absolute difference and relative difference variables, calculate both

month_over_month$absolute_diff <- month_over_month$month_second - month_over_month$month_first #calculate absolute difference (period 2 - period 1)

month_over_month$relative_diff <- ((month_over_month$month_second - month_over_month$month_first)/month_over_month$month_first) * 100 #calculate relative difference (period 2 - period 1)/period 1 * 100 to show percentage

#month_over_month <- format(round(month_over_month, 3), nsmall = 3) #round numbers to 3 decimal places


#rename columns and flip the dataset back to get a different view of the table

month_over_month <- rename(month_over_month, `2013-05` = month_first) 
month_over_month <- rename(month_over_month, `2013-06` = month_second)

month_over_month_2 <- as.data.frame(t(month_over_month))



#create plots to analyze the month over month comparisons

#simple ggplot bar graph that shows the comparison of a specific statistic over the course of the specific months and year

ggplot(condensed, aes(x=YearMonth, y=ECR_avg, color=YearMonth)) + 
  geom_col(position="dodge")

#sideways bar graph to display the comparison of a specific statistic over the course of the specific months and year

ggplot(data=condensed,aes(x=YearMonth, y=ECR_avg)) + 
  geom_bar(stat='identity', aes(color=YearMonth)) +
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="ECR Averages") +
  labs(title = 'ECR Averages by YearMonth',
       y='ECR Averages',x='YearMonth')+ 
  geom_hline(yintercept = mean(condensed$ECR_avg),size = 1, color = 'blue') #includes mean line

#box plot displaying the spread/range of a specific statistic and comparing over the course of the specific months and year

ggplot(data = merged, aes(x=YearMonth, y=ECR, color=YearMonth)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Spread of Average ECR by YearMonth',
       y='ECR_avg',x='YearMonth')


#create a list of the names of the sheets set as the desired dataframes to be output, write the sheets to an output xlsx file

retailer_calculations <- list("month_by_device" = month_by_device, "month_over_month" = month_over_month)

write.xlsx(retailer_calculations, file = "retailer_calculations.xlsx")



############ PART 3
### extras

#other way to calculate month over month statistics using lag()

monthly_ECR <- condensed %>%
  mutate(
    ECR_MoM = (ECR_total - lag(ECR_total)) / lag(ECR_total),
  ) #drawback is this can only be done one variable at a time


#can also do specific month over month calculations by category (i.e. device)

monthly_ECR_by_device <- merged %>%
  mutate(
    YearMonth = format(dim_date, "%Y-%m")
  ) %>%
  group_by(YearMonth, device) %>%
  summarize(
    Monthly_ECR = sum(ECR, na.rm = TRUE)
  ) %>%
  arrange(YearMonth, device)
##this calculates the total monthly ECR grouped by device used

#then we can calculate the month over month statistics of the variable we are analyzing
#based on the previous section

monthly_report_by_device <- monthly_ECR_by_device %>%
  group_by(device) %>%
  mutate(
    MoM_ECR = (Monthly_ECR - lag(Monthly_ECR)) / lag(Monthly_ECR),
    )


#we can then graph the results from the month over month calculation of our selected variable

library(scales)
ggplot(monthly_report_by_device, aes(x=YearMonth, y=MoM_ECR, fill=device)) + 
  geom_col(position="dodge") +
  scale_y_continuous(labels = scales::percent)



############ THINGS I DIDN'T USE

#condensed <- merged %>%
#mutate(month = format(dim_date, "%m"), year = format(dim_date, "%Y")) %>%
#group_by(year, month) %>%
#summarise(
#  sessions = sum(sessions),
#  transactions = sum(transactions),
#  QTY = sum(QTY)) %>%
#arrange(month)

#condensed$Date<-as.Date(with(condensed,paste(year,month,sep="-")),"%Y-%m")

#condensed$Date <- as.yearmon(paste(condensed$year, condensed$month), "%Y %m")

#condensed <- transform(condensed, Date = as.Date(Date, frac = 1))
#condensed$Date <- format(condensed$Date, "%m-%Y")

#condensed$Date <- read.zoo(text = condensed$Date, FUN = as.yearmon)

#condensed$Date <- as.Date(condensed$Date, "%m/%Y")

#condensed <- arrange(condensed$Date)

#need to get years aligned before setting this variable
#addsToCart <- addsToCart %>%
#  arrange(month_num)
