# installing packages
install.packages("Information")

# Loading packages
library(tidyverse)
library(haven)
library(estimatr)
library(plm)
library(dplyr)
library(Information)
library(ggplot2)
library(tidyr)
library(readr)

# Setting working directory
setwd("/Users/ASUS/Library/CloudStorage/OneDrive-Personal/Desktop/SUSS/ANL553/ECA")

# reading data
read_csv("eca_data.csv") -> eca_data

####### 1a - EDA ######

# understanding structure of dataset (class, length , content)
str(eca_data)

# checking for NULL values
null_check <- sapply(eca_data, function(x) any(sapply(x, is.null)))
print(null_check)

# checking class type of dataset
class(eca_data$pop)
class(eca_data$numsales)
class(eca_data$storeid)
class(eca_data$day)

# summary statistics of dataset columns
summary(eca_data$pop)

# Group the data by "storeid" and calculate mean sales of each storeid
eca_data_grouped <- eca_data %>% group_by(storeid)

eca_data_summary <- eca_data_grouped %>%
  summarize(Mean_sales=mean(numsales))

View(eca_data_summary)

# getting min and max values of mean_sales column
min_value <- min(eca_data_summary$Mean_sales)
max_value <- max(eca_data_summary$Mean_sales)

# getting the storeids of the min and max mean sales values

index_max <- which.max(eca_data_summary$Mean_sales)
index_min <- which.min(eca_data_summary$Mean_sales)

id_max <- eca_data_summary$storeid[index_max]
id_min <- eca_data_summary$storeid[index_min]

# grouping by storeids and the population size of region near supermarket
eca_data_grouped_pop <- eca_data %>% group_by(storeid, pop ) %>% distinct(storeid, pop)
View(eca_data_grouped_pop)

# getting min and max values of pop column
min_pop_value <- min(eca_data_grouped_pop$pop)
max_pop_value <- max(eca_data_grouped_pop$pop)

# finding the storeid of the lowest/highest population region around the supermarket
index_max <- which.max(eca_data_grouped_pop$pop)
index_min <- which.min(eca_data_grouped_pop$pop)

id_maximum <- eca_data_summary$storeid[index_max]
id_minimum <- eca_data_summary$storeid[index_min]

# calculating total sales over 15 days for each storeid
sum_sales <- eca_data %>%
  group_by(storeid) %>%
  summarize(sumvalue = sum(numsales, na.rm = TRUE))

# comparing average sales of each management team (i.e groups of 50)

## specifying the rows numbers for each set
set1_rows <- 1:50
set2_rows <- 51:100
set3_rows <- 101:150
set4_rows <- 151:200

average_sales <- sapply(list(set1_rows, set2_rows, set3_rows, set4_rows), function(rows) {
  rowMeans(sum_sales[rows, "sumvalue"])
})

## calculating the mean of each column (which represents each management team)
final_result <- colMeans(average_sales) 
print(final_result)

# creating visualization to show average sales of each management team
names(average_sales) <- c("Team1" , "Team2", "Team3", "Team4")

as.data.frame(average_sales) -> average_sales

long_data <- average_sales %>%
  gather(category, value)

long_data_mean <- long_data %>%
  group_by(category) %>%
  summarize(meanvalue=mean(value))

ggplot(long_data_mean, aes(x = category, y = meanvalue ,fill=category)) +
  geom_bar(stat = "identity") +
  labs(x = "Categories", y = "Mean Values") +
  ggtitle("Mean sales figures of each management team")+ 
  geom_text(aes(label=meanvalue , vjust=-0.7))+
  theme(legend.position ='None',
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))
  
# population mean by each group of 50 storeids managed by each of the 4 teams

set1_rows <- 1:50
set2_rows <- 51:100
set3_rows <- 101:150
set4_rows <- 151:200

pop_group_teams <- sapply(list(set1_rows, set2_rows, set3_rows, set4_rows), function(rows) {
  rowMeans(eca_data_grouped_pop[rows, "pop"])
})

result <- colMeans(pop_group_teams) 
print(result) 

# visualization of the population mean of each team

## converting pop_group_teams to a dataframe format
as.data.frame(pop_group_teams) -> pop_group_teams 
is.data.frame(pop_group_teams)

## converting the column names of “pop_group_teams” table
names(pop_group_teams) <- c("Team1" , "Team2", "Team3", "Team4")

## converting to long format
a <- pop_group_teams %>%
  gather(category, value) 

## grouping by the different teams and their mean population values
a_mean <- a %>%
  group_by(category) %>%
  summarize(meanvalue=mean(value))

## plotting scatterplot
ggplot(a_mean, aes(x = category, y = meanvalue, color = category)) +
  geom_point() +
  labs(
    x = "Teams",
    y = "Mean population value",
    title = "Population mean of each group of 50 storeIDs",
    subtitle = "Per group corresponds to one management team"
  ) + 
  geom_text(aes(label = meanvalue, vjust = -0.7)) +
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 20)),
    axis.title.y = element_text(size = 12, margin = margin(r = 20)),
    legend.position = 'None'
  )

# (1b) - Evaluating empirical approaches
eca_data_1 <- eca_data[,c('storeid', 'day', 'numsales')]
View(eca_data_1)
df_wide <- pivot_wider(eca_data_1 , names_from = day, values_from = numsales)
            
