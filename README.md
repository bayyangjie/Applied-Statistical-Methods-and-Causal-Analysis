## Project objective
- To evaluate the effect of a treatment effect (price discount) on the response variable (sales of products)

## What was done
- Performed EDA on dataset to gain a better understanding of the data structure and identify key trends
- Evaluated suitability of empirical approaches such as linear regression, fixed effects, difference-in-difference to study the effects of a treatment effect on the response variable
- Constructing and testing of null hypothesis using the most ideal empirical approach

## Data understanding
Data quality checks such as checking for missing values, and verifying the correct class of the variables. Descriptive statistical and mathematical functions were also utilized for obtaining the mean, maximum and minimum values of the variables. Functions such as str() were also used to understand the dataset's structure.
Employing the ggplot2 package, visualizations were created to gain an understanding of the relationships between variables.

# Visualization - Means sales figure per team

```{r}
names(average_sales) <- c("Team1" , "Team2", "Team3", "Team4")

as.data.frame(average_sales) -> average_sales

long_data <- average_sales %>%
  gather(category, value)

long_data_mean <- long_data %>%
  group_by(category) %>%
  summarize(meanvalue=mean(value))

ggplot(long_data_mean, aes(x = category, y = meanvalue ,color=category)) +
  geom_point() +
  labs(x = "Categories", y = "Mean Values") +
  ggtitle("Mean sales figures of each management team")+ 
  geom_text(aes(label=meanvalue , vjust=-0.5) , size=3.5)+
  theme(legend.position ='None',
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12))
```

Mean sales figure of each team:
![Image 1](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/Picture%202.png?raw=true) <br> <br>

Different category names are created and assigned to each Team that handles 50 storeids each. The sales of each of the 4 teams that handle 50 storeids each are then calculated and summarized under the variable 'meanvalue'. A scatterplot was then plotted to show the relationship between the mean sales figures of each team. The different colours of each point are then filled according to each team category.

# Visualization - Region population mean of storeids under each team
```
# population mean by each group of 50 storeids managed by each team
set1_rows <- 1:50
set2_rows <- 51:100
set3_rows <- 101:150
set4_rows <- 151:200

pop_group_teams <- sapply(list(set1_rows, set2_rows, set3_rows, set4_rows), function(rows) {
  rowMeans(eca_data_grouped_pop[rows, "pop"])
})

result <- colMeans(pop_group_teams) 
result
## [1] 100065.6  99885.6  99920.7 100036.6
```
Each column represents the population mean of the storeids handled by each team

```
# converting pop_group_teams to a dataframe format
as.data.frame(pop_group_teams) -> pop_group_teams 
is.data.frame(pop_group_teams)

## [1] TRUE

# converting the column names of “pop_group_teams” table
names(pop_group_teams) <- c("Team1" , "Team2", "Team3", "Team4")
converting to long format
a <- pop_group_teams %>%
  gather(category, value)

# grouping by the different teams and their mean population values
a_mean <- a %>%
  group_by(category) %>%
  summarize(meanvalue=mean(value))
a_mean

## # A tibble: 4 × 2
##   category meanvalue
##   <chr>        <dbl>
## 1 Team1      100066.
## 2 Team2       99886.
## 3 Team3       99921.
## 4 Team4      100037.
```
Creating custom labels for each of the four columns and summarizing the population mean of the storeids under each Team category. 

```
ggplot(a_mean, aes(x = category, y = meanvalue, color = category)) +
  geom_point() +
  labs(
    x = "Teams",
    y = "Mean population value",
    title = "Population mean of storeids of each team",
    subtitle = "50 storeids under each team"
  ) + 
  geom_text(aes(label = meanvalue, hjust =1.2)) +
  theme(
    axis.title.x = element_text(size = 12, margin = margin(t = 20)),
    axis.title.y = element_text(size = 12, margin = margin(r = 20)),
    legend.position = "None"
  )
```
Scatterplot creation of the mean population size of the storeids under each region. The scatterplot shows that the storeids under Team1 are in a region that has the highest population mean. 'Category' which represents the different Teams is passed into the aes() function of ggplot() to use colour to define the points by different colours.

Population mean of storeids by team:
![Image 2](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/Picture%201.png?raw=true) <br> <br>


## Statistical modelling

Using the 'stats' package in R, simple linear regression was performed to determine the effect of discount alone on the response variable 'sales', multiple linear regression was also performed to determine the effect of multiple variables on 'sales'. Analytical insights about the correlation between the predictor variables and response variable were then formulated based on the magnitude of the p-values and coefficient estimates of each predictor variable. These insights included understanding which predictor variable had the most and least impact on the response variable as well as the significance of each predictor variable.

Linear regression (regressing only against discount applied):
```
lm_model1 <- lm(numsales ~ Discount_Applied , binary_tbl)
summary(lm_model1)
```
From the lm() output, for every 10% increase in discount, the sales in the dairy product is increased by 151.331. The output also shows that the estimated y-intercept value is 3086.389 if there were no discount applied.
The p-value of 0.00443 is much lesser than the alpha significance level of 0.05, so this indicates a statistically significant relationship between the outcome "numsales" and the "Discount_Applied" variables.

![Image 3](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/linear%20regressoin2.png?raw=true) <br> <br>

Linear regression (regressing against discount applied, population and interaction term 'Discount_Applied*pop'):
```
lm_model3 <- lm(numsales ~ Discount_Applied + pop + Discount_Applied*pop , binary_tbl)
summary(lm_model3)
```
The coefficient estimates of "Discount_Applied" & "pop" both increased with "Discount_Applied" increasing the most significantly after introducing the interaction term of "Discount_Applied * pop". Additionally, the p-value of the interaction term is < 0.05. This suggests that the relationship between numsales and discount applied is also affected by the presence of the population variable.
In terms of p-values, after introducing the interaction term, the p-values of the predictor variables "Discount_Applied" (0.00443 > 0.036) and "pop" (2.73e-06 > 1.7e-05) both increased by a fair amount. While the p-values still remain < 0.05, the increase in p-value could suggest that the predictor variables become less statistically significant in the presence of the interaction term.

![Image 4](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/linear%20regression1.png?raw=true) <br> <br>

Difference-in-Difference:
```
formula <- numsales ~ Discount_Applied + day*Discount_Applied
# time variable is not converted to factor because time is measured in days (continuous numeric scale)
did_model <- lm(formula , binary_tbl)
summary(did_model)
```
The DID coefficient is denoted by "day:Discount_Applied". The negative value of -3.781 suggests that the treatment (i.e discount) has led to a decrease in the outcome variable for the treated group compared to control group over time.

The p-value of the DID coefficient is > 0.05 which makes the interaction term not statistically significant. This means that we are unable to conclude that the treatment has a significant effect on 'numsales' over time.

DID model in this case may not be suitable since it is not investigating changes over time of a treatment effect. The study also does not involve time-invariant factors for establishing causality. Linear model method can be used for providing valuable insights into pre and post treatment changes.

![Image 5](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/difference-in-difference.png?raw=true) <br> <br>

Fixed effects:
```
binary_tbl_1 <- binary_tbl %>%
  mutate(Discount_group = ifelse(
    (storeid >= 1 & storeid <= 50), 1, 0
  ))

FE_model <- lm(numsales ~ Discount_Applied + Discount_group + day + pop , binary_tbl_1)
summary(FE_model)
```
In binary_tbl_1, a new column "Discount_group" is created. This column is a binary column that clusters the entity-specific column "storeid" into binary value '1' (cluster of storeids 1 to 50 that had discount applied on days 8 & 9) and binary value '0' (cluster of storeids 51 to 200 that had no discounts applied at all).

The purpose of creating the entity-specific dummy variable is to account for unobserved heterogeneity specific to the entity, enabling us to isolate within-entity variation and study the effects of independent variables more rigorously in a fixed effects regression analysis.

We can see from the output that the treatment effect "Discount_Applied" here is not statistically significant (i.e 0.2503 > 0.05). This could mean that there may still be unobserved or unaccounted-for confounding factors that influence the outcome even though fixed effects should help control for variations that are specific to the entity (i.e 'storeid' in this case). These factors could confound the relationship between the treatment and the outcome.

![Image 6](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/fe.png?raw=true)
