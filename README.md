# Applied-Statistical-Methods-and-Causal-Analysis

## Background of project
- Performed EDA on dataset to gain a better understanding of the data structure and identify key trends
- Evaluated suitability of empirical approaches such as linear regression, fixed effects, difference-in-difference to study the effects of a treatment effect on the response variable
- Constructing and testing of null hypothesis using the most ideal empirical approach

## Visualizations
Mean sales figure of each team:
![Image 1](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/Picture%202.png?raw=true) <br> <br>

Population mean of storeids by team:
![Image 2](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/Picture%201.png?raw=true) <br> <br>

## Statistical modelling
Linear regression (regressing only against discount applied):
```
lm_model1 <- lm(numsales ~ Discount_Applied , binary_tbl)
summary(lm_model1)
```
![Image 3](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/linear%20regressoin2.png?raw=true) <br> <br>

Linear regression (regressing against discount applied, population and interaction term 'Discount_Applied*pop'):
```
lm_model3 <- lm(numsales ~ Discount_Applied + pop + Discount_Applied*pop , binary_tbl)
summary(lm_model3)
```
![Image 4](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/linear%20regression1.png?raw=true) <br> <br>

Difference-in-Difference:
```
formula <- numsales ~ Discount_Applied + day*Discount_Applied
# time variable is not converted to factor because time is measured in days (continuous numeric scale)
did_model <- lm(formula , binary_tbl)
summary(did_model)
```
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
![Image 6](https://github.com/bayyangjie/Applied-Statistical-Methods-and-Causal-Analysis/blob/main/Images/fe.png?raw=true)
