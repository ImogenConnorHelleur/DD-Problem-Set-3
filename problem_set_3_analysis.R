library(tidyverse)
library(haven)

fire_data <- read_dta("data_fire_01_08_updated.dta")

#multiple regression 
fire_regression <- lm(fire ~ 
                        defor + 
                        soy +
                        fallow +
                        pasture +
                        pri_forest +
                        sec_forest +
                        p_index +
                        soil_sandy +
                        soil_clayey +
                        soil_hclayey +
                        povm025 +
                        povM025m05 +  
                        povM05m1 +
                        temp_JJ_av +
                        temp_AO_av,
                      data = fire_data)

fire_regression_summary <- summary(fire_regression)

# #show F-stat, R^2 adjusted and p-value
# Residual standard error: 236.5 on 2860 degrees of freedom
# Multiple R-squared:  0.4731,	Adjusted R-squared:  0.4703 
# F-statistic: 171.2 on 15 and 2860 DF,  p-value: < 2.2e-16

#plot residuals
ggplot(fire_regression, 
       aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residuals for fire multiple regression)")

#plot residuals histogram distribution
ggplot(fire_regression, 
       aes(x = fire_regression$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of residuals for fire multiple regression', x = 'Residuals', y = 'Frequency')