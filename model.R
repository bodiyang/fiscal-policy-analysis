library(dplyr)
library(ggplot2)

## (1) Prediction of sale-weighted-average CO2
# read in data and remove the missing data items
data = read.csv('ICCT offline excercise.csv')
data <- data %>% na.omit()

# calculate the total co2 emmission by each model (sales number * co2 emmision of each car)
new_data <- data %>% mutate(total_co2_bymodel = regs * co2_nedc_avg) 

# calculate the total co2 emmision each year, total sales of vehicles each year
total_co2 <- aggregate(x= new_data$total_co2_bymodel, by= list(new_data$reg_year), FUN = sum)
total_regs <- aggregate(x= new_data$regs, by= list(new_data$reg_year), FUN = sum)

# calculate the sale weighted average co2 emmission
sale_wgt_co2 <- data.frame(reg_year=total_co2$Group.1, total_co2_emission=total_co2$x, total_regs=total_regs$x)  %>% mutate(sale_weighted_co2_avg = total_co2_emission / total_regs)


## Regression Model
model_co2_1 = lm(formula = sale_weighted_co2_avg ~ reg_year, data=sale_wgt_co2)
summary(model_co2_1)
model_co2_2 <- lm(formula = sale_weighted_co2_avg ~ reg_year + I(reg_year^2), data=sale_wgt_co2)
summary(model_co2_2)
model_co2_3 = lm(formula = sale_weighted_co2_avg ~ reg_year + I(reg_year^2) + I(reg_year^3), data=sale_wgt_co2)
summary(model_co2_3)

# model1: linear regression model: sale_weighted_co2_avg = -3.1136 * reg_year + 6391.5357
# model2: Polynomial regression model:  sale_weighted_co2_avg = 1036273.7098 - 1026.6031 * reg_year + 0.2543 * reg_year^2
# model3: Polynomial regression model (not defined because of singularities):  sale_weighted_co2_avg = 1.036e+06 - 1.027e+03 * reg_year + 2.543e-01 * reg_year^2


## Making prediction
predt <- data.frame('reg_year'=c(2020,2021,2022,2023,2024,2025))
newdatat <- predict(model_co2_2, predt)
predt$co2 <- newdatat


## Plotting
p <- ggplot(sale_wgt_co2, aes(reg_year,sale_weighted_co2_avg)) + geom_line() + geom_point(color = 'black') + labs(title =  'Fig1: PV (sale-weighted) average' ~CO[2]~ 'emission in France', x='Year', y = 'sale weighted average' ~CO[2]~ 'emission', caption = 'black point: historical data; blue line: regression model; red point: predicted value')

# p + stat_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE)
# p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) 
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + geom_point(aes(x=2020, y=112.2062), colour="red") + geom_point(aes(x=2021, y=113.1611), colour="red") + geom_point(aes(x=2022, y=114.6246), colour="red") + geom_point(aes(x=2023, y=116.5967), colour="red") + geom_point(aes(x=2024, y=119.0773), colour="red") + geom_point(aes(x=2025, y=122.0665), colour="red")





## (2) Calculating the Bonus-Malus

# pick up the vehicles eligible for bunus, whose CO2 emission is less than 20g/km
bonus_car <- new_data[new_data$co2_nedc_avg <= 20, ] 
# pick up the vehicles not eligible for bunus, whose CO2 emission is large than 20g/km
non_bonus_car <-  new_data[new_data$co2_nedc_avg > 20, ] 
non_bonus_car_low <-  non_bonus_car[non_bonus_car$co2_nedc_avg < 138, ]
non_bonus_car_nonlow <-  non_bonus_car[non_bonus_car$co2_nedc_avg >= 138, ]
non_bonus_car_high <- non_bonus_car[non_bonus_car$co2_nedc_avg > 213, ]
non_bonus_car_mid <-  non_bonus_car_nonlow[non_bonus_car_nonlow$co2_nedc_avg <= 213, ] 

# data frame of vehicles eligible for bunus, whose CO2 emission is less than 20g/km
bonus_car_frame <- aggregate(x= bonus_car$regs, by= list(bonus_car$reg_year), FUN = sum)
# data frame of vehicles for malus and whose CO2 emission is less than 138g/km
malus_car_low_frame <- aggregate(x= non_bonus_car_low$regs, by= list(non_bonus_car_low$reg_year), FUN = sum)
# data frame of vehicles for malus and whose CO2 emission is between 138g/km and 213g/km
malus_car_mid_frame <- aggregate(x= non_bonus_car_mid$regs, by= list(non_bonus_car_mid$reg_year), FUN = sum)
# data frame of vehicles for malus and whose CO2 emission is more than 213g/km
malus_car_high_frame <- aggregate(x= non_bonus_car_high$regs, by= list(non_bonus_car_high$reg_year), FUN = sum)

# calculate the bonus and malus under each group
bonus <- bonus_car_frame %>% mutate(bonusfee = 6000*x)
malus_low <- malus_car_low_frame %>% mutate(fee = 50*x)
malus_high <- malus_car_high_frame %>% mutate(fee = 20000*x)

non_bonus_car_mid <- non_bonus_car_mid %>% mutate(sepfee = 266*co2_nedc_avg - 36658 )
non_bonus_car_mid <- non_bonus_car_mid %>% mutate(fee = sepfee * regs )
malus_mid <- aggregate(x= non_bonus_car_mid$fee, by= list(non_bonus_car_mid$reg_year), FUN = sum)

# total malus
totalmalus <- data.frame('totalmalus'= malus_low$fee + malus_high$fee + malus_mid$x)
totalmalus$reg_year <- malus_low$Group.1

# predict the malus
malus_model <- lm(totalmalus ~ poly(reg_year,2), data = totalmalus)
summary(malus_model)
predt_mal <- data.frame('reg_year'=c(2020,2021,2022,2023,2024,2025))
newdatat_mal <- predict(malus_model, newdata = predt_mal)
predt_mal$malusfee <- newdatat_mal

predt_mal

plotmal <- ggplot(totalmalus, aes(reg_year,totalmalus)) + geom_line() + geom_point(color = 'black') + labs(title =  'Fig2: Estimated PV Malus Fee (based on 2020 policy)', x='Year', y = 'Malus Fee', caption = 'black point: historical data; blue line: regression model; red point: predicted value')
plotmal + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + geom_point(aes(x=2020, y=1200136075), colour="red") + geom_point(aes(x=2021, y=1720284802), colour="red") + geom_point(aes(x=2022, y=2379973427), colour="red") + geom_point(aes(x=2023, y=3179201949), colour="red") + geom_point(aes(x=2024, y=4117970367), colour="red")+ geom_point(aes(x=2025, y=5196278684), colour="red")


# predict the bonus
bonus <- bonus %>% mutate(reg_year=Group.1)
bonus_model <- lm(bonusfee ~ poly(reg_year, 1), data = bonus)
summary(bonus_model)
predt_bon <- data.frame('reg_year'=c(2020,2021,2022,2023,2024,2025))
newdatat_bon <- predict(bonus_model, newdata = predt_bon)
predt_bon$bonusfee <- newdatat_bon

predt_bon

plotbon <- ggplot(bonus, aes(reg_year,bonusfee)) + geom_line() + geom_point(color = 'black') + labs(title =  'Fig3: Estimated PV Bonus Fee (based on 2020 policy)', x='Year', y = 'Bous Fee', caption = 'black point: historical data; blue line: regression model; red point: predicted value')
plotbon + stat_smooth(method = "lm", formula = y ~ x , size = 1) + geom_point(aes(x=2020, y=3501600), colour="red") + geom_point(aes(x=2021, y=3025200), colour="red") + geom_point(aes(x=2022, y=2548800), colour="red") + geom_point(aes(x=2023, y=2072400), colour="red") + geom_point(aes(x=2024, y=1596000), colour="red")+ geom_point(aes(x=2025, y=1119600), colour="red")

