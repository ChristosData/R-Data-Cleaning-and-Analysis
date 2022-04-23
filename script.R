rm(lists=ls()) #removes all variables stored previously 
library(Hmisc) #import
data <- read.csv("~/Downloads/COVID19_line_list_data.csv")
describe(data)

#14 missing death values because values are either 1,0, or date of death
#cleaned up death column
data$death_dummy <- as.integer(data$death != 0) 


# death rate
sum(data$death_dummy) / nrow(data) *100 

# AGE
# claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#The difference between average age of dead/alive is 20 years, but is this statistically significant?

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)

#since p-value < 0.05, we reject the null hypothesis
#here p value ~ 0. This finding is statistically significant

# GENDER
# claim: gender has no effect 
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%
mean(women$death_dummy, na.rm = TRUE) #3.7%

#testing statistical significance
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.95)
#95% confidence: men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.05, so reject null hypothesis so this is statistically significant
#men have higher death rate than women, and this is representative of the population