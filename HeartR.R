library(corrplot)
library(ggplot2)
library(magrittr)
library(dplyr)

heart <- read.csv("~/Downloads/heart.csv")
heart <- na.omit(heart)
# looking at correlation between heart disease and other variables
corrplot(cor(heart), type="upper")

# The correlation plot shows that ‘restecg’, ‘fbs’, and ‘chol’ parameters 
# are very weakly correlated with the target variable. so we can remove them

heart = subset(heart, select=c(-restecg, -chol,-fbs))

# changing categorical variables to meaningful string values
heart$sex[heart$sex == 0] = "female"
heart$sex[heart$sex == 1] = "male" 
heart$cp[heart$cp == 0] = "typical angina"
heart$cp[heart$cp == 1] = "atypical angina"
heart$cp[heart$cp == 2] = "non-anginal pain"
heart$cp[heart$cp == 3] = "asymptomatic" 
heart$exang[heart$exang == 0] = "no"
heart$exang[heart$exang == 1] = "yes" 
heart$slope[heart$slope == 0] = "upsloping"
heart$slope[heart$slope == 1] = "flat"
heart$slope[heart$slope == 2] = "downsloping" 
heart$thal[heart$thal == 1] = "normal"
heart$thal[heart$thal == 2] = "fixed defect"
heart$thal[heart$thal == 3] = "reversible defect" 
heart$target1 = heart$target
heart$target1[heart$target1 == 0] = "no heart disease"
heart$target1[heart$target1 == 1] = "heart disease"

#dataset is ready for exploratory data analysis

# checking proportion of the people with heart disease and with no heart disease in the dataset.
round(prop.table(table(heart$target1)),2)

#51% have heart disease, 49% do not

# Age analysis - distibution of age in the dataset
ggplot(heart, aes(x=age)) +
  geom_histogram() + ggtitle("Distribution of age of the population")+
  xlab("Age") + ylab("Density")

#slightly right skewed. majority population = 55-60 years

#looking at age group buckets
heart$age_grp = cut(heart$age, breaks = seq(25, 77, 4))

# finding the number of people with heart disease for each age group

target_by_age = heart %>%
  group_by(age_grp) %>%
  summarise(heart_disease = sum(target))
target_by_age

#making a bar plot of the data found above

target_by_age %>%
  ggplot(aes(x=age_grp, y=heart_disease)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") + ylab("No. of People with Heart Disease") + ggtitle("No of Heart disease in Age Group") + 
  theme_bw()

# 49-53 age group has the highest number of people with heart disease

# looking at proportion of people with heart disease in each group

prop_in_age = heart %>%
  group_by(age_grp) %>%
  summarise(heart_disease_proportion = round(sum(target)/n(), 3)*100)
prop_in_age

# bar plot of data above
prop_in_age %>%
  ggplot(aes(x=age_grp, y=heart_disease_proportion)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("") + ylab("Proportion of People with Heart Disease") + ggtitle("Proportion of Heart disease in Age Groups") + 
  theme_bw()

#In the age group of below 30 years, 100% of people have heart disease. 
#This isn't the case in real-life. It is clear that this is not a representative sample. 
#It is not possible to infer any conclusion about how age contributes to heart disease from this dataset.

# Looking at gender
round(prop.table(table(heart$sex)),2)
# 30% female, 70% male

round(prop.table(table(heart$sex, heart$target1)), 2)
#in the female population in our sample, there are more with heart disease than without


ggplot(heart, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Type of Slope") +
  ylab("Count") +
  ggtitle("Analysis of types of slope") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#with a downsloping number of no heart disease is higher than those with heart disease
#whilst in the flat surface, the results are the opposite.

#checking if the trend is the same in male and female population

male_data = heart[heart$sex=="male",]
female_data = heart[heart$sex=="female",]

#analysis for males
ggplot(male_data, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Type of Slope") +
  ylab("Count") +
  ggtitle("Analysis of types of slope for males") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))

#analysis for females

ggplot(female_data, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Type of Slope") +
  ylab("Count") +
  ggtitle("Analysis of types of slope for females") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


#The plot of the male population follows the same trend as the overall bar plot for analysis of slope. 
# But in the female population, the trend is very different.


#as per the first correlation plot, the number of vessels has a good correlation with heart disease

#mosaic plot for visualisation

mosaicplot(table(heart$target1, heart$ca), col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), las=1, main="Heart Disease for Major Vessels")
#About 2/3 of the people having heart disease have no major vessel. 

#Mosaic plot for males
mosaicplot(table(male_data$target1, male_data$ca), col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), las=1, main="Major Vessels in Males")

#Mosaic plot for females
mosaicplot(table(female_data$target1, female_data$ca), col=c("#754869", "coral", "skyblue", "#423f42", "#ed18c6"), las=1, main="Major Vessels in Females")

#ST Depression Induced by Exercise Relative to Rest(oldpeak)

#boxplots showing distribution of ST depression for people with heart disease and no heart disease
ggplot(heart, aes(x = target1, y = oldpeak)) + ylab("ST Depression") + xlab("Haert Disease State")+ ggtitle("ST Depression Induced by Exercise vs Haert Disease")+
  geom_boxplot()

#On the no heart disease side, the interquartile range is about about 2 points higher than on the heart disease side (1).

ggplot(heart, aes(x = age, y = oldpeak,color=target1, size = factor(oldpeak))) + 
  geom_point(alpha=0.3) + labs(color = "Heart Disease State")+guides(size=FALSE) + xlab("Age") + ylab("ST Depression") + ggtitle("Age vs Resting Blood Pressure Separated by Heart Condition")

