#logistic regression in R

dir()

g <- read.csv(file =  "data_logistic.csv", header=TRUE, sep=',')
dim(g) #403 rows, in this case patients, and 23 columnns
names(g)
#insurance: 0=none, 1=government, 2=private
#fh = family history of diabetes (yes/no, where 1=yes, 0=no)
#smoking: 1=current, 2=never and 3=ex

#To do any analysis, my preference is to make one variable per column rather than refer directly to the column within the data set every time

chol <- g[,"chol"] # cholesterol is continuous, so it’s easy
gender <- as.factor(g[,"gender"]) # but gender isn’t.
dm <- as.factor(g[,"dm"]) # neither is dm (diabetets)

#To see how many males and females we have, you can use the “table” command. 
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(100*prop.table(t),digits=1) # get proportions rounded to 1dp

#Irritatingly, however, “table” excludes missing values by default. 
#To see these – and we ALWAYS want to see these – use an “exclude=NULL” option when making the variable:
dm2 <- factor(dm, exclude=NULL)
table(dm2)

#For continuous variables, though, use “summary”:
summary(chol) #The “summary” command does give us missing values (“NAs”) by default, which is perfect. We can see that the median cholesterol is similar to the mean, so the distribution is likely roughly symmetrical. 
#There’s a large range.
height <- g[,'height']
weight <- g[,'weight']
summary(height)
summary(weight)


#How to calculate body mass index (BMI) from height and weight

height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)

# it’s very common to categorise BMI
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) #using the argument margin = 1 we can specify that the table gives us the row percentages

#You could make this a hypothesis test to see whether obesity is statistically associated with diabetes – in which case, you’d use a chi-squared test to do this.
#This makes it easy to see whether obese people are overrepresented in those with diabetes via a cross-tabulation:

dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 
dm_by_bmi_category 

# age-sex distribution of your set of patients
#To make it manageable, go for just four age groups: under 45, 45-64, 65-74 and 75 or over.
age <- g[,"age"]
summary(age)
age_categorised <- ifelse(age < 45, "a", 
                          ifelse(age >= 45 & age <= 64, "b", 
                                 ifelse(age > 65 & age <= 74, "c", 
                                        ifelse(age > 75, "d", NA)))) 

table(age_categorised)

sex_by_age_category <- table(age_categorised, gender, exclude = NULL) 
round(100 * prop.table(sex_by_age_category, margin = 1), digits = 1)

#Simple logistic regression: how to run a model with only one predictor
#predictor: age (we specified before it is categorical variable ie factor)
#outcome: diabetes
#This means we are saying that the log odds of having diabetes differs by gender alone. 
m <- glm(dm ~ gender, family=binomial (link=logit))
summary(m)

#to model by age
#this assumes that the relation between age and the log odds of having diabetes is linear 
#is it linear?
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 
# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 
# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 
# calculate the log odds 
logodds <- log(odds) 
# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 

m <- glm(dm ~ age, family=binomial (link=logit))
summary(m)
#important: 13 obs deleted, 
#the important coeff is intercept ie. -4.404 (pod intercept estimate)
#i za age 0.05
#Log odds of having diabetes= intercept + (coefficient for age) * age in years = -4.4045 + 0.0525 * age in years
#interpretation: the log odds if you’re 25 is 0.0525 higher than if you’re 24  
exp(0.052465) #1.053 this is usually reported,  It’s the amount by which your odds increases when you get a year older.
#“Pr(>|z|)” column daje p i zbilja je mala vrijednost. Age is a statistically significant predictor.


#to model by gender (compared to females)
#how is gender lablled
contrasts(gender) #0 is female
#which is reference category\
levels(gender) 

m <- glm(dm ~ gender, family=binomial (link=logit)) 
summary(m) 
#the log odds for having diabetes for males is 0.0869 higher than that for females. 
exp(0.08694) #or exp(m$coefficients) #1.09  the odds ratio for males is higher compared with females, which is 1.09 
#p= 0.759 , we don’t have any good evidence of a gender difference in diabetes odds in this sample.

#run this as males as ref category
gender <- relevel(gender, ref = "male") 
levels(gender) 

m <- glm(dm ~  gender, family=binomial (link=logit)) 
summary(m) 
m$coefficients 

#what percentage of people from buckingham have diabetes
names(g)
table(g$location)
levels(g$location)
location <- g[, "location"]

diabetes_by_location <- table(dm, location, exclude = NULL) 
diabetes_by_location
round(100 * prop.table(diabetes_by_location, margin = 2), digits = 1) #15.5 % ljudi zi Buckingham imaju dijabates kad se ukljuci NA
#bez NA 16.3

#fit a logistic regression with “location” as the predictor variable. 
#What are the log odds of having diabetes being from Louisa compared with Buckingham
m <- glm(dm ~  location, family=binomial (link=logit)) 
m$coefficients #-0.14
exp(m$coefficients)

#is location a statistically significant predictor of having diabetes in our sample
summary(m)  #no

#Multiple logistic regression
hist(age)

#Histograms are affected by the choice of bins, so some people prefer to use fancier plots instead to describe the distribution, such as kernel density plots (also known simply as density plots):
d <- density(age) 
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title 


#describe continuos variables: BMI, HDL and cholesterol
names(g)
summary(bmi) #contains missing values
hist(bmi) #normal


hdl <- g[,"hdl"]
summary(hdl) 
hist(hdl) #normal
hdl.no.na <- chol[is.na(hdl)==0]
d <- density(hdl.no.na)
plot(d, main ="")

m <- glm(dm ~  hdl, family=binomial (link=logit)) 
summary(m)
exp(m$coefficients)

summary(chol)
#has a missing value which needs to be excluded before calling the density funciton
chol.no.na <- chol[is.na(chol)==0]
d <- density(chol.no.na)
plot(d,main = "") 

#Assessing crude relations between predictors and the outcome
#gender
# define the gender variable 
gender <- as.factor(g[,"gender"]) 
# cross tabulation 
dm_by_gender <- table(gender, dm) # not including NA values because there aren't that many 
# proportion of diabetes status by gender 
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 
dm_by_gender_prop

#age
# define the age variable (continuous) 
age <- age <- g[,"age"] 
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 
# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 
# calculate the log odds 
logodds_age <- log(odds_age) 
# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 

OR

# age grouping converting continuous variable to a categorical (ordinal) one  
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 



# create a cross tabulation of age and diabetes status  
dm_by_age_grouped <- table(age_grouped, dm) 

# output the frequencies of diabetes status by age 
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 

# calculate the odds of having diabetes 
odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 

# calculate the log odds 
logodds_age_grouped <- log(odds_age_grouped) 

# plot the age groups found in the sample against the log odds of having diabetes 
dotchart(logodds_age_grouped) 

##Relation between the cholesterol and the outcome
# define chol as a continuous variable 
chol <- g[,"chol"] 

# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by cholesterol 
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 
# calculate the odds of having diabetes 
odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 
# calculate the log odds 
logodds_chol <- log(odds_chol) 
# plot the cholesterol found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 

# categorising chol into an ordinal variable 

# https://www.medicalnewstoday.com/articles/315900.php 
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA))) 
# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 
# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by cholesterol 
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 
# calculate the odds of having diabetes 
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 
# calculate the log odds 
logodds_chol_categorised <- log(odds_chol_categorised) 
# plot the cholesterol categories found in the sample against the log odds of having diabetes 
dotchart(logodds_chol_categorised)


#bmi

#bmi 
height <- g[,"height"] 
weight <- g[,"weight"] 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
bmi <- weight.si/height.si^2 


# categorising BMI 

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 
# create a cross tabulation of BMI and diabetes status  
dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by BMI 
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 
# calculate the odds of having diabetes 
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 
# calculate the log odds 
logodds_bmi_categorised <- log(odds_bmi_categorised) 
# plot the BMI categories found in the sample against the log odds of having diabetes 
dotchart(logodds_bmi_categorised) 

#Correlation between predictors
#if predictors are correlated they will give the same result
#we might expect cholesterol and HDL to go together in some way
#calculate the Pearson correlation coefficient between two continuous, (roughly) normally distributed variables in R

cor.test(x=chol,y=hdl,method="pearson") 
#This code excludes patients with missing data and tells us that cholesterol and HDL are indeed correlated (p=0.00017) but only weakly (r=0.19 to two decimal places).


#run age, gender and bmi as predictors
m <- glm(dm ~  age + gender + bmi, family=binomial (link=logit)) 
m$coefficients
exp(m$coefficients)
summary(m)
exp(confint(m))

#The standard errors are all pretty small, so that’s fine.
#The log odds ratio for age is 0.055 and its p value is tiny, so you can conclude that age is significantly – and positively – associated with the risk of getting diabetes. 
#If you exponentiate its coefficient we get 1.057, or 1.06 to two decimal places. 
#This means that a one-year increase in age is associated with six percent higher odds of being diagnosed with diabetes
# For age, the odds ratio is 1.06 with 95% CI 1.04 to 1.08,

# The odds ratio for a unit increase in BMI is exp(0.073879) = 1.08, with 95% CI 1.03 to 1.13, p=0.00153 (or 0.002 to three decimal places, as is usual reporting practice). 
#That’s a pretty low p value, so you can conclude that people with higher BMIs are more at risk of diabetes.


dir()

#EXAM
#try another model with these predictor variables: age, cholesterol and insurance type
g <- read.csv("test_logistic.csv")
names(g)
age <- g[,"age"]
chol <- g[,"chol"]
insurance<- as.factor(g[,"insurance"])
levels(insurance)

m <- glm(dm ~  age + chol + insurance, family=binomial (link=logit)) 
m$coefficients
round(exp(m$coefficients), digits = 2)
summary(m)
exp(confint(m)) 

#REPORT: Age and cholesterol are statistically significant predictors. While patients with either government or private insurance had lower odds than those with no insurance, the differences were not statistically significant.

#TESTING THE MODEL FIT
#McFadden’s r-squared:
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
# check your model 
summary(full_model) 

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 
# check 
summary(null_model) 

# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 
# print it 
R2 
## 'log Lik.' 0.1361385 (df=5) #This R-squared of about 14% is typical of logistic regression models and is actually not too bad (but not great).

#c statistic or ROC curves
install.packages("DescTools") 
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
# check your model 
summary(full_model) 
Cstat(full_model) 


#Hosmer-Lemeshow statistic and test
install.packages("ResourceSelection") 
library("ResourceSelection")
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 
full_model$y
# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL
# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 
# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 
# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 

#the p-value is 0.1879, suggesting good calibration

#analyse table of deviance

# analyse table of deviance 
anova(full_model, test = "Chisq") 
#n our case, adding the variables age and cholesterol significantly reduce the deviance and improve the model fit, as indicated by their low p-values, but including the insurance variable does not improve the model fit enough to justify the loss in degrees of freedom, as indicated by its high p-value of 0.2896.

#Backwards elimination
#fit a model with those six predictors and apply backwards elimination to remove any that are not statistically significant

dm <- as.factor(g[,"dm"]) 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 
summary(model) 
anova(model, test = "Chisq")
#neither of the blood pressure variables is associated with the risk of diabetes. Now drop them and create a new model:

model <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit)) 
summary(model) 

anova(model, test = "Chisq")

#But why is blood pressure not significant here despite what the literature says? One way to find out is to see if it correlates with other variables.
cor.test(systolic, hdl) # not significant
cor.test(systolic, bmi) # significant 
cor.test(systolic, chol) # very significant
cor.test(systolic, age) # extremely significant 

#run the model with: age, BMI, cholesterol, HDL, systolic BP, and diastolic BP from earlier but also add in gender, location, frame, insurance, and smoking


dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
ratio <- g[,'ratio'] 
location <- as.factor(g[,'location']) 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
frame <- as.factor(g[,'frame']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 


model2 <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial(link = logit))
summary(model2) 
anova(model2, test = "Chisq")

#but you can rport bmi although it does not reach arbitrarly decided p value












