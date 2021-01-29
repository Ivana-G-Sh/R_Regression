#LINEAR REGRESSION
#4. July 2020
dir()

COPD <- read.csv("COPD_student_dataset.csv")
names(COPD)

#disease: COPD, CAT
#walking ability: MWT-6 min walking test
#lung function: FEV, FVC
#anxiety and depression HAD
#qaulity of life: SGRG
#comorbidities: diabetes, muscular hypertension, atrialfib, IHD

library(ggplot2)

#Pearson correlation
cor(COPD$FEV1, COPD$SGRQ)

#scatter plot
ggplot(COPD, aes(x=FEV1, y=SGRQ)) + geom_point() + geom_smooth(method=lm)



#linear regression
#how will FEV1 (lung capacity) increase with quality of life
#lm(outcome ~ predictor, data =dataframe)
SGRQ_FEV1 <- lm(SGRQ~FEV1, data=COPD)
summary(SGRQ_FEV1) #FEV1 means beta is -8.226, p value is low
#for each one unit (1l/sec) in increase in predictor value ie FEV1 it is estimated that quality of life is estimated to decrease (i.e. improve) by 8.226 units
#SQRQ = 53.4 - 8.2*FEV1
#Adjusted R-squared:  0.08288 - meaning the model is explaining around eight percent of the total variability in the observed data
confint(SGRQ_FEV1)

#run a QQ plot
predict(SGRQ_FEV1)
residuals(SGRQ_FEV1)
#Set a plotting format of 4 graphs: 
par(mfrow=c(2,2))
#View the 4 resulting plots
plot(SGRQ_FEV1)
#QQplot, allows us to assess normality assumptions for the residuals - Except for a slight drift in both the tails of the distribution, the assumption of normality looks reasonably satisfied.
#residua;ls vs fitted - To assess the assumption of constant variance, we look at the graph where the residuals are plotted by the fitted value. In the plots, you can see that as the fitted values increase, the residuals are evenly scattered around zero. So, the constant variants assumption, is also satisfied
dev.off()


#assess the association between the walking distance and lung function in COPD patients
hist(COPD$MWT1Best)
hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)
#pinpointing outliers
subset(COPD, MWT1Best > 600 | MWT1Best < 150)

#summary statistics for MWT1Best
list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))
#mean and median are different
#1 missing value

#summary statistic for FEV
list("Summary" = summary(COPD$FEV1), "Mean" = mean(COPD$FEV1, na.rm=TRUE), "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), "Range" = range(COPD$FEV1, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm=TRUE))

#plot (plot the predictor variable on the x-axis and the outcome variable on the y-axis.)
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

#correlation test
#Pearson
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs")

#Spearman
cor.test(COPD$FEV1, COPD$MWT1Best, use="complete.obs", method="spearman")

#When the data are normally distributed, you should expect Pearson’s and Spearman’s to be reasonably similar - but there will be greater differences when distributions are skewed or there are outliers.

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data=COPD)
summary(MWT1Best_FEV1)
#The (Intercept) indicates the regression constant α.  
#FEV1 indicates the linear effect of lung function, i.e. β.
#for a one unit increase in FEV1, is predicted that the walking distance will increase on average by 74 metres
# the regression model explains 21% of the variability in the observations.

confint(MWT1Best_FEV1)
#95% chance the true population parametre will lie somewhere in the range from 46 metres up to 102 metres.


#To check model assumptions
par(mfrow=c(2,2)) 
plot(MWT1Best_FEV1)

#This function allows to check for linearity, homoscedasticity, independence, and normality of your assumptions. 
#Four plots are generated: 
#The first is a constant variance plot, which checks for the homogeneity of the variance and the linear relation. If you see no pattern in this graph, then your assumptions are met. 
      #in this case it is uniforsm to some point. Heteroscedasticity - unequal variance of one variable acorss the other variable
#The second plot is a Q-Q plot, which checks that the residuals follow a normal distribution. The points should fall on a line if the normality assumption is met. 
#The third plot allows to detect heterogeneity of the variance. 
#The fourth plot allows for the detection of points that have a large impact on the regression coefficients. 


#fit a linear regression model between walking distance (MWT1best) and age (AGE)
#walking distance is the outcome variable and age is the predictor variable
lm(outcome ~ predictor, data =dataframe)
MWT1Best_AGE <- lm(MWT1Best~AGE, data=COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)
#for one unit increase in age, the walking distance will dicrease on average by 3.1 meters
plot(MWT1Best_AGE)
#The QQ plot suggests some violation of the assumption of normality
#The plot shows values lying off the straight line including the middle section of the plot
residualVals <- residuals(MWT1Best_AGE)
hist(residualVals, main = "Histogram of residuals", xlab = "Residuals") 
dev.off()
#Multiple Regression
#run a multiple linear regression to check whether lung function and age are predictors of walking distance

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)

#using another measure of lung capacity FVC
#walking distance (MWT1best) is the outcome variable and AGE and FVC are the predictor variables.
#The first step is to produce the simple linear regression model between MWT1best and FVC
lr1 <- lm(MWT1Best~FVC, data = COPD) # Run the regression, assigning the output to a new variable lr
summary(lr1) # View the output of the regression 
confint(lr1) # View the 95% confidence intervals of the regression 
#α = 254.95 and β=48.63 and adjusted R2 = 0.19.

#You should have already fitted the model between MWT1best and AGE and found the following:
lr2 <- lm(MWT1Best~AGE, data = COPD)
summary(lr2)
confint(lr2)
#α = 616.45 and β=−3.10, and adjusted R2 = 0.04. 


lr3 <- lm(MWT1Best~FVC+AGE, data = COPD) 
summary(lr3) 
confint(lr3) 
#α = 425.38, β1=46.06 and β2 = -2.33.
#α is the estimated walking distance you would expect for people aged 0 years and with a FVC value of 0. 
#β1 is the average increase in walking distance for every one unit increase in FVC, keeping age held constant. 
#β2 is the average increase in walking distance for every one year increase in age, keeping FVC held constant.

#is there a colinearity between age and FVC
cor.test(COPD$AGE, COPD$FVC, use="complete.obs", method="spearman")  #low rho

#Looking at the scatterplot between AGE and FVC you should see a fairly random scatter of points like below: 
plot(COPD$AGE, COPD$FVC, xlab ="AGE", ylab ="FVC") #no linear relationshp

#assumptions
plot(lr3)

#GOOD PRACTICE IN R
dim(COPD) 
#Getting to know your variables 
class(COPD$AGE) #integer, R will treat it as continuos
summary(COPD$AGE)
hist(COPD$AGE) # seem to be a slight negative skew to our patient ages: the tail at the lower end is longer than the tail at the upper end. 

class(COPD$COPDSEVERITY) #categorical
table(COPD$COPDSEVERITY, exclude=NULL)

class(COPD$gender) #change it to categorical so its is not treated as continuos
COPD$gender <- as.factor(COPD$gender) 
class(COPD$gender)
table(COPD$gender, exclude = NULL) 

#If you want to change a variable to a numeric data type, use the command as.numeric(). 
#If you want to change a variable to a character data type, use the command as.character(). 
#If you want to change a variable to a integer data type, use the command as.integer(). 

#Changing the reference category of a categorical variable 
relevel()
COPD$copd <- relevel(COPD$copd, ref=3) #da je 3 referentna kategorija
#this function only works with variables saved as factors!

#creating a new variable comorbidity
#you need to create the vector comorbid.
#you do know that you want it to be the same length as the other variables
comorbid <- length(COPD$Diabetes)
#comorbid will have a value of 1 if Diabetes = 1, OR muscular = 1, OR hypertension = 1, OR AtrialFib = 1, OR IHD = 1.
#comorbid will have a value of 0 if ALL of the above variables = 0.`
comorbid <- length(COPD$Diabetes)
comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 | COPD$AtrialFib == 1| COPD$IHD == 1 ] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)
print(comorbid)
str(comorbid)
#If you want to add this new variable to your existing dataset (COPD), you can do so using the following command: 
  COPD$comorbid <- comorbid
  
#Run a Good Practice Analysis
COPD <- read.csv("COPD_student_dataset.csv")
library(Hmisc)
describe(COPD)
#The categorical variables are: copd, gender, smoking and comorbid
class(COPD$gender)
COPD$gender <- factor(COPD$gender)
class(COPD$gender)
describe(COPD$gender)
library(gmodels)
CrossTable(COPD$copd) 
sum(is.na(COPD$copd)) 
summary(COPD$copd)
#The continuous variables are: AGE, PackHistory, CAT, FEV1, FEV1PRED, FVC, FVCPRED, HAD, SGRQ. The R code for summary statistics and histograms is
hist(COPD$PackHistory)
summary(COPD$PackHistory)
hist(COPD$CAT) #has outlier
COPD$CAT[COPD$CAT > 40] <- NA
describe(COPD$CAT)
#Examine the relationship between your candidate predictor variables
my_data <- COPD[, c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(my_data)
round(cor_matrix,2)
pairs(~AGE +PackHistory + FEV1 + FEV1PRED + FVC + CAT + HAD + SGRQ, data = COPD)
#To examine associations between categorical variables
#CrossTable(mydata$myrowvar, mydata$mycolvar)
CrossTable(COPD$hypertension, COPD$IHD) #8 patients had IHD only, and 11 patients had hypertension only, one person had both hypertension and IHD and 81 had neither
#Fit a simple linear regression model
lr1 <- lm(MWT1Best ~ gender, data =COPD)
summary(lr1)
confint(lr1)  

#interactions between the two predicotors (diabetes and atrial fibrilation)
COPD$Diabetes <- as.integer(COPD$Diabetes)
COPD$AtrialFib <- as.integer(COPD$AtrialFib) #da bi ih mogli pomnoziti u trecu varijablu
DAF <- COPD$Diabetes * COPD$AtrialFib 
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=COPD) 
summary(r1)
#A person with both diabetes and atrial fibrillation has an estimated average walking distance of 218.3 metres.
#MWT1best= 428.1−7.7∗Diabetic−72.0∗AtrialFib−130.1∗(Diabetic∗AtrialFib)1  =428.1−7.7−72.0−130.1
#You can obtain this output directly in R using the prediction() command from the ‘prediction’ package
library(prediction)
list("Diabetes" = prediction(r1, at = list(Diabetes = c(0,1))), "AtrialFib" = prediction (r1, at = list(AtrialFib + c(0,1))), "Diabetes*AtrialFib"=predicition(r1, at = list(Diabetes = c(0,1), AtrialFib = c(0,1))))
#interactions between binary and continuos variable
#CAT score, and is a continuous variable, and smoking status, a binary variable with the categories, current or ex-smoker.
# whether the effect of disease severity on lung function varies by smoking status.



#change the reference category from female ot male
mlr1 <- lm(MWT1Best~AGE+FEV1+factor(gender)+factor(smoking), data=COPD)
summary(mlr1)
confint(mlr1)
levels(COPD$gender) #0 is femlae, and 1 is male
COPD$smoking <- as.factor(COPD$smoking)
levels(COPD$smoking)
COPD$smoking <- relevel(COPD$smoking, ref=2)
mlr1 <- lm(MWT1Best~AGE+FEV1+factor(gender)+factor(smoking), data=COPD)
summary(mlr1)
confint(mlr1)