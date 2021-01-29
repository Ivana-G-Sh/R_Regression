#PARALLEL SLOPES MODEL

#two explanatory variables: one was numeric and one was categorical.
babies <- read.csv("babies.csv")
#We would like to build a model for birthweight as a function of the mother's age and whether this child was her first (parity == 0). 
lm(bwt ~ age + parity, data=babies)

#build a model for birthweight as a function of the length of gestation and the mother's smoking status
lm(bwt ~ gestation + smoke, data=babies)ž

#fit of the parallel slopes model 
library(openintro)
marioKart
mario_kart <- marioKart
names(mario_kart)
mod <- lm(formula = totalPr ~ wheels + cond, data = mario_kart)
summary(mod) #R and R adju you can get with summary()

# R2 adj is better than R2
#add random noise
mario_kart_noisy <- mario_kart %>%
  mutate(noise = rnorm(nrow(mario_kart)))
mod2 <- lm(totalPr ~ wheels + cond + noise, data = mario_kart_noisy)
summary(mod2) #sad se R2 mijenja dok adjusted se ne mijenja, poanta vjećbe je koristiti adjusted R2

#Predictions
#Once we have fit a regression model, we can use it to make predictions for unseen observations or retrieve the fitted values
#augment() function from the broom package, which returns a data.frame with the response varible (y), the relevant explanatory variables (the x's), the fitted value (ŷ ) and some information about the residuals (e). augment() will also take a newdata argument that allows you to make prediction
library(broom)
augment(mod)

#Fitting a model with interaction
#lm(y ~ x + z + x:z, data = mydata)
#The use of the colon (:) here means that the interaction between x and z will be a third term in the model
# include interaction
names(mario_kart)
lm(formula = totalPr ~ cond + duration + cond:duration, data = mario_kart)
# interaction plot
ggplot(mario_kart, aes(y = totalPr, x = duration, color = cond)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) #this adds simple regression line

#simpsons paradox
slr <- ggplot(mario_kart, aes(y = totalPr, x = duration)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = 0)
# model with one slope
lm(formula = totalPr ~  duration, data = mario_kart)
# plot with two slopes
slr + aes(color=cond)

#Multiple linear regression (MLR)
#Adding a numerical explanatory variable
#so at the end you have 3 numerical variables that are connected and thus data space is threedimensional instead of 2D
#Fit a multiple linear regression model for total price as a function of the duration of the auction and the starting price.
lm(formula = totalPr ~  duration + startPr, data = mario_kart) 
#interpretation: For each additional day the auction lasts, the expected final price declines by $1.51, after controlling for starting price.

#visualization of MLR
library(plotly)
# draw the 3D scatterplot
#Run the plot_ly command to draw 3D scatterplot for totalPr as a function of duration and startPr by mapping the z variable to the response and the x and y variables to the explanatory variables. 
#Duration should be on the x-axis and starting price should be on the y-axis.
p <- plot_ly(data = mario_kart, z = ~totalPr, x = ~duration, y = ~startPr, opacity = 0.6) %>%
  add_markers() 

# draw the plane
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)

#adding a third categorical variable (smooking in babies dataset)
lm(bwt ~ gestation + age + smoke, data=babies)

lm(formula = totalPr ~ duration + startPr + cond, data = mario_kart)
#interpret: The expected premium for new (relative to used) MarioKarts is $6.76, after controlling for the duration and starting price of the auction

#visualization:  now have two explanatory variables and one categorical variable. Our model now takes the geometric form of two parallel planes!
# draw the 3D scatterplot
p <- plot_ly(data = mario_kart, z = ~totalPr, x = ~duration, y = ~startPr, opacity = 0.6) %>%
  add_markers(color = ~cond) 

# draw two planes
plane0 #ne znam od kud aim ovaj matrix
plane1
p %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)

#adding even more variables:
lm(bwt ~ . -case, data=babies) #include all variables in the dataset except for the case number
lm(formula = totalPr ~ duration + startPr + cond + wheels + nBids, 
   data = mario_kart)
#Each additional wheel is associated with an increase in the expected auction price of $10.16, after controlling for auction duration, starting price, number of bids, and the condition of the item

#logisitic regression
#models binary response (like alive, not alive)
#We use the glm() function instead of lm()
#We specify the family argument and set it to binomial. 
#This tells the GLM function that we want to fit a logistic regression model to our binary response. [The terminology stems from the assumption that our binary response follows a binomial distribution.]
#Use glm() to fit a logistic regression model for Acceptance as a function of GPA
MedGPA <- read.csv("medGPA.csv")
glm(Acceptance ~ GPA, data = MedGPA, family = binomial)

# scatterplot with jitter
data_space <- ggplot(data = MedGPA, aes(y = Acceptance, x = GPA)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# add logistic regression line
data_space +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))

#using bins to understand how the binary response changes - binning
MedGPA_binned <- read.csv("binned.csv")
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = acceptance_rate)) + 
  geom_point() + geom_line()

# augmented model
mod <- glm(formula = Acceptance ~ GPA, family = binomial, data = MedGPA)
MedGPA_plus <- mod %>%
  augment(type.predict = "response")

# logistic model on probability scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = .fitted), color = "red")

#logistic model on odds scale
#Most people tend to interpret the fitted values on the probability scale and the function on the log-odds scale.
# compute odds for bins
MedGPA_binned <- MedGPA_binned %>%
  mutate(odds = acceptance_rate / (1 - acceptance_rate))
# plot binned odds
data_space <- ggplot(data = MedGPA_binned, aes(x = mean_GPA, y = odds)) + 
  geom_point() + geom_line()

# compute odds for observations
MedGPA_plus <- MedGPA_plus %>%
  mutate(odds_hat = .fitted / (1 - .fitted))
# logistic model on odds scale
data_space +
  geom_line(data = MedGPA_plus, aes(x = GPA, y = odds_hat), color = "red")

#Making probabilistic predictions
#Create a new data frame which has one variable called GPA and one row, with the value 3.51.
new_data <- data.frame(GPA = 3.51)
#Use augment() to find the expected probability of admission to medical school for a student with a GPA of 3.51.
augment(mod, newdata=new_data, type.predict = "response")

#evaluation of the model witht he confusion matrix
#Create a data frame with the actual observations, and their fitted probabilities, and add a new column, Acceptance_hat, with the binary decision by rounding the fitted probabilities.
tidy_mod <- augment(mod, type.predict = "response") %>%
  mutate(Acceptance_hat = round(.fitted))

#Compute the confusion matrix between the actual (Acceptance) and predicted acceptance (Acceptance_hat).
tidy_mod %>%
  select(Acceptance, Acceptance_hat) %>% 
  table()


###################Data: Italian restaurants##########################

dir()

nyc <- read.csv("restaurants.csv")
glimpse(nyc)
pairs(nyc) #Error in plot.new() : figure margins too large
par("mar") 
par(mar=c(1,1,1,1))
pairs(nyc) #price and food are strongly correlated

#Price as a function of Food
# Price by Food plot
nyc %>% ggplot(aes(y = Price, x = Food)) +   geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Price by Food model
lm(formula = Price ~ Food, data = nyc) #Each additional rating point of food quality is associated with a increase in Price for 2 dollars"

#Let's expand our model into a parallel slopes model by including the East variable in addition to Food.
#Use lm() to fit a parallel slopes model for Price as a function of Food and East

lm(formula = Price ~ Food + East, data = nyc)
#Each additional rating point of food quality is associated with a $2.88 increase in the expected price of meal, after controlling for location.
#The premium for an Italian restaurant in NYC associated with being on the east side of 5th Avenue is $1.46, after controlling for the quality of the food.

#multiple regression: food and service influence on the Price
# fit model
lm(formula = Price ~ Food + Service, data = nyc) 

# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers() 

# draw a plane
p %>%
  add_surface(x = ~x, y = ~y, z =  ~plane, showscale = FALSE) #ne znam kako izracunaju taj plane

#Response variable: price
#Expanatiry: Food, Service, Decor, Est(categorical)

lm(Price ~ Food + Service +East, data=nyc)
#The premium for being on the East side of 5th Avenue is just less than a dollar, after controlling for the quality of food and service.


#Does the East variable clearly separate the data into two distinct groups? Or are the points all mixed up together?
# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers(color = ~factor(East)) 

# draw 3D scatterplot
p <- plot_ly(data = nyc, z = ~Price, x = ~Food, y = ~Service, opacity = 0.6) %>%
  add_markers(color = ~factor(East)) 

# draw two planes

p %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE)


#Use lm() to fit a parallel planes model for Price as a function of Food, Service, Decor, and East
lm(Price ~ Food + Service +Decor + East, data=nyc)
#Once we control for the quality of food, decor, and location, the additional information conveyed by service is negligible.