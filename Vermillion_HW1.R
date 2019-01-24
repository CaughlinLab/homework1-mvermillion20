# Homework 1 
install.packages("ggplotgui")
#library(ggplotgui)
#ggplot_shiny(math_scores)
setwd("H:/HierarchicalModel/Homework/homework1-mvermillion20-master")
math_scores <- read.csv("math_scores.csv")
miracle_food <- read.csv("miracle_food.csv")



# Question 1 
head(math_scores) #gives top 6 values of variable 
# (a) Scatterplot of the data and best fit line 
plot(math_scores$LSD_concentration,math_scores$MATH_score,main='Math Scores Regression',col="darkseagreen4",pch=19,
     xlab="LSD Concentration",ylab = "Math Score")
# (b) Parameter estimates and 95% CI for slope and intercept 
model1<-lm(MATH_score~LSD_concentration, data = math_scores)
math_coef<-coef(model1) # Get coefficients for linear model 
math_95<-confint(model1) # get 95% coefficients for slope and intercept parameters 
curve(89.123874-9.00946*x,add=T,col="darkseagreen3") # plot Best Fit Least Squared Model from line 13
# (c) Metric of linear model fit RMSE 
#Function to Calcualte RMSE value for plot 
rmse=function(y_hat,y)
{
  return(sqrt(mean((y_hat-y)^2)))
}

y_hat <- 89.123874-9.00946*(math_scores$LSD_concentration) # compute the Y vales for the model 
LSD_RMSE<-rmse(y_hat,math_scores$MATH_score) # find RMSE 
 #(A) What level of LSD do you need to ensure a math score of 85%? i.e. Y=85 what is x? 
LSD_Level<- (85-89.123874)/(-9.00946) 
# (B) How well does LSD tissue concentration predict test preformance? Based on the RMSE of 6.022 LSD tissue performance is a good indication of 
# math test preformance, however, there is only 7 samples and more data should be collected to confirm the model. 



# Question 2 
miracle<-read.csv("miracle_food.csv") # Load data 
head(miracle) # Read the first 6 lines of data 
#(a) scatterplot of data and best fit line using curve 
plot(miracle_food$pomegranate,miracle_food$Weight_loss,main='Miracle Food Regression',xlab = "Number of pomegranates",
     ylab = "Weight Loss",col="deepskyblue4")
model2<-lm(Weight_loss~pomegranate, data=miracle_food)
#(b) 
miracle_coef<-coef(model2) # find coefficients for linear regression model 
miracle_95 <- confint(model2) # find 95% 
curve(-0.1789-0.5251*x,add=T,col="deepskyblue3") # add model to the current data
#(c) 
v_mod<--0.1789-0.5251*(miracle_food$pomegranate) # find the output of the model 
RMSE_mirace <- rmse(v_mod,miracle_food$Weight_loss) #calculate the RMSE 


# Claculate the R value for your model 
SS = sum((miracle_food$Weight_loss-mean(miracle_food$Weight_loss))^2) #sum of squares 
Rss = sum((miracle_food$Weight_loss-v_mod)^2) # Residual values 
R = (SS-Rss)/(SS) # calcuate the R value 


# Question 3
#A. MAE function 
MAE=function(n,y,y_hat)
{ 
  return ((1/n)*sum(abs(y-y_hat)))
}
#  import R2 function
R2=function(y_hat,y) 
  {
  RSS<-sum((((y_hat))-(y))^2)
  
  TSS<-sum(((y)-(mean(y)))^2)23.
  
  return(1-RSS/TSS)
  }

#B. 
# Comparison of RMSE, R2 AND MAE for dataset 1
n<-length(math_scores$MATH_score)
R2_LSD <- R2(y_hat,math_scores$MATH_score) # compute R2 for math problems 
MAE_LSD <- MAE(n,math_scores$MATH_score,y_hat)# compute MAE for math problems 

# Comparison of RMSE, R2 and MAE for dataset 2
n2<-length(miracle_food$Weight_loss)
R2_miracle <- R2(v_mod,miracle_food$Weight_loss)
MAE_miracle <- MAE(1000,miracle_food$Weight_loss,v_mod)


# Question 4
# Create a predictor variable 
coffee_conc<-runif(150)
#Decide on intercept and slope values 
inter<- 2.5
slope<- 23.2
Fish_movement<-rnorm(n=150,mean=inter+slope*coffee_conc,sd=1.5)
# Plot Data 
plot(coffee_conc,Fish_movement,main="Fish Movement related to Coffee Concentration",col="coral3",ylab = "Fish Movement",
     xlab = "Coffee Concentration of Water")
# Linear regression
model3 <- lm(Fish_movement~coffee_conc)
fish_coef<- coef(model3) # calculate slope and intercept using coef 

# Question 5
uneq_vary<-1.6*coffee_conc
Fish_movement_uneq<-rnorm(n=150,mean=inter+slope*coffee_conc,uneq_vary)
plot(Fish_movement_uneq~coffee_conc,main="Generated Data with Unequal Variance",col="darkorange2",ylab = "Fish Movement",
     xlab = "Coffee Concentration of the Water")






  

