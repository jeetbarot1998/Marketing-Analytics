# I HAVE TAKEN SIGNIFICANCE LEVEL OF 5% or 0.05 or CONFIDENCE LEVEL OF 95% ALL THROUGHOUT THE PRACTICE SESSION.
#############################################################
#
# Marketing Analytics Assignment 2.
#
# In this assignment, you are asked to use some of the 
# commands demonstrated in the video examples to perform a conjoint
# analyis.
#
# You will then be asked to interpret the # results.  
#
# For this assignment, you may use any resources necessary
# to be able to execute the code.  However, the interpretation 
# should reflect your own thinking.
#
# You are to work on the problem alone, without help 
# from any other person.
#
# In this assignment, you will create a portfolio with only two
# stocks.  Use the sample code for reference.
###############################################################

# INSTRUCTIONS:  Enter your code between the comments line 
# as directed.  Comments begin with a hashtag '#'.

# For example

### Start Code

### End Code

# If you are asked to interpret results, make sure to put your
# answers in a comment line.

################################################################ 

###  required libraries  ###
# Run the following code to load the packages (libraries)
# If the library package does not load properly, make sure 
# that you have installed the package onto  your system.  
# Use the 'Packages' tab in RStudio to install the package.

library(mosaicData)
data("SaratogaHouses")
attach(SaratogaHouses)


#####################################################
# QUESTION 1: Use the str() command on SaratogaHouses

# Broadly describe the data.

### Start Code
str(SaratogaHouses)
### End Code
# There are 1728 observations and 16 variables.
# 3 are Categorical, 3 are YESN AND NO, Rest anre numerical 

#* 
#*

#* End Question 1
######################################################
# 
#####################################################
# QUESTION 2:
# First we will look at the impact of fuel type on the price.

# Make a box plot of price by fuel type using boxplot()


### Start Code
boxplot(price~fuel)
### End Code


# What is the thick line in the middle of the box represent?
# Medain of the Types of Fuels



# What is the lower and upper edges of the box represent?

# Inter Quartile range( 25 to 75 % of data data lies in that range)

#* Do the box plots look the same or difference.  Explain in one or two sentences.
# They are different


#* End Question 2
######################################################

#####################################################
# QUESTION 3:
# 
# Run an ANoVA using Price and Fuel.  Save the results of the ANOVA analysis
# in a variable AOV1 and use the aov() function.
# Display the results using the summary() function


### Start Code
AOV1 <- aov(price~fuel,data=SaratogaHouses)
summary(AOV1)
### End Code

# What is the null hypothesis?
# Null hypothesis is that fuel types has no effect on the price of the house

# What is the p-value for fuel?  Interpret the results
# P value is 2*10^-16. This is very small and thus shows high confidence of direct effect of 
# Fuel of the dependent var. Price.


#* End Question 3
######################################################
# 

#####################################################
# QUESTION 4:
#
# Make a box plot of price by waterfront type using boxplot()

### Start Code
boxplot(price~waterfront)
### End Code


# Do the boxplots look the same or different. Explain.
# No the box plots are not the same, The price of Houses facing waterfront is much higher tha 
# those which are not facing waterfront.
# There are many outliers in the house not facing waterfront


# Run an ANoVA using Price and waterfront  Save the results of the ANOVA analysis
# in a variable AOV2 and use the aov() function.
AOV2 <- aov(price~waterfront,data=SaratogaHouses)
# Display the results using the summary() function
summary(AOV2)

# What is the null hypothesis?
# Null hypothesis H0,is that " There is no effect of waterfront on the price of house"



# What is the p-value for waterfront?  Interpret the results.
# As the P value =1.21e-10 is very samll, we can reject the null hypothesis H0


#* End Question 4
######################################################

#####################################################
# QUESTION 5:

# Run the following code

#The following code creates two column vectors. One with all
#the prices of houses that have a water front, and a column of prices
#of houses without water front.

PWithWater <- price[waterfront== "Yes"]
PWoutWater <- price[waterfront=="No"]

# Using PWithWater and PWoutWater, perfrom a t-test using t.test()
t.test(PWithWater,PWoutWater)

# What is the null hypothesis?
# Null Hpothesis is that the means are equal
#* 

# What is the p-value for the t-test?  Interpret the results.
# P value is 0.0011199
# This means the result is highly relevent

# Let's assume equal variances of the two groups. Perform a t-test assuming equal
# variances, e.g, t.test(x,y, var.equal=TRUE)
t.test(PWithWater,PWoutWater,var.equal = TRUE)


# What is the null hypothesis?
# Null Hpothesis is that the means are equal given that the varience is same for both of them.
#*

# What is the p-value for the t-test?  Intperet the results.
# p-value = 1.21e-10, this shows that the results are highly accurate.
#*



#* End Question 5
######################################################
#####################################################
# QUESTION 6:
# 


# Run an additive ANoVA using Price, waterfront, and fuel.  Save the results of the ANOVA analysis
# in a variable AOV3 and use the aov() function.
AOV3 <- aov(price~waterfront+fuel,data=SaratogaHouses)
# Display the results using the summary() function
summary(AOV3)


# What is the p-value for waterfront?  Interpret the results.
# P-value= 2.18e-11 , This means that waterfront has a direct impact on price



# What is the p-value for fuel?  Interpret the results.
# P-Value = 2e-16 , This means that fuel has a direct impact on price





#* End Question 6
######################################################
#####################################################
# QUESTION 7:
# 


# Run an two-way ANoVA using Price, waterfront, and fuel with interaction.
# Save the results of the ANOVA analysis
# in a variable AOV4 and use the aov() function.
AOV4 <- aov(price~waterfront*fuel,data=SaratogaHouses)
# Display the results using the summary() function
summary(AOV4)



# What is the p-value for waterfront?  Interpret the results.
# P-Value = 2.18e-11 , This shows that the waterfront factor has a direct impact on the price of the house


# What is the p-value for fuel?  Interpret the results.
# P-Value = 2e-16 ,This shows Type of fuel has a great impact on the price of house

# What is the p-value for the interaction of fuel and waterfront?  Interpret the results.
# P-Value = 0.0513 , The P value is more than 0.05 ie. the significance level is low and above the threshold selected.
# The combined effect of waterfront and fuel has no huge impact on the price of the house


# Run the following

interaction.plot(waterfront,fuel,price)

#Interpret the plot as best as you can. 
# The Interaction lot shows the variation of price(dependent var) as the response of the change
# in the independent variables (waterfront,fuel). We can see that the price changes depending 
# on whether the house is facing waterfront or not. The price of house is higher for houses facing
# waterfront. The houses Running on Gas has the highes price. We can see that the fuel types "oil"
# price increases than "electric" as we approach from waterfront = "yes" to waterfront ="NO".


#* End Question 7
######################################################
