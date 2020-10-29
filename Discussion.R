
library(mosaicData)
data("SaratogaHouses")
attach(SaratogaHouses)

----------------------------------------

# Run an ANoVA using Price and Fuel.  Save the results of the ANOVA analysis
# in a variable AOV1 and use the aov() function.

AOV1 <- aov(price~fuel,data=SaratogaHouses)
summary(AOV1)

#Q: What is the null hypothesis?
#ANS: Null hypothesis is that fuel types has no effect on the price of the house

#Q. What is the p-value for fuel?  Interpret the results
#ANS: P value is 2*10^-16. This is very small and thus shows high confidence of direct effect of Fuel of the dependent var. Price.

------------------------------------------

boxplot(price~waterfront)

#Q. Do the boxplots look the same or different. Explain.
#ANS: No the box plots are not the same, The price of Houses facing waterfront is much higher than those which are not facing waterfront. There are many outliers in the house not facing waterfront

--------------------------------------------

AOV2 <- aov(price~waterfront,data=SaratogaHouses)
summary(AOV2)

#Q. What is the null hypothesis?
#ANS: Null hypothesis H0,is that " There is no effect of waterfront on the price of house"

#Q. What is the p-value for waterfront?  Interpret the results.
#ANS: As the P value =1.21e-10 is very samll, we can reject the null hypothesis H0

---------------------------------------------

PWithWater <- price[waterfront== "Yes"]
PWoutWater <- price[waterfront=="No"]

# Using PWithWater and PWoutWater, perfrom a t-test using t.test()
t.test(PWithWater,PWoutWater)

#Q. What is the null hypothesis?
#ANS: Null Hpothesis is that the means are equal 

#Q.  What is the p-value for the t-test?  Interpret the results.
#ANS: P value is 0.0011199, This means the result is highly relevent

#Q. What is the null hypothesis?
#ANS: Null Hpothesis is that the means are equal given that the varience is same for both of them.

#Q. What is the p-value for the t-test?  Intperet the results.
#ANS: p-value = 1.21e-10, this shows that the results are highly accurate.

--------------------------------

AOV3 <- aov(price~waterfront+fuel,data=SaratogaHouses)
# Display the results using the summary() function
summary(AOV3)


#Q. What is the p-value for waterfront?  Interpret the results.
#ANS: P-value= 2.18e-11 , This means that waterfront has a direct impact on price



#Q. What is the p-value for fuel?  Interpret the results.
#ANS: P-Value = 2e-16 , This means that fuel has a direct impact on price

----------------------------------

# 2-WAY ANOVA
AOV4 <- aov(price~waterfront*fuel,data=SaratogaHouses)
summary(AOV4)

#Q. What is the p-value for waterfront?  Interpret the results.
#ANS: P-Value = 2.18e-11 , This shows that the waterfront factor has a direct impact on the price of the house


#Q. What is the p-value for fuel?  Interpret the results.
#ANS: P-Value = 2e-16 ,This shows Type of fuel has a great impact on the price of house

#Q. What is the p-value for the interaction of fuel and waterfront?  Interpret the results.
#ANS: P-Value = 0.0513 , The P value is more than 0.05 ie. the significance level is low and above the threshold selected.The combined effect of waterfront and fuel has no huge impact on the price of the house

--------------------------------------

interaction.plot(waterfront,fuel,price)

#Q. Interpret the plot as best as you can. 
#ANS: The Interaction lot shows the variation of price(dependent var) as the response of the change in the independent variables (waterfront,fuel). We can see that the price changes depending 
# on whether the house is facing waterfront or not. The price of house is higher for houses facingwaterfront. The houses Running on Gas has the highes price. We can see that the fuel types "oil"
# price increases than "electric" as we approach from waterfront = "yes" to waterfront ="NO".
