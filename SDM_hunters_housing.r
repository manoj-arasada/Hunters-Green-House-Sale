library(sjPlot)
library(sjmisc)
library(sjlabelled)
rm(list=ls())
library(readxl)
library(corrplot)
# Load the dataset
house_data <- read_excel("HuntersGreenHomeSales.xlsx")

#display data
house_data

#changing column names to lower
colnames(house_data)=tolower(make.names(colnames(house_data)))
attach(house_data)

#converting categorical values as factors
house_data$roof=as.factor(house_data$roof)
house_data$pool=as.factor(house_data$pool)
house_data$spa=as.factor(house_data$spa)
house_data$splsale=as.factor(house_data$splsale)


levels(house_data$roof)
levels(house_data$pool)
levels(house_data$splsale)
levels(house_data$spa)

#mode function
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}


mode_garages = calc_mode(garages)
mode_garages



house_data$garages[is.na(garages)] <- mode_garages
length(garages[is.na(garages)])


full_model = lm(pricesold~houseage,data = house_data)
length(full_model$fitted.values)
summary(full_model)

num_house_data = subset(house_data,select = -c(slnoskm,status,address,roof,pool,spa,subdivn,pendingdate,datesold,splsale))
cor(num_house_data)
num_house_data$romt = (num_house_data$beds+num_house_data$bathsfull)
cor(num_house_data$romt,num_house_data$sqft)


#model for pricesold
#create required columns 
house_data$extrasqft = house_data$lotsqft - house_data$sqft



hist((house_data$pricesold), breaks = 30, col = "blue", main = "Histogram of price", xlab = "Value")
hist(log(house_data$pricesold), breaks = 30, col = "blue", main = "Histogram of pricesold", xlab = "Value")

plot((house_data$sqft), (house_data$pricesold))

plot((house_data$beds), (house_data$pricesold))

plot((house_data$garages), (house_data$pricesold))

plot((house_data$roof), (house_data$pricesold))

plot((house_data$houseage), (house_data$pricesold))

plot((house_data$pool), (house_data$pricesold))



house_data$yearsold <- as.POSIXct(datesold, format = "%m/%d/%Y")
format(house_data$yearsold, format="%Y")

house_data$houseage = as.numeric(format(house_data$yearsold, format="%Y")) - house_data$yrblt

house_data$houseage

plot((house_data$houseage), (house_data$pricesold))




#model1
psmod1=lm(pricesold~beds+bathstotal+roof+garages+lotsqft+yrblt +pool+lppersqft+listprice+spa+splsale, data = house_data)

summary(psmod1)






#model3

psmod2=lm(pricesold~beds+bathstotal+garages+roof+I(lotsqft - sqft)+houseage+pool+lppersqft, data = house_data)


summary(psmod2)

tab_model(psmod1,psmod2)


length(psmod2$fitted.values)
plot(psmod2)


plot(house_data$pricesold,psmod2$fitted.values,
     pch=19,main="Actuals v. Fitted, price")
abline(0,1,col="red",lwd=3)

qqnorm(psmod2$residuals,pch=19,
       main="Normality Plot, price")
qqline(psmod2$residuals,lwd=3,col="red")


hist(psmod2$residuals,col="red",
     main="Residuals, price",
     probability=TRUE)
curve(dnorm(x,mean(psmod2$residuals),
            sd(psmod2$residuals)),
      from=min(psmod2$residuals),
      to=max(psmod2$residuals),
      lwd=3,col="blue",add=TRUE)









#ADOM

hist((house_data$adom_agentdaysonmarket), breaks = 30, col = "blue", main = "Histogram of ADOM", xlab = "Value")
hist(log(house_data$adom_agentdaysonmarket), breaks = 30, col = "blue", main = "LOG Histogram of ADOM", xlab = "Value")

plot((house_data$sqft), log(house_data$adom_agentdaysonmarket))

plot((house_data$beds), (house_data$adom_agentdaysonmarket))

plot((house_data$garages), (house_data$adom_agentdaysonmarket))

plot((house_data$roof), (house_data$adom_agentdaysonmarket))

plot((house_data$houseage), log(house_data$adom_agentdaysonmarket))

plot((house_data$pool), (house_data$pricesold))



house_data$yearsold <- as.POSIXct(datesold, format = "%m/%d/%Y")
format(house_data$yearsold, format="%Y")

house_data$houseage = as.numeric(format(house_data$yearsold, format="%Y")) - house_data$yrblt

house_data$houseage

plot((house_data$houseage), (house_data$pricesold))

house_data$logadom=log(house_data$adom_agentdaysonmarket)

(house_data$logadom)
house_data$logadom[!is.finite(house_data$logadom)] <- -1000


#model1
admod1=lm(adom_agentdaysonmarket~beds+bathstotal+sqft+roof+garages+lotsqft+yrblt +pool+lppersqft+listprice+splsale, data = house_data)

summary(admod1)


#model2
admod2=lm(adom_agentdaysonmarket~I((((beds+bathstotal))/lppersqft)^2)+I(listprice*listprice)+I(lppersqft*lppersqft)+pool+garages+houseage+roof+I(lotsqft-sqft)+splsale, data = house_data)

summary(admod2)
plot((house_data$adom_agentdaysonmarket),(admod2$fitted.values),
     pch=19,main="Actuals v. Fitted, price")
abline(0,1,col="red",lwd=3)


tab_model(admod1,admod2)


length(psmod2$fitted.values)
plot(psmod2)


plot((house_data$adom_agentdaysonmarket),(admod1$fitted.values),
     pch=19,main="Actuals v. Fitted, price")
abline(0,1,col="red",lwd=3)

qqnorm(psmod2$residuals,pch=19,
       main="Normality Plot, price")
qqline(psmod2$residuals,lwd=3,col="red")


hist(psmod2$residuals,col="red",
     main="Residuals, price",
     probability=TRUE)
curve(dnorm(x,mean(psmod2$residuals),
            sd(psmod2$residuals)),
      from=min(psmod2$residuals),
      to=max(psmod2$residuals),
      lwd=3,col="blue",add=TRUE)

































