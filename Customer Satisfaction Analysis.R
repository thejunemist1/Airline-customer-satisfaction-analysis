
#store the file in data
data<- read.csv("Satisfaction Survey.csv")
str(data)
View(data)

#removing . from column names
column <- colnames(data)
column
column <- gsub("\\.","",column)
column
colnames(data)<- column
str(data)

#rename colums with big names
colnames(data)[8]<- "FlightOtherAirline"
colnames(data)[10]<- "LoyaltyCards"
colnames(data)[11]<- "ShoppingAmount"
colnames(data)[12]<- "EatandDrink"
colnames(data)[28]<- "ArrivalDelay"
str(data)

#Removing anamolies from satisfaction


count(data, Satisfaction)
# Satisfaction     n
# <fct>           <int>
#  1             2999
#  2            23587
#  2.5              2
#  3            36984
#  3.5              2
#  4            53758
#  4.00.2.00        2
#  4.00.5           1
#  4.5              2
#  5            12552


#identify the rows which have 4.00.2.00 as satisfaction
condition <- which(data$Satisfaction == "4.00.2.00")
#removing the rows
data<-data[-condition,]
data[condition,]
#check if removed
length(which(data$Satisfaction=="4.00.2.00"))


length(which(data$Satisfaction=="4.00.5"))
#identify the rows which have 4.00.5 as satisfaction
condition1 <- which(data$Satisfaction == "4.00.5")
#removing the rows
data<-data[-condition1,]
data[condition1,]
#check if removed
length(which(data$Satisfaction=="4.00.5"))

#Checking for NA
sum(is.na(data))
#converting data.satisfaction from factor to numeric
v1<- data$Satisfaction
c<-as.numeric(levels(v1))[v1]
data$Satisfactionnum <- c
str(data)

sum(is.na(data))
sum(is.na(data$AirlineStatus))
sum(is.na(data$Satisfaction))
sum(is.na(data$Age))
sum(is.na(data$Gender))
sum(is.na(data$PriceSensitivity))
sum(is.na(data$YearofFirstFlight))
sum(is.na(data$NoofFlightspa))
sum(is.na(data$FlightOtherAirline))
sum(is.na(data$TypeofTravel))
sum(is.na(data$LoyaltyCards))
sum(is.na(data$ShoppingAmount))
sum(is.na(data$EatandDrink))
sum(is.na(data$Class))
sum(is.na(data$DayofMonth))
sum(is.na(data$Flightdate))
sum(is.na(data$AirlineCode))
sum(is.na(data$AirlineName))
sum(is.na(data$OrginCity))
sum(is.na(data$OrginState))
sum(is.na(data$DestinationCity))
sum(is.na(data$DestinationState))
sum(is.na(data$ScheduledDepartureHour))
sum(is.na(data$DepartureDelayinMinutes))
sum(is.na(data$ArrivalDelayinMinutes))
sum(is.na(data$Flightcancelled))
sum(is.na(data$Flighttimeinminutes))
sum(is.na(data$FlightDistance))
sum(is.na(data$ArrivalDelay))
sum(is.na(data$Satisfactionnum))

# > sum(is.na(data$Flighttimeinminutes))
# [1] 2738
# > sum(is.na(data$DepartureDelayinMinutes))
# [1] 2345
# > sum(is.na(data$ArrivalDelayinMinutes))
# [1] 2738

datana<-data[which(is.na(data$Flighttimeinminutes)), ]
View(datana)
#remove rows with NA
data<- data[-which(is.na(data$Flighttimeinminutes)),]
sum(is.na(data))


#change anything with 2 factors into num to use with lm

data$Gendernum <- case_when(
  data$Gender=="Female" ~ 0,
  data$Gender=="Male" ~1)

data$ArrivalDelaynum <- case_when(
  data$ArrivalDelay == "no" ~ 0,
  data$ArrivalDelay == "yes" ~ 1 )

data$Flightcancellednum <- case_when(
  data$Flightcancelled == "No" ~ 0, 
  data$Flightcancelled == "Yes" ~ 1)

#dynamic variables for age, age=yound,middleage,old.Make three columns young(0 or 1)

data$young <- case_when(
  data$Age <= 30 ~ 1, 
  data$Age >30 ~ 0)

data$middleage <- case_when(
  data$Age > 30 & data$Age <= 55 ~ 1, 
  data$Age > 55 ~ 0,
  data$Age <= 30 ~ 0)

data$old <- case_when(
  data$Age <= 55 ~ 0, 
  data$Age > 55 ~ 1)


data$yearsfromfirstflight <- 2018 - data$YearofFirstFlight

View(data)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
#Segrigating data according to the flights

GoingNorthdata<-filter(data, data$AirlineName == "GoingNorth Airlines Inc. ")


OnlyJetsdata<-filter(data, data$AirlineName == "OnlyJets Airlines Inc. ")


FlyFastdata<-filter(data, data$AirlineName == "FlyFast Airways Inc. ")


Cheapseatsdata<-filter(data, data$AirlineName == "Cheapseats Airlines Inc. ")
View(Cheapseatsdata)

EnjoyFlyingdata<-filter(data, data$AirlineName == "EnjoyFlying Air Services")


Oursindata<-filter(data, data$AirlineName == "Oursin Airlines Inc. ")


Northwestdata<-filter(data, data$AirlineName == "Northwest Business Airlines Inc. ")


FlyHeredata<-filter(data, data$AirlineName == "FlyHere Airways")


Southeastdata<-filter(data, data$AirlineName == "Southeast Airlines Co. ")


Sigmadata<-filter(data, data$AirlineName == "Sigma Airlines Inc. ")


PaulSmithdata<-filter(data, data$AirlineName == "Paul Smith Airlines Inc. ")


FlyToSundata<-filter(data, data$AirlineName == "FlyToSun Airlines Inc. ")


CoolYoungdata<-filter(data, data$AirlineName == "Cool&Young Airlines Inc. ")


Westdata<-filter(data, data$AirlineName == "West Airways Inc. ")


passengercount <- count(data,AirlineName)
passengercount
# AirlineName                             n
#   <fct>                               <int>
# 1 "Cheapseats Airlines Inc. "         26058
# 2 "Cool&Young Airlines Inc. "          1288
# 3 EnjoyFlying Air Services             8927
# 4 "FlyFast Airways Inc. "             15407
# 5 FlyHere Airways                      2481
# 6 "FlyToSun Airlines Inc. "            3407
# 7 "GoingNorth Airlines Inc. "          1568
# 8 "Northwest Business Airlines Inc. " 13837
# 9 "OnlyJets Airlines Inc. "            5395
# 10 "Oursin Airlines Inc. "            10968
# 11 "Paul Smith Airlines Inc. "        12248
# 12 "Sigma Airlines Inc. "             17037
# 13 "Southeast Airlines Co. "           9577
# 14 "West Airways Inc. "                1688
passengercount<-as.data.frame(passengercount)


install.packages("sqldf")
library(sqldf)


meansat<-sqldf("select AirlineName, avg(Satisfaction) from data group by AirlineName order by 
            avg(Satisfaction)")
meansat
#                   AirlineName           avg(Satisfaction)
# 1          GoingNorth Airlines Inc.           3.297194
# 2            OnlyJets Airlines Inc.           3.346803
# 3              FlyFast Airways Inc.           3.352567
# 4          Cheapseats Airlines Inc.           3.357318
# 5           EnjoyFlying Air Services          3.360199
# 6              Oursin Airlines Inc.           3.386534
# 7  Northwest Business Airlines Inc.           3.394798
# 8                    FlyHere Airways          3.395002
# 9            Southeast Airlines Co.           3.396888
# 10              Sigma Airlines Inc.           3.397547
# 11         Paul Smith Airlines Inc.           3.399167
# 12           FlyToSun Airlines Inc.           3.425301
# 13         Cool&Young Airlines Inc.           3.442547
# 14                West Airways Inc.           3.486967
meansat<- as.data.frame(meansat)


airlinecountsat<- merge(passengercount,meansat,by="AirlineName")
View(airlinecountsat)

#As Cheapseats Airlines Inc. has the highest passenger count of 26058 and a relatively low 
#satisfaction of 3.357318, there is scope for improvement of satisfaction and large data for analysis.

# Cheapseats Airlines Inc
#**************************
install.packages("ggplot2")
library(ggplot2)

m1 <- ggplot(Cheapseatsdata, aes(x = Cheapseatsdata$PriceSensitivity, y = Cheapseatsdata$Satisfaction))  + geom_point()
m1

m2 <- ggplot(Cheapseatsdata, aes(x = Cheapseatsdata$AirlineStatus, y = Cheapseatsdata$Satisfaction)) + geom_point()
m2

m3 <- ggplot(Cheapseatsdata, aes(x = Cheapseatsdata$Age, y = Cheapseatsdata$Satisfaction)) + geom_point()
m3

m4 <- ggplot(Cheapseatsdata, aes(x = Cheapseatsdata$Gender, y = Cheapseatsdata$Satisfaction)) + geom_point()
m4


m5 <- ggplot(Cheapseatsdata, aes(x = Cheapseatsdata$LoyaltyCards, y = Cheapseatsdata$Satisfaction)) + geom_point()
m5

plot(Cheapseatsdata$PriceSensitivity, Cheapseatsdata$Satisfaction)


sum(is.na(Cheapseatsdata))
str(Cheapseatsdata)

# Linear Modeling for Cheapseats Airlines Inc (A)
#*********************************************

lm1 <- lm(formula = Cheapseatsdata$Satisfactionnum~Cheapseatsdata$Age+Cheapseatsdata$PriceSensitivity+
            Cheapseatsdata$YearofFirstFlight+Cheapseatsdata$NoofFlightspa+Cheapseatsdata$FlightOtherAirline+
            Cheapseatsdata$LoyaltyCards+Cheapseatsdata$ShoppingAmount+Cheapseatsdata$EatandDrink+
            Cheapseatsdata$DayofMonth+Cheapseatsdata$ScheduledDepartureHour+
            Cheapseatsdata$DepartureDelayinMinutes+Cheapseatsdata$ArrivalDelayinMinutes+
            Cheapseatsdata$Flighttimeinminutes+Cheapseatsdata$FlightDistance, data=Cheapseatsdata)
lm1
summary(lm1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0762 -0.6520  0.1758  0.6038  2.5849 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                            -8.973e+00  3.907e+00  -2.297 0.021645 *  
#   Cheapseatsdata$Age                     -1.148e-02  3.818e-04 -30.070  < 2e-16 ***
#   Cheapseatsdata$PriceSensitivity        -1.919e-01  1.070e-02 -17.940  < 2e-16 ***
#   Cheapseatsdata$YearofFirstFlight        6.729e-03  1.947e-03   3.457 0.000548 ***
#   Cheapseatsdata$NoofFlightspa           -1.335e-02  4.259e-04 -31.341  < 2e-16 ***
#   Cheapseatsdata$FlightOtherAirline      -1.409e-03  7.358e-04  -1.914 0.055570 .  
# Cheapseatsdata$LoyaltyCards            -3.656e-02  6.131e-03  -5.964 2.49e-09 ***
#   Cheapseatsdata$ShoppingAmount          -1.023e-04  1.097e-04  -0.933 0.350806    
# Cheapseatsdata$EatandDrink              1.342e-04  1.135e-04   1.182 0.237115    
# Cheapseatsdata$DayofMonth              -6.727e-05  6.725e-04  -0.100 0.920312    
# Cheapseatsdata$ScheduledDepartureHour  -3.330e-03  1.367e-03  -2.436 0.014856 *  
#   Cheapseatsdata$DepartureDelayinMinutes  3.503e-04  6.845e-04   0.512 0.608855    
# Cheapseatsdata$ArrivalDelayinMinutes   -2.437e-03  6.743e-04  -3.613 0.000303 ***
#   Cheapseatsdata$Flighttimeinminutes     -1.128e-03  4.583e-04  -2.460 0.013887 *  
#   Cheapseatsdata$FlightDistance           1.397e-04  5.708e-05   2.448 0.014359 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9226 on 25654 degrees of freedom
# Multiple R-squared:  0.104,	Adjusted R-squared:  0.1035 
# F-statistic: 212.7 on 14 and 25654 DF,  p-value: < 2.2e-16

lm2 <- lm(formula = Cheapseatsdata$Satisfactionnum~Cheapseatsdata$Age+Cheapseatsdata$PriceSensitivity+
            Cheapseatsdata$YearofFirstFlight+Cheapseatsdata$NoofFlightspa+
            Cheapseatsdata$LoyaltyCards+Cheapseatsdata$ScheduledDepartureHour+Cheapseatsdata$ArrivalDelayinMinutes+
            Cheapseatsdata$Flighttimeinminutes+Cheapseatsdata$FlightDistance, data=Cheapseatsdata)
lm2
summary(lm2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0692 -0.6535  0.1766  0.6029  2.5865 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           -9.028e+00  3.904e+00  -2.313  0.02076 *  
#   Cheapseatsdata$Age                    -1.139e-02  3.779e-04 -30.144  < 2e-16 ***
#   Cheapseatsdata$PriceSensitivity       -1.914e-01  1.066e-02 -17.947  < 2e-16 ***
#   Cheapseatsdata$YearofFirstFlight       6.752e-03  1.945e-03   3.471  0.00052 ***
#   Cheapseatsdata$NoofFlightspa          -1.331e-02  4.195e-04 -31.733  < 2e-16 ***
#   Cheapseatsdata$LoyaltyCards           -4.098e-02  5.696e-03  -7.195 6.39e-13 ***
#   Cheapseatsdata$ScheduledDepartureHour -3.185e-03  1.331e-03  -2.394  0.01668 *  
#   Cheapseatsdata$ArrivalDelayinMinutes  -2.101e-03  1.687e-04 -12.454  < 2e-16 ***
#   Cheapseatsdata$Flighttimeinminutes    -1.202e-03  4.338e-04  -2.770  0.00560 ** 
#   Cheapseatsdata$FlightDistance          1.491e-04  5.399e-05   2.762  0.00574 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9226 on 25659 degrees of freedom
# Multiple R-squared:  0.1038,	Adjusted R-squared:  0.1035 
# F-statistic: 330.1 on 9 and 25659 DF,  p-value: < 2.2e-16

lm3 <- lm(formula = Cheapseatsdata$Satisfactionnum~Cheapseatsdata$Age+Cheapseatsdata$PriceSensitivity+
            Cheapseatsdata$YearofFirstFlight+Cheapseatsdata$NoofFlightspa+
            Cheapseatsdata$LoyaltyCards+Cheapseatsdata$ArrivalDelayinMinutes+
            Cheapseatsdata$Flighttimeinminutes+Cheapseatsdata$FlightDistance, data=Cheapseatsdata)
lm3
summary(lm3)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0629 -0.6552  0.1758  0.6029  2.6260 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          -9.108e+00  3.904e+00  -2.333 0.019671 *  
#   Cheapseatsdata$Age                   -1.139e-02  3.779e-04 -30.145  < 2e-16 ***
#   Cheapseatsdata$PriceSensitivity      -1.910e-01  1.066e-02 -17.914  < 2e-16 ***
#   Cheapseatsdata$YearofFirstFlight      6.770e-03  1.946e-03   3.479 0.000503 ***
#   Cheapseatsdata$NoofFlightspa         -1.331e-02  4.195e-04 -31.716  < 2e-16 ***
#   Cheapseatsdata$LoyaltyCards          -4.099e-02  5.696e-03  -7.196 6.38e-13 ***
#   Cheapseatsdata$ArrivalDelayinMinutes -2.187e-03  1.648e-04 -13.264  < 2e-16 ***
#   Cheapseatsdata$Flighttimeinminutes   -1.136e-03  4.330e-04  -2.624 0.008695 ** 
#   Cheapseatsdata$FlightDistance         1.442e-04  5.395e-05   2.672 0.007545 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9227 on 25660 degrees of freedom
# Multiple R-squared:  0.1036,	Adjusted R-squared:  0.1033 
# F-statistic: 370.6 on 8 and 25660 DF,  p-value: < 2.2e-16

lm4 <- lm(formula = Cheapseatsdata$Satisfactionnum~Cheapseatsdata$Age+Cheapseatsdata$PriceSensitivity+
            Cheapseatsdata$YearofFirstFlight+Cheapseatsdata$NoofFlightspa+
            Cheapseatsdata$LoyaltyCards+Cheapseatsdata$ArrivalDelayinMinutes, data=Cheapseatsdata)
lm4
summary(lm4)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0642 -0.6538  0.1787  0.6028  2.6491 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                          -9.2305351  3.9043408  -2.364 0.018078 *  
#   Cheapseatsdata$Age                   -0.0113888  0.0003780 -30.133  < 2e-16 ***
#   Cheapseatsdata$PriceSensitivity      -0.1908933  0.0106638 -17.901  < 2e-16 ***
#   Cheapseatsdata$YearofFirstFlight      0.0068248  0.0019457   3.508 0.000453 ***
#   Cheapseatsdata$NoofFlightspa         -0.0133181  0.0004195 -31.745  < 2e-16 ***
#   Cheapseatsdata$LoyaltyCards          -0.0409905  0.0056966  -7.196 6.39e-13 ***
#   Cheapseatsdata$ArrivalDelayinMinutes -0.0022126  0.0001644 -13.455  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9228 on 25662 degrees of freedom
# Multiple R-squared:  0.1033,	Adjusted R-squared:  0.1031 
# F-statistic: 492.8 on 6 and 25662 DF,  p-value: < 2.2e-16

lm5 <- lm(formula = Satisfactionnum~PriceSensitivity, data=Cheapseatsdata)
lm5
summary(lm5)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5814 -0.4075  0.4186  0.5925  2.1143 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      3.58139    0.01546  231.72   <2e-16 ***
#   Cheapseatsdata$PriceSensitivity -0.17391    0.01116  -15.58   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9698 on 25667 degrees of freedom
# Multiple R-squared:  0.009366,	Adjusted R-squared:  0.009327 
# F-statistic: 242.7 on 1 and 25667 DF,  p-value: < 2.2e-16



#change anything with 2 factors into num to use with lm
#use num datatype for lm, and do lm for all flights
#dynamic variables for age, age=yound,middleage,old.Make three columns young(0 or 1),
#reduce year of first flight to how many years since first flight
#find R mean square for individual colums and see which one affects satisfaction the most
#use buckets for svm

lineara1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
              young+middleage+old+yearsfromfirstflight,data=Cheapseatsdata)
lineara1
summary(lineara1)
str(Cheapseatsdata)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.9974 -0.6557  0.1505  0.5636  2.9547 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.5167929  0.0285086 123.359  < 2e-16 ***
#   PriceSensitivity      -0.1498349  0.0103971 -14.411  < 2e-16 ***
#   NoofFlightspa         -0.0114707  0.0004097 -28.001  < 2e-16 ***
#   LoyaltyCards          -0.0709222  0.0055014 -12.892  < 2e-16 ***
#   ArrivalDelayinMinutes -0.0022779  0.0001597 -14.267  < 2e-16 ***
#   young                  0.4112757  0.0174842  23.523  < 2e-16 ***
#   middleage              0.7262625  0.0145815  49.807  < 2e-16 ***
#   old                           NA         NA      NA       NA    
# yearsfromfirstflight  -0.0078835  0.0018893  -4.173 3.02e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8959 on 25661 degrees of freedom
# Multiple R-squared:  0.1548,	Adjusted R-squared:  0.1545 
# F-statistic: 671.3 on 7 and 25661 DF,  p-value: < 2.2e-16

lineara2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=Cheapseatsdata)
lineara2
summary(lineara2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5814 -0.4075  0.4186  0.5925  2.1143 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       3.58139    0.01546  231.72   <2e-16 ***
#   PriceSensitivity -0.17391    0.01116  -15.58   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9698 on 25667 degrees of freedom
# Multiple R-squared:  0.009366,	Adjusted R-squared:  0.009327 
# F-statistic: 242.7 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=Cheapseatsdata)
lineara3
summary(lineara3)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6818 -0.5853  0.3182  0.6237  2.4277 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    3.6818176  0.0101766  361.79   <2e-16 ***
#   NoofFlightspa -0.0160804  0.0004138  -38.86   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9469 on 25667 degrees of freedom
# Multiple R-squared:  0.05558,	Adjusted R-squared:  0.05554 
# F-statistic:  1510 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=Cheapseatsdata)
lineara4
summary(lineara4)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8067 -0.4409  0.4128  0.7054  1.7054 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.294613   0.007700  427.85   <2e-16 ***
#   LoyaltyCards 0.073154   0.005328   13.73   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9708 on 25667 degrees of freedom
# Multiple R-squared:  0.007291,	Adjusted R-squared:  0.007252 
# F-statistic: 188.5 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=Cheapseatsdata)
lineara5
summary(lineara5)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3956 -0.3956  0.6044  0.6109  2.4797 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.3955702  0.0067013  506.70   <2e-16 ***
#   ArrivalDelayinMinutes -0.0021666  0.0001731  -12.52   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9714 on 25667 degrees of freedom
# Multiple R-squared:  0.006067,	Adjusted R-squared:  0.006028 
# F-statistic: 156.7 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara6 <- lm(formula = Satisfactionnum~young,data=Cheapseatsdata)
lineara6
summary(lineara6)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3757 -0.3757  0.6243  0.6243  1.7037 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.375718   0.006795 496.806  < 2e-16 ***
#   young       -0.079443   0.015204  -5.225 1.75e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9739 on 25667 degrees of freedom
# Multiple R-squared:  0.001063,	Adjusted R-squared:  0.001024 
# F-statistic:  27.3 on 1 and 25667 DF,  p-value: 1.753e-07

lineara7 <- lm(formula = Satisfactionnum~middleage,data=Cheapseatsdata)
lineara7
summary(lineara7)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6520 -0.6520  0.3480  0.9393  1.9393 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.060711   0.008244  371.25   <2e-16 ***
#   middleage   0.591299   0.011591   51.01   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9285 on 25667 degrees of freedom
# Multiple R-squared:  0.09206,	Adjusted R-squared:  0.09202 
# F-statistic:  2602 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara8 <- lm(formula = Satisfactionnum~old,data=Cheapseatsdata)
lineara8
summary(lineara8)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5513 -0.5513  0.4487  0.4487  2.0991 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.551317   0.006897  514.93   <2e-16 ***
#   old         -0.650443   0.012712  -51.17   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9282 on 25667 degrees of freedom
# Multiple R-squared:  0.09257,	Adjusted R-squared:  0.09253 
# F-statistic:  2618 on 1 and 25667 DF,  p-value: < 2.2e-16

lineara9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=Cheapseatsdata)
lineara9
summary(lineara9)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3714 -0.3690  0.6286  0.6430  1.6503 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           3.385879   0.022988 147.289   <2e-16 ***
#   yearsfromfirstflight -0.002409   0.002052  -1.174     0.24    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9744 on 25667 degrees of freedom
# Multiple R-squared:  5.37e-05,	Adjusted R-squared:  1.474e-05 
# F-statistic: 1.378 on 1 and 25667 DF,  p-value: 0.2404

lineara10 <- lm(formula = Satisfactionnum~Age,data=Cheapseatsdata)
lineara10
summary(lineara10)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6850 -0.6726  0.2652  0.6384  2.0614 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.9338269  0.0168806  233.04   <2e-16 ***
#   Age         -0.0124399  0.0003425  -36.32   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9503 on 25667 degrees of freedom
# Multiple R-squared:  0.04888,	Adjusted R-squared:  0.04884 
# F-statistic:  1319 on 1 and 25667 DF,  p-value: < 2.2e-16

# Old age, middle age and then no of flights pa really fas strong co-relations with satisfaction for 
# Cheapseats

# Linear Modeling for Sigma Airlines Inc.(B)
#*****************************************

linearb1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
                young+middleage+old+yearsfromfirstflight,data=Sigmadata)
linearb1
summary(linearb1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.1010 -0.6467  0.1409  0.5456  2.7398 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            3.4612826  0.0344574 100.451   <2e-16 ***
#   PriceSensitivity      -0.1245699  0.0125394  -9.934   <2e-16 ***
#   NoofFlightspa         -0.0111760  0.0004965 -22.508   <2e-16 ***
#   LoyaltyCards          -0.0586489  0.0067616  -8.674   <2e-16 ***
#   ArrivalDelayinMinutes -0.0021023  0.0001693 -12.418   <2e-16 ***
#   young                  0.3880553  0.0213691  18.160   <2e-16 ***
#   middleage              0.7095957  0.0176865  40.121   <2e-16 ***
#   old                           NA         NA      NA       NA    
# yearsfromfirstflight  -0.0029857  0.0022903  -1.304    0.192    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.883 on 16793 degrees of freedom
# Multiple R-squared:  0.1546,	Adjusted R-squared:  0.1543 
# F-statistic: 438.8 on 7 and 16793 DF,  p-value: < 2.2e-16

linearb2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=Sigmadata)
linearb2
summary(linearb2)

# Multiple R-squared:  0.006644,	Adjusted R-squared:  0.006585 

linearb3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=Sigmadata)
linearb3
summary(linearb3)

#Multiple R-squared:  0.05591,	Adjusted R-squared:  0.05586

linearb4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=Sigmadata)
linearb4
summary(linearb4)

#Multiple R-squared:  0.009295,	Adjusted R-squared:  0.009236 

linearb5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=Sigmadata)
linearb5
summary(linearb5)

#Multiple R-squared:  0.007392,	Adjusted R-squared:  0.007333 

linearb6 <- lm(formula = Satisfactionnum~young,data=Sigmadata)
linearb6
summary(linearb6)

#Multiple R-squared:  0.001057,	Adjusted R-squared:  0.0009977 

linearb7 <- lm(formula = Satisfactionnum~middleage,data=Sigmadata)
linearb7
summary(linearb7)

#Multiple R-squared:  0.09477,	Adjusted R-squared:  0.09472 

linearb8 <- lm(formula = Satisfactionnum~old,data=Sigmadata)
linearb8
summary(linearb8)

#Multiple R-squared:  0.0947,	Adjusted R-squared:  0.09464

linearb9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=Sigmadata)
linearb9
summary(linearb9)

#Multiple R-squared:  1.325e-05,	Adjusted R-squared:  -4.628e-05 

linearb10 <- lm(formula = Satisfactionnum~Age,data=Sigmadata)
linearb10
summary(linearb10)

#Multiple R-squared:  0.0512,	Adjusted R-squared:  0.05114

# Linear Modeling for FlyFast Airways Inc. (C)
#*****************************************

linearc1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
                young+middleage+old+yearsfromfirstflight,data=FlyFastdata)
linearc1
summary(linearc1)

#Multiple R-squared:  0.1603,	Adjusted R-squared:  0.1599

linearc2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=FlyFastdata)
linearc2
summary(linearc2)

#Multiple R-squared:  0.007439,	Adjusted R-squared:  0.007371 

linearc3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=FlyFastdata)
linearc3
summary(linearc3)

#Multiple R-squared:  0.06104,	Adjusted R-squared:  0.06097

linearc4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=FlyFastdata)
linearc4
summary(linearc4)

#Multiple R-squared:  0.008209,	Adjusted R-squared:  0.008141

linearc5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=FlyFastdata)
linearc5
summary(linearc5)

#Multiple R-squared:  0.009062,	Adjusted R-squared:  0.008995

linearc6 <- lm(formula = Satisfactionnum~young,data=FlyFastdata)
linearc6
summary(linearc6)

#Multiple R-squared:  0.0009307,	Adjusted R-squared:  0.0008627 

linearc7 <- lm(formula = Satisfactionnum~middleage,data=FlyFastdata)
linearc7
summary(linearc7)

#Multiple R-squared:  0.09161,	Adjusted R-squared:  0.09155 

linearc8 <- lm(formula = Satisfactionnum~old,data=FlyFastdata)
linearc8
summary(linearc8)

#Multiple R-squared:  0.09355,	Adjusted R-squared:  0.09348 

linearc9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=FlyFastdata)
linearc9
summary(linearc9)

linearc10 <- lm(formula = Satisfactionnum~Age,data=FlyFastdata)
linearc10
summary(linearc10)

#Multiple R-squared:  0.05086,	Adjusted R-squared:  0.05079

# Linear Modeling for Northwest Business Airlines Inc. (D)
#*****************************************************

lineard1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
                young+middleage+old+yearsfromfirstflight,data=Northwestdata)
lineard1
summary(lineard1)

#Multiple R-squared:  0.1495,	Adjusted R-squared:  0.149

lineard2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=Northwestdata)
lineard2
summary(lineard2)

#Multiple R-squared:  0.008339,	Adjusted R-squared:  0.008266 

lineard3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=Northwestdata)
lineard3
summary(lineard3)

#Multiple R-squared:  0.04919,	Adjusted R-squared:  0.04912

lineard4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=Northwestdata)
lineard4
summary(lineard4)

#Multiple R-squared:  0.007295,	Adjusted R-squared:  0.007222 

lineard5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=Northwestdata)
lineard5
summary(lineard5)

#Multiple R-squared:  0.00754,	Adjusted R-squared:  0.007467

lineard6 <- lm(formula = Satisfactionnum~young,data=Northwestdata)
lineard6
summary(lineard6)

#Multiple R-squared:  0.001291,	Adjusted R-squared:  0.001218

lineard7 <- lm(formula = Satisfactionnum~middleage,data=Northwestdata)
lineard7
summary(lineard7)

#Multiple R-squared:  0.09244,	Adjusted R-squared:  0.09238 

lineard8 <- lm(formula = Satisfactionnum~old,data=Northwestdata)
lineard8
summary(lineard8)

#Multiple R-squared:  0.08984,	Adjusted R-squared:  0.08977

lineard9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=Northwestdata)
lineard9
summary(lineard9)

#Multiple R-squared:  1.23e-06,	Adjusted R-squared:  -7.264e-05

lineard10 <- lm(formula = Satisfactionnum~Age,data=Northwestdata)
lineard10
summary(lineard10)

#Multiple R-squared:  0.04919,	Adjusted R-squared:  0.04912

# Linear Modeling for Paul Smith Airlines Inc (E)
#********************************************

lineare1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
                young+middleage+old+yearsfromfirstflight,data=PaulSmithdata)
lineare1
summary(lineare1)

#Multiple R-squared:  0.1441,	Adjusted R-squared:  0.1436 

lineare2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=PaulSmithdata)
lineare2
summary(lineare2)

#Multiple R-squared:  0.007797,	Adjusted R-squared:  0.007715 

lineare3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=PaulSmithdata)
lineare3
summary(lineare3)

#Multiple R-squared:  0.05766,	Adjusted R-squared:  0.05758 

lineare4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=PaulSmithdata)
lineare4
summary(lineare4)

#Multiple R-squared:  0.006582,	Adjusted R-squared:  0.0065

lineare5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=PaulSmithdata)
lineare5
summary(lineare5)

#Multiple R-squared:  0.004389,	Adjusted R-squared:  0.004306

lineare6 <- lm(formula = Satisfactionnum~young,data=PaulSmithdata)
lineare6
summary(lineare6)

#Multiple R-squared:  0.002165,	Adjusted R-squared:  0.002082

lineare7 <- lm(formula = Satisfactionnum~middleage,data=PaulSmithdata)
lineare7
summary(lineare7)

#Multiple R-squared:  0.08892,	Adjusted R-squared:  0.08885

lineare8 <- lm(formula = Satisfactionnum~old,data=PaulSmithdata)
lineare8
summary(lineare8)

#Multiple R-squared:  0.0818,	Adjusted R-squared:  0.08173

lineare9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=PaulSmithdata)
lineare9
summary(lineare9)

#Multiple R-squared:  0.0001352,	Adjusted R-squared:  5.219e-05

lineare10 <- lm(formula = Satisfactionnum~Age,data=PaulSmithdata)
lineare10
summary(lineare10)

#Multiple R-squared:  0.0406,	Adjusted R-squared:  0.04052 


# Linear Modeling for Oursin Airlines Inc.(F)
#*****************************************

linearf1 <- lm(formula = Satisfactionnum~PriceSensitivity+NoofFlightspa+LoyaltyCards+ArrivalDelayinMinutes+
                young+middleage+old+yearsfromfirstflight,data=Oursindata)
linearf1
summary(linearf1)

#Multiple R-squared:  0.1499,	Adjusted R-squared:  0.1493

linearf2 <- lm(formula = Satisfactionnum~PriceSensitivity,data=Oursindata)
linearf2
summary(linearf2)

#Multiple R-squared:  0.008871,	Adjusted R-squared:  0.008779 

linearf3 <- lm(formula = Satisfactionnum~NoofFlightspa,data=Oursindata)
linearf3
summary(linearf3)

#Multiple R-squared:  0.05964,	Adjusted R-squared:  0.05956 

linearf4 <- lm(formula = Satisfactionnum~LoyaltyCards,data=Oursindata)
linearf4
summary(linearf4)

#Multiple R-squared:  0.00927,	Adjusted R-squared:  0.009179 

linearf5 <- lm(formula = Satisfactionnum~ArrivalDelayinMinutes,data=Oursindata)
linearf5
summary(linearf5)

#Multiple R-squared:  0.007503,	Adjusted R-squared:  0.007411

linearf6 <- lm(formula = Satisfactionnum~young,data=Oursindata)
linearf6
summary(linearf6)

#Multiple R-squared:  0.000864,	Adjusted R-squared:  0.0007715 

linearf7 <- lm(formula = Satisfactionnum~middleage,data=Oursindata)
linearf7
summary(linearf7)

#Multiple R-squared:  0.08472,	Adjusted R-squared:  0.08464 

linearf8 <- lm(formula = Satisfactionnum~old,data=Oursindata)
linearf8
summary(linearf8)

#Multiple R-squared:  0.08611,	Adjusted R-squared:  0.08602 

linearf9 <- lm(formula = Satisfactionnum~yearsfromfirstflight,data=Oursindata)
linearf9
summary(linearf9)

#Multiple R-squared:  0.0005349,	Adjusted R-squared:  0.0004424 

linearf10 <- lm(formula = Satisfactionnum~Age,data=Oursindata)
linearf10
summary(linearf10)

#Multiple R-squared:  0.04818,	Adjusted R-squared:  0.04809 


# #Visualization of Data
# #Mean Satisfaction Vs Mean Age - Bar plot
# meanSatisfaction <- Cheapseatsdata %>%group_by(Age) %>% summarize(m1 = mean(Satisfactionnum))
# meanAge<-as.data.frame(meanSatisfaction)
# View(meanAge)
# 
# meanAgeVsSatisfaction <- ggplot(meanAge,aes(Age,m1)) + geom_bar(stat="identity",color = "white", fill= "blue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Mean Age", y="Mean Satisfaction")
# meanAgeVsSatisfaction
# 
# #Plotting jitter for important variables
# meanAgeVsSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$Age,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="purple")
# meanAgeVsSatisfactionJitter<-meanAgeVsSatisfactionJitter+labs(x="Age",y="Customer Satisfaction")
# meanAgeVsSatisfactionJitter
# 
# #Mean Satisfaction Vs Gender - Bar Plot
# meanGender <- Cheapseatsdata %>%
#   group_by(Gender) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanGender<-as.data.frame(meanGender)
# View(meanGender)
# 
# GenderVsSatisfaction <- ggplot(meanGender,aes(Gender,m1)) + geom_bar(stat = "identity", width = 0.1, color = "black") +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1)) + labs(x="Gender", y="Mean Satisfaction")
# GenderVsSatisfaction
# 
# #Plotting jitter for important variables
# GenderVsSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$Gender,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Red")
# GenderVsSatisfactionJitter<-meanAgeVsSatisfactionJitter+labs(x="Gender",y="Customer Satisfaction")
# GenderVsSatisfactionJitter
# 
# #Mean Satisfaction Vs Mean Price Sensitivity (Added just to know the visualization)
# meanPriceSensitivity <- Cheapseatsdata %>%
#   group_by(PriceSensitivity) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanPriceSensitivity <- as.data.frame(meanPriceSensitivity)
# View(meanPriceSensitivity)
# 
# meanPriceSensitivityVsMeanSatisfaction <- ggplot(meanPriceSensitivity,aes(PriceSensitivity,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Red", fill="Blue") +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) + labs(x=" Price Sensitivity", y="Mean Satisfaction")
# meanPriceSensitivityVsMeanSatisfaction
# 
# #Mean Satisfaction Vs Mean No of Flights PA (Added just to know the visualization) #Something wrong...Pricesensitivity and NoOfFlightsPA have same plots.. Dont add in ppt
# meanNoOfFlightsPA <- Cheapseatsdata %>%
#   group_by(NoofFlightspa) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanNoOfFlightsPA <- as.data.frame(meanNoOfFlightsPA)
# View(meanNoOfFlightsPA)
# 
# meanPriceSensitivityVsMeanSatisfaction <- ggplot(,aes(PriceSensitivity,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Red", fill="Blue") +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "lightblue",colour = "lightblue", size = 0.5, linetype = "solid")) + labs(x=" Price Sensitivity", y="Mean Satisfaction")
# meanPriceSensitivityVsMeanSatisfaction
# 
# meanNoOfFlightsPAVsMeanSatisfaction <- ggplot(meanNoOfFlightsPA,aes(NoofFlightspa,m1)) + geom_bar(stat = "identity", width = 1,breaks=20, color = "Black", fill="white" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "lightgreen",colour = "lightgreen", size = 0.5, linetype = "solid")) + labs(x=" No of Flights PA", y="Mean Satisfaction")
# meanNoOfFlightsPAVsMeanSatisfaction
# 
# #Mean Satisfaction Vs Mean Type Of Travel 
# meanTypeOfTravel <- Cheapseatsdata %>%
#   group_by(TypeofTravel) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanTypeOfTravel <- as.data.frame(meanTypeOfTravel)
# View(meanTypeOfTravel)
# 
# meanTypeOfTravelVsMeanSatisfaction <- ggplot(meanTypeOfTravel,aes(TypeofTravel,m1)) + geom_bar(stat = "identity", width = .2, color = "Blue", fill="Yellow") +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "magenta",colour = "magenta", size = 0.5, linetype = "solid")) + labs(x=" Type Of Travel", y="Mean Satisfaction")
# meanTypeOfTravelVsMeanSatisfaction
# 
# #Plotting jitter for important variables
# meanTypeOfTravelVsMeanSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$TypeofTravel,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Black", alpha=0.05)
# meanTypeOfTravelVsMeanSatisfactionJitter<-meanTypeOfTravelVsMeanSatisfactionJitter+labs(x="Type Of Travel",y="Customer Satisfaction")
# meanTypeOfTravelVsMeanSatisfactionJitter
# 
# #Mean Satisfaction Vs Mean Class (Added just to know the visualization)
# meanClass <- Cheapseatsdata %>%
#   group_by(Class) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanClass <- as.data.frame(meanClass)
# View(meanClass)
# 
# meanClassVsMeanSatisfaction <- ggplot(meanClass,aes(Class,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x=" Class", y="Mean Satisfaction")
# meanClassVsMeanSatisfaction
# 
# #Plotting jitter for important variables...Doesnt work..Dont know why!
# meanClassVsMeanSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$Class,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Blue", alpha=0.05)
# meanClassVsMeanSatisfactionJitter<-meanClassVsMeanSatisfactionJitter+labs(x="Class",y="Customer Satisfaction")
# meanClassVsMeanSatisfactionJitter
# 
# #Mean Satisfaction Vs Mean Arrival Delay
# meanArrivalDelay <- Cheapseatsdata %>%
#   group_by(ArrivalDelay) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanArrivalDelay <- as.data.frame(meanArrivalDelay)
# View(meanArrivalDelay)
# 
# meanArrivalDelayVsMeanSatisfaction <- ggplot(meanArrivalDelay,aes(ArrivalDelay,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x="Arrival Delay", y="Mean Satisfaction")
# meanArrivalDelayVsMeanSatisfaction
# 
# #Plotting jitter for important variables
# meanArrivalDelayVsMeanSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$ArrivalDelay,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Green", alpha=0.05)
# meanArrivalDelayVsMeanSatisfactionJitter<-meanArrivalDelayVsMeanSatisfactionJitter+labs(x="Arrival Delay",y="Customer Satisfaction")
# meanArrivalDelayVsMeanSatisfactionJitter
# 
# #Mean Satisfaction Vs Mean Loyalty cards
# meanLoyaltyCards <- Cheapseatsdata %>%
#   group_by(LoyaltyCards) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanLoyaltyCards <- as.data.frame(meanLoyaltyCards)
# View(meanLoyaltyCards)
# 
# meanLoyaltyCardsVsMeanSatisfaction <- ggplot(meanLoyaltyCards,aes(LoyaltyCards,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x="Loyalty Cards", y="Mean Satisfaction")
# meanLoyaltyCardsVsMeanSatisfaction
# 
# #Plotting jitter for important variables
# meanLoyaltyCardsVsMeanSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$LoyaltyCards,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Green", alpha=0.05)
# meanLoyaltyCardsVsMeanSatisfactionJitter<-meanLoyaltyCardsVsMeanSatisfactionJitter+labs(x="Loyalty Cards",y="Customer Satisfaction")
# meanLoyaltyCardsVsMeanSatisfactionJitter
# 
# #Mean Satisfaction Vs Mean ArrivalDelay
# meanLoyaltyCards <- Cheapseatsdata %>%
#   group_by(LoyaltyCards) %>%
#   summarize(m1 = mean(Satisfactionnum))
# 
# meanLoyaltyCards <- as.data.frame(meanLoyaltyCards)
# View(meanLoyaltyCards)
# 
# meanLoyaltyCardsVsMeanSatisfaction <- ggplot(meanLoyaltyCards,aes(LoyaltyCards,m1)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x="Loyalty Cards", y="Mean Satisfaction")
# meanLoyaltyCardsVsMeanSatisfaction
# 
# #Plotting jitter for important variables
# meanLoyaltyCardsVsMeanSatisfactionJitter<-ggplot(Cheapseatsdata,aes(x=Cheapseatsdata$LoyaltyCards,y=Cheapseatsdata$Satisfaction))+geom_jitter(color="Green", alpha=0.05)
# meanLoyaltyCardsVsMeanSatisfactionJitter<-meanLoyaltyCardsVsMeanSatisfactionJitter+labs(x="Loyalty Cards",y="Customer Satisfaction")
# meanLoyaltyCardsVsMeanSatisfactionJitter



install.packages("RColorBrewer")
library(RColorBrewer)

#satisfaction vs age
Satisfactionage <- ggplot(data,aes(x=Age)) + geom_histogram(aes(fill=Satisfaction),position = "dodge",binwidth = 7) + 
  scale_fill_brewer(palette="Dark2")
Satisfactionage

#satisfaction vs Gender
Satisfactiongen <- ggplot(data,aes(x=Gender)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth = 7,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactiongen

#satisfaction vs Airline Status
Satisfactionas <- ggplot(data,aes(x=AirlineStatus)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth = 7,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactiongas

#satisfaction vs class
Satisfactioncl <- ggplot(data,aes(x=Class)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth = 7,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactioncl

# Satisfactioncl <- ggplot(data,aes(x=Class)) + geom_boxplot(aes(fill=Satisfaction, y=Satisfaction),alpha=.5) + 
#   scale_fill_brewer(palette="Dark2")
# Satisfactioncl

#satisfaction vs Loyalty cards
Satisfactionlc <- ggplot(Cheapseatsdata,aes(x=LoyaltyCards)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth = 7,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactionlc

#satisfaction vs Arrival Delay in Minutes
Satisfactionad <- ggplot(data,aes(x=ArrivalDelayinMinutes)) + geom_histogram(aes(fill=Satisfaction),binwidth =30,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactionad


meanNoOfFlightsPAVsMeanSatisfaction <- ggplot(meanNoOfFlightsPA,aes(NoofFlightspa,m1)) + geom_bar(stat = "identity", width = 1, color = "Black", fill="white" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "lightgreen",colour = "lightgreen", size = 0.5, linetype = "solid")) + labs(x=" No of Flights PA", y="Mean Satisfaction")
meanNoOfFlightsPAVsMeanSatisfaction

#satisfaction vs Noofflightspa
Satisfactionpa <- ggplot(data,aes(x=NoofFlightspa)) + geom_histogram(aes(fill=Satisfaction),binwidth = 10,alpha=.5, position="stack") + 
  scale_fill_brewer(palette="Dark2")
Satisfactionpa

# #satisfaction vs price sensitivity
# Satisfactionps <- ggplot(data,aes(x=PriceSensitivity)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth =7,alpha=.5, position="stack") + 
#   scale_fill_brewer(palette="Dark2")
# Satisfactionps

# #satisfaction vs price sensitivity
# Satisfactionps <- ggplot(data,aes(x=PriceSensitivity)) + geom_histogram(aes(fill=Satisfaction),position = "stack",binwidth = 7) + 
#   scale_fill_brewer(palette="Dark2")
# Satisfactionps

#satisfaction vs Loyalty cards
Satisfactionlc <- ggplot(Cheapseatsdata,aes(x=LoyaltyCards)) + geom_histogram(aes(fill=Satisfaction),stat="count",binwidth = 7,alpha=.5, position="dodge") + 
  scale_fill_brewer(palette="Dark2")
Satisfactionlc

Loyal0sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==0),]
mean(Loyal0sat$Satisfactionnum)
#3.244759
Loyal1sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==1),]
mean(Loyal1sat$Satisfactionnum)
#3.468102
Loyal2sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==2),]
mean(Loyal2sat$Satisfactionnum)
#3.564748
Loyal3sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==3),]
mean(Loyal3sat$Satisfactionnum)
#3.411168
Loyal4sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==4),]
mean(Loyal4sat$Satisfactionnum)
#3.308851
Loyal5sat<-Cheapseatsdata[which(Cheapseatsdata$LoyaltyCards==5),]
mean(Loyal5sat$Satisfactionnum)
#3.205128



#satisfaction vs Noofflightspa
Satisfactionpa <- ggplot(Cheapseatsdata,aes(x=NoofFlightspa)) + geom_histogram(aes(fill=Satisfaction),binwidth = 5,alpha=.5,
                  position="stack") + scale_fill_brewer(palette="Dark2")
Satisfactionpa

pa5sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=5),]
a1<-mean(pa5sat$Satisfactionnum)

pa10sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=10 & Cheapseatsdata$NoofFlightspa>5), ]
a2<-mean(pa10sat$Satisfactionnum)

pa15sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=15 & Cheapseatsdata$NoofFlightspa>10), ]
a3<-mean(pa15sat$Satisfactionnum)

pa20sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=20 & Cheapseatsdata$NoofFlightspa>15), ]
a4<-mean(pa20sat$Satisfactionnum)

pa25sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=25 & Cheapseatsdata$NoofFlightspa>20), ]
a5<-mean(pa25sat$Satisfactionnum)

pa30sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=30 & Cheapseatsdata$NoofFlightspa>25), ]
a6<-mean(pa30sat$Satisfactionnum)

pa35sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=35 & Cheapseatsdata$NoofFlightspa>30), ]
mean(pa35sat$Satisfactionnum)
a7<-
pa40sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=40 & Cheapseatsdata$NoofFlightspa>35), ]
a8<-mean(pa40sat$Satisfactionnum)

pa45sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=45 & Cheapseatsdata$NoofFlightspa>40),]
a9<-mean(pa45sat$Satisfactionnum)

pa50sat<-Cheapseatsdata[which(Cheapseatsdata$NoofFlightspa<=50 & Cheapseatsdata$NoofFlightspa>45),]
a10<-mean(pa50sat$Satisfactionnum)


Noofairpa<-c("Less than 5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","45-50")

meanSat1<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

df2<-data.frame(Noofairpa,meanSat1)
View(df2)

NooffVsMeanSatisfaction <- ggplot(df2,aes(Noofairpa,meanSat1)) + 
                            geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) + theme_bw() +  
                            theme(axis.text = element_text(size = 12,angle = 0, hjust = 1),
                            panel.background = element_rect(fill = "grey",colour = "grey", size = 1, linetype = "solid")) +
                            labs(x="No. of flights in the past", y="Mean Satisfaction")
NooffVsMeanSatisfaction

meanArrivalDelayVsMeanSatisfaction <- ggplot(ArrivalDelayinMinutes,aes(ArrivalDelayinMinutes,Satisfactionnum)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x="Arrival Delay", y="Mean Satisfaction")
meanArrivalDelayVsMeanSatisfaction

#satisfaction vs age
Satisfactionage <- ggplot(data,aes(x=Age)) + geom_histogram(aes(fill=Satisfaction),position = "dodge",binwidth = 7) + 
  scale_fill_brewer(palette="Dark2")
Satisfactionage

age30sat<-Cheapseatsdata[which(Cheapseatsdata$Age<=30),]
mean(age30sat$Satisfactionnum)
#3.296275

age55sat<-Cheapseatsdata[which(Cheapseatsdata$Age<=55 & Cheapseatsdata$Age>30),]
mean(age55sat$Satisfactionnum)
#3.65201

ageoldsat<-Cheapseatsdata[which(Cheapseatsdata$Age>55),]
mean(ageoldsat$Satisfactionnum)
#2.900873

AgeLimit<- c("< 30-young","31 to 55","Above 55")
meanSat <- c(3.296275,3.65201,2.900873)
agesati<- data.frame(AgeLimit,meanSat)
View(agesati)

meanArrivalDelayVsMeanSatisfaction <- ggplot(agesati,aes(AgeLimit,meanSat)) + geom_bar(stat = "identity", width = 0.2, color = "Black", fill="Pink" ) +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 0, hjust = 1), panel.background = element_rect(fill = "Orange",colour = "Orange", size = 0.5, linetype = "solid")) + labs(x="Arrival Delay in Minutes", y="Mean Satisfaction")
meanArrivalDelayVsMeanSatisfaction
#satisfaction vs ArrivalDelayinMinutes
Satisfactionad <- ggplot(data,aes(x=ArrivalDelayinMinutes)) + geom_histogram(aes(fill=Satisfaction),position = "stack",binwidth = 7) + xlim(0,200) + ylim(0,15000) 
  scale_fill_brewer(palette="Dark2")
Satisfactionad

ad30sat<- Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=30),]
mean(ad30sat$Satisfactionnum)
#3.392108 
ad60sat<-Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=60 & Cheapseatsdata$ArrivalDelayinMinutes>30),]
mean(ad60sat$Satisfactionnum)
#3.179932
ad90sat<-Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=90 & Cheapseatsdata$ArrivalDelayinMinutes>60),]
mean(ad90sat$Satisfactionnum)
#3.192084
ad120sat<-Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=120 & Cheapseatsdata$ArrivalDelayinMinutes>90),]
mean(ad120sat$Satisfactionnum)
#3.237197
ad150sat<-Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=150 & Cheapseatsdata$ArrivalDelayinMinutes>120),]
mean(ad150sat$Satisfactionnum)
#3.237209
ad180sat<-Cheapseatsdata[which(Cheapseatsdata$ArrivalDelayinMinutes<=180 & Cheapseatsdata$ArrivalDelayinMinutes>150),]
mean(ad180sat$Satisfactionnum)
#3.192308

time<-c("<30 mins","30-60 mins","60-90mins","90-120mins","120-150mins","150-180mins")
timesat<- c(3.392108,3.179932,3.192084,3.237197,3.237209,3.192308)
timedf<-data.frame(time,timesat)
View(timedf)

GenderVsSatisfaction <- ggplot(timedf,aes(time,timesat)) + geom_bar(stat = "identity", width = 0.1, color = "black") +  theme_bw() +  theme(axis.text = element_text(size = 12,angle = 90, hjust = 1)) + labs(x="Arrival Delay in Minutes", y="Mean Satisfaction")
GenderVsSatisfaction

#satisfaction$yearsfromfirstflight
Satisfactionff <- ggplot(data,aes(x=)) + geom_histogram(aes(fill=Satisfaction),position = "stack",binwidth = 7) + xlim(0,200) + ylim(0,15000) 
scale_fill_brewer(palette="Dark2")
Satisfactionff


# A rules


install.packages("arules")
library(arules)
install.packages("arulesViz",dependencies = TRUE)
library(arulesViz)
install.packages("RJSONIO")
library(RJSONIO)


#rules for Cheapseats Airline

summary(Cheapseatsdata)
View(Cheapseatsdata)

#For Satisfaction
createBucketSurvey <- function(vec){
  vBuckets<- replicate(length(vec), "Average")
  vBuckets[vec>3.5] <- "High"
  vBuckets[vec<3.5] <- "Low"
  return(vBuckets)
}
#For Age
createBucketAge <- function(vec){
  vBuckets <- replicate(length(vec),"Middle")
  vBuckets[vec>55] <- "High"
  vBuckets[vec<30] <- "Low"
  return(vBuckets)
}
#For Larger Than 5
createBuckets <- function(vec) {
  q <- quantile(vec, c(0.4,0.6))
  vBuckets <- replicate(length(vec),"Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
Sat1 <- createBucketSurvey(Cheapseatsdata$Satisfactionnum)
CustAge1 <- createBucketAge(Cheapseatsdata$Age)
NoofFlight1 <- createBuckets(Cheapseatsdata$NoofFlightspa)
LoyaltyCards1 <- createBuckets(Cheapseatsdata$LoyaltyCards)
ArrivalDelayMins1 <- createBuckets(Cheapseatsdata$ArrivalDelayinMinutes)

ruleDF1 <- data.frame(Sat1,CustAge1,NoofFlight1,LoyaltyCards1,ArrivalDelayMins1,Cheapseatsdata$Gender)
str(Cheapseatsdata)
View(ruleDF1)
summary(ruleDF1)
CheapseatsdataX <- as(ruleDF1,"transactions")
View(CheapseatsdataX)
inspect(CheapseatsdataX)
itemFrequency(CheapseatsdataX)
itemFrequencyPlot(CheapseatsdataX)
#shows frequencyplot

ruleset1 <- apriori(CheapseatsdataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat1=High")))
inspect(ruleset1)
summary(ruleset1)

#{CustAge1=Middle,                                                              
#Cheapseatsdata.Gender=Male} => {Sat1=High} 0.1746465  0.7310828 1.454516  4483
#{CustAge1=Middle,                                                              
#ArrivalDelayMins1=Low}      => {Sat1=High} 0.1730492  0.7053033 1.403227  4442

ruleset1L <- apriori(CheapseatsdataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat1=Low")))

inspect(ruleset1L)
summary(ruleset1L)
#{CustAge1=High,                                                                 
#NoofFlight1=High}             => {Sat1=Low} 0.1360396  0.8100209 1.628607  3492
#{CustAge1=High,                                                                 
# NoofFlight1=High,                                                              
#LoyaltyCards1=Low}            => {Sat1=Low} 0.1273910  0.8152580 1.639137  3270



#rule for Sigma Airlines

summary(Sigmadata)
View(Sigmadata)

Sat2 <- createBucketSurvey(Sigmadata$Satisfactionnum)
PriceSen2 <- createBuckets(Sigmadata$PriceSensitivity)
CustAge2 <- createBucketAge(Sigmadata$Age)
ArrivalDelayMin2 <-  createBuckets(Sigmadata$ArrivalDelayinMinutes)
LoyaltyCards2 <- createBuckets(Sigmadata$LoyaltyCards)
NoofFlight2 <- createBuckets(Sigmadata$NoofFlightspa)

t2 <- table(CustAge2)
t2

ruleDF2 <- data.frame(Sat2,PriceSen2,CustAge2,ArrivalDelayMin2,LoyaltyCards2,NoofFlight2)

str(Sigmadata)
View(ruleDF2)
summary(ruleDF2)
SigmadataX <- as(ruleDF2,"transactions")
View(SigmadataX)

inspect(SigmadataX)
itemFrequency(SigmadataX)
itemFrequencyPlot(SigmadataX)
#shows frequencyplot

ruleset2 <- apriori(SigmadataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat2=High")))

inspect(ruleset2)
summary(ruleset2)
#{CustAge2=Middle,                                                        
#ArrivalDelayMin2=Low,                                                   
#NoofFlight2=Low}      => {Sat2=High} 0.1181477  0.7499056 1.443699  1985
#[4] {PriceSen2=Low,                                                          
#CustAge2=Middle,                                                        
#NoofFlight2=Low}      => {Sat2=High} 0.1529671  0.7447117 1.433700  2570
#######Middle age people, with low number of flight before, with low arrival delay mins, with low price sensitivity = high satisfaction.#######

ruleset2L <- apriori(SigmadataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat2=Low")))

inspect(ruleset2L)
summary(ruleset2L)
#{CustAge2=Old,NoofFlight2=High}                   => {Sat2=Low} --1.640057
#{CustAge2=Old,LoyaltyCards2=Low,NoofFlight2=High} => {Sat2=Low} --1.6456
######Focus on Old People, with Low Loyalty Cards, and have high number of Flight to improve the satisfaction.




#Rule for FlyFast
summary(FlyFastdata)
View(FlyFastdata)

Sat3 <- createBucketSurvey(FlyFastdata$Satisfactionnum)
PriceSen3 <- createBuckets(FlyFastdata$PriceSensitivity)
CustAge3 <- createBucketAge(FlyFastdata$Age)
YearsFlight3 <- createBuckets(FlyFastdata$yearsfromfirstflight)
NoofFlight3 <- createBuckets(FlyFastdata$NoofFlightspa)
LoyaltyCards3 <- createBuckets(FlyFastdata$LoyaltyCards)
ArrivalDelayMins3 <- createBuckets(FlyFastdata$ArrivalDelayinMinutes)

ruleDF3 <- data.frame(Sat3,PriceSen3,CustAge3,YearsFlight3,NoofFlight3,LoyaltyCards3,ArrivalDelayMins3)
str(FlyFastdata)
View(ruleDF3)
summary(ruleDF3)
FlyFastdataX <- as(ruleDF3,"transactions")
View(FlyFastdataX)

inspect(FlyFastdataX)
itemFrequency(FlyFastdataX)
itemFrequencyPlot(FlyFastdataX)
#shows frequencyplot

ruleset3 <- apriori(FlyFastdataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat3=High")))

inspect(ruleset3)
summary(ruleset3)
#{CustAge3=Middle,                                                         
#NoofFlight3=Low,                                                         
#ArrivalDelayMins3=Low} => {Sat3=High} 0.1028241  0.7562563 1.492103  1511
#{PriceSen3=Low,                                                           
#CustAge3=Middle,                                                         
#ArrivalDelayMins3=Low} => {Sat3=High} 0.1486220  0.7356012 1.451351  2184
#####Middle age person, have low number of flight before, with low delay mins,PriceSen is low have high satisfaction.

ruleset3L <- apriori(FlyFastdataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat3=Low")))

inspect(ruleset3L)
summary(ruleset3L)

#{CustAge3=Old,NoofFlight3=High}                   => {Sat3=Low} -- 1.642174
#{CustAge3=Old,NoofFlight3=High,LoyaltyCards3=Low} => {Sat3=Low} -- 1.647203
######Old people, with high number of flight before, have low loyalty cards tend to have low Satisfaction.

#Rule for Northwest Airline
summary(Northwestdata)
View(Northwestdata)

Sat4 <- createBucketSurvey(Northwestdata$Satisfactionnum)
PriceSen4 <- createBuckets(Northwestdata$PriceSensitivity)
CustAge4 <- createBucketAge(Northwestdata$Age)
YearsFlight4 <- createBuckets(Northwestdata$yearsfromfirstflight)
NoofFlight4 <- createBuckets(Northwestdata$NoofFlightspa)
LoyaltyCards4 <- createBuckets(Northwestdata$LoyaltyCards)
ArrivalDelayMins4 <- createBuckets(Northwestdata$ArrivalDelayinMinutes)

ruleDF4 <- data.frame(Sat4,PriceSen4,CustAge4,YearsFlight4,NoofFlight4,LoyaltyCards4,ArrivalDelayMins4)
str(Northwestdata)
View(ruleDF4)
summary(ruleDF4)
NorthwestdataX <- as(ruleDF4,"transactions")
View(NorthwestdataX)

inspect(NorthwestdataX)
itemFrequency(NorthwestdataX)
itemFrequencyPlot(NorthwestdataX)
#shows frequencyplot

ruleset4 <- apriori(NorthwestdataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat4=High")))

inspect(ruleset4)
summary(ruleset4)
# {CustAge4=Middle,                                                         
#NoofFlight4=Low,                                                         
#ArrivalDelayMins4=Low} => {Sat4=High} 0.1100524  0.7438842 1.436316  1490
#{PriceSen4=Low,                                                           
#  CustAge4=Middle,                                                         
#  ArrivalDelayMins4=Low} => {Sat4=High} 0.1596868  0.7221109 1.394275  2162 
#####Middle age person, low number of flight, Arrival Delay mins low, have high satisfaction.



ruleset4L <- apriori(NorthwestdataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat4=Low")))

inspect(ruleset4L)
summary(ruleset4L)
#{CustAge4=Old,NoofFlight4=High}                   => {Sat4=Low} --1.647814
#{CustAge4=Old,NoofFlight4=High,LoyaltyCards4=Low} => {Sat4=Low} --1.647299
#######Old people, with high number of flight before and have low number of loyalty cards, satisfaction is low.


#Rule for Paul Smith

summary(PaulSmithdata)
View(PaulSmithdata)

Sat5 <- createBucketSurvey(PaulSmithdata$Satisfactionnum)
PriceSen5 <- createBuckets(PaulSmithdata$PriceSensitivity)
CustAge5 <- createBucketAge(PaulSmithdata$Age)
YearsFlight5 <- createBuckets(PaulSmithdata$yearsfromfirstflight)
NoofFlight5 <- createBuckets(PaulSmithdata$NoofFlightspa)
LoyaltyCards5 <- createBuckets(PaulSmithdata$LoyaltyCards)
ArrivalDelayMins5 <- createBuckets(PaulSmithdata$ArrivalDelayinMinutes)

ruleDF5 <- data.frame(Sat5,PriceSen5,CustAge5,YearsFlight5,NoofFlight5,LoyaltyCards5,ArrivalDelayMins5)
str(PaulSmithdata)
View(ruleDF5)
summary(ruleDF5)
PaulSmithdataX <- as(ruleDF5,"transactions")
View(PaulSmithdataX)

inspect(PaulSmithdataX)
itemFrequency(PaulSmithdataX)
itemFrequencyPlot(PaulSmithdataX)
#shows frequencyplot

ruleset5 <- apriori(PaulSmithdataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat5=High")))

inspect(ruleset5)
summary(ruleset5)

#{PriceSen5=Low,                                                           
#CustAge5=Middle,                                                         
#NoofFlight5=Low,                                                         
#ArrivalDelayMins5=Low} => {Sat5=High} 0.1018173  0.7780596 1.490683  1227
#{CustAge5=Middle,                                                         
#NoofFlight5=Low,                                                         
#ArrivalDelayMins5=Low} => {Sat5=High} 0.1252178  0.7667683 1.469050  1509



ruleset5L <- apriori(PaulSmithdataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat5=Low")))

inspect(ruleset5L)
summary(ruleset5L)
#{CustAge5=Old,NoofFlight5=High}                   => {Sat5=Low} --1.670996
#{CustAge5=Old,NoofFlight5=High,LoyaltyCards5=Low} => {Sat5=Low} --1.673459


#Rule for Oursin Airline
summary(Oursindata)
View(Oursindata)

Sat6 <- createBucketSurvey(Oursindata$Satisfactionnum)
PriceSen6 <- createBuckets(Oursindata$PriceSensitivity)
CustAge6 <- createBucketAge(Oursindata$Age)
YearsFlight6 <- createBuckets(Oursindata$yearsfromfirstflight)
NoofFlight6 <- createBuckets(Oursindata$NoofFlightspa)
LoyaltyCards6 <- createBuckets(Oursindata$LoyaltyCards)
ArrivalDelayMins6 <- createBuckets(Oursindata$ArrivalDelayinMinutes)

ruleDF6 <- data.frame(Sat6,PriceSen6,CustAge6,YearsFlight6,NoofFlight6,LoyaltyCards6,ArrivalDelayMins6)
str(Oursindata)
View(ruleDF6)
summary(ruleDF6)
OursindataX <- as(ruleDF6,"transactions")
View(OursindataX)

inspect(OursindataX)
itemFrequency(OursindataX)
itemFrequencyPlot(OursindataX)
#shows frequencyplot

ruleset6 <- apriori(OursindataX,
                    parameter = list(support=0.1,confidence=0.6),
                    appearance = list(default="lhs",rhs=("Sat6=High")))

inspect(ruleset6)
summary(ruleset6)
#{CustAge6=Middle,                                                         
#NoofFlight6=Low,                                                         
#ArrivalDelayMins6=Low} => {Sat6=High} 0.1122014  0.7412844 1.433982  1212
#[3] {PriceSen6=Low,                                                           
#CustAge6=Middle,                                                         
#NoofFlight6=Low}       => {Sat6=High} 0.1546010  0.7305337 1.413185  1670

ruleset6L <- apriori(OursindataX,
                     parameter = list(support=0.1,confidence=0.6),
                     appearance = list(default="lhs",rhs=("Sat6=Low")))

inspect(ruleset6L)
summary(ruleset6L)
#{CustAge6=Old,NoofFlight6=High}                   => {Sat6=Low} --1.661154
#{CustAge6=Old,NoofFlight6=High,LoyaltyCards6=Low} => {Sat6=Low} --1.672928


# SVM

library(kernlab)
#Create a function to add a column in the dataset to indicate satisfaction is high or low.
getSatLevel<- function(data){
  highSat<- replicate(length(data$Satisfactionnum), "Low")
  highSat[data$Satisfactionnum > 3.5] <- "High" 
  data$highSatisfaction<- highSat
}

Cheapseatsdata$highSat<- getSatLevel(Cheapseatsdata)
Sigmadata$highSat<- getSatLevel(Sigmadata)
FlyFastdata$highSat<- getSatLevel(FlyFastdata)
Northwestdata$highSat<- getSatLevel(Northwestdata)
PaulSmithdata$highSat<- getSatLevel(PaulSmithdata)
Oursindata$highSat<-getSatLevel(Oursindata)

View(Oursindata)

# Create a function to split dataset into train data and test data.
getTrainTestData<- function(data){
  RandIndex <- sample(1:dim(data)[1])
  CutPoint2_3 <- floor(2 * dim(data)[1]/3)
  TrainData <- data[RandIndex[1:CutPoint2_3],]   #Take the first 2/3 as training data
  TestData <- data[RandIndex[(CutPoint2_3+1):dim(data)[1]],] 
  # Take the last 1/3 data as test data.
  return(list(TrainData, TestData))
}
#Create svm, choose those most significant variables we find in linear model as varibles of SVM
getSVM<- function(TrainData){
  svmOutput <- ksvm(highSat ~ Age + PriceSensitivity + yearsfromfirstflight + NoofFlightspa + LoyaltyCards + ArrivalDelayinMinutes, data=TrainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
  return(svmOutput)
}

# Create a funtion to predict outcomes by using test data.
getSvmPred <- function(svmOutput, TestData)
{
  svmPred <- predict(svmOutput, TestData, type = "votes")   # Use model to predict test data.
  SatPred<- svmPred[1,]
  return(SatPred)
}

# Create matrix of real data and predict data.
getMatrix <- function(testHighSat,svmPred)
{
  comptable <- data.frame(testHighSat,svmPred)
  tab<-table(comptable)
  return(tab)
}

# Compute error rate.
errorRate <- function(tab)
{
  errorRate <- ((tab[1,1]+tab[2,2])/(tab[1,1]+tab[1,2]+tab[2,1]+tab[2,2]))*100
  return(paste(round(errorRate,2), "%"))
}

### Predict Satisfaction of each airline company.

## Predict Cheapseats. 
#Get CheapSeats train data and test data.
CheapseatsTrain<- getTrainTestData(Cheapseatsdata)[[1]]
CheapseatsTest<- getTrainTestData(Cheapseatsdata)[[2]]
dim(Cheapseatsdata)
dim(CheapseatsTrain)

#Create SVM for Cheapseats Company. 
# Choose those most significant variables we find in linear model as varibles of SVM.
CheapseatsSVM<- ksvm(highSat ~ Age + PriceSensitivity + YearofFirstFlight + NoofFlightspa + LoyaltyCards + ArrivalDelayinMinutes, data= CheapseatsTrain, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE )
CheapseatsSVM
CheapseatsSatPred<- getSvmPred(CheapseatsSVM, CheapseatsTest)
CheapseatsTab<- getMatrix(CheapseatsTest$highSat, CheapseatsSatPred)
CheapseatsTab
CheapseatsErrorRate<- errorRate(CheapseatsTab)
CheapseatsErrorRate
# 29.6 %

##Predict Sigma (Because next five companies have same significant variables, so use getSVM() function created before to create their SVMs.
#Get Sigma train data and test data.
SigmaTrain<- getTrainTestData(Sigmadata)[[1]]
SigmaTest<- getTrainTestData(Sigmadata)[[2]]
#Create SVM for Cheapseats Company. 
#SigmaSVM<- ksvm(highSat ~ Age + PriceSensitivity + yearsfromfirstflight + NoofFlightspa + LoyaltyCards + ArrivalDelayinMinutes, data= SigmaTrain, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE )
SigmaSVM<- getSVM(SigmaTrain)
SigmaSatPred<- getSvmPred(SigmaSVM, SigmaTest)
SigmaTab<- getMatrix(SigmaTest$highSat, SigmaSatPred)
SigmaErrorRate<- errorRate(SigmaTab)
SigmaErrorRate
# "28.99 %"

# Predict FlyFast.
FlyFastTrain<- getTrainTestData(FlyFastdata)[[1]]
FlyFastTest<- getTrainTestData(FlyFastdata)[[2]]
#FlyFastSVM<- ksvm(highSat ~ Age + PriceSensitivity + yearsfromfirstflight + NoofFlightspa + LoyaltyCards + ArrivalDelayinMinutes, data= FlyFastTrain, kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE )
FlyFastSVM<- getSVM(FlyFastTrain)
FlyFastSatPred<- getSvmPred(FlyFastSVM, FlyFastTest)
FlyFastTab<- getMatrix(FlyFastTest$highSat, FlyFastSatPred)
FlyFastErrorRate<- errorRate(FlyFastTab)
FlyFastErrorRate
#29.7 %

# Predict Northwest.
NorthwestTrain<- getTrainTestData(Northwestdata)[[1]]
NorthwestTest<- getTrainTestData(Northwestdata)[[2]]
NorthwestSVM<- getSVM(NorthwestTrain)
NorthwestSatPred<- getSvmPred(NorthwestSVM, NorthwestTest)
NorthwestTab<- getMatrix(NorthwestTest$highSat, NorthwestSatPred)
NorthwestErrorRate<- errorRate(NorthwestTab)
NorthwestErrorRate
# 28.72 %

# Predict Paul Smith.
PaulSmithTrain<- getTrainTestData(PaulSmithdata)[[1]]
PaulSmithTest<- getTrainTestData(PaulSmithdata)[[2]]
PaulSmithSVM<- getSVM(PaulSmithTrain)
PaulSmithSatPred<- getSvmPred(PaulSmithSVM, PaulSmithTest)
PaulSmithTab<- getMatrix(PaulSmithTest$highSat, PaulSmithSatPred)
PaulSmithErrorRate<- errorRate(PaulSmithTab)
PaulSmithErrorRate
# 28.6 %

# Predict Oursin.
OursinTrain<- getTrainTestData(Oursindata)[[1]]
OursinTest<- getTrainTestData(Oursindata)[[2]]
OursinSVM<- getSVM(OursinTrain)
OursinSatPred<- getSvmPred(OursinSVM, OursinTest)
OursinTab<- getMatrix(OursinTest$highSat, OursinSatPred)
OursinErrorRate<- errorRate(OursinTab)
OursinErrorRate
# 26.66 %