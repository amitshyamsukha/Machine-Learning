#################################################
## Subject : Model using Multi linear regression to predict car price 
## Author : Amit Shyamsukha
## Version : 1.0
## Date : 23/07/2017
##History :   
################################################
## setwd("C:/Users/HP-PC/Downloads")
install.packages("car")
install.packages("MASS")

library("MASS")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library("car")

# Load the file 

cars<- read.csv("CarPrice_Assignment.csv" , header = T )

str(cars) 

##----------------------------------------------------------------------------
## Missing value checking 

missing_values  <- cars %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

sum(is.na(cars)) ## It apperas there is no missing values in the data set 

##----------------------------------------------------------------------

which(duplicated(cars)) ## There is no duplicated row in data set 

summary(cars$fueltype)       ## Two distinct type 
summary(cars$aspiration)     ## Two distinct type 
summary(cars$doornumber)     ## Two distinct type 
summary(cars$carbody)        ## five  distinct type 
summary(cars$drivewheel)     ## three distinct type
summary(cars$enginelocation) ## Two distinct values 
summary(cars$enginetype)     ## Sevewn distinct type 
summary(cars$cylindernumber) ## Sevewn distinct type 
summary(cars$fuelsystem)     ## eight distinct types 

## Plot the existing data to get idea about dataset 


## Segmented univariate analysis
boxplot(cars$price)
ggplot(cars , aes ( x= price )) + geom_histogram()
boxplot(cars$price~cars$fueltype)
boxplot(cars$price~cars$aspiration)
boxplot(cars$price~cars$doornumber)
boxplot(cars$price~cars$carbody)
boxplot(cars$price~cars$drivewheel)
boxplot(cars$price~cars$enginelocation)
boxplot(cars$price~cars$enginetype)
boxplot(cars$price~cars$cylindernumber)
boxplot(cars$price~cars$fuelsystem)

## Bi-variate analysis 

ggplot(cars, aes(x=cars$symboling , y = price)) + geom_point()
ggplot(cars, aes(x=wheelbase , y = price)) + geom_point()+geom_smooth()   
ggplot(cars, aes(x=carlength , y = price)) + geom_point() + geom_line()+geom_smooth()
ggplot(cars, aes(x=carwidth , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=carheight , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=curbweight , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=enginesize , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=boreratio , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=stroke , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=compressionratio , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=horsepower , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=peakrpm , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=citympg , y = price)) + geom_point() + geom_line()
ggplot(cars, aes(x=highwaympg , y = price)) + geom_point() + geom_line()

## Find out the outlier 
bstat <- boxplot(cars$price)

## Remove the outliers
cars_final <- filter(cars , !cars$price %in% bstat$out)

View(cars_final)

# Replace  the levels with 1 and 0 
levels(cars_final$fueltype)<-c(1,0)
#Convert the fueltype to numeric varaiable 
cars_final$fueltype<- as.numeric(levels(cars_final$fueltype))[cars_final$fueltype]

levels(cars_final$aspiration)<-c(1,0)
cars_final$aspiration<- as.numeric(levels(cars_final$aspiration))[cars_final$aspiration]

levels(cars_final$doornumber)<-c(1,0)
cars_final$doornumber<- as.numeric(levels(cars_final$doornumber))[cars_final$doornumber]

levels(cars_final$enginelocation)<-c(1,0)
cars_final$enginelocation<- as.numeric(levels(cars_final$enginelocation))[cars_final$enginelocation]

#----------------------------------------------------------------
#Converting "drivewheel" into dummies . 
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = cars_final))

#check the dummy_drivewheel data frame.
View(dummy_drivewheel)

#This column should be removed from the newly created dummy_drivewheel dataframe containing the dummy values for the variable "drivewheel". 
dummy_drivewheel <- dummy_drivewheel[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
cars_final <- cbind(cars_final[,-8], dummy_drivewheel)
##-------------------------------------------------------------------
#Converting "carbody" into dummies . 

dummy_carbody <- data.frame(model.matrix( ~carbody, data = cars_final))
dummy_carbody <- dummy_carbody[,-1]
cars_final <- cbind(cars_final[,-7], dummy_carbody)
View(cars_final)

##------------------------------------------------------------------------
#Converting "enginetype" into dummies . 

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = cars_final))
dummy_enginetype <- dummy_enginetype[,-1]
cars_final <- cbind(cars_final[,-13], dummy_enginetype)
View(cars_final)

##------------------------------------------------------------------------
#Converting "cylindernumber" into dummies . 
dummy_cylnde <- data.frame(model.matrix( ~cylindernumber, data = cars_final))
dummy_cylnde <- dummy_cylnde[,-1]
cars_final <- cbind(cars_final[,-13], dummy_cylnde)
View(cars_final)

##----------------------------------------------------------------------
#Converting "fuelsystem" into dummies .
dummy_fuel <- data.frame(model.matrix( ~fuelsystem, data = cars_final))
dummy_fuel <- dummy_fuel[,-1]
cars_final <- cbind(cars_final[,-14], dummy_fuel)
View(cars_final)

###-------------------------------------------------------------------------
# Convert CarName into character column 
  
cars_final$CarName <- as.character(cars_final$CarName)
# Create the new derived column company name and assign first part of car name to this column
cars_final$company_name <- sapply(strsplit(cars_final$CarName ,' ') , function(x) x[1])
# Convert the company name to upper character 

cars_final$company_name <- toupper(cars_final$company_name)
unique(cars_final$company_name)
## Address the data quality issues in company name column

cars_final$company_name <- ifelse(cars_final$company_name == "TOYOUTA" , "TOYOTA" ,cars_final$company_name )
cars_final$company_name <- ifelse(cars_final$company_name == "MAXDA" , "MAZDA" ,cars_final$company_name )
cars_final$company_name <- ifelse(cars_final$company_name %in% c("VOKSWAGEN" , "VW") , "VOLKSWAGEN" ,cars_final$company_name )
##boxplot(cars_final$price~cars_final$company_name)
##----------------------------------------------------------------------
# Convert the company column into dummies 
dummy_company <- data.frame(model.matrix( ~company_name, data = cars_final))
dummy_company <- dummy_company[,-1]
cars_final <- cbind(cars_final[,-47], dummy_company)
View(cars_final)

#-----------------------------------------------------------------------
# Remove the original column CarName
cars_final$CarName <- NULL;
# Remove the  column car id since it is a sequence. Not used for analysis
cars_final$car_ID <- NULL ; 

unique(cars_final$enginelocation)
# Since enginelocation contains only NULL so it is not required in model building
cars_final$enginelocation <- NULL;
unique(cars_final$enginetypedohcv)
unique(cars_final$cylindernumbertwelve)
# Since enginetypedohcv and cylindernumbertwelve contains only single values So not required for model building.
cars_final$enginetypedohcv <- NULL 
cars_final$cylindernumbertwelve <- NULL ; 

## Add derived metrics 
cars_final$stroketobore <- cars_final$stroke /cars_final$boreratio

## Add derived metrics 
cars_final$weightvssize <- cars_final$curbweight/(cars_final$carheight*cars_final$carlength*cars_final$carwidth)


##---------------------------------------------------------------

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(cars_final), 0.7*nrow(cars_final))
# generate the train data set
train = cars_final[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = cars_final[-trainindices,]

corr <- cor ( train)

###-Build the first model 

model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

# Pass the model_1 in the vif function
vif(model_1)

# Look at summary of the model again to see the P values
summary(model_1)

## Run stepaic function

step <- stepAIC(model_1 , direction = "both")

## Model after removing the varaiables suggested by stepaic 

model_2 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio + stroke + horsepower + 
               peakrpm + citympg + highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE + company_nameHONDA + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)


vif(model_2)
summary(model_2)

## Removed company_nameHONDA as per P value 

model_3 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio + stroke + horsepower + 
               peakrpm + citympg + highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

vif(model_3)
summary(model_3)

## Removed stroke as per P value 

model_4 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio  + horsepower + 
               peakrpm + citympg + highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_4)

## Removed citympg as per P value 

model_5 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio  + horsepower + 
               peakrpm  + highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_5)

## Removed horsepower as per P value 

model_6 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio   + 
               peakrpm  + highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_6)

## Removed peakrpm as per P value 

model_7 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio   + 
                  highwaympg + drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_7)

## Removed highwaympg  as per P value 
 
model_8 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
               carheight + curbweight + boreratio   + 
                 drivewheelfwd + drivewheelrwd + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcv + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
               company_nameBUICK + company_nameDODGE  + 
               company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
               company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_8)

## Removed enginetypeohc as per P value 

model_9 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                drivewheelfwd + drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel  + enginetypeohcv + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_9)



## Removed fuelsystemspdi as per P value 

model_10 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                drivewheelfwd + drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel  + enginetypeohcv + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_10)

## Removed enginetypeohcv as per P value 

model_11 <-lm(formula = price ~ aspiration + doornumber + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                drivewheelfwd + drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_11)

## Removed doornumber as per P value 

model_12 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                drivewheelfwd + drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA + fuelsystemspfi, data = train)

summary(model_12)

## Removed fuelsystemspfi as per P value 


model_13 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                drivewheelfwd + drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA , data = train)

summary(model_13)

## Removed drivewheelfwd as per P value 

model_14 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                carheight + curbweight + boreratio   + 
                  drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA , data = train)
summary(model_14)

## Removed boreratio
model_14_1 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                carheight + curbweight    + 
                drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA , data = train)
summary(model_14_1)

## Removed fuelsystem2bbl
model_14_2 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                  carheight + curbweight    + 
                  drivewheelrwd + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  enginetypel    + 
                  fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                  company_nameBUICK + company_nameDODGE  + 
                  company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                  company_nameSAAB + company_nameTOYOTA , data = train)
summary(model_14_2)


## Removed fuelsystem2bbl
model_14_3 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                  carheight + curbweight    + 
                  drivewheelrwd + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  enginetypel      + company_nameAUDI + company_nameBMW + 
                  company_nameBUICK + company_nameDODGE  + 
                  company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                  company_nameSAAB + company_nameTOYOTA , data = train)
summary(model_14_3)

## Removed company_nameDODGE
model_14_4 <-lm(formula = price ~ aspiration  + wheelbase + carlength + 
                  carheight + curbweight    + 
                  drivewheelrwd + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  enginetypel      + company_nameAUDI + company_nameBMW + 
                  company_nameBUICK + 
                  company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                  company_nameSAAB + company_nameTOYOTA , data = train)
summary(model_14_4)

## Removed carlength , carheight &  curbweight  and replaced by weightvssize

model_15 <-lm(formula = price ~ aspiration  + wheelbase +  boreratio   + 
                drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA +weightvssize , data = train)

summary(model_15)


##Replaced boreratio by stroketobore derived metrics. Adjusted R value remaind same. But VIF got reduced 

model_16 <-lm(formula = price ~ aspiration  + wheelbase +  stroketobore   + 
                drivewheelrwd + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA +weightvssize , data = train)

summary(model_16)

## Removed highest VIF carbodysedan.  Adjusted R2 got reduced a bit. But it reduced VIF significantly

model_17 <-lm(formula = price ~ aspiration  + wheelbase +  stroketobore   + 
                drivewheelrwd + 
                carbodyhardtop + carbodyhatchback  + carbodywagon + 
                enginetypel   + fuelsystem2bbl + 
                fuelsystemmpfi  + company_nameAUDI + company_nameBMW + 
                company_nameBUICK + company_nameDODGE  + 
                company_nameMITSUBISHI + company_namePLYMOUTH + company_namePORSCHE + 
                company_nameSAAB + company_nameTOYOTA +weightvssize , data = train)

summary(model_17)





# Predict the car prices in the testing dataset
Predict_1 <- predict(model_17,test[,-18])
test$car_predicted_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$car_predicted_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$car_predicted_price)^2 ## Model able to predict vraiation in price by 89%


## Conclusion :   
## Model 17 is used to predict the car price.
## Critical drivers for predicting prices are as below : 
## With Rear Wheel driving, car price is getting higher
## With larger Wheel base  car price is getting higher. 
## With higher Stroke to bore ratio car price is getting higher. 
## Standard or Turbo aspiration engine is influencing the price. 
## With higher Car weight by volume ratio  car price is getting higher 
## With Engine type l and fuel system mpficar price is getting higher 
## Car body is affecting the price. 
## Car brand value is influencing the price.  

