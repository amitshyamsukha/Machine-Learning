
library(stringr)
library(tidyr)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
setwd("C:/Amit/Upgrad/HR_casestudy")

##==========================================================================
## 1. Data loading and merging them into a single data frame 
##===========================================================================

## Load the multiple input files into dataframe 

emp_general_info <- read.csv ( "general_data.csv" , header = T, stringsAsFactors = F )
emp_survey<- read.csv("employee_survey_data.csv" , header = T, stringsAsFactors = F)
mgr_rating <- read.csv ("manager_survey_data.csv" , header = T,  stringsAsFactors = F)
employee_in_time <- read.csv ( "in_time.csv" , stringsAsFactors = F  )
employee_out_time <- read.csv ( "out_time.csv" , stringsAsFactors = F  )

## Basic structure and duplicate checking 

nrow(emp_general_info) ###4410
nrow(emp_survey)       ###4410
nrow(mgr_rating)       ###4410
nrow(employee_in_time) ###4410
nrow(employee_out_time) ###4410

## There is no duplicate row in data set 
length(which(duplicated (emp_general_info)))
length(which(duplicated (emp_survey)))
length(which(duplicated (mgr_rating)))
length(which(duplicated (employee_in_time)))
length(which(duplicated (employee_out_time)))



str(emp_general_info)
length(unique(emp_general_info$EmployeeID)) ## 4410
str(emp_survey)
length(unique(emp_survey$EmployeeID)) ## 4410
length(unique(mgr_rating$EmployeeID)) ## 4410
setdiff(emp_general_info$EmployeeID ,emp_survey$EmployeeID ) ## No diff 
setdiff(emp_survey$EmployeeID , mgr_rating$EmployeeID )      ## No diff 
length(unique(employee_in_time$X)) ##4410 rows 
length(unique(employee_out_time$X)) ##4410 rows 
setdiff (employee_in_time$X , employee_out_time$X )  ## No diff 
setdiff (emp_general_info$EmployeeID, employee_in_time$X)    ## No diff 

str(employee_in_time)
# Convert in time data in wide format 
employee_in_time_formatted <- gather  (employee_in_time , day, intime , 2:262 )
# Convert out  time data in wide format 
employee_out_time_formatted <- gather  (employee_out_time , day, outime , 2:262 )
# Convert it into date format 
employee_in_time_formatted$intime <- as.POSIXlt(employee_in_time_formatted$intime,format="%Y-%m-%d %H:%M:%S")
employee_out_time_formatted$outime <- as.POSIXlt(employee_out_time_formatted$outime,format="%Y-%m-%d %H:%M:%S")

colnames(employee_in_time_formatted) <- c( "Emp_id" , "Day" , "in_time")
colnames(employee_out_time_formatted) <- c( "Emp_id" , "Day" , "out_time")

# Merge in and out details to have a data frame having employee , in time and out time
emp_in_out_time <- merge(employee_in_time_formatted ,employee_out_time_formatted, by=c("Emp_id" , "Day"))

## R has appended X infront of Day column. So removing X from day 
emp_in_out_time$Day <- substr(emp_in_out_time$Day , 2,11)
emp_in_out_time$Day <- as.POSIXlt(emp_in_out_time$Day,format="%Y.%m.%d")
## Office timing by subtracting out time from in time
emp_in_out_time$office_time <- emp_in_out_time$out_time - emp_in_out_time$in_time
# Calculate average employee hour
avg_emp_hr <- aggregate(emp_in_out_time$office_time , by = list(emp_in_out_time$Emp_id ) , FUN=  mean , na.rm= TRUE )
colnames(avg_emp_hr) <- c("EmployeeID" , "Avg_Hrs")

length(unique(avg_emp_hr$EmployeeID)) ## 4410 rows 
setdiff(emp_general_info$EmployeeID ,avg_emp_hr$EmployeeID )

## Merge all the data frames into a single data frame 
temp_emp1 <- merge(emp_general_info,emp_survey , by = "EmployeeID" )
nrow(temp_emp1)
temp_emp1 <- merge(temp_emp1,mgr_rating , by = "EmployeeID" )
nrow(temp_emp1)
emp_merged <- merge(temp_emp1,avg_emp_hr , by = "EmployeeID" )
nrow(emp_merged)
View(emp_merged)

## Check if there is any issue with case sensitivity 
sapply(emp_merged, function(x) length(unique(toupper(x)))-length(unique(tolower(x)))) ## all 0

attrition_percentage <- length(which(emp_merged$Attrition=='Yes'))/nrow(emp_merged) ## 16%
##===============================================================
## 2. EDA analysis 
##================================================================

## Create a function univariate_categorical for plotting bar chart for categorical variable 

univariate_categorical <- function(dataset,varname,value, Title){
  dataset %>% count(varname = factor(value), Attrition = factor(Attrition)) %>% 
    mutate(pct = round(prop.table(n) * 100))%>%
    ggplot(aes(x = varname , y = pct, fill = Attrition)) + 
    geom_bar(stat = 'identity' , position = 'dodge') + 
    geom_text(aes(y = pct + 3,    # nudge above top of bar
                  label = paste0(pct, '%')),    # prettify
              position = position_dodge(width = .9), 
              size = 3) + ggtitle(Title) +labs(y = "Percent", x = varname)
}



## Plotting for Business travel column. People who are travelling frequently are prone to attrition. 
univariate_categorical(emp_merged , "Travel" , emp_merged$BusinessTravel , "Business Travel vs Attrition")
## We have  reasonable number of individuals in each group for  Business travel 
ggplot(emp_merged , aes ( x= BusinessTravel , fill = Attrition ))+ geom_bar()

# Total 3 departments. As per % analysis HR is highest contributor 
univariate_categorical(emp_merged , "Department" , emp_merged$Department ,"Department vs Attrition")

# There are relatively less number of individuals for HR 
ggplot(emp_merged , aes ( x= emp_merged$Department , fill = Attrition ))+ geom_bar()

## Seems  it is not significant. 
univariate_categorical(emp_merged , "Education" , emp_merged$Education , "Education vs Attrition")

## HR is top contributor as per %. But number wise less. 
univariate_categorical(emp_merged ,"Education Field" ,  emp_merged$EducationField , "EducationField vs Attrition")
ggplot(emp_merged , aes ( x= emp_merged$EducationField , fill = Attrition ))+ geom_bar()

## 1 for attrition and 0 for non attrition 
emp_merged$Attrition_num <- ifelse(emp_merged$Attrition=="Yes",1,0)

##  Dpartment wise education analysis 
agg_edu_field <- aggregate(Attrition_num ~ Department + EducationField, data = emp_merged, mean)
agg_edu_field <- agg_edu_field[order(agg_edu_field$Department,  agg_edu_field$Attrition_num),]

## Eduction level of employee ( who are leaving )  of different departments 
## For different departments education level has different significance on attrition
ggplot(agg_edu_field, aes(x = Department, y = Attrition_num, fill = EducationField)) + 
  geom_bar(aes(fill = EducationField), stat = "identity", position=position_dodge()) 

## gender is not  significant parameter 
univariate_categorical(emp_merged , "Gender" , emp_merged$Gender  , "Gender vs Attrition")

## Not significant 
univariate_categorical(emp_merged , "Job Level" , emp_merged$JobLevel  , "JobLevel vs Attrition")

## Research Director role is contributing most to attrition as per % analysis 
univariate_categorical(emp_merged , "Job Role" , emp_merged$JobRole  , "JobRole vs Attrition")
ggplot(emp_merged , aes (emp_merged$JobRole  , fill = Attrition))+ geom_bar()

## Singles are contributing most to attrition
univariate_categorical(emp_merged , "Marital Status " , emp_merged$MaritalStatus  , "MaritalStatus vs Attrition")
ggplot(emp_merged , aes (MaritalStatus  , fill = Attrition))+ geom_bar()

## Not that significant 
univariate_categorical(emp_merged , "Stock Option" , emp_merged$StockOptionLevel  , "StockOptionLevel vs Attrition")
ggplot(emp_merged , aes (StockOptionLevel  , fill = Attrition))+ geom_bar()

## If statisfaction is low,  attrition is more 
univariate_categorical(emp_merged , " Env Satisfaction" , emp_merged$EnvironmentSatisfaction  , "EnvironmentSatisfaction vs Attrition")
ggplot(emp_merged , aes (emp_merged$EnvironmentSatisfaction  , fill = Attrition))+ geom_bar()

## Department wise satisfaction 
ggplot(emp_merged , aes (emp_merged$EnvironmentSatisfaction  , fill = Department))+ geom_bar()


## If low contributing most to attrition
univariate_categorical(emp_merged , "Job Satisfaction" , emp_merged$JobSatisfaction  , "JobSatisfaction vs Attrition")
ggplot(emp_merged , aes (emp_merged$JobSatisfaction  , fill = Attrition))+ geom_bar()

## Department wise job satisfaction analysis 
agg_dept_job_satis <- aggregate(Attrition_num ~ Department + JobSatisfaction, data = emp_merged, mean)
agg_dept_job_satis <- agg_dept_job_satis[order( agg_dept_job_satis$Department , -agg_dept_job_satis$Attrition_num),]

## Department wise job satisfaction 
ggplot(agg_dept_job_satis, aes ( x= Department , y = Attrition_num, fill = JobSatisfaction)) + geom_bar(stat = "identity")


## If low contributing most to attrition
univariate_categorical(emp_merged , "Work Life Balance" , emp_merged$WorkLifeBalance  , "WorkLifeBalance vs Attrition")
ggplot(emp_merged , aes (emp_merged$WorkLifeBalance  , fill = Attrition))+ geom_bar()


## Department wise work life balancde  analysis 
agg_dept_work_balance <- aggregate(Attrition_num ~ Department + WorkLifeBalance, data = emp_merged, mean)
agg_dept_work_balance <- agg_dept_work_balance[order( agg_dept_work_balance$Department , -agg_dept_work_balance$Attrition_num),]

## Department wise work life balance   analysis 
ggplot(agg_dept_work_balance, aes ( x= Department , y = Attrition_num, fill = WorkLifeBalance)) + geom_bar(stat = "identity")

## Work life balance and average office hour analysis 
emp_merged$Avg_Hrs <- as.numeric(emp_merged$Avg_Hrs)
boxplot(emp_merged$Avg_Hrs~emp_merged$WorkLifeBalance)

## If involvement is less contributing more  to attrition mostly.
univariate_categorical(emp_merged , " Job Involvement" , emp_merged$JobInvolvement  , "JobInvolvement vs Attrition")
ggplot(emp_merged , aes (emp_merged$JobInvolvement  , fill = Attrition))+ geom_bar()

## Not so significant. Attrition is higher for higher performance  
univariate_categorical(emp_merged , " Performance" , emp_merged$PerformanceRating  , "PerformanceRating vs Attrition")

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

## Check the distribution of age column
hist(emp_merged$Age, breaks = 100,  xlab = "Age")

## There is overlapping   role with age 
ggplot(emp_merged, aes(x=emp_merged$JobRole,y=Age))+ geom_boxplot(width=0.1)

## There is overlapping   joblevel  with age 
ggplot(emp_merged, aes(x= factor(emp_merged$JobLevel),y=Age))+ geom_boxplot(width=0.1)

## Attrition count with age. 

plot_grid(ggplot(emp_merged, aes(Age ,  fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

## Strong co-relation between age and TotalWorkingYears
ggplot(emp_merged , aes ( x= Age , y= TotalWorkingYears))+geom_point()

plot_grid(ggplot(emp_merged, aes(DistanceFromHome, fill = Attrition))+ geom_histogram(binwidth = 2),
          ggplot(emp_merged, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

## Outlier is present for monthly income 
plot_grid(ggplot(emp_merged, aes(MonthlyIncome ,  fill = Attrition))+ geom_histogram(binwidth = 5000),
          ggplot(emp_merged, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

boxplot(MonthlyIncome ~ JobRole, data = emp_merged)
boxplot(MonthlyIncome ~ JobLevel, data = emp_merged)

## TotalWorkingYears and MonthlyIncome analysis 
ggplot(emp_merged , aes ( x= TotalWorkingYears , y =MonthlyIncome  ))+geom_point() + geom_smooth()

## Outlier is present for NumCompaniesWorked
plot_grid(ggplot(emp_merged, aes(NumCompaniesWorked, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)


plot_grid(ggplot(emp_merged, aes(PercentSalaryHike ,  fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

boxplot (PercentSalaryHike~Department, data = emp_merged)  

## Outlier is present 
plot_grid(ggplot(emp_merged, aes(TotalWorkingYears, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

## Monthly income and TotalWorkingYears analysis 
ggplot(emp_merged , aes ( x= TotalWorkingYears , y =MonthlyIncome )) + geom_point()

## Outlier is present
plot_grid(ggplot(emp_merged, aes(TrainingTimesLastYear, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

## Outlier is present 
plot_grid(ggplot(emp_merged, aes(YearsAtCompany, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

quantile(emp_merged$YearsAtCompany)

## Outlier is present
plot_grid(ggplot(emp_merged, aes(YearsSinceLastPromotion, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

quantile(emp_merged$YearsSinceLastPromotion)

## Outlier is present
plot_grid(ggplot(emp_merged, aes(YearsWithCurrManager, fill = Attrition))+ geom_histogram(binwidth = 1),
          ggplot(emp_merged, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

quantile(emp_merged$YearsWithCurrManager)

## AVerage working hr vs attrition 
emp_merged$Avg_Hrs <- as.numeric(emp_merged$Avg_Hrs)
plot_grid(ggplot(emp_merged, aes(Avg_Hrs, fill = Attrition))+ geom_histogram(binwidth = .5),
          ggplot(emp_merged, aes(x="",y=Avg_Hrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ncol = 1)

boxplot (Avg_Hrs~WorkLifeBalance, data = emp_merged)  


##===========================================================================================
## Missing value handling  
##==========================================================================================

sapply(emp_merged, function(x) sum(is.na(x))) ## we have NA for NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction , JobSatisfaction ,WorkLifeBalance 

length(which(is.na(emp_merged))) ## We have around 2% rows having NA. Since it is only 2% so removing them

## Missing value  treatment. Omit the missing 

emp_info  <- na.omit(emp_merged)
str(emp_info)

## Check unique number of values in each columns 
sapply(emp_info, function(x) length(unique(x))) ## StandardHours, Over18 , EmployeeCount
emp_info$Over18 <- NULL   ## Only  1 value. SO removing it
emp_info$EmployeeCount <- NULL ## Only  1 value. SO removing it
emp_info$EmployeeID <- NULL ## Unique employee id . So dont need it for analysis
str(emp_info)

## Outlier treatment 

## Create a function for capping & flooring values with provided percentile values 

remove_outliers <- function(x , lower_quantile, upper_quantile) {
  qnt <- quantile(x, probs=c(lower_quantile, upper_quantile), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < qnt[1]] <- qnt[1]
  y[x > qnt[2]] <- qnt[2]
  y
}


## display 0 to 100 percentile for all the columns having outlier to decide values which will be used for outlier substitution
sapply(emp_info[,c("MonthlyIncome","NumCompaniesWorked", "TotalWorkingYears" , "TrainingTimesLastYear" ,"YearsAtCompany",
                   "YearsSinceLastPromotion", "YearsWithCurrManager")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) 


## BY looking at the spread of data  in each field appropiate value to replace outlier has been determined
emp_info$MonthlyIncome <- remove_outliers (emp_info$MonthlyIncome,0, .9 ) 
emp_info$NumCompaniesWorked <- remove_outliers (emp_info$NumCompaniesWorked,0, .96 ) 
emp_info$TotalWorkingYears <- remove_outliers (emp_info$TotalWorkingYears,0, .95 )
emp_info$TrainingTimesLastYear <- remove_outliers (emp_info$TrainingTimesLastYear,.04, .87 )
emp_info$YearsAtCompany <- remove_outliers (emp_info$YearsAtCompany,0, .93 )
emp_info$YearsSinceLastPromotion <- remove_outliers (emp_info$YearsSinceLastPromotion,0, .92 )
emp_info$YearsWithCurrManager <- remove_outliers (emp_info$YearsWithCurrManager,0, .98 )


## Replacing categorical varaible with proper labels 
emp_info$Education <- sapply(emp_info$Education, switch , '1' = 'below_college','2' = 'college' , '3'= 'bachelor', '4'= 'master' , '5'= 'Doctor')
emp_info$EnvironmentSatisfaction  <- sapply(emp_info$EnvironmentSatisfaction, switch , '1' = 'Low','2' = 'Medium' , '3'= 'High', '4'= 'Very_high' )
emp_info$JobSatisfaction <- sapply(emp_info$JobSatisfaction, switch , '1' = 'Low','2' = 'Medium' , '3'= 'High', '4'= 'Very_high' )
emp_info$JobInvolvement <- sapply(emp_info$JobInvolvement, switch , '1' = 'Low','2' = 'Medium' , '3'= 'High', '4'= 'Very_high' )
emp_info$WorkLifeBalance  <- sapply(emp_info$WorkLifeBalance, switch , '1' = 'Bad','2' = 'Good' , '3'= 'Better', '4'= 'Best'  )
emp_info$PerformanceRating  <- sapply(emp_info$PerformanceRating, switch , '1' = 'Low','2' = 'Good' , '3'= 'Excellent', '4'= 'Outstanding'  )


## Derived column addiitons 

## Calculate median salary for each job role 
median_role_salary <- aggregate(MonthlyIncome ~ JobRole, data = emp_info, median)
names(median_role_salary)[2] <- "role_median_salary"
## Merge with master data frame 
emp_info <- merge(emp_info ,median_role_salary , by = "JobRole" )
## Calculate individual employee's salary difference from median salary for their job role 
emp_info$sal_med_diff  <- emp_info$MonthlyIncome - emp_info$role_median_salary
## Dont need role_median_salary any more 
emp_info$role_median_salary <- NULL

## Converting Yes to 1 and No to 0
emp_info$Attrition <-  ifelse(emp_info$Attrition=="Yes",1,0)

# creating a dataframe of categorical features
emp_info_chr <- emp_info[,-c(2,3,6,10,12,13,14,15,16,17,18,19,20,21,27,28,29)]

## Convert them into factor 
emp_info_fact <- data.frame(sapply(emp_info_chr, function(x) factor(x)))
str(emp_info_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(emp_info_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =emp_info_fact))[,-1]))

emp_final <- cbind(emp_info[,c(2,3,6,10,12,13,14,15,16,17,18,19,20,21,27,28,29)],dummies) 
View(emp_final) #4300 obs. of  54 variables

## Creating one more derived column over_time

emp_final$Avg_Hrs <- as.numeric(emp_final$Avg_Hrs)

emp_final$over_time <- emp_final$Avg_Hrs - as.numeric(emp_final$StandardHours)

## Scale the continious varaiables 
emp_final$Age <- scale (emp_final$Age )
emp_final$DistanceFromHome <- scale(emp_final$DistanceFromHome)
emp_final$MonthlyIncome <- scale(emp_final$MonthlyIncome)
emp_final$PercentSalaryHike <- scale(emp_final$PercentSalaryHike )
emp_final$TotalWorkingYears <- scale(emp_final$TotalWorkingYears)
emp_final$TrainingTimesLastYear <- scale(emp_final$TrainingTimesLastYear)
emp_final$YearsAtCompany <- scale(emp_final$YearsAtCompany)
emp_final$YearsSinceLastPromotion <- scale(emp_final$YearsSinceLastPromotion)
emp_final$YearsWithCurrManager <- scale(emp_final$YearsWithCurrManager )
emp_final$Avg_Hrs <- scale(emp_final$Avg_Hrs)
##emp_final$over_time <- scale(emp_final$over_time)
emp_final$sal_med_diff <- scale(emp_final$sal_med_diff)

emp_final$Attrition_num <- NULL ; ## Internally created vraiable. Not required for analysis. 

corr <- cor(emp_final)


## Split the data in training and test data set 
set.seed(100)

indices = sample.split(emp_final$Attrition, SplitRatio = 0.7)

train = emp_final[indices,]

test = emp_final[!(indices),]

## Start modeeling 

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

library(car)
vif(model_2)


## Removed monthly income as vif is too high 

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xcollege + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)

summary(model_3)
vif(model_3)
## Removed age as it is  highly co-related with TotalWorkingYears

model_4 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.xcollege + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)

summary(model_4)
vif(model_4)

## removed BusinessTravel.xTravel_Rarely as vif is high 

model_5 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently +  
                 Education.xcollege + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)

summary(model_5)
vif(model_5)

## removed MaritalStatus.xMarried  as vif is high 

model_6 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently +  
                 Education.xcollege + EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender +  MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)


summary(model_6)
vif(model_6)


## Rdemoved EducationField.xLife.Sciences  as vif is high 
model_7 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently +  
                 Education.xcollege + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender +  MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high + WorkLifeBalance.xBest + 
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)

summary(model_7)
vif(model_7)

## Rdemoved  WorkLifeBalance.xBest as vif  value  is high 
model_8 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently +  
                 Education.xcollege + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender +  MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)

summary(model_8)
vif(model_8)
## Rdemoved  EducationField.xOther as p is too high  
model_9 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 BusinessTravel.xTravel_Frequently +  
                 Education.xcollege + EducationField.xMarketing + 
                 EducationField.xMedical +  EducationField.xTechnical.Degree + 
                 Gender +  MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                 JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                 WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                 JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
               data = train)


summary(model_9)
vif(model_9)


## Rdemoved  EducationField.xMarketing as ps is high 
model_10 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                  EducationField.xMedical +  EducationField.xTechnical.Degree + 
                  Gender +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_10)
vif(model_10)
## JobRole.xSales.Executive as p is too high  


model_11 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Scientist + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                  EducationField.xMedical +  EducationField.xTechnical.Degree + 
                  Gender +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_11)
vif(model_11)


## EducationField.xMedical as p is too high 


model_12 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Scientist + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                    EducationField.xTechnical.Degree + 
                  Gender +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)


summary(model_12)
vif(model_12)

## Gender as ps is too high 

model_13 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Scientist + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                  EducationField.xTechnical.Degree + 
                     MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_13)
vif(model_13)

## EducationField.xTechnical.Degree as p is too high 

model_14 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Scientist + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_14)
vif(model_14)

## JobRole.xResearch.Scientist as ps is too high 
model_15 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  BusinessTravel.xTravel_Frequently +  
                  Education.xcollege + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_15)
vif(model_15)

## Education.xcollege as ps is high 
model_16 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs + sal_med_diff + JobRole.xManufacturing.Director + 
                  BusinessTravel.xTravel_Frequently +  
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_16)
vif(model_16)

## sal_med_diff as p is too high 
model_17 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs +  JobRole.xManufacturing.Director + 
                  BusinessTravel.xTravel_Frequently +  
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow + EnvironmentSatisfaction.xVery_high + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_17)
vif(model_17)

## removed EnvironmentSatisfaction.xVery_high as ps is high
model_18 <-  glm(formula = Attrition ~   NumCompaniesWorked + 
                   TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                   YearsWithCurrManager + Avg_Hrs +  JobRole.xManufacturing.Director + 
                   BusinessTravel.xTravel_Frequently +  
                   MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.xLow  + 
                   JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                   WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                   JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                 data = train)

summary(model_18)
vif(model_18)

## removed JobRole.xManufacturing.Director as p is high 

model_19 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs +   
                  BusinessTravel.xTravel_Frequently +  
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow  + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                  JobInvolvement.xMedium + JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_19)
vif(model_19)

## Removed JobInvolvement.xMedium  as p is high 
model_19_1 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs +   
                  BusinessTravel.xTravel_Frequently +  
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow  + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow + 
                   JobInvolvement.xVery_high, family = "binomial", 
                data = train)

summary(model_19_1)
vif(model_19_1)

## Removed JobInvolvement.xVery_high as p is high 
model_19_2 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + Avg_Hrs +   
                    BusinessTravel.xTravel_Frequently +  
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.xLow  + 
                    JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                    WorkLifeBalance.xBetter + WorkLifeBalance.xGood + JobInvolvement.xLow  
                    , family = "binomial", 
                  data = train)

summary(model_19_2)
vif(model_19_2)


## Removed JobInvolvement.xLow as p is high 
model_19_3 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + Avg_Hrs +   
                    BusinessTravel.xTravel_Frequently +  
                    MaritalStatus.xSingle + 
                    EnvironmentSatisfaction.xLow  + 
                    JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                    WorkLifeBalance.xBetter + WorkLifeBalance.xGood   
                  , family = "binomial", 
                  data = train)

summary(model_19_1)
vif(model_19_1)

## removed TrainingTimesLastYear as p is not significant 

model_20 <- glm(formula = Attrition ~   NumCompaniesWorked + 
                  TotalWorkingYears  + YearsSinceLastPromotion + 
                  YearsWithCurrManager + Avg_Hrs +   
                  BusinessTravel.xTravel_Frequently +  
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.xLow  + 
                  JobSatisfaction.xLow + JobSatisfaction.xVery_high +  
                  WorkLifeBalance.xBetter + WorkLifeBalance.xGood   
                , family = "binomial", 
                data = train)


summary(model_20)
vif(model_20)


#### Output of model : 
#NumCompaniesWorked :- 	If number if previous companies are high, chance of attrition is getting increased.
#TotalWorkingYears:- 	With increase in total working experience chances of attirtion is getting reduced. 
#YearsSinceLastPromotion :- 	If time since last promotion is getting increased attrition chanhce is also getting increased. 
#YearsWithCurrManager	:- If working longer under a same manager chance of attrition is getting reduced. 
#Avg_Hrs :- 	If people are working longer in office chance of attrition is getting increased
#BusinessTravel :- 	If people needs to travel frequently chance of attrition is getting increased
#MaritalStatus:- 	If not married chance of attrition is getting increased
#EnvironmentSatisfaction:- With friendly environment chance of attrition is getting reduced 
#JobSatisfaction:- 	With increasing job satisfaction chance of attrition is getting reduced 
#WorkLifeBalance :- 	With increasing work  life balance  chance of attrition is getting reduced 


#final_model <- model_20
final_model <- model_20

## Model Eavlution 
## Predict the attrition on test data set based on final model

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= .5, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition ==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

#install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf  ## Look art the sensitivity , specificity and accuracy of model 

# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)

# Creating cutoff values from .01 to .8  for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]    ##  cut odd value is .16 


# Let's choose a cutoff value of .16 for final model

test_pred_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]   ## Accuracy 

sens <- conf_final$byClass[1]  ## Sensitivity 

spec <- conf_final$byClass[2]   ## Specificity 

acc

sens

spec

View(test)

### KS -statistic - Test Data ######

test_pred_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_pred_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  ## 41%. It indicates  how well model discriminates  between two classes. 


# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attrition, test_pred, groups = 10)  ## With first foure decile we are able to predict around 73% prediction.  






