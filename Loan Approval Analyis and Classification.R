library(readxl)
library(modeest)
library(dplyr)

mydata<- read_excel("E:/Sem9/Data Science/Project/IDS Sec-F Midterm Summer 24-25 Loan Approval Classification Dataset - modified.xlsx")
mydata

is.na(mydata) # checks if a value is missing in a columns every row
colSums(is.na(mydata)) #sums up a columns total missing

mydata$person_age[mydata$person_age == ""] <- NA 
mydata
colSums(is.na(mydata))
mydata
#missing value handle
#age
mean_age<- round(mean(mydata$person_age,na.rm =TRUE)) # ignoring NA we will calculate mean of age
mean_age
mydata$person_age[is.na(mydata$person_age)] <- mean_age #all missing values will be replace with the mean
is.na
#loanstatus
mean_loanstatus<-round(mean(mydata$loan_status,na.rm =TRUE))
mean_loanstatus
mydata$loan_status[is.na(mydata$loan_status)] <- mean_loanstatus
mydata['loan_status']
#loanpercent income
mean_loanpercentincome<-round(mean(mydata$loan_percent_income,na.rm =TRUE))
mean_loanpercentincome
mydata$loan_percent_income[is.na(mydata$loan_percent_income)] <- mean_loanpercentincome
mydata['loan_percent_income']

#personincome
mean_income<- round(mean(mydata$person_income,na.rm =TRUE))
mean_income
mydata$person_income[is.na(mydata$person_income)] <- mean_income
mydata['person_income']
#gender
mode_gender <-mfv(mydata$person_gender)
mydata$person_gender[is.na(mydata$person_gender)]<-mode_gender #for catagorical mode is used
mydata['person_gender']
#education
mode_education <-mfv(mydata$person_education)
mydata$person_education[is.na(mydata$person_education)]<-mode_education
mydata['person_education']

# detecting the noisy values 
Detect_NoisyValue <- levels(factor(mydata$person_home_ownership))
#factor is used for catagorical value it groups data, and levels extract unique catagorical values
Detect_NoisyValue

# Find rows with "OOWN" , "RENTT" and replace with "OWN"  "RENT"
mydata$person_home_ownership[which(mydata$person_home_ownership == "OOWN")] <- "OWN"
mydata$person_home_ownership[which(mydata$person_home_ownership == "RENTT")] <- "RENT"
library(dplyr)
mydata %>% # take the data set and pass it to next function which is select
select(person_home_ownership)
mydata['person_home_ownership']


# detecting the invalid values for age(cant be more than 100,not possible)
library(dplyr)
invalid <- mydata$person_age < 0 | mydata$person_age > 100
invalid_ages <- mydata %>% # invalid_age contains data that are age<0 and age>100 
filter(invalid)


#handling invalid values
age_median <-median(mydata$person_age, na.rm = TRUE)
#replace INVALID data with NULL,then use median 
mydata$person_age[invalid] <- NA # replace all invalid age with NA
mydata$person_age[is.na(mydata$person_age)] <- age_median # then change NA with median
levels(factor(mydata$person_age))
mydata['person_age']


#duplicate value
duplicated(mydata) # gives true false of all duplicate data
sum(duplicated(mydata)) # gives a sum of how many duplicate data
which(duplicated(mydata)) #  gives the row of duplicate data
fixed_mydata <- distinct(mydata) # removes duplicate it used dplye
fixed_mydata
which(duplicated(fixed_mydata))

# oultiers
#age
q1_age <- quantile(fixed_mydata$person_age, 0.25, na.rm = TRUE)    
q3_age <- quantile(fixed_mydata$person_age, 0.75, na.rm = TRUE)    
iqr_age <- q3_age - q1_age     
lower_bound_age <- q1_age - 1.5 * iqr_age    
upper_bound_age <- q3_age + 1.5 * iqr_age 
outliers_age <- fixed_mydata$person_age[fixed_mydata$person_age <lower_bound_age 
                                           | fixed_mydata$person_age > upper_bound_age]    
outliers_age 




#income
q1_income <- quantile(fixed_mydata$person_income, 0.25, na.rm = TRUE) 
q3_income <- quantile(fixed_mydata$person_income, 0.75, na.rm = TRUE) 
iqr_income <- q3_income - q1_income   
lower_bound_income <- q1_income - 1.5 * iqr_income    
upper_bound_income <- q3_income + 1.5 * iqr_income 
outliers_income <- fixed_mydata$person_income[fixed_mydata$person_income <  
                                                   lower_bound_income | fixed_mydata$person_income > upper_bound_income]    
outliers_income
#emp_exp
q1_exp <- quantile(fixed_mydata$person_emp_exp, 0.25, na.rm = TRUE) 
q3_exp <- quantile(fixed_mydata$person_emp_exp, 0.75, na.rm = TRUE) 
iqr_exp <- q3_exp - q1_exp    
lower_bound_exp <- q1_exp - 1.5 * iqr_exp  
upper_bound_exp <- q3_exp + 1.5 * iqr_exp 
outliers_exp <- fixed_mydata$person_emp_exp[fixed_mydata$person_emp_exp < 
                                                 lower_bound_exp | fixed_mydata$person_emp_exp > upper_bound_exp]  
outliers_exp 

#loan amount
q1_loan_amnt <- quantile(fixed_mydata$loan_amnt, 0.25, na.rm = TRUE) 
# quantile is a buil in function
q3_loan_amnt <- quantile(fixed_mydata$loan_amnt, 0.75, na.rm = TRUE) 

iqr_loan_amnt <- q3_loan_amnt - q1_loan_amnt   
lower_bound_loan_amnt <- q1_loan_amnt - 1.5 * iqr_loan_amnt   
upper_bound_loan_amnt <- q3_loan_amnt+ 1.5 * iqr_loan_amnt 
outliers_loan_amnt <- fixed_mydata$loan_amnt[fixed_mydata$loan_amnt <      
                                                lower_bound_loan_amnt | fixed_mydata$loan_amnt > upper_bound_loan_amnt]   
outliers_loan_amnt





q1_cre_score <- quantile(fixed_mydata$credit_score, 0.25, na.rm = TRUE) 
q3_cre_score <- quantile(fixed_mydata$credit_score, 0.75, na.rm = TRUE) 
# it tells you the value in certain percentage
iqr_cre_score <- q3_cre_score- q1_cre_score   
lower_bound_cre_score <- q1_cre_score- 1.5 * iqr_cre_score   
upper_bound_cre_score <- q3_cre_score + 1.5 * iqr_cre_score 
outliers_cre_score <- fixed_mydata$credit_score[fixed_mydata$credit_score < 
                                                     lower_bound_cre_score | fixed_mydata$credit_score > upper_bound_cre_score]   
outliers_cre_score 


summary(fixed_mydata$person_income)
s <- summary(fixed_mydata$person_income)
Q1 <- as.numeric(s["1st Qu."])
Q3 <- as.numeric(s["3rd Qu."])
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val
outlier_values <- fixed_mydata$person_income[
  fixed_mydata$person_income < lower_bound | fixed_mydata$person_income > upper_bound]
outlier_values




## outlier handling




fixed_mydata$person_income <- ifelse( 
 
fixed_mydata$person_income < lower_bound, round(lower_bound), 
 
ifelse(fixed_mydata$person_income > upper_bound, round(upper_bound), 
        
       fixed_mydata$person_income) 
 
)

# if incomes come below lower bond value replace it with round lower bound value





#emp_exp
fixed_mydata$person_emp_exp <- ifelse( 
  fixed_mydata$person_emp_exp < lower_bound_exp, round(lower_bound_exp), 
  ifelse(fixed_mydata$person_emp_exp > upper_bound_exp, round(upper_bound_exp), 
         fixed_mydata$person_emp_exp) 
) 
fixed_mydata$person_emp_exp 
#creditscore
fixed_mydata$credit_score <- ifelse( 
  fixed_mydata$credit_score < lower_bound_cre_score, round(lower_bound_cre_score), 
  ifelse(fixed_mydata$credit_score > upper_bound_cre_score, round(upper_bound_cre_score), 
         fixed_mydata$credit_score) 
) 
fixed_mydata$credit_score


#normalization
normalizeddata_income <- fixed_mydata
normalizeddata_income$person_income<- round((normalizeddata_income$person_income -
                                               min(normalizeddata_income$person_income, na.rm = TRUE)) /
                                              (max(normalizeddata_income$person_income, na.rm = TRUE) - 
                                                 min(normalizeddata_income$person_income, na.rm = TRUE)),2)
normalizeddata_income$person_income









#numerical to catagorical

fixed_mydata$loan_status <- factor( 
  fixed_mydata$loan_status, 
  levels = c(0, 1), 
  labels = c("No", "Yes") 
) 
fixed_mydata$loan_status



# Convert categorical to numerical
fixed_mydata$previous_loan_defaults_on_file <- ifelse(
  fixed_mydata$previous_loan_defaults_on_file == "Yes", 1, 0
)

fixed_mydata$previous_loan_defaults_on_file


#filter data
filtered_data <- filter(fixed_mydata, loan_status == "Yes")
filtered_data[, c("person_age", "person_gender", "loan_status")]

# for filter dplyr library needed

head(filtered_data)n# shows first 6 rows


# imbalance to balanced datasheet

str(fixed_mydata)
# checks datatype and values

imbalanced_data <- fixed_mydata %>%
  mutate(across(where(is.character), as.factor))# character is converted to factor(factor is group data)
#mutate is used to modify the column 
str(imbalanced_data)

library(ROSE)# used for SMOTE, and SMOTE is used for oversampling
#oversampling means add new samples where sample is less

set.seed(199)


table(imbalanced_data$loan_status)
# loan_status value imbalanced value

balanced_data <- ROSE(loan_status ~ ., 
                      data = imbalanced_data,  # we are working on imbalanced dataset
                      N = 400,  # Total row , it is better to double  
                      p = 0.5)$data   # probability ,half half yes and NO, minority 0.5



# Check new class distribution
table(balanced_data$loan_status)
balanced_data
balanced_data$person_age <- round(balanced_data$person_age) # points value age need to be discarded

#spliting datas for training and testing

set.seed(199) # for oversampling a random value


index <- sample(1:nrow(balanced_data), 0.7 * nrow(balanced_data))
#70 percent randomly choosen

train_data <- balanced_data[index, ] # 70 percen training sample stored in index
test_data  <- balanced_data[-index, ] # all rows except those are in index

train_data
test_data

table(train_data$loan_status)
table(test_data$loan_status)

#central tendency 

#loanintrate
mean_loan_int_rate   <- mean(fixed_mydata$loan_int_rate)
median_loan_int_rate <- median(fixed_mydata$loan_int_rate)
mode_loan_int_rate   <- mfv(fixed_mydata$loan_int_rate)


mean_loan_int_rate   
median_loan_int_rate 
mode_loan_int_rate   
summary (fixed_mydata)
#loanpercentincome

mean_loan_percent_income   <- mean(fixed_mydata$loan_percent_income)
median_loan_percent_income <- median(fixed_mydata$loan_percent_income)
mode_loan_percent_income   <- mfv(fixed_mydata$loan_percent_income)

mean_loan_percent_income   
median_loan_percent_income
mode_loan_percent_income   


# personhomeownership
mode_home_ownership <- mfv(fixed_mydata$person_home_ownership)

mode_home_ownership

# loanintent
mode_loan_intent <- mfv(fixed_mydata$loan_intent)
mode_loan_intent


# measureofSpread 

# personincome
range_income <- range(fixed_mydata$person_income, na.rm = TRUE)
iqr_income   <- IQR(fixed_mydata$person_income, na.rm = TRUE)
var_income   <- var(fixed_mydata$person_income, na.rm = TRUE)
sd_income    <- sd(fixed_mydata$person_income, na.rm = TRUE)

range_income
iqr_income
var_income
sd_income


# loanamnt
range_loan   <- range(fixed_mydata$loan_amnt, na.rm = TRUE)
iqr_loan     <- IQR(fixed_mydata$loan_amnt, na.rm = TRUE)
var_loan     <- var(fixed_mydata$loan_amnt, na.rm = TRUE)
sd_loan      <- sd(fixed_mydata$loan_amnt, na.rm = TRUE)

range_loan
iqr_loan
var_loan
sd_loan



