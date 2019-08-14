# read the csv and specify all data types including factors with levels found
# in the accompanying documentation. Be sure that the ordinal Education has the 
# names listed in order from lowest to highest for easier conversion to a number.

library(readr)
bank <- read_delim("~/MiningBDS/Bank Customers Dataset/bank.csv",
";", escape_double = FALSE, col_types = cols(Class = col_factor(levels = c("no",
 "yes")), Default = col_factor(levels = c("no","yes")),
 Education = col_factor(levels = c("primary", "secondary", "tertiary")),
 Housing = col_factor(levels = c("no","yes")), Job = col_factor(levels = c("admin.",
 "unknown", "unemployed", "management", "housemaid", "entrepreneur", "student", 
 "blue-collar", "self-employed","retired", "technician", "services")),
 Loan = col_factor(levels = c("no","yes")), Marital = col_factor(levels = c("married",
"divorced", "single"))), trim_ws = TRUE)

# remove class           
bank$Class <- NULL

# make the ordinal and binary varibles into numbers (1,2,3 for Education and 0,1
# for Housing, Loan, and Default)

bank$Education <- as.numeric(bank$Education)
bank$Housing<-as.numeric(bank$Housing)-1
bank$Loan<-as.numeric(bank$Loan)-1
bank$Default<-as.numeric(bank$Default)-1

# add a row of ID numbers

bank$ID <- seq.int(nrow(bank))

# these are the future input values for the function we will run below.
# 1230, 5032, 10001, 24035, 28948, 35099, 37693, 39543, 40002, 42192

library(dplyr)

# create a function that will take the rowID as an input and return 
# the ten rows which are most similar to it.

matfunc <- function(rowID)
{
  
#the numeric variables take the absolute value of the difference
#between the variable we are looking for and all dataframe rows
#divided by the max-min of the numeric columns.  
  
  age_numeric <- matrix(abs(bank$Age[rowID]-bank$Age))/(max(bank$Age)-min(bank$Age))
                       
  balance_numeric <- matrix(abs(bank$Balance[rowID]-bank$Balance))/
                    (max(bank$Balance)-min(bank$Balance))
  
#the categorical variables' dissimilarity will be 1 if the 
#values are different, and zero if they are the same. 
  
  default_cat <- matrix(abs(bank$Default[rowID]-bank$Default)) 
  housing_cat <- matrix(abs(bank$Housing[rowID]- bank$Housing))
  loan_cat <- matrix(abs(bank$Loan[rowID]-bank$Loan))
  job_cat <- matrix(ifelse(bank$Job[rowID]!=bank$Job, 1, 0))
  marital_cat <- matrix(ifelse(bank$Marital[rowID]!=bank$Marital, 1, 0))

#the ordinal variable is the absolute value of the difference between the current
#ID and all other rows divided by the max-min values of the ordinal column.
  
  education_ord <- matrix(abs(bank$Education[rowID]-bank$Education)/
                  (max(bank$Education)-min(bank$Education)))

#average all of the calculations to find the total dissimilarity 
  
  bank$Dissimilarity <- matrix((age_numeric + balance_numeric + default_cat + housing_cat + loan_cat + 
                       job_cat + marital_cat + education_ord)/8)

#return the 10 rows with the smallest dissimilarity. Specify to print 11 because
#this will include the ID we are looking at
  
  top_n(bank, -11, bank$Dissimilarity)
}

# run the function with each of the 10 IDs and view the results

matfunc(1230)

matfunc(5032)

matfunc(10001)

matfunc(24035)

matfunc(28948)

matfunc(35099)

matfunc(37693)

matfunc(39543)

matfunc(40002)

matfunc(42192)
