#A/B TESTING RESULTS ANALYSIS

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

setwd("D:/Current_Project")

ab_data = read.csv("ab_data.csv")
head(ab_data)
cont = read.csv("countries.csv")

head(cont)

#MERGING THE TWO DATA SETS "ab_data" and "country"

data0 = merge(ab_data, cont, by.x = "user_id",by.y = "user_id",all=TRUE)

View(data0)

#NUMBER OF USERS IN DATASET
nrow(data0)

#NUMBER OF UNIQUE USERS IN DATASET
nrow(distinct(data0, user_id, .keep_all = TRUE))

#AT FIRST WE NEED TO REMOVE THE DATA IS WHICH DECREASES THE ACCURACY OF OUR ANALYSIS
#HERE WE HAVE 2 DATA ROWS OF SOME USERS WHICH CREATES A CONFUSION WEATHER THIS USER RECIEVED NEW PAGE OR OLD PAGE

temp3 = filter(data0, (group == "treatment"& landing_page == "new_page") | (group == "control"& landing_page == "old_page"))
#View(temp3)
#nrow(temp3)

# BY USING THIS METHOD OF REMOVING ONLY THOSE WHICH HAS IRREGULAR COMBINATIONS WE ARE REMOVING 1 DATA ROW
# OF SUCH USERS WHICH HAS 2 DATA ROW
# LET'S CHECK IF THEIR IS ANY USER_ID LEFT WITH 2 DATA ROWS IN TEMP3?
B = temp3[duplicated(temp3$user_id),]$user_id

#THE DUPLICATE USER ID MAY CHANGE FROM DATA TO DATA SO STORING DUPLICATE 
#USER ID VALUES IN A VARIABLE B
temp3[temp3$user_id == B,]

#View(temp4)

# HERE WE FOUND THAT THEIR IS ONE USER WHICH HAS COMBINATION OF GROUP='TREATMENT' AND LANDING_PAGE='NEW_PAGE'
# WE HAVE TO REMOVE ONE ROW FROM THIS ONE ALSO BECAUSE WHAT I BELIEVE AND ACCORDING TO SAMPLING METHODS SIMPLE 
#RANDOM SAMPLING WITHOUT REPLACEMENT IS BETTER THAN SIMPLE RANDOM SAMPLING WITH REPLACEMENT

A = which(temp3$user_id == B,arr.ind = TRUE)#STORING THE INDEX VALUES TO A VARIABLE A
A
data1 = temp3[-A[1:(length(A)-1)], ]
#THE INDEX MAY CHANGE FROM ONE SYSTEM TO OTHER SO WE USE 
#View(data1)

#AFTER REMOVING THE DATA WHICH NEEDS TO BE REMOVED WE HAVE TO CHECK FOR NULL VALUES

anyNA(data1)

#NOW WE CAN WORK WITH OUR DATA

#THE PROBABILITY OF CONVERTING REGARDLESS OF PAGE IS 

Cnvrt_Prob = mean(data1$converted == 1)
Cnvrt_Prob
cat("The probability of an individual converting regardless of the page they receive is:",Cnvrt_Prob)

#THE PROBABILITY OF CONVERSION OF TREATMENT GROUP 
#AS CONVERTED COLUMN HAS BINARY 0 AND 1 ONLY WE CAN MEAN FUNCTION
temp5 = data1[data1$group == 'treatment',]
TR_Cnvrt_Prob = mean(temp5$converted)
TR_Cnvrt_Prob

#THE PROBABILITY OF CONVERSION OF CONTROL GROUP 
temp6 = data1[data1$group == 'control',]
CON_Cnvrt_Prob = mean(temp6$converted)
CON_Cnvrt_Prob

Obs_Diff = TR_Cnvrt_Prob-CON_Cnvrt_Prob
Obs_Diff

#HERE WE CONCLUDE THE PROBABILITY OF CONVERSION OF BOTH THE PAGES ARE ALMOST SAME (NOT MUCH DIFFERENCE) 
#i.e. 0.001578239 AND EVEN THE OLD_PAGE LEADS IN CONVERSION SLIGHTLY
#TR_Cnvrt_ProB -  0.1188081
#CON_Cnvrt_Prob - 0.1203863
#SO THERE IS NOT SUFFIECIENT EVIDENCE PRESENT TO SAY THAT NEW_PAGE LEADS TO MORE CONVERSION

#_________________________________________________________________________________________________________

#HYPOTHSIS TESTING

#HERE WE TAKE NULL HYPOTHESIS AS
#H0:TR_Cnvrt_Prob - CON_Cnvrt_Prob<=0
#H1:TR_Cnvrt_Prob - CON_Cnvrt_Prob>0

#WE WILL ASSUME THAT OLD_PAGE IS BETTER UNLESS THE NEW_PAGE PROVES TO BE DEFINITELY BETTER AT A TYPE I ERROR 
#OF 5%
#ALSO WE ASSUMED THAT THEY ARE EQUAL TO Cnvrt_Prob
p_new = Cnvrt_Prob
p_old = Cnvrt_Prob

n_New = nrow(filter(data1,landing_page == "new_page"))
n_New

n_Old = nrow(filter(data1,landing_page == "old_page"))
n_Old

#STIMULATE DIFFERENCES IN CONVERSION RATES FOR NULL HYPOTHESIS

p_diffs = 0
for (i in 1:10000)
{
  new_page_cnrt = rbinom( n_New, size = 1, prob = c(p_new,1-p_new))
  old_page_cnrt = rbinom( n_Old, size = 1, prob = c(p_old,1-p_old))
  p_diffs = append( p_diffs, mean(new_page_cnrt) - mean(old_page_cnrt))
}

hist( p_diffs,col = "blue")
abline( v = Obs_Diff, col = "red")

mean( p_diffs >= Obs_Diff )

#HERE OUR P VALUE WE GET IS 0.9062094 WHICH IS GREATER THAN 0.05 (OUR ALPHA) SO WE CAN'T REJECT OUR
#NULL HYPOTHESIS

#WE CAN ALSO USE THE FUNCTIONS TO TEST POSSIBLE REJECTION OF OUR NULL HYPOTHESIS
#FOR THIS WE WILL USE prop.Test() FUNCTION

Num_Cnvrt_New = nrow(filter(data1,group == "treatment"& converted == 1))
Num_Cnvrt_New

Num_Cnvrt_Old = nrow(filter(data1,group == "control"& converted == 1))
Num_Cnvrt_Old

n_New
n_Old



prop.test(x = c( Num_Cnvrt_New, Num_Cnvrt_Old ), n = c( n_New, n_Old ), 
          p = NULL,alternative = "greater",conf.level = 0.95, correct = TRUE)

#HERE WE GET P VALUE 0.9041 WHICH IS ALMOST EQUAL TO WHAT VALUE WE GOT EARLIER
#SO WE CAN'T REJECT THE NULL HYPOTHESIS

#__________________________________________________________________________________________

#REGRESSION APPROACH

#SINCE THE "CONVERTED" COLUMN i.e. DEPENDENT VARIABLE HAS ONLY TWO VALUES 0's AND 1's, SO WE WILL 
#USE LOGISTIC REGRESSION FOR THIS


#install.packages(fastDummies)
library(fastDummies)

data1$ab_page = ifelse(data1$landing_page == "new_page",1,0)
View(data1)

model_1 = glm(converted ~ ab_page, family = binomial(link="logit"), data = data1)
summary(model_1)

#THE P VALUE WE GET FROM THIS MODEL IS 0.190 WHICH IS STILL GREATER THAN 0.05 SO WE STILL CAN'T
#REJECT THE NULL HYPOTHESIS

#SINCE WE HAVE ONE MORE ATTRIBUTE "COUNTRY" AND WE NEED TO FIND WHICH PAGE WORKS BETTER IN WHICH
#COUNTRY SO THAT WE CAN IMPLIMENT THAT PAGE IN THAT PERTICULAR COUNTRY

#COUNTRIES WE NEED TO COVER

unique(data1$country)

#WE HAVE THREE COUNTRIES "US", "UK", "CA"
#WE HAVE TO CREATE DUMMY VARIABLES FOR THESE COUNTRIES

View(data1)


model_2 = glm(converted ~ country, family = binomial(link = "logit"),data = data1)
summary(model_2)

#FOR CHECKING IF IS THERE IS ANY EFFECT OF LANDING PAGE WITH COUNTRY ON CONNVERSION

model_3 = glm(converted ~ country * ab_page, family = binomial(link = "logit"),data = data1)
summary(model_3)
