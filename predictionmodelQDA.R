library(magrittr)
library(MASS)




#read in our data
df<-read.csv("C:/Users/jiahe/OneDrive/Desktop/stat learning hmwk/AI 2022/GPAClean.csv",header=TRUE, sep=",")
df <- df[,2:7]




#Create a for loop to modify College GPA into a categorical variable
gpacat = rep(NA, 1000)

for (i in 1:1000){
  if (df$c_gpa[i] > 3.5){
    gpacat[i]=2
  }
  if (df$c_gpa[i] <=3.5 && df$c_gpa > 2.7){
    gpacat[i]=1
  }
  if (df$c_gpa[i] <= 2.7){
    gpacat[i]=0
  }
  
}

df$c_gpa = gpacat




#split our modified dataset into test and train

testdat<-df[700:1000,]
traindat<-df[1:699,]

#fit our inital model using only train data
Qmodel <- qda(traindat$c_gpa~. , data = traindat)



#test the data

testQDA = as.numeric(predict(Qmodel, newdata= testdat[1:5])$class)

ErrorRate = mean(testQDA != testdat$c_gpa)
ErrorRate
#error is only 20 percent




##Enter the data of a hypothetical student wanting to use AcademicGatorAID
##Multiple students can be entered in as a dataframe with classifications save under
##a new column for ease of use

newstudent<- data.frame(ACT = 20, SAT =-0.8, PHLE = 0, p_income = 20000, hs_gpa= 2.0)

testnewst = as.numeric(predict(Qmodel, newdata= newstudent)$class)

#Here is your classification!
testnewst

#record the prediction in new column
newstudent$pred = testnewst
newstudent

