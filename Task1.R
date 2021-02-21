#Code by Rhythm Sanghi
#Linear Regression with R Language


#Install all packages that are required 
#install.packages("ggplot2")



# Importing all libraries required in this notebook
library(readxl)
library("ggplot2")


#Reading the data from the remote link
student_data <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
student_data

#To set names
names(student_data)<-data.frame("Hours","Scores")
x=student_data$Hours
y=student_data$Scores

#Plotting the distribution of scores 
plot(x, y, main="Hours vs Percentage",xlab = "Hours Studied",ylab = "Percentage Score")
#The inference from the graph:  positive linear relation between the number of hours studied and percentage of score.






# scores = a + (hours*b)
# a = intercept
# b = constant
# Scores is dependent, Hours is independent
#Scores=y, Hours=x 
lmstudent_data = lm(y~x , data = student_data)
lmstudent_data
summary(lmstudent_data)
scatter.smooth(x=student_data$Hours, y=student_data$Scores, main="Hours vs Percentage",xlab = "Hours Studied",ylab = "Percentage Score")  # scatterplot


#Now to make predictions we need to train our algorithm
dt = sort(sample(nrow(student_data), nrow(student_data)*.7))
train<-student_data[dt,]
test<-student_data[-dt,]
test
train


#Linear regression for the training data
lmstudent_data_train = lm(y~x , data = train)
lmstudent_data_train
summary(lmstudent_data_train)

#Testing Data 
new_data<-data.frame(x=test)
prediction_test=predict(lmstudent_data_train,newwdata=new_data)

#Comparing Actual vs predicted 
AvP<-data.frame(cbind(Actual=student_data$Scores,Predicted=prediction_test))
AvP
plot(AvP,col="blue")


#To find the Accuracy of the model 
correlation_accuracy <- cor(AvP$Actual,AvP$Predicted)
correlation_accuracy



#To test for the value=9.25
predicted_for_value <- predict.lm(lmstudent_data_train,newdata=data.frame(x=9.25))
predicted_for_value


#For evaluation the performance of the model we use r squared value 
preds <- AvP$Predicted
actual <- AvP$Actual
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq
#higher r-squared value indicates a better fit for the model






