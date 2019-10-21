#install.packages('mice')

library(mice)#used to predict the missing values 
library(gmodels)
library(e1071)
#install.packages("randomForest")
library(randomForest)
#step1:load the data 
diabetes_data<-read.csv(file.choose())

str(diabetes_data)


table(diabetes_data$Outcome)



#step2:explore the data 
#Convert '0' values into NA as no one can have thos factors as null
diabetes_data[, 2:7][diabetes_data[, 2:7] == 0] <- NA


  str(diabetes_data)



#predict  missing values 

mice_mod <- mice(diabetes_data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)




#Transfer the predicted missing values into the main data set
diabetes_data$Glucose <- mice_complete$Glucose
diabetes_data$BloodPressure <- mice_complete$BloodPressure
diabetes_data$SkinThickness <- mice_complete$SkinThickness
diabetes_data$Insulin<- mice_complete$Insulin
diabetes_data$BMI <- mice_complete$BMI

str(diabetes_data)

diabetes_data$Outcome<- factor(diabetes_data$Outcome, levels = c("ND", "D"),
                             labels = c( "non_diabetic","diabetic"))


print(diabetes_data$Outcome)

round(prop.table(table(diabetes_data$Outcome)) * 100, digits = 1)








#normalizing the data 
# create the  normaize  fucntion to normalize values  irrespective of how big it is
normalize<-function(x){
  return ((x - min(x)) / (max(x) - min(x)))}

#last  feature is elimated as  it is the target 
diabetes_ds_n<- as.data.frame(lapply(diabetes_data[1:8], normalize))



#creating the train and test dataset for diabetes 
diabetes_train <- diabetes_ds_n[1:614,]
diabetes_test <- diabetes_ds_n[615:768,]


diabetes_train_labels <- diabetes_data[1:614, 9]
diabetes_test_labels <- diabetes_data[615:768, 9]


prop.table(table(diabetes_train_labels))
prop.table(table(diabetes_train_labels))

diabetes_classifier <- naiveBayes(diabetes_train, diabetes_train_labels)


diabetes_test_pred <- predict(diabetes_classifier, diabetes_test)



CrossTable(diabetes_test_pred, diabetes_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))


#using the zscore tranformation in order to improve the predictive accuracy 

#using the scale () to achieve that 
diabetes_z <- as.data.frame(scale(diabetes_data[-9]))


#creating the  train and  the test data set 
diabetes_z_train <- diabetes_z[1:614, ]
diabetes_z_test <- diabetes_z[615:768, ]
diabetes_z_train_labels <- diabetes_data[1:614, 9]
diabetes_z_test_labels <- diabetes_data[615:768, 9]



diabetes_classifier_Z <- naiveBayes(diabetes_z_train, diabetes_z_train_labels)
diabetes_test_pred_Z <- predict(diabetes_classifier_Z, diabetes_z_test)



CrossTable(diabetes_test_pred_Z, diabetes_z_test_labels,prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))

diabetes_classifier_Z_l <- naiveBayes(diabetes_z_train,diabetes_z_train_labels ,laplace = 1)
diabetes_test_pred_Z_l <- predict(diabetes_classifier_Z_l, diabetes_z_test)
CrossTable(diabetes_test_pred_Z_l,diabetes_z_test_labels ,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))


