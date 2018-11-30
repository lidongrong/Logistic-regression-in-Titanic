#Data can be found on https://www.kaggle.com/c/titanic/data, the following is just an example

train=read.csv("c:/Users/a/desktop/train.csv")
test=read.csv("c:/Users/a/desktop/test.csv")

#We first import the data. It is clear that name has nothing to do with survival, thus we don't care about
#the name variable.
Survived=train$Survived
Class=train$Pclass
Sex=train$Sex
Age=train$Age
SibSp=train$SibSp
Parch=train$Parch
Fare=train$Fare
Embarked=train$Embarked
Cabin=train$Cabin
Ticket=train$Ticket

#Since Ticket,Cabin variables are too complecated, we also leave it out.
Interested=data.frame(Survived,Class,Sex,Age,SibSp,Parch,Fare,Embarked)



#some points in Fare are not in our favor. Thus, we transform them:
Fare=log(Fare)
Ship=data.frame(Survived,Class,Sex,Age,SibSp,Parch,Fare,Embarked)

#Now we clean the data, leave out all data with NA or NaN

#We use vector Kill to store the lines that have to be deleted
Kill=vector(mode="numeric")

for(i in 1:length(Ship[,2])){
  for(j in 1:length(Ship[1,])){
    if(is.na(Ship[i,j])|is.nan(Ship[i,j])|is.infinite(Ship[i,j])){
      Kill=c(Kill,i)
      break
    }
  }
}

#We now delete the lines
for(k in 1:length(Kill)){
  Ship=Ship[-(Kill[k]-(k-1)),]
}

#Regress
TitanicModel=glm(formula=Survived~Class+Sex+Age+SibSp+Parch+Fare+Embarked,family=binomial,data=Ship)
summary(TitanicModel)
```

#This is the full model, now we begin to select the best model based on AIC.

#We wish to find the smallest AIC. First, we set up a very huge original AIC
FinalAic=1000000000000000000000000000


#Since there are no many parameters, we can compare AIC among all submodels
Index=2:(length(Ship[1,]))


#Count the AIC of each model,and find the one with the smallest AIC
for(k in 1:6){
  Combination=combn(Index,k)
  
  for(i in 1:length(Combination[1,])){
    
    Variables=Combination[,i]
    SubData=data.frame(Ship[,Variables])
    
    Model=glm(formula=Ship[,1]~.,family=binomial,data=SubData)
    ac=AIC(Model)
    
    
    
    
    #always record the smallest aic and the model
    if(ac<FinalAic){
      FinalAic=ac
      FinalModel=Model
      
    }
  }
}


summary(FinalModel)





#Predict the result
Predicted=predict(FinalModel,data.frame(Class=test$Pclass,Sex=test$Sex,Age=test$Age,SibSp=test$SibSp))
Predicted

#Count the probabilty for survival
PreSurvived=exp(Predicted)/(1+exp(Predicted))



#Get the true surviving condition. If one has a survival probability>0.7, we take he/she survives.
for(i in 1:length(PreSurvived)){
  if(!is.na(PreSurvived[i])){
    if(PreSurvived[i]>=0.7){
      PreSurvived[i]=1
    }
    else{
      PreSurvived[i]=0
    }
  }
}

#See the accuracy of the model
GenderSubmission=read.csv("c:/Users/a/desktop/gender_submission.csv")

TrueSurvival=GenderSubmission$Survived

#calculate the accuracy rate. total=total number of subjects. survival=accurately predicted subjects
total=0
survival=0
for(i in 1:length(PreSurvived)){
  if(!is.na(PreSurvived[i])){
    if(PreSurvived[i]==TrueSurvival[i]){
      surviva=survival+1
      
      survival=survival+1
    }
    total=total+1
  }
}

#Print the accuracy rate
survival/total
