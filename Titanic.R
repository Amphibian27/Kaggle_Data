test.data<-read.csv('C:/Users/Sysnet/Desktop/kaggle/test.csv')
train.data<-read.csv('C:/Users/Sysnet/Desktop/kaggle/train.csv')
summary(train.data)
boxplot(train.data[,c(6,10)],ylim=c(0,100))

barplot(height = train.data$Fare,width = train.data$Gender)
plot(density(train.data$Fare, na.rm = TRUE))
library(ggplot2)
ggplot(data = train.data,aes(x=Fare))+geom_histogram(binwidth = 10)
qplot(data = train.data,Age,binwidth=5)+facet_wrap(~Survived)
counts<-table(train.data$Survived,train.data$Sex)
sum(train.data$Fare>100,na.rm = T)
train.data$aukaat<-ifelse(train.data$Fare>100,"High","Low")
barplot(counts,legend.text = c(0,1))
#Survival calculation on the basis of Gender
counts[2]/(counts[1]+counts[2])
counts[4]/(counts[3]+counts[4])
#On the basis of class
counts_class<-table(train.data$Survived,train.data$Pclass)
barplot(counts_class)
a.class<-counts_class[2]/(counts_class[1]+counts_class[2])
b.class<-counts_class[4]/(counts_class[3]+counts_class[4])
c.class<-counts_class[6]/(counts_class[5]+counts_class[6])
#on the basis of age
train.data$Child<-ifelse(train.data$Age<=12,1,2)
counts_all<-table(train.data$Survived,train.data$age_c,train.data$Sex,train.data$Pclass)

barplot(counts_all)
#Replacing the unnecessary columns so that we can go for our model fit
train.data<-train.data[,-c(1,9,12)]
#Now for model fit we need to change the character value to numeric Lets do it
train.data$Sex<-gsub("female",1,train.data$Sex)
train.data$Sex<-gsub("male",2,train.data$Sex)
train.data$age_c<-gsub("Child",1,train.data$age_c)
train.data$age_c<-gsub("Young",3,train.data$age_c)
train.data$age_c<-gsub("senior",3,train.data$age_c)
master_vector = grep("Master.",train.data$Name,fixed = T)
mr_vector<-grep("Mr.",train.data$Name,fixed = T)
miss_vector<-grep("Miss.",train.data$Name,fixed = T)
mrs_vector<-grep("Mrs.",train.data$Name,fixed = T)
dr_vector<-grep("Dr.",train.data$Name,fixed = T)
train.data$Name<-as.character(train.data$Name)
train.data[master_vector,]$Name <- "Master"
train.data[mr_vector,]$Name <- "Mr"

train.data[miss_vector,]$Name <- "Miss"
train.data[mrs_vector,]$Name <- "Mrs"
train.data[dr_vector,]$Name <- "Dr"
master<-round(mean(train.data$Age[train.data$Name=="Master"],na.rm = T),2)
mrs<-round(mean(train.data$Age[train.data$Name=="Mrs"],na.rm = T),2)
mr<-round(mean(train.data$Age[train.data$Name=="Mr"],na.rm = T),2)
miss<-round(mean(train.data$Age[train.data$Name=="Miss"],na.rm = T),2)
dr<-round(mean(train.data$Age[train.data$Name=="Dr"],na.rm = T),2)


#imputing null values with meaning values\
for(i in 1:nrow(train.data)){
if(is.na(train.data[i,5])){
  if(train.data$Name[i]=="Master"){
    train.data$Age[i]<-master
  }else if(train.data$Name[i]=="Mrs"){
      train.data$Age[i]<-mrs
    }else if(train.data$Name[i]=="Mr"){
        train.data$Age[i]<-mr
      }else if(train.data$Name[i]=="Miss"){
          train.data$Age[i]<-miss
        }else if(train.data$Name[i]=="Dr"){
            train.data$Age[i]<-dr
        }else { print("Uncaught Title")}
}
}
train.data<-train.data[,-c(10,11)]
#Add more variables 
train.data$Child<-ifelse(train.data$Age<=12,1,2)
train.data$Family<-train.data$SibSp+train.data$Parch+1
train.data$Mother<-ifelse(train.data$Name=="Mrs" & train.data$Parch>0,1,2)

#Now we are pretty much done with our train data now its the time for Test data
#Repeat the same process with our test data also

test.data<-test.data[,-c(1,8,11)]
#Defining the name with the title for the age thing
master_vectorT<-grep("Master",test.data$Name,fixed = T)
mr_vectorT<-grep("Mr.",test.data$Name,fixed = T)
miss_vectorT<-grep("Miss.",test.data$Name,fixed = T)
mrs_vectorT<-grep("Mrs.",test.data$Name,fixed = T)
dr_vectorT<-grep("Dr.",test.data$Name,fixed = T)

#Putting the names
test.data$Name<-as.character(test.data$Name)
test.data$Name[master_vectorT]<-"Master"
test.data$Name[mr_vectorT]<-"Mr"
test.data$Name[miss_vectorT]<-"Miss"
test.data$Name[mrs_vectorT]<-"Mrs"
test.data$Name[dr_vectorT]<-"Dr"


#Lets find the mean of ages now and impute for missing values
masterT<-round(mean(test.data$Age[test.data$Name=="Master"],na.rm = T),2)
mrT<-round(mean(test.data$Age[test.data$Name=="Mr"],na.rm = T),2)
missT<-round(mean(test.data$Age[test.data$Name=="Miss"],na.rm = T),2)
mrsT<-round(mean(test.data$Age[test.data$Name=="Mrs"],na.rm = T),2)
drT<-round(mean(test.data$Age[test.data$Name=="Dr"],na.rm = T),2)
#Now finally impute for missing value
for(i in 1:nrow(test.data)){
  if(is.na(test.data[i,4])){
    if(test.data$Name[i]=="Master"){
      test.data$Age[i]<-masterT
    }else if(test.data$Name[i]=="Mr"){
      test.data$Age[i]<-mrT
    }else if(test.data$Name[i]=="Miss"){
      test.data$Age[i]<-missT
    }else if(test.data$Name[i]=="Mrs"){
      test.data$Age[i]<-mrsT
    }else if(test.data$Name[i]=="Dr"){
      test.data$Age[i]<-drT
    }else {
      print("no value to impute")
      }
  }
}

#Now assign new variables and change the character to numeric

test.data$Sex<-ifelse(test.data$Sex=="male",2,1)
test.data$Child<-ifelse(test.data$Age<=12,1,2)
test.data$Family<-test.data$SibSp+test.data$Parch+1
test.data$Mother<-ifelse(test.data$Name=="Mrs" & test.data$Parch>0,1,2)

test.data<-test.data[,-c(7,8)]
train.data<-train.data[,-c(8,9)]


#Its time to fit your model guys we gonna move ahead with logistic regression since the data is binary
train.glm<-glm(Survived~Pclass+Sex+Age+Sex*Pclass+Child+Family+Mother,data = train.data,family = binomial)
summary(train.glm)
p.hats<-predict.glm(train.glm,newdata = test.data,type = "response")
train.data$Sex<-as.numeric(train.data$Sex)
sum(is.na(p.hats))
survival<-vector()

for(i in 1:length(p.hats)){
  if(p.hats[i] > 0.5){
    survival[i]<-1
  }else{
    survival[i]<-0
  }
  }

p.hats[is.na(p.hats)]
test.data$Name["O'Donoghue, Ms. Bridget"]
test.data[89,7]<-2

Kaggle_F<-cbind(test.data$PassengerId,survival)
colnames(Kaggle_F)<-c("PassangerId","Survival_Status")
write.csv(Kaggle_F,file = "SurvivalA.csv",row.names = F)
getwd()
#The end-----------------------------------------------------------