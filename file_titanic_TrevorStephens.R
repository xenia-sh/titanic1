#Titanic dataset, solution by Trevor Stephens 

#first, files are imported (using "Import Dataset")

str(train)
#lookint at the dataset 

table(train$Survived)
#out of 891 people in the train dataset we now know that 549 did not survive and 342 survived 

prop.table(table(train$Survived))
#now we see the percentages of survived and not survived instead of values 

test_new$Survived <- rep(0,418)
#adding variable Survived which is ) for everyone

submit <- data.frame(PassengerId=test_new$PassengerId, Survived = test_new$Survived)
#the file that is submitted will be with 62% accoracy because 62% of people really did not survive

#step 0 - improvement begins. gender-class model 

summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
#the percentages are for each category (all 4 figures in the table are summed up into one)
prop.table(table(train$Sex, train$Survived),1)
#values for female sum up to 1, and male also sum up to 1 

test_new$Survived <- 0  #everyone to zero 
test_new$Survived[test_new$Sex == 'female'] <-1
#the prediction accuracy is 76,5% now

summary(train$Age)
#the minimum, maximum, median, 177 values which are Not identified 

train$Child <-0 
train$Child [train$Age < 18] <-1 
#everyone is a child who is less than 18 years old 

aggregate (Survived~Child + Sex, data=train, FUN=sum)
#we want to have a break down not only on female and male, but on children and male and female, to see who survived 
#this is the number of survivors 

aggregate (Survived~Child + Sex, data=train, FUN=length) 
#the number of people in each dataset, survived or not

aggregate (Survived~Child + Sex, data=train, FUN=function (x) {sum(x)/length(x)})
#the percentage of survivors in each category 

#splitting fares into categories 
train$Fare2 <- '30+' 
#everything is now above 30 dollars 
train$Fare2[train$Fare <30 & train$Fare >=20] <- '20-30'
#the category between 20 and 30 is created 
train$Fare2[train$Fare <20 & train$Fare >=10] <- '10-20'
#the category between 10 and 20 is created 
train$Fare2[train$Fare <10] <- '<10' 
#the category lower than 10 is created 

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function (x) {sum(x)/length(x)})
#females who paid above 20 have much lesser chances of survival (miss on a lifeboat) 
#there's a visible drop in survival status

test_new$Survived <-0
test_new$Survived[test_new$Sex == 'female'] <-1
test_new$Survived[test_new$Sex == 'female' & test_new$Pclass ==3 & test_new$Fare >=20] <-0 
#78% of accuracy which is nice 

library(rpart)
#Recursive partitioning and decision trees 

?rpart
#anova - when prediction is a continuous variable 
#poisson - when prediction is 2 variables? 
#class - when prediction just 0 or 1 
#exp - survival object 

fit  <- rpart(Survived ~ Pclass + Sex + Age +SibSp +Parch +Fare + Embarked, data=train, method="class")
#fit is the model that we've built on all(?) the variables that we have
#actually not all, there's name, id, ticket id 
#also we're using not the categories of fares that we made but original fares
#also nothing about mothers and children mentioned earlier 
plot(fit) 
text(fit)

install.packages('rattle')
install.packages('rpart.plot') 
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
fancyRpartPlot(fit, main="Decision Tree", cex=0.7) 
#this is how the font becomes readable, in first option it's not readable and zoom in does not help
#we now see the conditions that make passengers less/more likely to survive 
#these are, roughly speaking, sex, fare, age (younger children have higher chance, class and point of embarkation (for women though))

Prediction <-predict(fit, test_new, type="class")
submit <-data.frame(PassengerId = test_new$PassengerId, Survived = Prediction)
#this prediction is 78% (slightly higher than the previous one)

#the essence is that now we continue splitting to groups of 2 (tree "goes" to the very bottom)
#cp is the metric stopping splits which are not important enough (and now we're setting it to 0
fit <- rpart(Survived ~ Pclass + Sex +Age+SibSp+Parch+Fare+Embarked, data=train, method="class", control =rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit) 

#R is warning that labs do not fit and there may be some overplotting 
#and it's not wrong 
#accuracy has fallen to about 74% 

#feature engineering begins
train$Name[1]

test_new$Survived <-NA 
#they don't have survived anyway so we've made them NA 
test_new$Child<-NA
test_new$Fare2 <-NA
combi <- rbind(train, test_new)
#now they do have the same number of columns so bind. hoho

#strings are automatically imported as factors 
#casting back into string so that could split them later 

combi$Name <-as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')[[1]] #the whole string
strsplit(combi$Name[1], split='[,.]')[[1]][2] #the second element (meaning the title)

#we can't just copy it for the whole Title column because they would all be same 
#need to use sapply 
combi$Title <-sapply(combi$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})
#eliminating empty spaces 
combi$Title <- sub(' ', '', combi$Title) 
table(combi$Title)

#same as in Megan solution - combining titles into categories 
#because some of them are quite rare 
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1 
#the size of the family - the person itself+parents+children 

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#extracting the Surname from the full name of the passenger

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="") 
#surname and size of the family 

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
#small families are marked (because there's a lot of them)

table(combi$FamilyID)
#1025 small families 

famIDs <- data.frame(table(combi$FamilyID)) 
#family sizes and surnames in a separate table

famIDs <- famIDs[famIDs$Freq <= 2,]
#dataframe should show unexpectedly small family ID groups and not all of them 
#the previous table is of those who did not work with the assumptions - number of people and frequencies

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
#this is overwriting families whose number of people do not correspond 
#and factoring it 

train <- combi[1:891,]
test <- combi[892:1309,]
#splitting the dataset again 

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class") #constructing the model again 
fancyRpartPlot(fit,main="Decision Tree", cex=0.5) #representing the model in a graphic form 
#title, fare and point of embarkation (as well as combination of family size and family surname) are the most important 
#the score of the model is 79.426%

#random forest
summary(combi$Age) 
#263 people do not have the age 

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], #those who do not have the age  
                method="anova")  #because it's not yes or no but age has to be predicted 
#predicting the age using other variables (same model as for Survived previously) 
#family ID is left out 
fancyRpartPlot(Agefit,main="Decision Tree", cex=0.5)

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
#actually replace the missing age values 

summary(combi)
#summary of the dataset 
#2 people are missing embarked, and 1 fare 

which(combi$Embarked == '')
#now we find out that those who are empty are 62 and 830 (instead of looking through the entire dataset)

combi$Embarked[c(62,830)] = "S" 
#just because majority of passengers embarked there (914 from 1309) 
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))
#now we find which one misses fare (1044) 
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
#replacing it by median 

#we want to cut off not 2 but 3 people families as small 
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
#factor -> string (through as.character) -> factor 

library(randomForest)
set.seed(415)   
#results are reproducible (otherwise there will be different classifications) 

fit <- randomForest(as.factor(Survived) ~ Pclass + Age + Sex  + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, #importance of variables 
                    ntree=2000) #number of trees we want to grow 

#train <- combi[1:891,]
#test <- combi[892:1309,]
#splitting the dataset again 

#problem is the age - somehow the model before does not replace values across the dataset - because I forgot the line with the replacement after modeling age values

varImpPlot(fit)
#plotting the importance of variables
#by far the most important is title then fare 
#then class, age and sex (also family surname with the number of people in it)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#the prediction made, its accuracy is about 77% which in fact is not an improvement of the best score 

#last part and last attempt on improving the score 
install.packages('party')
library(party) 

set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))  
#the same model as in random forest library, but a different method and different library

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#prediction using the model constructed 
#although it's longer than random forest 

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Program Files/R/Titanic/forest_TrevorS.csv", row.names = FALSE)

#0.80861 of accuracy for me (unlike 81 and something as it is for Trevor) and this is the end of the solution 



