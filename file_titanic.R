#loading packages
#solution by Meghan Risdal, Kaggle
library('ggplot2')
library(ggplot2)
library('ggplot2')
install.packages('ggplot2', dependencies = TRUE)
library('ggplot2')
library('ggthemes')
install.packages('ggthemes', dependancies=TRUE)
install.packages('ggthemes', dependencies=TRUE)
library('ggthemes')
library('scales')
library('dplyr')
install.packages('dplyr', dependenies=TRUE)
#dplyr is for data manipulation
library ('mice')
install.packages('mice', dependencies=TRUE)
library('randomForest') #classification and prediction?
train <-read.csv('C:\Users\ADM\Downloads\train.csv', stringsAsFactors = F) #read and input the train file
train <-read.csv('C:\|Users\ADM\Downloads\train.csv', stringsAsFactors = F) #read and input the train file
train <-read.csv('C:\\Users\ADM\Downloads\train.csv', stringsAsFactors = F) #read and input the train file
train <-read.csv('C:/Users/ADM/Downloads/train.csv', stringsAsFactors = F) #read and input the train file
View(train)
View(train)
test <-read.csv('C:/Users/ADM/Downloads/est.csv', stringAsFactors=F) #read and input the test file
test <-read.csv('C:/Users/ADM/Downloads/test.csv', stringAsFactors=F) #read and input the test file
test <-read.csv('C:/Users/ADM/Downloads/test.csv', stringAsFactors = F) #read and input the test file
test <-read.csv('C:\Users\ADM\Downloads\test.csv', stringAsFactors = F)
test <-read.csv('C:/Users/ADM/Downloads/test.csv', stringAsFactors = F)
test <-read.csv('C:/Users/ADM/Downloads/test.csv', stringAsFactors = F)
test <-read.csv('C:/Users/ADM/Downloads/test.csv')
View(test)
View(train)
View(test)
View(test)
View(test)
test.remove
file.remove(test)
file.remove('test.csv')
file.remove(test)
file.remove(C:/Users/ADM/Downloads/test.csv)
View(test)
View(test)
file.remove(test)
file.remove('test.csv')
test1 <-read.csv('C:/Users/ADM/Downloads/test.csv')
View(test1)
View(test1)
test2 <-read.csv('C:/Users/ADM/Downloads/test(1).csv')
test2 <-read.csv('C:/Users/ADM/Downloads/test (1).csv')
View(test2)
View(test2)
View(test1)
file.remove('test1.csv')
file.remove('test1')
rm(test1)
rm(test)
#at last I've found how to remove files!
full <-bind_rows(train, test2)
View(full)
View(full)
str(full)
#feature engineering - essentially it's finding new field from existing data (like titles from names)
full$Title <-gsub('(.*, )|(\\..*)', '', full$Name)
#title from the name - everything before the comma?
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer') #new variable gathering all the rare ones
full$Title[full$Title == 'Mlle']  <- 'Miss'
#mlles become misses because there are only 2 of them
full$Title[full$Title == 'Ms']  <- 'Miss'
#Ms become Miss
full$Title[full$Title == 'Mme']  <- 'Mrs'
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
#everything under rare title will be rare title. that's why they have been united previously
table(full$Sex, full$Title)
full$Surname <-sapply(full$Name, function(x) strsplit(x, split='[,.]')[[1]][1])
#the purpose is to take the surname from the full name (split the string and remove the intervals)
cat(paste ('we have <b>', nlevels(factor(full$Surname)), '/<b> unique surnames.'))
#why do we need to add <b> and /<b>
?
  view(test)
fdf
#now we have titles and surnames
#the next step is families
#family suze is typically the person + siblings + children
full$Size <- full$SibSp+full$Parch+1
full$Family <-paste (full$Surname, full$Size, sep='_')
#add the surname and the size
#we have to visualize the link between survival and family size
ggplot(full[1:981], aes(x=Size, fill=factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x='Family Size') +
  theme_few()
ggplot(full[1:891], aes(x=Size, fill=factor(Survived))) +
  + geom_bar(stat='count', position='dodge') +
  + scale_x_continuous(breaks=c(1:11)) +
  + labs(x='Family Size') +
  + theme_few()
ggplot(full[1:891,], aes(x = Size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()
#complicated graph on who survived (the bigger the family is, the less is chance of survival)
#now we have to do a similar thing as with titles - we have to create categories for families - singleton; small; large
full$SizeD[full$Size==1] <-'singleton'
full$SizeD[full$Size<5 & full$Size>1] <-'small'
full$SizeD[full$Size>4] <-'large'
#mosaic plot to show family sizes and survivals (same as previous but different graph form)
mosaicplot(table(full$SizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
#there is penalty for being single
full$Cabin [1:28]
strsplit(full$Cabin[2], NULL) [[1]]
#we split it to find passenger deck
full$Deck <- factor(sapply(full$Cabin, function (x) strsplit(x,NULL) [[1]][1]))
#there are some missing values and we have to input some
full[c(62,830), 'Embarked']
#these two are missing the place they embarked in
cat(paste ('they paid', full[c(62,830), 'Fare'][[1]][1], 'and' full[c(62,830), 'Fare'] [[1]][2], 'their classes are', full[c(62,830), 'Pclass'] [[1]][1], 'and', full[c(62,830), 'Pclass'][[1]][2]))
cat(paste('they paid', full[c(62, 830), 'Fare'][[1]][1], 'and', full[c(62, 830), 'Fare'][[1]][2], ' their classes are', full[c(62, 830), 'Pclass'][[1]][1], 'and', full[c(62, 830), 'Pclass'][[1]][2], '</b>.'))
cat(paste('they paid', full[c(62, 830), 'Fare'][[1]][1], 'and', full[c(62, 830), 'Fare'][[1]][2], ' their classes are', full[c(62, 830), 'Pclass'][[1]][1], 'and', full[c(62, 830), 'Pclass'][[1]][2]))
#excluding IDs of passengers who miss payment amount or class
embark_fare <- full %>% filter(PassengerId != 62 & PassengerID !=830)
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId !=830)
#1307 not 1309 because they are excluded
ggplot (embark_fare, aes(x = Embarked, y=Fare, fill=factor(Pclass)))+geom_boxplot() + geom_hline(aes(yintercept=80), colour = 'red', linetype='dashed', lwd=2) + scale_y_continuous(labels=dollar_format()) + theme_few()
#the medium fare for red left box is 80 and passenger who paid 80 must be in that embarkment point (the probability is high)
#and the other passenger also embarked from C
full$Embarked[c(62,830)] <- 'C'
#Passenger 1044 doesn't have a fare value
full[1044, ]
#he is a thirs class passenger who also embarked in 'S'
#we need to find other passengers with same characteristics and look at them
ggplot(full[full$Pclass=='3' & full$Embarked =='S', ], aes (x=Fare)) + geom_density (fill='#99d6ff', alpha=0.4) + geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='red', linetype='dashed', lwd=1) + scale_x_continuous(labels=dollar_format()) + theme_few()
#the one that is prevailing is 8.05
#it should be substituted for missing fare value
full$Fare [1044] <-median(full$Pclass =='3' & full$Embarked =='S', ]$Fare, na.rm=TRUE)
full$Fare[1044] <-median(full$Pclass =='3' & full$Embarked =='S', ]$Fare, na.rm=TRUE)
full$Fare[1044] <-median(full[full$Pclass =='3' & full$Embarked =='S', ]$Fare, na.rm=TRUE)
full[1044, ]
#now we have the fare hohoho
#imputation with MICE package - because not everybody has age values but we can generate and substitute them
sum(is.na(full$Age))
#263 people have their ages missing
#the other library that can be used is rpart (recursive partitioning)
factor_vars <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname', 'Family', 'SizeD')
#variables are madeinto factors
#full[factor_vars] <-lapply(full[factor_vars], function(x) as.factor(x))
full[factor_vars] <-lapply(full[factor_vars], function(x) as.factor(x))
#we made a vector of variables here
set.seed(129)
#random seed is generated
#age will be predicted based on other variables
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method='rf')
install.packages(mice)
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method='rf')
install.packages("mice")
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method='rf')
library(mice)
mice_mod <- mice(full[, !names(full) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method='rf')
mice_output <- complete(mice_mod)
#saving the output
View(mice_mod)
View(mice_mod)
#plotting the output and comparing to original one
par(mfrow=c(1,2))
hist(full$Age, freq =F, main ='Age: Original Data', col='darkgreen', ylim=c(0,0.04))
#original output without mice imputation
hist(mice_output$Age, freq=F, main='Age: MICE Output', col='lightgreen', ylim=c(0,0.04))
#new output with imputation
#we replace it
full$Age <- mice_output$Age
sum(is.na(full$Age))
#no one has missing age now
#we are about to create Child and Mother Variables
#mother has to be female
#mother has to be above 18 years old
#mother has to have more than one child
#(one or more)
#mpther does not have the "Miss" title
#plotting the age/survival relationsip
ggplot(full[1:891,], aes(Age, fill=factor(Survived)))+geom_histogram()+facet_grid(.~Sex)+theme_few()
#clearly less males survived than females
#clearly the age where survival is best was 20-60 for women and 20-40 for men
full$Child[full$Age <18] <-'Child'
full$Child[full$Age >=18] <-'Adult'
#show how many children and adults survived comparatively
table(full$Child, full$Survived)
#creating a mother variables next
full$Mother <- 'Not Mother'
#first, fill in for everyone
full$Mother[full$Sex == 'female' & full$Parch>0 & full$Age>18 &full$Title != 'Miss'] <- 'Mother'
#all the conditions for being a mother
table (full$Mother, full$Survived)
full$Child <-factor(full$Child)
full$Mother <-factor(full$Mother)
md.pattern(full)
#this was the check that all the variables are full
#modeling time
train <-full[1:891,]
#train set separated again
test <-full[892:1309,]
#test set separated again
set.seed(754)
#setting random seed - not sure why though
rf_model <- randomForest(factor(Survived)~Pclass + Sex + Age +SibSp+Parch+Fare+Embarked+Title+SizeD+Child+Mother, data=train)
#model is built, Survived is dependent and all others including generated ones are explanatory variables
plot(rf_model, ylim=c(0.36))
plot(rf_model, ylim=c(0,0.36))
#from 0 to 0,36 are limits because it's model error
legend ('topright', colnames(rf_model$err.rate),col=1:3, full=1:3)
legend ('topright', colnames(rf_model$err.rate),col=1:3, fill=1:3)
plot(rf_model, ylim=c(0,0.36))
legend ('topright', colnames(rf_model$err.rate),col=1:3, fill=1:3)
#this line does not seem to work....
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
plot(rf_model, ylim=c(0,0.36))
#the error of the model is about 20% so it should be around 80% correctness, maybe a bit more
#also deaths are more accurately predicted then survivals are
#variable importance in the solution
importance <-importance(rf_model)
varImportance <-data>frame(Variables=row.names(importance), Importance=round(importance [ ,'MeanDecreaseGini'], 2))
varImportance <-data.frame(Variables=row.names(importance), Importance=round(importance [ ,'MeanDecreaseGini'], 2))
#rank variable created based on importance for being used later
rankImportance <-varImportance %>%
  mutate(Rank=paste0('#',dense_rank(desc(Importance))))
#visualization of importance of variables
ggplot(rankImportance, aes(x=reorder(Variables,Importance),y=Importance, fill=Importance))+geom_bar(stat='identity')+geom_text(aes(x=Variables, y=0.5, label=Rank), hjust=0, vjust=0.55, size=4, colour='red')+ labs(x='Variables')+coord_flip()+theme_few()
#the highest importance is title ans sex (also fare)
#prediction itself
prediction <-predict(rf_model, test)
solution <-data.frame(PassengerId=test$PassengerId, Survived=prediction)
#saving only passenger id and whether they survived or not according to prediction the model made
write.csv(solution, file='rf_mod_Solution.csv', row_names=F)
write.csv(solution, file='rf_mod_Solution.csv', row.names=F)
#file is saved
#the end
#:)
savehistory("C:/Program Files/R/Titanic/titanic_history.Rhistory")
