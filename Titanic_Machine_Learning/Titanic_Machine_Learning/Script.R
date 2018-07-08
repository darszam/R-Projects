library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')

trainset = read.csv('train.csv', stringsAsFactors = F)
testset = read.csv('test.csv', stringsAsFactors = F)

fullset = bind_rows(trainset, testset)

str(fullset)
## 'data.frame':	1309 obs. of  12 variables:
## $PassengerId:int 1 2 3 4 5 6 7 8 9 10 ...
## $Survived:int 0 1 1 1 0 0 0 0 1 1 ...
## $Pclass:int 3 1 3 1 3 3 1 3 3 2 ...
## $Name:chr "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
## $Sex:chr "male" "female" "female" "female" ...
## $Age:num 22 38 26 35 35 NA 54 2 27 14 ...
## $SibSp:int 1 1 0 1 0 0 0 3 0 1 ...
## $Parch:int 0 0 0 0 0 0 0 1 2 0 ...
## $Ticket:chr "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
## $Fare:num 7.25 71.28 7.92 53.1 8.05 ...
## $Cabin:chr "" "C85" "" "C123" ...
## $Embarked:chr "S" "C" "S" "S" ...
head(fullset, 5)
fullset$Title <- gsub('(.*, )|(\\..*)', '', fullset$Name)
table(fullset$Sex, fullset$Title)
##      Capt Col Don Dona  Dr Jonkheer Lady Major Master Miss Mlle
##  female 0 0 0 1 1 0 1 0 0 260 2
##  male 1 4 1 0 7 1 0 2 61 0 0

##      Mme Mr Mrs Ms Rev Sir the Countess
## female 1 0 197 2 0 0 1
## male 0 757 0 0 8 1 0

## Titles related to status
rare_titles <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

## Changing titles like mlle, ms, mme 
fullset$Title[fullset$Title == 'Mlle'] <- 'Miss'
fullset$Title[fullset$Title == 'Ms'] <- 'Miss'
fullset$Title[fullset$Title == 'Mme'] <- 'Mrs'
fullset$Title[fullset$Title %in% rare_titles] <- 'Rare Title'

## Show titles counts and sex
table(fullset$Sex, fullset$Title)

## Getting surname from passenger name
fullset$Surname <- sapply(fullset$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('There are ', nlevels(factor(fullset$Surname)), ' unique surnames'))

## Family relativity
fullset$FamilySize <- fullset$SibSp + fullset$Parch + 1 #Family size = siblings + parents + children and passenger himself

fullset$Family <- paste(fullset$Surname, fullset$FamilySize, sep = '_')

ggplot(fullset[1:891,], aes(x = FamilySize, fill = factor(Survived))) +
    geom_bar(stat = 'count', position = 'dodge') + scale_x_continuous(breaks = c(1:11)) + labs(x = 'Family Size') + theme_few()

## There are 3 levels of surviving chance based on family size
fullset$FamilySizeDiscretized[fullset$FamilySize == 1] <- 'single'
fullset$FamilySizeDiscretized[fullset$FamilySize <5 & fullset$FamilySize>1] <- 'small'
fullset$FamilySizeDiscretized[fullset$FamilySize > 4] <- 'large'

mosaicplot(table(fullset$FamilySizeDiscretized, fullset$Survived), main = 'Family Size by Survival', shade = TRUE)

## Treat cabin variable

strsplit(fullset$Cabin[2], NULL)[[1]]
fullset$Deck <- factor(sapply(fullset$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

##A lot of missing values
fullset$Cabin
#fullset$Deck ## Now instead of "" there is <NA> value

## Next phase - fixing missing values 