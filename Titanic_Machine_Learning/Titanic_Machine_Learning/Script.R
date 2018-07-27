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

print('')
## Next phase - fixing missing values
fullset[c(62, 830), 'Embarked']

print('')

cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: 
**passenger class** and **fare**. We see that they paid $', fullset[c(62, 830), 'Fare'][[1]][1], 'and $', 
fullset[c(62, 830), 'Fare'][[1]][2], 'respectively and their classes are', fullset[c(62, 830), 'Pclass'][[1]][1],
'and', fullset[c(62, 830), 'Pclass'][[1]][2], '. So from where did they embark?'))

embarkment_fare <- fullset %>%
    filter(PassengerId != 62 & PassengerId != 830)

# ggplot2
ggplot(embarkment_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 80),
    colour = 'red', linetype = 'dashed', lwd = 2) +
               scale_y_continuous(labels = dollar_format()) +
               theme_few()

fullset$Embarked[c(62, 830)] <- 'C'

print('')
fullset[1044,]

ggplot(fullset[fullset$Pclass == '3' & fullset$Embarked == 'S',],
    aes(x = Fare)) +
       geom_density(fill = '#99d6ff', alpha = 0.4) +
       geom_vline(aes(xintercept = median(Fare, na.rm = T)),
       colour = 'red', linetype = 'dashed', lwd = 1) +
                  scale_x_continuous(labels = dollar_format()) +
                  theme_few()

fullset$Fare[1044] <- median(fullset[fullset$Pclass == '3' & fullset$Embarked == 'S',]$Fare, na.rm = TRUE)

# Predictive imputation
sum(is.na(fullset$Age))

factor_variables <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname', 'Family', 'FamilySizeDiscretized')

fullset[factor_variables] <- lapply(fullset[factor_variables], function(x) as.factor(x))

set.seed(128)

mice_mod <- mice(fullset[, !names(fullset) %in% c('PassengerId', 'Name', 'Ticket', 'Cabin', 'Family', 'Surname', 'Survived')], method = 'rf')

mice_output <- complete(mice_mod)

# Age distribution
par(mfrow = c(1, 2))
hist(fullset$Age, freq = F, main = 'Age: Original Data',
     col = 'darkgreen', ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = 'Age: Mice Output',
     col = 'lightgreen', ylim = c(0, 0.4))

# Replacing Age variable from mice model
fullset$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(fullset$Age))

# Feature Engineering vol.2
# plot to check corelation between age and survival
ggplot(fullset[1:891,], aes(Age, fill = factor(Survived))) +
    geom_histogram() +
    facet_grid(. ~ Sex) +
    theme_few()

# Creating Child variable with values 'Child' and 'Adult'
fullset$Child[fullset$Age < 18] <- 'Child'
fullset$Child[fullset$Age >= 18] <- 'Adult'

# Adding Mother variable
fullset$Mother <- 'Not Mother'
fullset$Mother[fullset$Sex == 'female' & fullset$Parch > 0 & fullset$Age > 18 & fullset$Title != 'Miss'] <- 'Mother'

# Number of survived mothers and non mothers
table(fullset$Mother, fullset$Survived)

# Change new variables to factor
fullset$Child <- factor(fullset$Child)
fullset$Mother <- factor(fullset$Mother)

md.pattern(fullset)

# Prediction chapter
trainset <- fullset[1:891,]
testset <- fullset[892:1309,]
set.seed(2317)

randomForest_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySizeDiscretized + Child + Mother, data = trainset)

plot(randomForest_model, ylim = c(0, 0.36))
legend('topright', colnames(randomForest_model$err.rate), col = 1:3, fill = 1:3)

importance <- importance(randomForest_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[, 'MeanDecreaseGini'], 2))

rankImportance <- varImportance %>%
    mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# ggplot2

ggplot(rankImportance, aes(x = reorder(Variables, Importance),
    y = Importance, fill = Importance)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust = 0, vjust = 0.55, size = 4, colour = 'red') +
              labs(x = 'Variables') +
              coord_flip() +
              theme_few()

# Making prediction
prediction <- predict(randomForest_model, testset)
solution_csv <- data.frame(PassengerID = testset$PassengerId, Survived = prediction)

write.csv(solution_csv, file = 'randomforest_model_solution.csv', row.names = F)