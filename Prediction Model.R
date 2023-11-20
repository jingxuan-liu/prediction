# SNAPSHOT RANDOM FOREST

library(readxl)
library(caTools)
?caTools
library(MASS)
??MASS
library(randomForest)

# Set Working Directory
setwd("C:\\Users\\JLiu\\Desktop\\data\\Validity study_two schools\\Random Forest Analysis")

# Read in excel file with sheet name
input.data <- read_excel("SAO_2019_2020_USA_Math_Subsample.xlsx")
summary(input.data)

# Screen out exminees without GPA data
gpa <- input.data[!is.na(input.data$'current_average_grade'),]
summary(gpa)

# Rename variables with a space in it
names(gpa)[names(gpa) == "current_average_grade"] <- "Math_grade"
names(gpa)[names(gpa) == "Total"] <- "SSAT_Total"
names(gpa)[names(gpa) == "user_id"] <- "ID"

datafile<-gpa
datafile <- subset(gpa, select = c('ID','gender','ethnicity','math_class_size','Math_grade','IE','IN','OP','RS','SA','SC','TW',
                               'IE_Category','IN_Category','OP_Category','RS_Category','SA_Category','SC_Category','TW_Category','RT','QT','VT','SSAT_Total',
                               'initiative_math', 'intellectual_engagement_math',	'open_mindedness_math',	'resilience_math',	'self_control_math', 
                                 'social_awareness_math',	'teamwork_math','initiative_parent', 'intellectual_engagement_parent',	'open_minded_parent',  'resilience_parent', 'self_control_parent', 
                                 'social_awareness_parent', 'teamwork_parent'))
#datafile$English_grade <- as.factor(datafile$English_grade)
datafile$Math_grade <- as.factor(datafile$Math_grade)
summary(datafile)

# Create Train/Test datasets (70% Training / 30% Test)
set.seed(101)
sample <- sample.split(datafile$Math_grade, SplitRatio = 0.70)

train <- subset(datafile, sample == TRUE)
test <- subset(datafile, sample == FALSE)
###############################################################################
# Regression Model#############################################################
###############################################################################

# Full Regression Model (Train)
full.reg.model <- lm(standard.gpa ~ VT + QT + RT + IE + IN + OP + RP + RS + SA + SC + TW, train)
summary(full.reg.model)

# Stepwise regression model (Train)
step.reg.model <- stepAIC(full.reg.model, direction = "both", trace = FALSE)
summary(step.reg.model)
###############################################################################
# Random Forest ###############################################################
###############################################################################

set.seed(101)
train.rf <- randomForest(Math_grade ~ VT + QT + RT + IE + IN + OP + RS + SA + SC + TW, data = train, mtry = 2, ntree = 150,
                         importance = TRUE, na.action = na.omit) 
print(train.rf)
train.rf2 <- randomForest(Math_grade ~ VT + QT + RT , data = train, mtry = 2, ntree = 100,
                         importance = TRUE, na.action = na.omit)
print(train.rf2)
train.rf3 <- randomForest(Math_grade ~ VT + QT + RT + IE + IN + OP + RS + SA + SC + TW + initiative_math+ intellectual_engagement_math +	open_mindedness_math +	resilience_math +	self_control_math + 
                            social_awareness_math +	teamwork_math, data = train, mtry = 4, ntree = 150,
                         importance = TRUE, na.action = na.omit) 
print(train.rf3)
train.rf4 <- randomForest(Math_grade ~ VT + QT + RT + IE + IN + OP + RS + SA + SC + TW + initiative_parent+ intellectual_engagement_parent +	open_minded_parent +	resilience_parent +	self_control_parent + 
                            social_awareness_parent +	teamwork_parent, data = train, mtry = 4, ntree = 150,
                          importance = TRUE, na.action = na.omit) 
print(train.rf4)

importance(train.rf)
par(mar=c(1,1,1,1))
varImpPlot(train.rf, type=2, main = 'Variable Importance', col='blue', pch = 19)

###############################################################################
# Predictions
###############################################################################

train.prediction <- train
test.prediction <- test

train.prediction$rf.predict <- predict(train.rf,train)
test.prediction$rf.predict <- predict(train.rf,test)

train.prediction$rf2.predict <- predict(train.rf2,train)
test.prediction$rf2.predict <- predict(train.rf2,test)

train.prediction$rf3.predict <- predict(train.rf3,train)
test.prediction$rf3.predict <- predict(train.rf3,test)

train.prediction$rf4.predict <- predict(train.rf4,train)
test.prediction$rf4.predict <- predict(train.rf4,test)

total <- rbind(train.prediction, test.prediction)
write.csv(total, "result_Math.csv")

plot(train.prediction$English_grade, train.prediction$rf.predict, main = 'Training Random Forest Model', xlim = c(2,4), ylim = c(2,4), col='blue')
plot(test.prediction$English_grade, test.prediction$rf.predict, main = 'Test Random Forest Model', xlim = c(2,4), ylim = c(2,4), col='red')


# Calculate MSE
train.reg.mse <- mean((train.prediction$GPA_1819 - train.prediction$reg.gpa)^2)
train.rf.mse <- mean((train.prediction$GPA_1819 - train.prediction$rf.gpa)^2)
test.reg.mse <- mean((test.prediction$GPA_1819 - test.prediction$reg.gpa)^2)
test.rf.mse <- mean((test.prediction$GPA_1819 - test.prediction$rf.gpa)^2)

train.reg.mse
train.rf.mse
test.reg.mse
test.rf.mse

# Calculate R-squared
train.reg.SSE = sum((train.prediction$GPA_1819 - train.prediction$reg.gpa)^2)
train.reg.SST = sum( (mean(train.prediction$GPA_1819) - train.prediction$GPA_1819)^2)
train.reg.R2 = 1 - train.reg.SSE/train.reg.SST

train.rf.SSE = sum((train.prediction$GPA_1819 - train.prediction$rf.gpa)^2)
train.rf.SST = sum( (mean(train.prediction$GPA_1819) - train.prediction$GPA_1819)^2)
train.rf.R2 = 1 - train.rf.SSE/train.rf.SST

test.reg.SSE = sum((test.prediction$GPA_1819 - test.prediction$reg.gpa)^2)
test.reg.SST = sum( (mean(test.prediction$GPA_1819) - test.prediction$GPA_1819)^2)
test.reg.R2 = 1 - test.reg.SSE/test.reg.SST

test.rf.SSE = sum((test.prediction$GPA_1819 - test.prediction$rf.gpa)^2)
test.rf.SST = sum( (mean(test.prediction$GPA_1819) - test.prediction$GPA_1819)^2)
test.rf.R2 = 1 - test.rf.SSE/test.rf.SST

train.reg.R2
train.rf.R2
test.reg.R2
test.rf.R2



