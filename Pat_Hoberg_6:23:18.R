# Pat Hoberg : Giants vs __OPPONENT__ : Pat Hoberg

# 124 pitches were called strikes/balls

# The robot-ump called 44 of those pitches as called strikes & 80 as balls

# Pat Hoberg called 44 of those pitches as called strikes & 80 as balls 

# Accuracy: 97%

Pat_Hoberg <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Pat_Hoberg_6:23:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Pat_Hoberg)
names(Pat_Hoberg)
is.na(Pat_Hoberg)
colSums(is.na(Pat_Hoberg)) 
Pat_Hoberg = Pat_Hoberg[,colSums(is.na(Pat_Hoberg)) == 0] 
dim(Pat_Hoberg)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Pat_Hoberg = Pat_Hoberg[ , !(names(Pat_Hoberg) %in% drops)]
dim(Pat_Hoberg)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Pat_Hoberg_train = Pat_Hoberg[0:(0.8 * nrow(Pat_Hoberg)),]
dim(Pat_Hoberg_train)
prop.table(table(Pat_Hoberg_train$type))
Pat_Hoberg_test = Pat_Hoberg[(0.8*nrow(Pat_Hoberg)):nrow(Pat_Hoberg),]
dim(Pat_Hoberg_test)
prop.table(table(Pat_Hoberg_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Pat_Hoberg_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Pat_Hoberg_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Pat_Hoberg_test$type))

# Subset for Borderline Calls
Pat_Hoberg$Borderline = ifelse(((abs(Pat_Hoberg$plate_x)> 0.748) & (abs(Pat_Hoberg$plate_x)<0.914)) 
                               & (((Pat_Hoberg$plate_z > Pat_Hoberg$sz_top-0.83) & (Pat_Hoberg$plate_z < Pat_Hoberg$sz_top+0.83))
                                  | (((Pat_Hoberg$plate_z)<Pat_Hoberg$sz_bot+0.83) & ((Pat_Hoberg$plate_z) > Pat_Hoberg$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Pat_Hoberg_Strikes = subset(Pat_Hoberg, Pat_Hoberg$type == "S")
Pat_Hoberg_Balls = subset(Pat_Hoberg, Pat_Hoberg$type == "B")

# Borderline
Pat_Hoberg_Borderline = subset(Pat_Hoberg, Pat_Hoberg$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Pat_Hoberg_Strikes$AdjustedCall = ifelse((Pat_Hoberg_Strikes$plate_x < 0.833 & Pat_Hoberg_Strikes$plate_x > -0.833) & (Pat_Hoberg_Strikes$plate_z > Pat_Hoberg_Strikes$sz_bot & Pat_Hoberg_Strikes$plate_z < Pat_Hoberg_Strikes$sz_top), 'S', 'B')
table(Pat_Hoberg_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Pat_Hoberg_Balls$AdjustedCall = ifelse((Pat_Hoberg_Balls$plate_x > 0.833 | Pat_Hoberg_Balls$plate_x < -0.833)|(Pat_Hoberg_Balls$plate_z < Pat_Hoberg_Balls$sz_bot | Pat_Hoberg_Balls$plate_z > Pat_Hoberg_Balls$sz_top),'B','S')
table(Pat_Hoberg_Balls$AdjustedCall)

# Borderline
Pat_Hoberg_Borderline$AdjustedCall = ifelse((Pat_Hoberg_Borderline$plate_x < 0.833 & Pat_Hoberg_Borderline$plate_x > -0.833) & (Pat_Hoberg_Borderline$plate_z > Pat_Hoberg_Borderline$sz_bot & Pat_Hoberg_Borderline$plate_z < Pat_Hoberg_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Pat_Hoberg_AdjustedCalls = rbind(Pat_Hoberg_Strikes,Pat_Hoberg_Balls)
Pat_Hoberg_AdjustedCalls$OnFieldRuling = ifelse(Pat_Hoberg_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Pat_Hoberg)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Pat_Hoberg_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Pat_Hoberg_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Pat_Hoberg_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Pat_Hoberg_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Pat_Hoberg_AdjustedCalls$Call = ifelse( ((Pat_Hoberg_AdjustedCalls$type == 'B') & ( (Pat_Hoberg_AdjustedCalls$AdjustedCall == "B") | (Pat_Hoberg_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Pat_Hoberg_AdjustedCalls$Call = ifelse( ((Pat_Hoberg_AdjustedCalls$type == 'S') & ((Pat_Hoberg_AdjustedCalls$AdjustedCall == "S") | (Pat_Hoberg_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Pat_Hoberg_AdjustedCalls$Call = ifelse( ( (Pat_Hoberg_AdjustedCalls$type == 'B') & ((Pat_Hoberg_AdjustedCalls$AdjustedCall == "S") & (Pat_Hoberg_AdjustedCalls$Borderline == "F") ) ), "I","C")
Pat_Hoberg_AdjustedCalls$Call = ifelse( ( (Pat_Hoberg_AdjustedCalls$type == 'S') & ((Pat_Hoberg_AdjustedCalls$AdjustedCall == "B") & (Pat_Hoberg_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Pat_Hoberg_AdjustedCalls$Call)
View(Pat_Hoberg_AdjustedCalls)


# Which Pitchers Recieved the InCorrect Calls
Pat_Hoberg_Incorrect = subset(Pat_Hoberg_AdjustedCalls, Pat_Hoberg_AdjustedCalls$Call == "I")
print(Pat_Hoberg_Incorrect$player_name)
