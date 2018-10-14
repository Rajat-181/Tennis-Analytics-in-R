Player_Data <- read.csv('Final_PlayerDataPerMatch.csv')

str(Player_Data)

Player_Data$Matches<-as.numeric(Player_Data$Matches)
Player_Data$Aces<-as.numeric(Player_Data$Aces)
Player_Data$Breakpoints_won<-as.numeric(Player_Data$Breakpoints_won)
Player_Data$double_faults<-as.numeric(Player_Data$double_faults)
Player_Data$first_serve_points_won<-as.numeric(Player_Data$first_serve_points_won)
Player_Data$first_serve_successful<-as.numeric(Player_Data$first_serve_successful)
Player_Data$Games_won<-as.numeric(Player_Data$Games_won)
Player_Data$Max_games_won_in_a_row<-as.numeric(Player_Data$Max_games_won_in_a_row)
Player_Data$Max_points_in_a_row<-as.numeric(Player_Data$Max_points_in_a_row)
Player_Data$Points_won<-as.numeric(Player_Data$Points_won)
Player_Data$Reciever_points_won<-as.numeric(Player_Data$Reciever_points_won)
Player_Data$second_serve_points_won<-as.numeric(Player_Data$second_serve_points_won)
Player_Data$second_serve_successful<-as.numeric(Player_Data$second_serve_successful)
Player_Data$service_games_won<-as.numeric(Player_Data$service_games_won)
Player_Data$service_points_won<-as.numeric(Player_Data$service_points_won)
Player_Data$tiebreaks_won<-as.numeric(Player_Data$tiebreaks_won)


DataQualityReport(Player_Data)

descrCor <-  cor(Player_Data[,5:ncol(Player_Data)])   # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])  

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- Player_Data[,5:ncol(Player_Data)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr) 

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])


Player_Data_2 <- cbind(Player_Data[,c(1:4)],filteredDescr)
backup2 <- Player_Data_2

##Dropping Player Names, Gender, Matches  
Player_Data_2 <- Player_Data_2[,-c(2:4)]

#Storing ID's 
Player_Data_2_ID <- Player_Data_2$Player_ID
Player_Data_2$Player_ID <- NULL#Removing IDs from final dataset 


#Y variables 
names(Player_Data_2)[6] <- 'y' 
Player_Data_TargetValue <- Player_Data_2$y #Target variable stored and remove 
Player_Data_2$y <- NULL


#Histograms 
hist(Player_Data_2$y, main="y", xlab="Ace", col="gold")
hist(Player_Data_2$Aces, main="Aces", xlab="Ace", col="gold")
hist(Player_Data_2$Breakpoints_won, main="Breakpoints_won", xlab="Ace", col="gold")
hist(Player_Data_2$double_faults, main="double_faults", xlab="Ace", col="gold")
hist(Player_Data_2$first_serve_points_won, main="first_serve_points_won", xlab="Ace", col="gold")
hist(Player_Data_2$first_serve_successful, main="first_serve_successful", xlab="Ace", col="gold")
hist(Player_Data_2$Max_games_won_in_a_row, main="Max_games_won_in_a_row", xlab="Ace", col="gold")
hist(Player_Data_2$Reciever_points_won, main="Reciever_points_won", xlab="Aces.y", col="gold")
hist(Player_Data_2$second_serve_points_won, main="second_serve_points_won", xlab="Aces.y", col="gold")
hist(Player_Data_2$second_serve_successful, main="second_serve_successful", xlab="Aces.y", col="gold")
hist(Player_Data_2$service_games_won, main="service_games_won", xlab="Aces.y", col="gold")
hist(Player_Data_2$tiebreaks_won, main="tiebreaks_won", xlab="Aces.y", col="gold")
#backup2 <- Player_Data_2

#Scaling all variables 
preProcValues <- preProcess(Player_Data_2, method = c("range","YeoJohnson"))

Player_Data_2 <- predict(preProcValues, Player_Data_2)

################################################################################
# Data partitioning
################################################################################
set.seed(1234) # set a seed so you can replicate your results

Player_Data_2 <- cbind(Player_Data_TargetValue,Player_Data_2)
names(Player_Data_2)[1] <- 'y'

#identify records that will be used in the training set. Here we are doing a
#85% train/ 15% test split. You might modify this.
inTrain <- createDataPartition(y = Player_Data_2$y,   # outcome variable
                               p = .85,   # % of training data you want
                               list = F)

# create your partitions
train_dataset <- Player_Data_2[inTrain,]  # training data set
validation <- Player_Data_2[-inTrain,]  # test data set

#d1_score <- final_score
#using linear regression
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)

lm_model <- train(y ~ .,
            data = train_dataset,
            method = "lm",
            importance=T,    # we add this in or it varImp cannot be computed
            trControl = ctrl,
            tuneLength = 10,
            metric = "RMSE"
            )

pred_val_lm <- predict(lm_model,validation)
postResample(pred_val_lm,validation$y)

summary(lm_model)


# automatic backward selection
library(leaps)
mb <- regsubsets(y ~ ., data=train
                 , nbest=1
                 , intercept=T
                 , method='backward'
                 , really.big=T
)

vars2keep <- data.frame(summary(mb)$which[which.max(summary(mb)$adjr2),])
names(vars2keep) <- c("keep")  
head(vars2keep)
library(data.table)
vars2keep <- setDT(vars2keep, keep.rownames=T)[]
vars2keep <- c(vars2keep[which(vars2keep$keep==T & vars2keep$rn!="(Intercept)"),"rn"])[[1]]

# here are the final features found to be statistically significant
as.data.frame(vars2keep)

final_var <- c('y','Aces','Breakpoints_won','double_faults','first_serve_points_won',
               'first_serve_successful','Reciever_points_won','second_serve_successful',
               'service_games_won','tiebreaks_won')

train_new <- train[,final_var]
validation_new <- validation[,final_var]

#using linear regression again
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  
                     summaryFunction = defaultSummary,
                     allowParallel=T)

lm_model_2 <- train(y ~ .,
                  data = train_new,
                  method = "lm",
                  importance=T,    # we add this in or it varImp cannot be computed
                  trControl = ctrl,
                  tuneLength = 10,
                  metric = "RMSE"
)

pred_val_lm_2 <- predict(lm_model_2,validation)
postResample(pred_val_lm_2,validation$y)


summary(lm_model_2)
