setwd('/Users/saulforman/Desktop/datafest')
rpe = read.csv('rpe.csv')
games = read.csv('games.csv')
gps = read.csv('gps.csv')
wellness = read.csv('wellness.csv')

#### MULTI-DEFINITION OF IMPORTANCE
## Define Average Active Sprint Speed
speed_sd = gps %>%
  filter(Speed != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(speed_sd = sd(Speed))
speed_mean = gps %>%
  filter(Speed != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(speed_mean = mean(Speed))
speed_summ = speed_mean %>% left_join(speed_sd, by = c('PlayerID', 'GameID'))
speed_summ$active_speed = speed_summ$speed_mean + 2*speed_summ$speed_sd
speed_summ = speed_summ %>%
  select(-c(speed_mean, speed_sd))
gps_sprint = gps %>%
  left_join(speed_summ, by = c('PlayerID', 'GameID')) %>%
  filter(Speed >= active_speed) %>%
  group_by(PlayerID, GameID) %>%
  summarise(AASS = mean(Speed))
## Define Average Active Acceleration Impulse
accel_sd = gps %>%
  filter(AccelImpulse != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(accel_sd = sd(AccelImpulse))
accel_mean = gps %>%
  filter(AccelImpulse != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(accel_mean = mean(AccelImpulse))
accel_summ = accel_mean %>% left_join(accel_sd, by = c('PlayerID', 'GameID'))
accel_summ$active_accel = accel_summ$accel_mean + 2*accel_summ$accel_sd
accel_summ = accel_summ %>%
  select(-c(accel_mean, accel_sd))
gps_sprint2 = gps %>%
  left_join(accel_summ, by = c('PlayerID', 'GameID')) %>%
  filter(AccelImpulse >= active_accel) %>%
  group_by(PlayerID, GameID) %>%
  summarise(AAAI = mean(active_accel))
## Each player's AAAI looks close (not perfectly) normal
boxplot(AAAI ~ PlayerID, data = gps_sprint2)
## In the aggregate, AAAI appears normally distributed around mean
hist(gps_sprint2$AAAI)

## Define Average Active Acceleration Load
load_sd = gps %>%
  filter(AccelLoad != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(load_sd = sd(AccelLoad))
load_mean = gps %>%
  filter(AccelLoad != 0) %>%
  group_by(PlayerID, GameID) %>%
  summarise(load_mean = mean(AccelLoad))
load_summ = load_mean %>% left_join(load_sd, by = c('PlayerID', 'GameID'))
load_summ$active_load = load_summ$load_mean + 2*load_summ$load_sd
load_summ = load_summ %>%
  select(-c(load_mean, load_sd))
gps_sprint3 = gps %>%
  left_join(load_summ, by = c('PlayerID', 'GameID')) %>%
  filter(AccelLoad >= active_load) %>%
  group_by(PlayerID, GameID) %>%
  summarise(AAAL = mean(active_load))
## Each player's AAAI looks close (not perfectly) normal
boxplot(AAAL ~ PlayerID, data = gps_sprint3)
## In the aggregate, AAAI appears normally distributed around mean
hist(gps_sprint2$AAAI)

## Average Length of Sprint
LOS = gps %>%
  left_join(speed_summ, by = c('PlayerID', 'GameID')) %>%
  filter(Speed >= active_speed)

LOS$Group = 0
Group = 1
for(i in 1:nrow(LOS)) {
  if(LOS[i,]$FrameID == (LOS[i+1,]$FrameID - 1)) {
    LOS[i,]$Group = Group
  } else {
    LOS[i,]$Group = Group
    Group = Group + 1 
  }
}

library(lubridate)
LOS2 = LOS %>%
  mutate(Time = hms(Time)) %>%
  group_by(PlayerID, GameID, Group) %>%
  summarise(max = max(Time))

LOS3 = LOS %>%
  mutate(Time = hms(Time)) %>%
  group_by(PlayerID, GameID, Group) %>%
  summarise(min = min(Time))

LOS4 = LOS2 %>% left_join(LOS3, by = c('PlayerID', 'GameID', 'Group'))
LOS4$duration = LOS4$max-LOS4$min
LOS_summ = LOS4 %>%
  group_by(PlayerID, GameID) %>%
  summarise(mean_duration = mean(duration))
hist(LOS_summ$mean_duration)

game_dates = games %>%
  select(GameID, Date)

game_rpe = rpe %>%
  filter(SessionType == 'Game') %>%
  inner_join(game_dates, by = c('Date'))

outcomes = gps_sprint %>% left_join(gps_sprint2, by = c('PlayerID', 'GameID')) %>%
  left_join(gps_sprint3, by = c('PlayerID', 'GameID')) %>%
  left_join(LOS_summ, by = c('PlayerID', 'GameID')) %>%
  left_join(game_rpe, by = c('PlayerID', 'GameID')) %>%
  select(-c(Date, Training, SessionType))

library(fastDummies)
append = dummy_cols(outcomes)
append = append %>% rename(Best_NA = BestOutOfMyself_NA, Best_Absolutely = BestOutOfMyself_Absolutely, Best_Somewhat = BestOutOfMyself_Somewhat)
colnames(append)[20] = 'Best_Not_at_All'
append = append[,-16]

max_game = game_dates %>%
  group_by(Date) %>%
  summarise(GOD = max(GameID)-min(GameID)+1)

append_games = append %>% left_join(game_dates, by = 'GameID') %>%
  left_join(max_game, by = 'Date') %>%
  select(-Date)

library(bnstruct)
## Convert to Matrix
append_imp = as.matrix(append_games[,c(3:15,17:19)])
## Impute Missing Values Using 5 Nearest Neighbors
append_games[,c(3:15,17:19)] = knn.impute(append_imp, k = 5, cat.var = NA, to.impute = 1:nrow(append_imp),
                           using = 1:nrow(append_imp))
## Drop Best_NA (Perfectly Colinear)
append_games = append_games[,-16] %>%
  left_join(game_dates, by = 'GameID')
head(append_games)
## Scale & Center
append_games = append_games %>%
  group_by(PlayerID, Date) %>%
  summarise(AASS = mean(AASS), AAAI = mean(AAAI), AAAL = mean(AAAL),
            mean_duration = mean(mean_duration), Duration = mean(Duration),
            RPE = mean(RPE), SessionLoad = mean(SessionLoad),
            DailyLoad = mean(DailyLoad), AcuteLoad = mean(AcuteLoad),
            ChronicLoad = mean(ChronicLoad), AcuteChronicRatio = mean(AcuteChronicRatio),
            ObjectiveRating = mean(ObjectiveRating), FocusRating = mean(FocusRating),
            Best_Somewhat = max(Best_Somewhat), Best_Not_at_All = max(Best_Not_at_All),
            GOD = max(GOD))
append_scale = append_games
append_scale[,3:18] = scale(append_scale[,3:18], center = TRUE, scale = TRUE)

## Create and Plot Covariance Matrix
cormat = round(cor(append_scale[,3:18]),2)
library(reshape2)
melted_cormat = melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Use PCA on Outcome Variables
out_pca = prcomp(append_scale[,3:18])
summary(out_pca)
append_scale2 = append_scale
append_scale2$pca = out_pca$x[,1]
append_scale2 = append_scale2 %>%
  select(PlayerID, Date, pca)

####### ASSEMBLE PREDICTORS MATRIX
wellness = wellness %>%
  select(-c(BedTime, WakeTime))
wellness[,'TrainingReadiness'] = as.numeric(sub('%', '', wellness$TrainingReadiness, fixed = TRUE))/100
wellness2 = dummy_cols(wellness,  select_columns = colnames(wellness)[10:15])
wellness2 = wellness2[,-(10:15)]
colnames(wellness2)[27] = 'NutritionAdjustment_IDK'
colnames(wellness2)[15] = 'Illness_Slightly_Off'
head(wellness2)
wellness_scaled = wellness2 %>%
  group_by(PlayerID) %>%
  select(-c(USGMeasurement_NA, NutritionAdjustment_NA, Nutrition_NA, Menstruation_NA,
            Menstruation_No, Illness_Yes, Pain_Yes)) %>%
  mutate(Fatigue = scale(Fatigue), Soreness = scale(Soreness), 
         Desire = scale(Desire), Irritability = scale(Irritability), 
         SleepHours = scale(SleepHours), SleepQuality = scale(SleepQuality),
         MonitoringScore = scale(MonitoringScore), USG = scale(USG), 
         TrainingReadiness = scale(TrainingReadiness), 
         Pain_No = scale(Pain_No), 
         Illness_No = scale(Illness_No), Illness_Slightly_Off = scale(Illness_Slightly_Off),
         Menstruation_Yes = scale(Menstruation_Yes), 
         Nutrition_Excellent = scale(Nutrition_Excellent),
         Nutrition_Okay = scale(Nutrition_Okay), Nutrition_Poor = scale(Nutrition_Poor), 
         NutritionAdjustment_Yes = scale(NutritionAdjustment_Yes), 
         NutritionAdjustment_No = scale(NutritionAdjustment_No), 
         NutritionAdjustment_IDK = scale(NutritionAdjustment_IDK), 
         USGMeasurement_No = scale(USGMeasurement_No), 
         USGMeasurement_Yes = scale(USGMeasurement_Yes)) %>%
  mutate(Date = as.character(as.Date(Date) + 1))

head(outcomes)
outcomes_t1 = outcomes[,-c(6:16)]
colnames(outcomes_t1)[3:5] = paste(colnames(outcomes)[3:5], 't1', sep = '_')
outcomes_t1 = outcomes_t1 %>% left_join(game_dates, by = 'GameID') %>%
  select(-GameID) %>%
  group_by(PlayerID) %>%
  mutate(AASS_t1 = scale(AASS_t1),
         AAAI_t1 = scale(AAAI_t1),
         AAAL_t1 = scale(AAAL_t1)) %>%
  mutate(Date = as.character(as.Date(Date) + 1)) %>%
  group_by(PlayerID, Date) %>%
  summarise(AASS_t1 = mean(AASS_t1),
            AAAI_t1 = mean(AAAI_t1),
            AAAL_t1 = mean(AAAL_t1))

X_scaled = wellness_scaled %>% left_join(outcomes_t1, by = c('PlayerID','Date'))


rpe_train = dummy_cols(rpe, select_columns = c('SessionType','BestOutOfMyself'))
head(rpe_train)
colnames(rpe_train)[19] = 'SessionType_Mobility_Recovery'

rpe_train = rpe_train %>%
  mutate(Date = as.character(as.Date(Date) + 1)) %>%
  select(-c(Training, RPE, AcuteChronicRatio, `BestOutOfMyself_Not at all`)) %>%
  group_by(PlayerID) %>%
  mutate(Duration = scale(Duration),
         SessionLoad = scale(SessionLoad), DailyLoad = scale(DailyLoad),
         AcuteLoad = scale(AcuteLoad), ChronicLoad = scale(ChronicLoad),
         ObjectiveRating = scale(ObjectiveRating), FocusRating = scale(FocusRating)) %>%
  group_by(PlayerID, Date) %>%
  summarise(Duration = mean(Duration), SessionLoad = mean(SessionLoad),
            DailyLoad = mean(DailyLoad), AcuteLoad = mean(AcuteLoad),
            ChronicLoad = mean(ChronicLoad), ObjectiveRating = mean(ObjectiveRating),
            FocusRating = mean(FocusRating), SessionType_Mobility_Recovery = max(SessionType_Mobility_Recovery),
            SessionType_Game = max(SessionType_Game), SessionType_Skills = max(SessionType_Skills),
            SessionType_Conditioning = max(SessionType_Conditioning),
            SessionType_Combat = max(SessionType_Combat), SessionType_NA = max(SessionType_NA),
            SessionType_Speed = max(SessionType_Speed), BestOutOfMyself_Absolutely = max(BestOutOfMyself_Absolutely),
            BestOutOfMyself_NA = max(BestOutOfMyself_NA))

rpe_train[,3:18] = sapply(rpe_train[,3:18], as.numeric)
data = rpe_train %>% left_join(append_scale2, by = c('PlayerID', 'Date')) %>%
  left_join(outcomes_t1, by = c('PlayerID', 'Date')) %>%
  filter(is.na(pca) != TRUE)

## Roughfix NAs for random forest
X = as.matrix(data[,3:22])
rf = as.data.frame(rfImpute(X, data$pca))

## Train Random Forest
library(caret)
library(randomForest)
inTrain = createDataPartition(rf$y, p=0.7, list = FALSE)
training = rf[inTrain,-18]
testing = rf[-inTrain,-18]
ctrl = trainControl(method = 'repeatedcv', number = 5, repeats = 5)
model = randomForest(y ~., data = training, method = 'rf', trControl = ctrl)
testing$predicted = predict(model, newdata=testing)
varImpPlot(model, type=2)
save(model, file = "mymodel.rda")



