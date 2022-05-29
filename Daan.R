library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(stargazer)
library(plm)
library(rpart)
library(rpart.plot)
library(randomForest) 
library(gbm) 
library(ROCR)
library(magrittr)
library(caret)


ISS = read.csv2("data/daan/ISS.csv", head = TRUE, sep=",")
ISSLegacy = read.csv2("data/daan/ISS legacy.csv", head = TRUE, sep=",")
boardEx = read.csv("data/daan/boardex2.csv")
execucomp = read.csv("data/daan/execucomp.csv") 
ids = read.csv("data/char/ciq common.csv")
fin = read.csv("data/daan/Fundamentals Daan.csv")

#Append Legacy ISS data to new format
ISSLegacy = ISSLegacy %>% 
  rename(
     year = YEAR,
     Ticker = TICKER,
     Name = NAME,
     Employment_CEO = EMPLOYMENT_CEO,
     Pcnt_Ctrl_Votingpower = PCNT_CTRL_VOTINGPOWER,
     Num_Of_Shares = NUM_OF_SHARES,
     Attend_LESS75_PCT = ATTEND_LESS75_PCT
  )

ISSLegacy$Employment_CEO = ifelse(ISSLegacy$Employment_CEO == 1, "Yes", "No")
ISSLegacy$Attend_LESS75_PCT = ifelse(ISSLegacy$Attend_LESS75_PCT == 1, "Yes", "No")


ISS = rbind(ISS, ISSLegacy)

# Filters: Data Selection
execucomp = execucomp[execucomp$CEOANN == "CEO", ]
ISS = ISS[ISS$Employment_CEO == "Yes", ]
boardEx = boardEx[grepl("CEO|Chief Executive Officer", boardEx$RoleName, ignore.case = TRUE), ]


# Change date to year
boardEx$AnnualReportDate = as.integer(substr(boardEx$AnnualReportDate, 1, 4))
execucomp$YEAR = as.integer(substr(execucomp$YEAR, 1, 4))
execucomp$BECAMECEO = as.integer(substr(execucomp$BECAMECEO, 1, 4))
execucomp$BECAMECEO = as.integer(substr(execucomp$BECAMECEO, 1, 4))

###############################################
#                                             #
#                MERGING STEP                 #
#                                             #
###############################################

# Merge execuComp and BoardEx based on the Ticker field
step1 = inner_join(x = execucomp, y = ids, by = c("GVKEY"= "gvkey"))
step2 = inner_join(x = step1, y = fin, by = c("GVKEY"= "gvkey", "YEAR" = "fyear"))
step3 = inner_join(x = step2, y = boardEx, by = c("tic"= "Ticker", "YEAR" = "AnnualReportDate"))
rawData = inner_join(x = step3, y = ISS, by = c("TICKER" = "Ticker", "YEAR" = "year"))

df = data.frame(rawData$GVKEY)

# ============== Control Variables ====================
df$ceoAge = rawData$AGE
df$ceoGender = ifelse(rawData$GENDER == "MALE", 1, 0)
df$firmSize = rawData$at
df$genderRatio = rawData$GenderRatio
df$industry = as.factor(substr(rawData$SPINDEX, 1, 2))
df$OtherBoards = ifelse(is.na(rawData$TotNoOthLstdBrd), 0, rawData$TotNoOthLstdBrd)
df$timeOtherComp = ifelse(is.na(rawData$AvgTimeOthCo), 0, rawData$AvgTimeOthCo)
df$year = rawData$YEAR
df$sales =rawData$sale
df$boardSize = rawData$NumberDirectors

# ================ Research variables ================
#CEO Duality
df$ceoDuality = ifelse(grepl("CEO|Chief Executive Officer", rawData$TITLEANN, ignore.case = TRUE) & grepl("Chairman|Chair", rawData$TITLEANN, ignore.case = TRUE), 1, 0)
hist(df$ceoDuality)

# CEO Tenure
df$ceoTenure <- rawData$YEAR - rawData$BECAMECEO
#df$ceoTenure2 = rawData$TimeRole

# CEO attendance
df$ceoAttendance = ifelse(rawData$Attend_LESS75_PCT == "Yes", 1, 0)

# CEO Voting power
df$ceoVotingPower = as.numeric(ifelse(is.na(rawData$Pcnt_Ctrl_Votingpower), 0, rawData$Pcnt_Ctrl_Votingpower))

# Bankruptcy score
df$bankruptcy_score = 3.3 * rawData$ebit/rawData$at + 1 * rawData$sale/rawData$at + 0.6 * rawData$mkvalt/rawData$lt + 1.2 * rawData$wcap/rawData$at + 1.4 * rawData$re/rawData$at

# --------------- Filters & transformations ----------------

negativeTenure = df[df$ceoTenure < 0,]
df = df[df$ceoTenure >= 0,]
hist(df$ceoTenure)
summary(df$ceoTenure)

df$ceoVotingPower[is.na(df$ceoVotingPower)] = 0


df = na.omit(df)



#remove duplicate GVkey-year combinations
df = df %>%
  group_by(rawData.GVKEY, year) %>%
  slice_head(n=1)

hist(df$bankruptcy_score, breaks = 100)



dfCor = df
dfCor$industry = NULL
correlation = cor(dfCor, method = c("spearman"))
stargazer(correlation)

summary(df)

# --------------- Plots ----------------
ggplot(df, 
  aes(x=ceoTenure, y=bankruptcy_score)) +
  geom_point() + 
  geom_smooth(method=glm, formula = bankruptcy_score ~ ceoTenure + ceoTenure^2, se=FALSE, fullrange=FALSE) +
  ggtitle("Plot of bankruptcy score vs CEO tenure") + 
  xlab("CEO charateristic") + ylab("Altman Z-score")

#Check group size and remove all panels of size 1
df = df %>%
  group_by(rawData.GVKEY) %>%
  mutate(freq = n()) %>% 
  ungroup()

df = df[df$freq > 1,]

###############################################
#                                             #
#                Regression                   #
#                                             #
###############################################

mdlA = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp
mdlB = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp + ceoTenure 
mdlC = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp + ceoTenure + ceoAttendance
mdlD = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp + ceoTenure + ceoAttendance + ceoVotingPower
mdlE = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality
mdlF = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + timeOtherComp + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality + I(ceoTenure^2)


df.p = pdata.frame(df, index=c("rawData.GVKEY", "year"))

occur = data.frame(table(row.names(df.p)))
duplicateRowNames = occur[occur$Freq > 1,]
table(index(df.p), useNA = "ifany")

#Check group size and remove all panels of size 1
df = df %>%
  group_by(rawData.GVKEY) %>%
  mutate(freq = n()) %>% 
  ungroup()


length(df$rawData.GVKEY)
summary(df.p)
stargazer(df.p, type = "latex", title="Descriptive statistics", digits=3)

rsltA = plm(mdlA, df.p,  model="within")
rsltB = plm(mdlB, df.p,  model="within")
rsltC = plm(mdlC, df.p,  model="within")
rsltD = plm(mdlD, df.p,  model="within")
rsltE = plm(mdlE, df.p,  model="within")
rsltF = plm(mdlF, df.p,  model="within")

rsltERandom = plm(mdlE, df.p,  model="random")
rsltEORS = plm(mdlE, df.p,  model="pooling")

# Hausman test
phtest(rsltE, rsltERandom)

pFtest(rsltE, rsltEORS) 

summary(rsltE)
stargazer(rsltA, rsltB, rsltC, rsltD, rsltE, rsltF, type = "latex", add.lines = list(c("Fixed Effect: Year", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Fixed Effect: Company", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))

###############################################
#                                             #
#            Machine Learning                 #
#                                             #
###############################################
df$bankruptcy_class = df$bankruptcy_score < 2.99


mdlA = as.factor(bankruptcy_class) ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize
mdlB = as.factor(bankruptcy_class) ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoTenure 
mdlC = as.factor(bankruptcy_class) ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoTenure + ceoAttendance
mdlD = as.factor(bankruptcy_class) ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoTenure + ceoAttendance + ceoVotingPower
mdlE = as.factor(bankruptcy_class) ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality


# Test the bankruptcy_class balance of the dataset
summary(df$bankruptcy_class)


# -----------------  Create training/test split ----------------------------  
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))


## set the seed to make your partition reproducible
train.index <- createDataPartition(df$bankruptcy_class, p = .7, list = FALSE)
train <- df[ train.index,]
test  <- df[-train.index,]

summary(train)
summary(test)

# K-fold Random Forest
numFolds <- caret::trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))

tree = caret::train(mdlE, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# -------------  Regression -------------------------
rsltReg <- lm(mdlE, data = train)

# -------------  Binomial regression ----------------
rsltLog <- glm(mdlE, data = train, family = binomial(link="logit"))


# ------------- classification tree ----------------
rsltTreeA <- rpart(mdlA, 
                   data=train,
                   method="class", 
                   parms = list(split = "information"))
rsltTreeB <- rpart(mdlE,
                   data=train,
                   method="class", 
                   parms = list(split = "information"))
# Make plots
rpart.plot(rsltTreeA, box.palette = "Blues", extra = 101, digits = 3)
rpart.plot(rsltTreeB, box.palette = "Blues", extra = 101, digits = 3)

plotcp(rsltTreeB)


# Print trees to find rules
print(rsltTreeA)
print(rsltTreeB)

# ------------- Random Forest ----------------
numA <- length(labels(terms(mdlA, data=train)))
numB <- length(labels(terms(mdlE, data=train)))

# Set the number of variables allowed per split
mA <- round(sqrt(numA))
mB <- round(sqrt(numB))

rsltFrstA <- randomForest(mdlA, data=train, ntree=100, mtry=mA, importance=TRUE, type="classification")
rsltFrstB <- randomForest(mdlE, data=train, ntree=100, mtry=mB, importance=TRUE, type="classification")

round(importance(rsltFrstA), 3)
round(importance(rsltFrstB), 3)

varImpPlot(rsltFrstA, pch = 19)
varImpPlot(rsltFrstB, pch = 19)

# ----------------------- gbm ---------------------

# Set seed of the random number generator to guarantee that
# the same starting point for randomization is selected
set.seed(1234)

# Number of trees
nTrees <- 50

# Train gbm with nTrees trees, depth = 2, shrinkage = 0.01 
# (learning rate). Different from randomForest, the function
# gbm specifies the distribution of the target by means of 
# parameter `family` -- as a result, the original formula
# mdlA/mdlB can be used
rsltGbmA <- gbm(mdlA, data=train, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)
rsltGbmB <- gbm(mdlE, data=train, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)

# Summary of the relative influence of the variables
# in the models
summary(rsltGbmA)
summary(rsltGbmB)



# ------------------- performance analysis ------------------

yvalue    <- test$bankruptcy_class
probTreeA <- predict(rsltTreeB, test, type="prob")[,2]
probFrstA <- predict(rsltFrstB, test, type="prob")[,2]
probGbmA  <- predict(rsltGbmB, test,  type="response", n.trees=nTrees)
probReg   <- predict(rsltReg, test,   type="response")
probLog   <- predict(rsltLog, test,   type="response")

predTreeA <- as.numeric(probTreeA > 0.5)
predFrstA <- as.numeric(probFrstA > 0.5)
predGbmA <-  as.numeric(probGbmA > 0.5)
predReg <-   as.numeric(probReg > 0.5)
predLog <-   as.numeric(probLog > 0.5)

# ---- II ----
length(test)
length(predTreeA)

table(Predicted = predTreeA, Observed = yvalue)
table(Predicted = predFrstA, Observed = yvalue)
table(Predicted = predGbmA,  Observed = yvalue)

# ---- I ----
pred.TreeA  <- prediction(probTreeA, yvalue)
pred.FrstA  <- prediction(probFrstA, yvalue)
pred.GbmA   <- prediction(probGbmA,  yvalue)
pred.Reg    <- prediction(probReg,   yvalue) 
pred.Log    <- prediction(probLog,   yvalue)

perf.TreeA  <- performance(pred.TreeA, measure="tpr", x.measure="fpr")
perf.FrstA  <- performance(pred.FrstA, measure="tpr", x.measure="fpr")
perf.GbmA   <- performance(pred.GbmA,  measure="tpr", x.measure="fpr")
perf.Reg    <- performance(pred.Reg,   measure="tpr", x.measure="fpr")
perf.Log    <- performance(pred.Log,   measure="tpr", x.measure="fpr")


plot(perf.TreeA, lty = 1, lwd = 2.0, col = "red", main="Classification performance on test set")
plot(perf.FrstA, lty = 1, lwd = 2.0, col = "blue", add = TRUE)
plot(perf.GbmA,  lty = 1, lwd = 2.0, col = "green", add = TRUE)
plot(perf.Reg,   lty = 1, lwd = 2.0, col = "gray", add = TRUE)
plot(perf.Log,   lty = 1, lwd = 2.0, col = "yellow", add = TRUE)

abline(a = 0, b = 1, lty = 3, lwd = 1.5)

legend(0.40,0.30, c("Classification tree",
                    "Random forests",
                    "Gradient boosting (50 Trees)",
                    "Linear Regression",
                    "Logistic Regression"),
       col = c("red", "blue", "green", "grey", "yellow"), lwd=3)

# ---- II ----
# Find auc values
aucTreeA <- performance(pred.TreeA, measure = "auc")@y.values[[1]]
aucFrstA <- performance(pred.FrstA, measure = "auc")@y.values[[1]]
aucGbmA  <- performance(pred.GbmA,  measure = "auc")@y.values[[1]]
aucReg   <- performance(pred.Reg,   measure = "auc")@y.values[[1]]
aucLog   <- performance(pred.Log,   measure = "auc")@y.values[[1]]
 
# Make data frame with results
dsAUC <- data.frame(
  Model=c("Classification tree", "Random forest", "Gradient boosting machine", "Linear Regression", "Logistic Regression"),
  AUC.A=c(aucTreeA, aucFrstA, aucGbmA, aucReg, aucLog))

stargazer(dsAUC, summary = FALSE,
          align = TRUE, no.space = TRUE, rownames = FALSE)

