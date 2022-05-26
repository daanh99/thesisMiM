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


ISS = read.csv2("data/daan/ISS.csv", head = TRUE, sep=",")
boardEx = read.csv("data/daan/boardex2.csv")
execucomp = read.csv("data/daan/execucomp.csv") 
ids = read.csv("data/char/ciq common.csv")
fin = read.csv("data/daan/Fundamentals Daan.csv")


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
df$TimeUntilRetire = rawData$TimeRetirement
df$year = rawData$YEAR
df$sales =rawData$sale

# ================ Research variables ================
#CEO Duality
df$ceoDuality = ifelse(grepl("CEO|Chief Executive Officer", rawData$TITLEANN, ignore.case = TRUE) & grepl("Chairman|Chair", rawData$TITLEANN, ignore.case = TRUE), 1, 0)
hist(df$ceoDuality)

# CEO Tenure
df$ceoTenure <- rawData$YEAR - rawData$BECAMECEO
df$ceoTenure2 = rawData$TimeRole

# CEO attendance
df$ceoAttendance = ifelse(rawData$Attend_LESS75_PCT == "Yes", 1, 0)

# CEO Voting power
df$ceoVotingPower = as.numeric(ifelse(is.na(rawData$Pcnt_Ctrl_Votingpower), 0, rawData$Pcnt_Ctrl_Votingpower))

# Bankruptcy score
df$bankruptcy_score = 3.3 * rawData$ebit/rawData$at + 1 * rawData$sale/rawData$at + 0.6 * rawData$mkvalt/rawData$lt + 1.2 * rawData$wcap/rawData$at + 1.4 * rawData$re/rawData$at

negativeTenure = df[df$ceoTenure < 0,]
df = df[df$ceoTenure >= 0,]
hist(df$ceoTenure)
summary(df$ceoTenure)

df$ceoVotingPower[is.na(df$ceoVotingPower)] = 0

summary(df)



df = na.omit(df)

hist(df$bankruptcy_score, breaks = 100)

df = df[df$bankruptcy_score < 20]

summary(df)

df$bankruptcy_class = df$bankruptcy_score < 2.99


#remove duplicate GVkey-year combinations
df = df %>%
  group_by(rawData.GVKEY, year) %>%
  slice_head(n=1)


dfCor = df
dfCor$industry = NULL
cor(dfCor, method = c("spearman"))

ggplot(df, 
  aes(x=ceoTenure, y=bankruptcy_score)) +
  geom_point() + 
  geom_smooth(method=glm , se=FALSE, fullrange=FALSE) +
  ggtitle("Plot of average amount of aquisitions vs compensation ratio per board size") + 
  xlab("CEO charateristic") + ylab("Altman Z-score")

###############################################
#                                             #
#                Regression                   #
#                                             #
###############################################


mdlA = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards
mdlB = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure 
mdlC = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance
mdlD = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance + ceoVotingPower
mdlE = bankruptcy_score ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality

df.p = pdata.frame(df, index=c("rawData.GVKEY", "year"))
summary(df.p)


occur = data.frame(table(row.names(df.p)))
duplicateRowNames = occur[occur$Freq > 1,]

#Check group size
grouped = df.p %>%
  group_by(rawData.GVKEY) %>%
  summarise(n())

rsltA = plm(mdlE, df.p, family = "binomial",  model="within")

summary(rsltA)
stargazer(rsltA, type = "text")

###############################################
#                                             #
#            Machine Learning                 #
#                                             #
###############################################

mdlA = bankruptcy_class ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards
mdlB = bankruptcy_class ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure 
mdlC = bankruptcy_class ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance
mdlD = bankruptcy_class ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance + ceoVotingPower
mdlE = bankruptcy_class ~ ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality


# ------------- classification tree ----------------
rsltTreeA <- rpart(mdlA, 
                   data=df,
                   method="class", 
                   parms = list(split = "information"))
rsltTreeB <- rpart(mdlE,
                   data=df,
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
numA <- length(labels(terms(mdlA, data=df)))
numB <- length(labels(terms(mdlE, data=df)))

# Set the number of variables allowed per split
mA <- round(sqrt(numA))
mB <- round(sqrt(numB))

rsltFrstA <- randomForest(mdlA, data=df, ntree=100, mtry=mA, importance=TRUE)
rsltFrstB <- randomForest(mdlE, data=df, ntree=100, mtry=mB, importance=TRUE)

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
rsltGbmA <- gbm(mdlA, data=df, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)
rsltGbmB <- gbm(mdlE, data=df, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)

# Summary of the relative influence of the variables
# in the models
summary(rsltGbmA)
summary(rsltGbmB)



# ------------------- performance analysis ------------------

yvalue    <- df$bankruptcy_class
probTreeA <- predict(rsltTreeB, type = "vector")
probFrstA <- predict(rsltFrstB, type="response")
probGbmA  <- predict(rsltGbmB, type="response", n.trees=nTrees)

predTreeA <- as.numeric(probTreeA > 0.5)
predFrstA <- as.numeric(probFrstA > 0.5)
predGbmA <- as.numeric(probGbmA > 0.5)

# ---- II ----
summary(yvalue)

table(Predicted = predTreeA, Observed = yvalue)
table(Predicted = predFrstA, Observed = yvalue)
table(Predicted = predGbmA, Observed = yvalue)


# ---- I ----
pred.TreeA  <- prediction(probTreeA, yvalue)
pred.FrstA  <- prediction(probFrstA, yvalue)
pred.GbmA   <- prediction(probGbmA,  yvalue)

perf.TreeA  <- performance(pred.TreeA, measure="tpr", x.measure="fpr")
perf.FrstA  <- performance(pred.FrstA, measure="tpr", x.measure="fpr")
perf.GbmA   <- performance(pred.GbmA,  measure="tpr", x.measure="fpr")

plot(perf.TreeA, lty = 1, lwd = 2.0, col = "red", main="Classification performance")
plot(perf.FrstA, lty = 1, lwd = 2.0, col = "blue", add = TRUE)
plot(perf.GbmA,  lty = 1, lwd = 2.0, col = "green", add = TRUE)


abline(a = 0, b = 1, lty = 3, lwd = 1.5)

legend(0.40,0.20, c("Classification tree",
                    "Random forests",
                    "Gradient boosting (50 Trees)"
                    ),
       col = c("red", "blue", "green"), lwd=3)

# ---- II ----
# Find auc values
aucTreeA <- performance(pred.TreeA, measure = "auc")@y.values[[1]]
aucFrstA <- performance(pred.FrstA, measure = "auc")@y.values[[1]]
aucGbmA  <- performance(pred.GbmA,  measure = "auc")@y.values[[1]]

# Make data frame with results
dsAUC <- data.frame(
  Model=c("Classification tree", "Random forest", "Gradient boosting machine"),
  AUC.A=c(aucTreeA, aucFrstA, aucGbmA))

stargazer(dsAUC, summary = FALSE,
          align = TRUE, no.space = TRUE, rownames = FALSE)

stargazer(dsAUC, summary = FALSE,
          align = TRUE, no.space = TRUE, rownames = FALSE,
          type="html", out="table.doc")

