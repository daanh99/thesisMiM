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
library(car)
library(lmtest)
library(corrplot)
library(xtable)
library(collapse)



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


hist(df$ceoVotingPower, breaks = 100)
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

df = data.frame(rawData.GVKEY = as.factor(rawData$GVKEY))

# ============== Control Variables ====================
df$ceoAge =         rawData$AGE
df$ceoGender =      ifelse(rawData$GENDER == "MALE", 1, 0)
df$firmSize =       rawData$at / 1000
df$genderRatio =    rawData$GenderRatio
df$industry =       as.factor(substr(rawData$SPINDEX, 1, 2))
df$OtherBoards =    ifelse(is.na(rawData$TotNoOthLstdBrd), 0, rawData$TotNoOthLstdBrd)
df$timeOtherComp =  ifelse(is.na(rawData$AvgTimeOthCo), 0, rawData$AvgTimeOthCo)
df$year =           rawData$YEAR
#df$sales =          rawData$sale / 1000
df$boardSize =      rawData$NumberDirectors
df$ceoOwnership =   as.numeric(ifelse(is.na(rawData$SHROWN_TOT_PCT), 0, rawData$SHROWN_TOT_PCT))
df$employees =      rawData$emp

# Altman's drivers
#df$mkvalt = rawData$mkvalt / 1000
#df$ebit = rawData$ebit / 1000
#df$lt = rawData$lt / 1000
#df$wcap = rawData$wcap / 1000
#df$re = rawData$re / 1000
#df$at = rawData$at / 1000

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
df$altman_Z_score = 3.3 * rawData$ebit/rawData$at + 1 * rawData$sale/rawData$at + 0.6 * rawData$mkvalt/rawData$lt + 1.2 * rawData$wcap/rawData$at + 1.4 * rawData$re/rawData$at

# --------------- Filters & transformations ----------------

negativeTenure = df[df$ceoTenure < 0,]
df = df[df$ceoTenure >= 0,]
hist(df$ceoTenure)
summary(df$ceoTenure)

df$ceoVotingPower[is.na(df$ceoVotingPower)] = 0

lengthTotal = nrow(df)
df = na.omit(df)
lengthNoNA = nrow(df)


#remove duplicate GVkey-year combinations
df = df %>%
  group_by(rawData.GVKEY, year) %>%
  slice_head(n=1)

hist(df$altman_Z_score, breaks = 100)

dfCor = df
dfCor$industry = NULL
dfCor$Freq = NULL
dfCor$rawData.GVKEY = NULL
correlation = cor(dfCor, method = c("pearson"))
upper<-correlation
upper[upper.tri(correlation)]<-""
upper<-as.data.frame(upper)
upper


corstars(dfCor, method="pearson", removeTriangle = "upper", result = "latex")


summary(df)
hist(df$ceoOwnership, breaks=100)
hist(df$timeOtherComp)

hist(df$firmSize, breaks = 100)
hist(log(df$mkvalt), breaks = 100)
hist(log(df$ebit), breaks = 100)
hist(log(df$lt), breaks = 100)
hist(df$wcap, breaks = 100)
hist(df$re, breaks = 100)

hist(log(df$emp))


# --------------- Plots ----------------
ggplot(df, 
  aes(x=ceoTenure, y=altman_Z_score)) +
  geom_point() + 
  geom_smooth(method=glm, formula = altman_Z_score ~ ceoTenure + ceoTenure^2, se=FALSE, fullrange=FALSE) +
  ggtitle("Plot of bankruptcy score vs CEO tenure") + 
  xlab("CEO charateristic") + ylab("Altman Z-score")

#Check group size and remove all panels of size 1
df = df %>%
  group_by(rawData.GVKEY) %>%
  mutate(freq = n()) %>% 
  ungroup()

df = df[df$freq > 1,]


df.toScale = df
df.toScale$industry = NULL
df.toScale$rawData.GVKEY = NULL
df.scaled = data.frame(scale(df.toScale))

df.scaled$industry = df$industry
df.scaled$rawData.GVKEY = df$rawData.GVKEY

#df = df.scaled

dfPower = data.frame(ceoAttendance = df$ceoAttendance)
dfPower$ceoDuality = df$ceoDuality
dfPower$ceoTenure = df$ceoTenure
dfPower$ceoVotingPower = df$ceoOwnership
wdbc.pr = prcomp(dfPower, scale = TRUE)
wdbc.pr
screeplot(wdbc.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))

plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

df$ceoPower = 0.03801804 * df$ceoAttendance + 0.39843998 * df$ceoDuality + 0.67516645 * df$ceoTenure + 0.61963737 * df$ceoVotingPower


df <- df %>%                            # Add lagged column
  group_by(rawData.GVKEY) %>%
  dplyr::mutate(lagged_altman_Z_score = dplyr::lag(altman_Z_score, n = 1, default = NA)) %>% 
  as.data.frame()

df$lagged_altman_Z_score

###############################################
#                                             #
#                Regression                   #
#                                             #
###############################################

#log(mkvalt) + log(ebit) + log(lt) + log(wcap) + log(re) +

df <- df %>%                            # Add lagged column
  group_by(rawData.GVKEY) %>%
  dplyr::mutate(altman_z_change = altman_Z_score - lagged_altman_Z_score) %>% 
  as.data.frame()

df$altman_z_change

hist(df$altman_z_change, breaks=100)

mdlA = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership
mdlB = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership + ceoTenure 
mdlC = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership + ceoAttendance
mdlD = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership + ceoVotingPower
mdlE = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership + ceoDuality
mdlF = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership + ceoTenure + ceoAttendance + ceoVotingPower + ceoDuality
mdlG = log(altman_Z_score) ~ ceoAge + ceoGender + genderRatio + log(firmSize) + (employees) + industry + OtherBoards + boardSize + ceoOwnership


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

df.p = na.omit(df.p)

rsltA = plm(mdlA, df.p,  model="within")
rsltB = plm(mdlB, df.p,  model="within")
rsltC = plm(mdlC, df.p,  model="within")
rsltD = plm(mdlD, df.p,  model="within")
rsltE = plm(mdlE, df.p,  model="within")
rsltF = plm(mdlF, df.p,  model="within")
rsltG = plm(mdlG, df.p,  model="within")


rsltApool = plm(mdlA, df.p,  model="pooling")
rsltBpool = plm(mdlB, df.p,  model="pooling")
rsltCpool = plm(mdlC, df.p,  model="pooling")
rsltDpool = plm(mdlD, df.p,  model="pooling")
rsltEpool = plm(mdlE, df.p,  model="pooling")
rsltFpool = plm(mdlF, df.p,  model="pooling")


result = df.p
result$fitted = fitted(rsltE)
result$residual = resid(rsltE)

# Hausman test
rsltFRandom = plm(mdlF, df.p,  model="random")
phtest(rsltF, rsltFRandom)

summary(rsltF)
stargazer(rsltA, rsltB, rsltC, rsltD, rsltE, rsltF, type = "latex", dep.var.caption = "", add.lines = list(c("Fixed Effect: Year", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Fixed Effect: Company", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))
stargazer(rsltApool, rsltBpool, rsltCpool, rsltDpool, rsltEpool, rsltFpool, type = "latex", dep.var.caption = "")


#hetroskidacity test
lmtest::bptest(rsltE,
               data = df.p)

hist(df$ceoVotingPower, breaks = 100)
hist(df$ceoOwnership, breaks = 100)

summary(df)

stargazer(vif(rsltDpool))

lrtest(rsltA, rsltB, rsltC, rsltD, rsltE, rsltF)


mean(rsltA$residuals)
mean(rsltB$residuals)
mean(rsltC$residuals)
mean(rsltD$residuals)
mean(rsltE$residuals)
mean(rsltF$residuals)



qqnorm(rsltF$residuals)
abline(qqline(rsltF$residuals))
plot(df$altman_Z_score, rsltF$residuals)

hist(rsltF$residual, breaks=1000)

shapiro.test(rsltF[['residuals']])
qqnorm(rsltC$residuals)
abline(qqline(rsltF$residuals))

plot(df$ceoVotingPower, rsltF$residuals)

hist(df$GenderRatio)


plot(rsltA$residuals, rsltA$assign)




#Plot
ggplot(result) + geom_point(aes(fitted, residual))

plot(rsltE, which=1, col=c("blue")) # Residuals vs Fitted Plot



###############################################
#                                             #
#            Machine Learning                 #
#                                             #
###############################################
df$class = df$altman_Z_score < 2.99


#mdlA = as.factor(class) ~ cash + ebit + ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize
#mdlB = as.factor(class) ~  ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoTenure 
#mdlC = as.factor(class) ~  ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoOwnership + ceoTenure + ceoAttendance
#mdlD = as.factor(class) ~  ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoOwnership + ceoTenure + ceoAttendance
#mdlE = as.factor(class) ~  ceoAge + ceoGender + firmSize + genderRatio + industry + OtherBoards + boardSize + ceoOwnership + ceoTenure + ceoAttendance + ceoDuality
#mdlF = as.factor(class) ~  ceoTenure + ceoAttendance + ceoOwnership + ceoDuality


# Test the class balance of the dataset
summary(df$class)


# -----------------  Create training/test split ----------------------------  
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))


## set the seed to make your partition reproducible
nID <- length(unique(df$rawData.GVKEY))
p = 0.75
set.seed(123)
inTrainID <- sample(unique(df$rawData.GVKEY), round(nID * p), replace=FALSE)

train <- df[df$rawData.GVKEY %in% inTrainID, ] 
test <- df[!df$rawData.GVKEY %in% inTrainID, ]


summary(train$class)
summary(test$class)

intersect(train$rawData.GVKEY, test$rawData.GVKEY)

# K-fold Random Forest
numFolds <- caret::trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.001, 0.5, 0.001))


#tree = caret::train(mdlE, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

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

yvalue    <- test$class
probTreeA <- predict(rsltTreeB, test, type="prob")[,2]
probFrstA <- predict(rsltFrstB, test, type="prob")[,2]
probGbmA  <- predict(rsltGbmB, test,  type="response", n.trees=nTrees)
probLog   <- predict(rsltLog, test,   type="response")

predTreeA <- as.numeric(probTreeA > 0.5)
predFrstA <- as.numeric(probFrstA > 0.5)
predGbmA <-  as.numeric(probGbmA > 0.5)
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
pred.Log    <- prediction(probLog,   yvalue)

perf.TreeA  <- performance(pred.TreeA, measure="tpr", x.measure="fpr")
perf.FrstA  <- performance(pred.FrstA, measure="tpr", x.measure="fpr")
perf.GbmA   <- performance(pred.GbmA,  measure="tpr", x.measure="fpr")
perf.Log    <- performance(pred.Log,   measure="tpr", x.measure="fpr")


plot(perf.TreeA, lty = 1, lwd = 2.0, col = "red", main="Classification performance on test set (With control vars)")
plot(perf.FrstA, lty = 1, lwd = 2.0, col = "blue", add = TRUE)
plot(perf.GbmA,  lty = 1, lwd = 2.0, col = "green", add = TRUE)
plot(perf.Log,   lty = 1, lwd = 2.0, col = "yellow", add = TRUE)

abline(a = 0, b = 1, lty = 3, lwd = 1.5)

legend(0.40,0.30, c("Classification tree",
                    "Random forests",
                    "Gradient boosting (50 Trees)",
                    "Logistic Regression"),
       col = c("red", "blue", "green", "yellow"), lwd=3)

# ---- II ----
# Find auc values
aucTreeA <- performance(pred.TreeA, measure = "auc")@y.values[[1]]
aucFrstA <- performance(pred.FrstA, measure = "auc")@y.values[[1]]
aucGbmA  <- performance(pred.GbmA,  measure = "auc")@y.values[[1]]
aucLog   <- performance(pred.Log,   measure = "auc")@y.values[[1]]
 
# Make data frame with results
dsAUC <- data.frame(
  Model=c("Classification tree", "Random forest", "Gradient boosting machine", "Logistic Regression"),
  AUC.A=c(aucTreeA, aucFrstA, aucGbm,, aucLog))

stargazer(dsAUC, summary = FALSE,
          align = TRUE, no.space = TRUE, rownames = FALSE)




corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  #mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  mystars <- ""
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
