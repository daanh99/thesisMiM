dfSurvey <- read.csv(file = "surveyResults.csv", stringsAsFactors = TRUE)

library(stargazer)
library(dplyr)
#install.packages("magrittr")
library(magrittr)
#install.packages("randomForest")
library(randomForest) 
#install.packages("car",dependencies = TRUE)
library(car)
library(ROCR)
library(randomForest) 
library(gbm) 
library(rpart)
library(rpart.plot)
library(psych)

colnames(dfSurvey) <- c("Timestamp",	"DG01Participation",	"DG01Age",	"DG02Nationality",	"DG03Fluency",	"LE01Edu",	"BC01TimesScooter",	"BC02regulations",	"BC03regulationChange",	"EU01useScooter",	"EU02ScooterAfterRegu",	"EU03regulationImpact",	"EC01Environ",	"EC02Planet",	"EC03Effort",	"AT01OwnVehicle",	"AR01TrustGov",	"AR02Rules",	"AR03Rules2",	"AR04Rules3",	"SS01Income",	"SS02buyScooter",	"SS03FinancialProblems",	"SS04Content")

# -----------------------------------------------------------------------------------------------------
# ------------------------ Cleaning the data ---------------------------------------------------------
# -----------------------------------------------------------------------------------------------------

# remove unnecessary columns
dfSurvey <- subset(dfSurvey, select = -c(Timestamp))
dfSurvey <- subset(dfSurvey, select = -c(DG01Participation))

# remove incomplete rows
colSums(is.na(dfSurvey))

# remove incomplete rows
dfSurvey[dfSurvey==""]<-NA
dfSurvey[dfSurvey==" "]<-NA

colSums(is.na(dfSurvey))
dfSurvey <- dfSurvey[complete.cases(dfSurvey),]
dfSurvey <- droplevels(dfSurvey)


# print summary for descriptive
summary(dfSurvey)

dfSurvey$DG02Nationality <- ifelse(dfSurvey$DG02Nationality == "Dutch",1, 0)

dfSurvey$BC01TimesScooter <- as.numeric(dfSurvey$BC01TimesScooter,                c("Not at all" = "1", "0 - 5 times" = "2", "5 - 10 times" = "3", "10 - 20 times" = "4", "more than 20 times" = "5"))
dfSurvey$BC02regulations <- as.numeric(dfSurvey$BC02regulations,                  c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$BC03regulationChange <- as.numeric(dfSurvey$BC03regulationChange,        c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EU01useScooter <- as.numeric(dfSurvey$EU01useScooter,                    c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EU02ScooterAfterRegu <- as.numeric(dfSurvey$EU02ScooterAfterRegu,        c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EU03regulationImpact <- as.numeric(dfSurvey$EU03regulationImpact,        c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EC01Environ <- as.numeric(dfSurvey$EC01Environ,                          c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EC02Planet <- as.numeric(dfSurvey$EC02Planet,                            c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$EC03Effort <- as.numeric(dfSurvey$EC03Effort,                            c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$AT01OwnVehicle <- as.numeric(dfSurvey$AT01OwnVehicle,                    c("Yes"="1", "No"="0"))
dfSurvey$AR01TrustGov <- as.numeric(dfSurvey$AR01TrustGov,                        c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$AR02Rules <- as.numeric(dfSurvey$AR02Rules,                              c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$AR03Rules2 <- as.numeric(dfSurvey$AR03Rules2,                            c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$AR04Rules3 <- as.numeric(dfSurvey$AR04Rules3,                            c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$SS01Income <- as.numeric(dfSurvey$SS01Income,                            c("0â¬" = "1", "0â¬ - 9,999â¬" = "2", "10,000â¬ - 24,999â¬" = "3", "25,000â¬ - 49,999" = "4", "50,000â¬ or more" = "5", "I'd rather not say" = "6"))
dfSurvey$SS02buyScooter <- as.numeric(dfSurvey$SS02buyScooter,                    c("Yes"="1", "No"="0"))
dfSurvey$SS03FinancialProblems <- as.numeric(dfSurvey$SS03FinancialProblems,      c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))
dfSurvey$SS04Content <- as.numeric(dfSurvey$SS04Content,                          c("Strongly Disagree" = "1", "Disagree" = "2", "Neutral" = "3", "Agree" = "4", "Strongly Agree" = "5"))

dfSurvey$SS01Income[dfSurvey$SS01Income=="6"] <- "1"

dfSurvey$SS01Income <- as.numeric(dfSurvey$SS01Income)

dfSurvey <- dfSurvey %>% mutate(BC02regulations = case_when(BC02regulations == 1 ~ 5,
                                                            BC02regulations == 2 ~ 4,
                                                            BC02regulations == 3 ~ 3,
                                                            BC02regulations == 4 ~ 2,
                                                            BC02regulations == 5 ~ 1),
                                AR03Rules2 = case_when(AR03Rules2 == 1 ~ 5,
                                                       AR03Rules2 == 2 ~ 4,
                                                       AR03Rules2 == 3 ~ 3,
                                                       AR03Rules2 == 4 ~ 2,
                                                       AR03Rules2 == 5 ~ 1),
                                AR04Rules3 = case_when(AR04Rules3 == 1 ~ 5,
                                                       AR04Rules3 == 2 ~ 4,
                                                       AR04Rules3 == 3 ~ 3,
                                                       AR04Rules3 == 4 ~ 2,
                                                       AR04Rules3 == 5 ~ 1),
                                SS03FinancialProblems = case_when(SS03FinancialProblems == 1 ~ 5,
                                                                  SS03FinancialProblems == 2 ~ 4,
                                                                  SS03FinancialProblems == 3 ~ 3,
                                                                  SS03FinancialProblems == 4 ~ 2,
                                                                  SS03FinancialProblems == 5 ~ 1))


#Content with financial situation is related to having no financial problems 

#more data cleaning

#Data cleaning for age
dfSurvey$DG01Age <- as.character(dfSurvey$DG01Age)
dfSurvey[dfSurvey$DG01Age == "Thirty three",1] <- "33"
dfSurvey$DG01Age <- as.numeric(dfSurvey$DG01Age)

#Data cleaning for nationality
dfSurvey$DG02Nationality <- ifelse(dfSurvey$DG02Nationality == "Dutch",1, 0)

#Data cleaning for education
dfSurvey[dfSurvey$LE01Edu == "BSc of Economics and Business at UU", 4] <- "Research university/WO graduate"
dfSurvey[dfSurvey$LE01Edu == "Bachelor Degree", 4] <- "Research university/WO graduate"
dfSurvey[dfSurvey$LE01Edu == "Bachelors", 4] <- "Research university/WO graduate"

#Assign values to their normal categories
dfSurvey[dfSurvey$LE01Edu == "Associate's Degree", 4] <- "University of Applied Sciences/HBO graduate"
dfSurvey[dfSurvey$LE01Edu == "Post-graduate teaching credential", 4] <- "Research university/WO graduate"

#remove empty columns
dfSurvey <- droplevels(dfSurvey)

# ------------------------------------------------------------------------------------------------------
# --------------------------------- Descriptives -------------------------------------------------------
# -----------------------------------------------------------------------------------------------------

summary(dfSurvey$DG01Age)
summary(as.factor(dfSurvey$DG02Nationality))
summary(dfSurvey$LE01Edu)
summary(as.factor(dfSurvey$BC01TimesScooter))
summary(as.factor(dfSurvey$BC02regulations))
summary(as.factor(dfSurvey$BC03regulationChange))
summary(as.factor(dfSurvey$EU01useScooter))
summary(as.factor(dfSurvey$EU02ScooterAfterRegu))
summary(as.factor(dfSurvey$EU03regulationImpact))
summary(as.factor(dfSurvey$EC01Environ))
summary(as.factor(dfSurvey$EC02Planet))
summary(as.factor(dfSurvey$EC03Effort))
summary(as.factor(dfSurvey$AT01OwnVehicle))
summary(as.factor(dfSurvey$AR01TrustGov))
summary(as.factor(dfSurvey$AR02Rules))
summary(as.factor(dfSurvey$AR03Rules2))
summary(as.factor(dfSurvey$AR04Rules3))
summary(as.factor(dfSurvey$SS01Income))
summary(as.factor(dfSurvey$SS02buyScooter))
summary(as.factor(dfSurvey$SS03FinancialProblems))
summary(as.factor(dfSurvey$SS04Content))


# -----------------------------------------------------------------------------------------------------
# ----------------------------------------- Analysis --------------------------------------------------
# -----------------------------------------------------------------------------------------------------

# Dependent variable
dfSurvey$Escooter_AR <- ifelse(dfSurvey$BC01TimesScooter > 1, 1, 0)
dfSurvey$Escooter_AR <- as.factor(dfSurvey$Escooter_AR)

# Creating the independent variables
dfSurvey$AttRules <- (dfSurvey$AR01TrustGov + dfSurvey$AR02Rules + dfSurvey$AR03Rules2 + dfSurvey$AR04Rules3) / 4
dfSurvey$Environment <- (dfSurvey$EC01Environ + dfSurvey$EC02Planet + dfSurvey$EC03Effort) / 3
dfSurvey$B_Change <- (dfSurvey$BC02regulations + dfSurvey$BC03regulationChange) / 2
dfSurvey$Av_Transport <- as.numeric(dfSurvey$AT01OwnVehicle)
dfSurvey$Economic <- (dfSurvey$SS01Income + dfSurvey$SS02buyScooter + dfSurvey$SS03FinancialProblems + dfSurvey$SS04Content) / 4

summary(dfSurvey$Escooter_AR)

# Summary of the constructed variables
stargazer(rbind(summary(dfSurvey$AttRules), summary(dfSurvey$Environment), summary(dfSurvey$B_Change), summary(dfSurvey$Av_Transport)), type = "html", out="summaryStatistics.doc")

#define the models 
#control model
mdlA <- Escooter_AR ~ DG01Age + DG02Nationality + LE01Edu

# model
mdlB <- Escooter_AR ~ AttRules + B_Change + Environment + Av_Transport + DG01Age + DG02Nationality + LE01Edu

#- -------------- Internal consistency -------------

alpha.AttRules <- alpha(dfSurvey[c("AR01TrustGov", "AR02Rules", "AR03Rules2", "AR04Rules3")], check.keys = FALSE)
alpha.Environment <- alpha(dfSurvey[c("EC01Environ", "EC02Planet", "EC03Effort")], check.keys = FALSE)
alpha.B_Change <- alpha(dfSurvey[c("BC01TimesScooter", "BC02regulations", "BC03regulationChange")], check.keys = FALSE)
alpha.EU <- alpha(dfSurvey[c("EU01useScooter", "EU02ScooterAfterRegu", "EU03regulationImpact")], check.keys = FALSE)
alpha.Economics <- alpha(dfSurvey[c("SS03FinancialProblems", "SS04Content")], check.keys = FALSE)

summary(alpha.AttRules)
summary(alpha.Environment)
summary(alpha.B_Change)
summary(alpha.Economics)


#




# -----------------------------------------------------------------------------------------------------
# ------------------------------------ Multivariate analysis ------------------------------------------
# -----------------------------------------------------------------------------------------------------
#model estimations of model A & B

rsltA <- lm(mdlA, data = dfSurvey)   

rsltB <- lm(mdlB, data = dfSurvey)

# summary of the estimation results 

summary(rsltA)
summary(rsltB)

#check multicollinearity of applied to estimation of model B

# to determine vif and tolerance
vif(rsltB)
1/vif(rsltB) 
alias( lm( mdlB ) )

vif <- vif(rsltB)[,1]
tol <- 1/vif(rsltB)[,1]
vif(rsltB)[,1]
cbind(vif, tol)

#calculate the confidence interval 
confint(rsltB)

view(dfSurvey)
#apply regression analysis 
rsltB <-lm(mdlB, data = dfSurvey)
rsltB.beta <- lm.beta ( rsltB )
summary(rsltB)

#calculation of model fit of both models A and B

R2 <- ssr / sst
R <- sqrt ( R2 )
R2adj <- 1 - ( sse / dfE ) /( sst / dfT )
rmse <- sqrt ( sse / dfE )

str(summary(rsltA))
rmse <- summary(rsltA)$sigma
r2 <- summary(rsltA)$r.squared 
r2adj <- summary(rsltA)$adj.r.squared

# Show outcomes
#cbind ( r2 , r2adj , rmse )
round(cbind(rmse, r2, r2adj), 3)

summary(rsltB)
rmse <- summary(rsltB)$sigma
r2 <- summary(rsltB)$r.squared 
r2adj <- summary(rsltB)$adj.r.squared
# Show outcomes
#cbind ( r2 , r2adj , rmse )
round(cbind(rmse, r2, r2adj), 3)


#overview result of regression analysis table 

stargazer(rsltA, rsltB, align = TRUE, intercept.bottom = FALSE, type = "html", out = "Multivariate Regression Results.doc")


# -----------------------------------------------------------------------------------------------------
# ------------------------------------ Predictive analysis --------------------------------------------
# -----------------------------------------------------------------------------------------------------
# logistic regression
glm(mdlA.n, data = dsBank.Train, 
    family = binomial(link="logit"))


# ------------- classification tree ----------------
rsltTreeA <- rpart(mdlA, 
                   data=dfSurvey,
                   method="class", 
                   parms = list(split = "information"))
rsltTreeB <- rpart(mdlB,
                   data=dfSurvey,
                   method="class", 
                   parms = list(split = "information"))
# Make plots
rpart.plot(rsltTreeA, box.palette = "Blues", extra = 101, digits = 3)
rpart.plot(rsltTreeB, extra = 101, digits = 3)

# Print trees to find rules
print(rsltTreeA)
print(rsltTreeB)

# ------------- Random Forest ----------------
numA <- length(labels(terms(mdlA, data=dfSurvey)))
numB <- length(labels(terms(mdlB, data=dfSurvey)))

# Set the number of variables allowed per split
mA <- round(sqrt(numA))
mB <- round(sqrt(numB))

rsltFrstA <- randomForest(mdlA, data=dfSurvey, ntree=100, mtry=mA, importance=TRUE)
rsltFrstB <- randomForest(mdlB, data=dfSurvey, ntree=100, mtry=mB, importance=TRUE)

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
rsltGbmA <- gbm(mdlA, data=dfSurvey, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)
rsltGbmB <- gbm(mdlB, data=dfSurvey, distribution="gaussian",
                n.trees=nTrees, interaction.depth=2, shrinkage = 0.01,
                bag.fraction=0.5,
                n.minobsinnode=5)

# Summary of the relative influence of the variables
# in the models
summary(rsltGbmA)
summary(rsltGbmB)



# ------------------- performance analysis ------------------

yvalue    <- dfSurvey$Escooter_AR
probTreeA <- predict(rsltTreeB, type = "prob")[,2]
probFrstA <- predict(rsltFrstB, type="prob")[,2]
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
plot(perf.GbmA,  lty = 1, lwd = 2.0, col = "green", add = TRUE)


abline(a = 0, b = 1, lty = 3, lwd = 1.5)

legend(0.40,0.20, c("Classification tree",
                    "Random forests",
                    "Gradient boosting (50 Trees)"),
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