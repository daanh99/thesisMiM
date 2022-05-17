library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(stargazer)

boardex = read.csv("data/char/Board Ex 17 May 2022.csv")
capitaliq = read.csv("data/char/capitaliq3.csv")
execucomp = read.csv("data/char/Execucomp May 17 2022.csv")
ids = read.csv("data/char/ciq common.csv")
fin = read.csv("data/char/Financial Ratios 17 May 2022.csv")


# Filter capital iq on acquire
capitaliq = filter(capitaliq, capitaliq$percentAcquired == 100 | capitaliq$percentOwnership == 100 | grepl("aquisition | aquire", capitaliq$TransAnnHeadline, ignore.case = TRUE))

#Turn each acquisition into a total number of acquisitions per company per year
capitaliq$announcedDate = substr(capitaliq$announcedDate, 1, 4)
capitaliq$announcedDate = as.integer(capitaliq$announcedDate)
#per company, how many times did 2000 / 2001 / 2002 occur in announced date
capitaliqAggregated = capitaliq %>%
  group_by(relcompanyid) %>%
  count(announcedDate)

capitaliqAggregated = rename(capitaliqAggregated, amountAquired = n)
  
# Keep all records that include CEO or chief executive officer
execucomp = filter(execucomp, grepl("CEO | Chief executive officer", execucomp$TITLE, ignore.case = TRUE))

# Change date to year
boardex$AnnualReportDate = as.integer(substr(boardex$AnnualReportDate, 1, 4))
execucomp$announcedDate = as.integer(substr(execucomp$announcedDate, 1, 4))
execucomp$BECAMECEO = as.integer(substr(execucomp$BECAMECEO, 1, 4))

###############################################
#
#            MERGING STEP
#
###############################################

# Merge execuComp and BoardEx based on the Ticker field
step1 = inner_join(x = boardex, y = execucomp, by = c("Ticker"= "TICKER", "AnnualReportDate" = "YEAR"))
step2 = inner_join(x = step1, y = ids, by = c("GVKEY"= "gvkey"))
step3 = inner_join(x = step2, y = fin, by = c("GVKEY"= "gvkey"))
df = left_join(x = step3, y = capitaliqAggregated, by = c("companyid" = "relcompanyid", "AnnualReportDate" = "announcedDate"))

rm(boardex, capitaliq, execucomp, step1, step2, ids, capitaliqAggregated, fin, step3)

df$TOTAL_ALT1[is.na(df$TOTAL_ALT1)] = 0
df$BONUS[is.na(df$BONUS)] = 0
df$amountAquired[is.na(df$amountAquired)] = 0


#df = select(df, "CONAME", "EXEC_FULLNAME", "amountAquired", "NumberDirectors", "BONUS", "TOTAL_ALT1", "AnnualReportDate","GenderRatio")

###############################################
#
#            Analysis
#
###############################################

# Filters
df = df[df$BONUS != 0 | df$TOTAL_ALT1 != 0, ]
df = df[!is.na(df$NumberDirectors), ]
df$compRatio = df$BONUS / (df$TOTAL_ALT1 + df$BONUS)


# Group by executive and average acquisitions, bonus, stock comp and num directors
#dfmean = df %>%
#  group_by(EXEC_FULLNAME) %>%
#  summarise_at(vars(amountAquired, BONUS, TOTAL_ALT1, NumberDirectors), list(mean = mean))

#dfmean$compRatio = dfmean$BONUS_mean / (dfmean$TOTAL_ALT1_mean + dfmean$BONUS_mean)
#dfmean = dfmean[dfmean$amountAquired_mean < 10, ]

df$compRatio = df$BONUS_mean / (df$TOTAL_ALT1_mean + df$BONUS_mean)

# --------------------------------------------- Descriptive stats -------------------------------

hist(dfmean$compRatio, breaks = 40)
hist(dfaqusyearly$amountAquired_mean)
hist(dfmean$compRatio, breaks = 40)

ggplot(df, aes(x=compRatio, y=amountAquired)) + geom_point() + geom_smooth(method=glm, method.args=list(family = "poisson"))  + ggtitle("Plot of average amount of aquisitions vs compensation ratio") +
  xlab("Compensation ratio") + ylab("Average aquisitions per year") +
  ylim(0, 10)

#============================== moderating factor 1: Board size -================================
# Fix directors
#dfmean$NumberDirectors_factor = cut(dfmean$NumberDirectors_mean, breaks = c(0, 8, 12, 30), labels = c("Small (0,8]", "Medium (8,12]", "Large 12,30]"))
#hist(dfmean$NumberDirectors_mean, breaks = 40)

#ggplot(dfmean, aes(x=compRatio, y=amountAquired_mean, color=NumberDirectors_factor)) +
#  geom_point() + 
#  geom_smooth(method=glm, method.args=list(family = "poisson") , se=FALSE, fullrange=FALSE) + ggtitle("Plot of average amount of aquisitions vs compensation ratio per board size") +
# xlab("Compensation ratio") + ylab("Average aquisitions per year") +
#  ylim(0, 10)



#============================== moderating factor 2: Yearly acquisitions -========================
# Calculate mean acquisitions per year
dfaqusyearly = df %>%
  group_by(AnnualReportDate) %>%
  summarise_at(vars(amountAquired), list(amountAquired_mean = mean))

ggplot(dfaqusyearly, aes(x=AnnualReportDate, y=amountAquired_mean)) + geom_bar(stat="identity")

df = inner_join(x = df, y = dfaqusyearly, by = c("AnnualReportDate"= "AnnualReportDate"))
#dfYearlyJoined = dfYearlyJoined[dfYearlyJoined$amountAquired < 10, ]

#dfYearlyJoined$amountAquired_factor = cut(dfYearlyJoined$amountAquired_mean, breaks = c(-1, 1.1, 1.25, 2), labels=c("cold", "normal", "hot"))

ggplot(df, aes(x=compRatio, y=amountAquired, color=amountAquired_factor)) +
  geom_point() + 
  geom_smooth(method=glm, method.args=list(family = "poisson"), se=FALSE, fullrange=TRUE)

ggplot(df, aes(x=compRatio, y=amountAquired)) +
  geom_point() + 
  geom_smooth(method=glm, method.args=list(family = "poisson"), se=FALSE, fullrange=TRUE) +
  ylim(0, 10)


# Descriptives and tables

#descriptive 1
stargazer(dfYearlyJoined, type = "html", title="Descriptive statistics", digits=1, out="descriptives.doc")


#============================== Control variables ===========================

df$aqusitionsNot0 = df$amountAquired > 0
#df$aquisition5yearAgo = df$
df$CEOTenure = df$BECAMECEO - df$AnnualReportDate

# Table 2
#----------------------------------------------------------
# Define models
#----------------------------------------------------------
mdlA <- amountAquired ~ AnnualReportDate + aqusitionsNot0 + GenderRatio + CONAME + AGE + roa + rd_sale + SPINDEX
mdlB <- amountAquired ~ AnnualReportDate + aqusitionsNot0 + GenderRatio + CONAME + AGE + roa + rd_sale + SPINDEX + NumberDirectors + amountAquired_mean
mdlC <- amountAquired ~ AnnualReportDate + aqusitionsNot0 + GenderRatio + CONAME + AGE + roa + rd_sale + SPINDEX + NumberDirectors + amountAquired_mean + amountAquired + compRatio


#----------------------------------------------------------
# Estimate the models
#----------------------------------------------------------
rsltA <- glm(mdlA, data = df, family = "poisson")
rsltB <- glm(mdlB, data = df, family = "poisson")
rsltC <- glm(mdlC, data = df, family = "poisson")



#----------------------------------------------------------
# Make a table (with stargazer)
#----------------------------------------------------------
stargazer(rsltA, rsltB, rsltC,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE)


