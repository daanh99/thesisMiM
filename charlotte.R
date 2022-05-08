#install.packages("dplyr")

library(dplyr)
library(fuzzyjoin)
library(ggplot2)


boardex = read.csv("data/boardex companyid.csv")
capitaliq = read.csv("data/capitaliq3.csv")
execucomp = read.csv("data/execucomp may 7 3.20pm.csv")
ids = read.csv("data/ciq common.csv")


# Filter capitaliq on aquire
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

###############################################
#
#            MERGING STEP
#
###############################################

# Merge execuComp and BoardEx based on the Ticker field
step1 = inner_join(x = boardex, y = execucomp, by = c("Ticker"= "TICKER", "AnnualReportDate" = "YEAR"))
step2 = inner_join(x = step1, y = ids, by = c("GVKEY"= "gvkey"))
df = left_join(x = step2, y = capitaliqAggregated, by = c("companyid" = "relcompanyid", "AnnualReportDate" = "announcedDate"))

rm(boardex, capitaliq, execucomp, step1, step2, ids, capitaliqAggregated)

df$TOTAL_ALT1[is.na(df$TOTAL_ALT1)] = 0
df$BONUS[is.na(df$BONUS)] = 0
df$amountAquired[is.na(df$amountAquired)] = 0


df = select(df, "CONAME", "EXEC_FULLNAME", "amountAquired", "NumberDirectors", "BONUS", "TOTAL_ALT1", "AnnualReportDate")

###############################################
#
#            Analysis
#
###############################################

# Filters
df = df[df$BONUS != 0 | df$TOTAL_ALT1 != 0, ]
df = df[!is.na(df$NumberDirectors), ]


# Group by executive and average aquisitions, bonus, stock comp and num directors
dfmean = df %>%
  group_by(EXEC_FULLNAME) %>%
  summarise_at(vars(amountAquired, BONUS, TOTAL_ALT1, NumberDirectors), list(mean = mean))

dfmean$compRatio = dfmean$BONUS_mean / (dfmean$TOTAL_ALT1_mean + dfmean$BONUS_mean)
dfmean = dfmean[dfmean$amountAquired_mean < 10, ]


hist(dfaqusyearly$amountAquired_mean)

hist(dfmean$compRatio, breaks = 40)

ggplot(dfmean, aes(x=compRatio, y=amountAquired_mean)) + geom_point() + geom_smooth(method=lm) 

#============================== moderating factor 1: Board size -========================
# Fix directors
dfmean$NumberDirectors_factor = cut(dfmean$NumberDirectors_mean, breaks = c(0, 8, 12, 30))
hist(dfmean$NumberDirectors_mean, breaks = 40)


ggplot(dfmean, aes(x=compRatio, y=amountAquired_mean, color=NumberDirectors_factor)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) 



#============================== moderating factor 2: Yearly acquisitions -========================
# Calculate mean acquisitions per year
dfaqusyearly = df %>%
  group_by(AnnualReportDate) %>%
  summarise_at(vars(amountAquired), list(amountAquired_mean = mean))


ggplot(dfaqusyearly, aes(x=AnnualReportDate, y=amountAquired_mean)) + geom_bar(stat="identity")

dfYearlyJoined = inner_join(x = df, y = dfaqusyearly, by = c("AnnualReportDate"= "AnnualReportDate"))
dfYearlyJoined$compRatio = dfYearlyJoined$BONUS / (dfYearlyJoined$TOTAL_ALT1 + dfYearlyJoined$BONUS)
#dfYearlyJoined$amountAquired_mean[is.na(dfYearlyJoined$amountAquired_mean)] = 0
dfYearlyJoined = dfYearlyJoined[dfYearlyJoined$amountAquired < 10, ]

dfYearlyJoined$amountAquired_factor = cut(dfYearlyJoined$amountAquired_mean, breaks = c(-1, 1.1, 1.25, 2), labels=c("cold", "normal", "hot"))

ggplot(dfYearlyJoined, aes(x=compRatio, y=amountAquired, color=amountAquired_factor)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) 





