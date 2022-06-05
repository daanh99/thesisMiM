library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(stargazer)
library(plm)

boardex = read.csv("data/char/Board Ex 1999.csv")
capitaliq = read.csv("data/char/capitaliq3.csv")
execucomp = read.csv("data/char/Execucomp 1995.csv")
ids = read.csv("data/char/ciq common.csv")
fin = read.csv("data/char/Compustat Fundamentals Annual 1995.csv")


# Filter capital iq on acquire
capitaliq = filter(capitaliq, capitaliq$percentAcquired == 100 | capitaliq$percentOwnership == 100 | grepl("aquisition|aquire", capitaliq$TransAnnHeadline, ignore.case = TRUE))

#Turn each acquisition into a total number of acquisitions per company per year
capitaliq$announcedDate = substr(capitaliq$announcedDate, 1, 4)
capitaliq$announcedDate = as.integer(capitaliq$announcedDate)
#per company, how many times did 2000 / 2001 / 2002 occur in announced date
capitaliqAggregated = capitaliq %>%
  group_by(relcompanyid) %>%
  count(announcedDate)

capitaliqAggregated = rename(capitaliqAggregated, amountAquired = n)

  
# Keetp all records that include CEO or chief executive officer
#execucomp = filter(execucomp, grepl("CEO | Chief executive officer", execucomp$TITLE, ignore.case = TRUE))
execucomp = filter(execucomp, execucomp$CEOANN == "CEO")

# Change date to year
boardex$AnnualReportDate = as.integer(substr(boardex$AnnualReportDate, 1, 4))
execucomp$BECAMECEO = as.integer(substr(execucomp$BECAMECEO, 1, 4))

###############################################
#
#            MERGING STEP
#
###############################################

# Merge execuComp and BoardEx based on the Ticker field
step1 = inner_join(x = boardex, y = execucomp, by = c("Ticker"= "TICKER", "AnnualReportDate" = "YEAR"))
step2 = inner_join(x = step1, y = ids, by = c("GVKEY"= "gvkey"))
step3 = left_join(x = step2, y = fin, by = c("GVKEY"= "gvkey", "AnnualReportDate" = "fyear"))
df = left_join(x = step3, y = capitaliqAggregated, by = c("companyid" = "relcompanyid", "AnnualReportDate" = "announcedDate"))

#rm(boardEx, capitaliq, execucomp, step1, step2, ids, capitaliqAggregated, fin, step3)

df$TOTAL_ALT1[is.na(df$TOTAL_ALT1)] = 0
df$BONUS[is.na(df$BONUS)] = 0
df$amountAquired[is.na(df$amountAquired)] = 0

#============================== Control variables ===========================

df$aqusitionsNot0 = df$amountAquired > 0
df$CEOTenure = df$AnnualReportDate - df$BECAMECEO
df$roa = df$ni / df$at
df$aquisitionFin = df$aqc
df$SPINDEX = as.factor(substr(df$SPINDEX, 1, 2))

df <- df %>%                            # Add lagged column
  group_by(CONAME) %>%
  dplyr::mutate(laggedAquisition = dplyr::lag(amountAquired, n = 1, default = NA)) %>% 
  as.data.frame()


summary(df$CEOTenure)
hist(df$CEOTenure)


negativeTenure = df[df$CEOTenure < 0,]
df = df[df$CEOTenure >= 0,]

#Remove complete NA rows
df <- df[!apply(is.na(df), 1, all),]

#remove duplicate GVkey-year combinations
df = df %>%
  group_by(GVKEY, AnnualReportDate) %>%
  slice_head(n=1)

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

df$compRatio = df$BONUS / (df$TOTAL_ALT1 + df$BONUS)

# --------------------------------------------- Descriptive stats -------------------------------

hist(df$compRatio, breaks = 40)
hist(df$amountAquired)
hist(df$compRatio, breaks = 40)

ggplot(df, aes(x=compRatio, y=amountAquired)) + geom_point() + geom_smooth(method=glm, method.args=list(family = "poisson"))  + ggtitle("Plot of average amount of aquisitions vs compensation ratio") +
  xlab("Compensation ratio") + ylab("Average aquisitions per year") +
  ylim(0, 10)

#============================== moderating factor 1: Board size -================================
# Fix directors
dfmean$NumberDirectors_factor = cut(dfmean$NumberDirectors_mean, breaks = c(0, 8, 12, 30), labels = c("Small (0,8]", "Medium (8,12]", "Large 12,30]"))
hist(dfmean$NumberDirectors_mean, breaks = 40)

ggplot(dfmean, aes(x=compRatio, y=amountAquired_mean, color=NumberDirectors_factor)) +
  geom_point() + 
  geom_smooth(method=glm, method.args=list(family = "poisson") , se=FALSE, fullrange=FALSE) + ggtitle("Plot of average amount of aquisitions vs compensation ratio per board size") +
 xlab("Compensation ratio") + ylab("Average aquisitions per year") +
  ylim(0, 10)



#============================== moderating factor 2: Yearly acquisitions -========================
# Calculate mean acquisitions per year
dfaqusyearly = df %>%
  group_by(AnnualReportDate = AnnualReportDate - 1) %>%
  summarise_at(vars(amountAquired), list(amountAquired_mean = mean))

step1 = inner_join(x = capitaliqAggregated, y = ids, by = c("relcompanyid"= "companyid"))
calpitaliqAggregatedWithIndustry = inner_join(x = step1, y = execucomp, by = c("gvkey" = "GVKEY", "announcedDate" = "YEAR"))

calpitaliqAggregatedWithIndustry$SPINDEX = substr(calpitaliqAggregatedWithIndustry$SPINDEX, 1, 2)


dfaqusyearly2 = calpitaliqAggregatedWithIndustry %>%
  group_by(announcedDate = announcedDate - 1, SPINDEX) %>%
  summarise_at(vars(amountAquired), list(amountAquired_mean_per_industry = mean))


df = inner_join(x = df, y = dfaqusyearly, by = c("AnnualReportDate"= "AnnualReportDate"))
df = inner_join(x = df, y = dfaqusyearly2, by = c("AnnualReportDate"= "announcedDate", "SPINDEX" = "SPINDEX"))

df = rename(df, amountAquired_mean_per_industry = amountAquired_mean_per_industry.x)

# Descriptives and tables
df = na.omit(df)

#Check group size
df = df %>%
  group_by(GVKEY) %>%
  mutate(freq = n()) %>% 
  ungroup()
  
df = df[df$freq > 1,]

# Create a panel dataframe
df.p = pdata.frame(df, index = c("GVKEY", "AnnualReportDate"))

# Test for duplicate row names
occur = data.frame(table(row.names(df.p)))
duplicateRowNames = occur[occur$Freq > 1,]

#descriptive 1
dfOnlyInteresting = select(df.p, "amountAquired", "NumberDirectors", "AnnualReportDate", "GenderRatio", "AGE", "roa", "xrd", "aquisitionFin", "aqusitionsNot0", "CEOTenure", "compRatio", "laggedAquisition", "amountAquired_mean")
stargazer(dfOnlyInteresting, type = "html", title="Descriptive statistics", digits=1, out="descriptives.doc")

summary(df$AnnualReportDate)

df.p$compXNumDirectors = df.p$compRatio * df.p$NumberDirectors
df.p$compXAquiredMean  = df.p$compRatio * df.p$amountAquired_mean

# Table 2
#----------------------------------------------------------
# Define models
#----------------------------------------------------------
mdlA <- amountAquired ~ GenderRatio + CEOTenure + AGE + roa + SPINDEX + aquisitionFin + xrd + laggedAquisition + NumberDirectors + amountAquired_mean_per_industry
mdlB <- amountAquired ~ GenderRatio + CEOTenure + AGE + roa + SPINDEX + aquisitionFin + xrd + laggedAquisition + NumberDirectors + amountAquired_mean_per_industry + compRatio
mdlC <- amountAquired ~ GenderRatio + CEOTenure + AGE + roa + SPINDEX + aquisitionFin + xrd + laggedAquisition + NumberDirectors + amountAquired_mean_per_industry + compRatio + compRatio:NumberDirectors
mdlD <- amountAquired ~ GenderRatio + CEOTenure + AGE + roa + SPINDEX + aquisitionFin + xrd + laggedAquisition + NumberDirectors + amountAquired_mean_per_industry + compRatio + compRatio:amountAquired_mean
mdlE <- amountAquired ~ GenderRatio + CEOTenure + AGE + roa + SPINDEX + aquisitionFin + xrd + laggedAquisition + NumberDirectors + amountAquired_mean_per_industry + compRatio + compRatio:NumberDirectors + compRatio:amountAquired_mean


#----------------------------------------------------------
# Estimate the models
#----------------------------------------------------------
rsltA <- plm(mdlA, data = df.p, family = "binomial", model="within")
rsltB <- plm(mdlB, data = df.p, family = "binomial", model="within")
rsltC <- plm(mdlC, data = df.p, family = "binomial", model="within")
rsltD <- plm(mdlD, data = df.p, family = "binomial", model="within")
rsltE <- plm(mdlE, data = df.p, family = "binomial", model="within")

#----------------------------------------------------------
# Make a table (with stargazer)
#----------------------------------------------------------
stargazer(rsltA, rsltB, rsltC, rsltD, rsltE, title = "With colon",  align=TRUE, no.space=TRUE, intercept.bottom = FALSE, add.lines = list(c("Year", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Company", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))
summary(rsltE)

#Hausman Test
rsltE_RandomEffects = plm(mdlE, data = df.p, family = "binomial", model="random")
phtest(rsltE, rsltE_RandomEffects)


logLik.plm <- function(object){
  out <- -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
  
  attr(out,"df") <- nobs(object) - object$df.residual
  attr(out,"nobs") <- plm::nobs(summary(object))
  return(out)
}

logLik(rsltA)
logLik(rsltB)
logLik(rsltC)
logLik(rsltD)
logLik(rsltE)


# Correlation Table
dfCor = dfOnlyInteresting
dfCor$industry = NULL
dfCor$completedAquisition = NULL
dfCor$AnnualReportDate = as.numeric(dfCor$AnnualReportDate)
correlation = cor(dfCor, method = c("pearson"))
stargazer(correlation, type="html", out="correlation.doc")

