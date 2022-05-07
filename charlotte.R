#install.packages("dplyr")

library(dplyr)
library(fuzzyjoin)

boardex = read.csv("data/boardex companyid.csv")
capitaliq = read.csv("data/capitaliq3.csv")
execucomp = read.csv("data/execucomp may 7 3.20pm.csv")
ids = read.csv("data/identifier table.csv")


# Filter capitaliq on aquire
capitaliq = filter(capitaliq, capitaliq$percentAcquired == 100 | capitaliq$percentOwnership == 100 | grepl("aquisition | aquire", capitaliq$TransAnnHeadline, ignore.case = TRUE))

#Turn each acquisition into a total number of acquisitions per company per year
capitaliq$announcedDate = substr(capitaliq$announcedDate, 1, 4)
capitaliq$announcedDate = as.integer(capitaliq$announcedDate)
#per company, how many times did 2000 / 2001 / 2002 occur in announced date
capitaliqAggregated = capitaliq %>%
  group_by(relcompanyname) %>%
  count(announcedDate)

capitaliqAggregated = rename(capitaliqAggregated, amountAquired = n)
  

# Keep all records that include CEO or chief executive officer
execucomp = filter(execucomp, grepl("CEO | Chief executive officer", execucomp$TITLE, ignore.case = TRUE))

# Change date to year
boardex$AnnualReportDate = as.integer(substr(boardex$AnnualReportDate, 1, 4))
execucomp$announcedDate = as.integer(substr(execucomp$announcedDate, 1, 4))

###############################################

#            MERGING STEP

###############################################

# Merge execuComp and BoardEx based on the Ticker field
step1 = inner_join(x = boardex, y = execucomp, by = c("Ticker"= "TICKER", "AnnualReportDate" = "YEAR"))
#step2 = merge(x = step1, y = ids, by.x = "Ticker", by.y = "ticker")
df = left_join(x = step1, y = capitaliqAggregated, by = c("CONAME" = "relcompanyname", "AnnualReportDate" = "announcedDate"))

test = stringdist_join(step1, capitaliqAggregated, 
                by= list(c("CONAME"= "relcompanyname"), c("AnnualReportDate" = "announcedDate")),
                mode='left', #use left join
                method = "jw", #use jw distance metric
                max_dist=5, 
                distance_col='dist')

rm(boardex, capitaliq, execucomp, step1, ids, capitaliqAggregated)

df$TOTAL_ALT1[is.na(df$TOTAL_ALT1)] = 0
df$BONUS[is.na(df$BONUS)] = 0

df$compRatio = df$BONUS / df$TOTAL_ALT1

df = select(df, "CONAME", "EXEC_FULLNAME" ,"BONUS", "TOTAL_ALT1", "NumberDirectors")
  
hist(df$BONUS)

hist(df$TOTAL_ALT1)

summary(df$compRatio)
hist(df$compRatio, breaks=100)
