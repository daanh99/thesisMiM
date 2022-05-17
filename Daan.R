library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(stargazer)

ISS = read.csv("data/daan/daan.csv")
boardEx = read.csv("data/daan/boardEx2.csv")
execucomp = read.csv("data/daan/execucomp.csv") 

df <- data.frame(test=c('W'))	

df$test = 0
execucomp = execucomp[execucomp$CEOANN == "CEO", ]


# Control Variables
df$year = ISS$year
df$industry = 0
df$ceoAge = 0
df$ceoGender = 0
df$crisis = 0


# Research variables
df$ceoTenure <- NA
df$test = execucomp$YEAR - execucomp$BECAMECEO
df$ceoGender

