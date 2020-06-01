#Pakker
library(readr)
library(dynlm)
library(lubridate)
library(devtools)
library(ggplot2)
library(countrycode)
library(rvest)
library(tidyr)
library(knitr)
library(ggthemes)
library(date)
library(zoo)
library(dplyr)
library(plyr)
library(stringr)
library(lubridate)
library(scales)
library(ggrepel)
library(stargazer)
library(waffle)
library(grid)
library(RColorBrewer)
library(reshape2)
library(tidyverse)
library(readxl)
library(astrochron)
library(mFilter)
library(tseries)
library(fpp)
library(FactoMineR)
library(gridExtra)
library(vars)
library(forecast)
library(aTSA)
library(pracma)
library(Quandl)
library(timeSeries)
options(scipen=999)
devtools::install_github("mikkelkrogsholm/statsDK")
library(statsDK)

#Plots-opsætning
theme_set(theme_light())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1", direction=1)
th <- theme(title = element_text(colour = "#404040"),
            plot.background=element_rect(fill="#f3f3f3"),  
            panel.background = element_rect(fill="#f3f3f3"), 
            legend.background = element_rect(fill="#f3f3f3"),
            plot.subtitle = element_text(color="#666666"),
            plot.caption = element_text(color="#AAAAAA", size=8),
            legend.key = element_rect(fill = "#f3f3f3", colour = "#f3f3f3"),
            plot.margin = unit(c(0.5, 0.7, 0.5, 0.7), "cm"))
# axis.title.x=element_blank() tilføjes, hvis x-titel ikke skal med
# axis.text.x=element_text(angle=90,hjust=1,vjust=0.5) tilføjes, hvis x-aksens navne skal roteres
# panel.grid.major.x = element_blank() tilføjes, hvis grid-stregerne skal fjernes (.minor.x eller y)
# HUSK at angive +labs(title = "", subtitle = "", x="", y="", caption = "Kilde:")
# Gemme plots: ggsave("p1.png", plot = p1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
# To liner plot.subtitle

#Data
setwd("C:/Users/Andreas/Dropbox/Speciale/Data")

Data <- read_excel("Udtrukket data.xlsx", sheet = "Samlet")

PCA_data <- Data[1:261,]

Polation <- read_excel("Udtrukket data.xlsx", sheet ="Interpolere")
Polation <- Polation %>% select(År, Rate)

expand_data <- function(x) {
  years <- min(x$År):max(x$År)
  quarters <- 1:4
  grid <- expand.grid(quarter=quarters, År=years)
  x$quarter <- 1
  merged <- grid %>% left_join(x, by=c('År', 'quarter'))
  return(merged)}

interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$Rate
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$yhat <- interpolation$y
  return(data)}

t <- interpolate_data(expand_data(Polation))

PCA_data$Konkursrate <- t$yhat[1:261]

#Sæsonjusteret
Data_season <- as.data.frame(PCA_data$Tid)
ts_rub = ts(PCA_data$`Reale Udlån - banker`, frequency = 4, start = 1951)
ts_rur = ts(PCA_data$`Reale Udlån - realkredit`, frequency = 4, start = 1951)
ts_kr = ts(PCA_data$`Korte rente`, frequency = 4, start = 1951)
ts_lr = ts(PCA_data$`Lange rente (10år)`, frequency = 4, start = 1951)
ts_rs = ts(PCA_data$Rentespread, frequency = 4, start = 1951)
ts_mr = ts(PCA_data$`Marginalrente (bank)`, frequency = 4, start = 1951)
ts_eer = ts(PCA_data$`Reale EER-indeks`, frequency = 4, start = 1951)
ts_ak = ts(PCA_data$`Reale Aktiepris indeks`, frequency = 4, start = 1951)
ts_hus = ts(PCA_data$`Reale Huspris indeks`, frequency = 4, start = 1951)
ts_kon = ts(PCA_data$Konkursrate, frequency = 4, start = 1951)
ts_bnp = ts(PCA_data$`Reale BNP`, frequency = 4, start = 1951)

decompose_rub = decompose(ts_rub, "additive")
decompose_rur = decompose(ts_rur, "additive")
decompose_kr = decompose(ts_kr, "additive")
decompose_lr = decompose(ts_lr, "additive")
decompose_rs = decompose(ts_rs, "additive")
decompose_mr = decompose(ts_mr, "additive")
decompose_eer = decompose(ts_eer, "additive")
decompose_ak = decompose(ts_ak, "additive")
decompose_hus = decompose(ts_hus, "additive")
decompose_kon = decompose(ts_kon, "additive")
decompose_bnp = decompose(ts_bnp, "additive")

adjust_rub = ts_rub - decompose_rub$seasonal
adjust_rur = ts_rur - decompose_rur$seasonal
adjust_kr = ts_kr - decompose_kr$seasonal
adjust_lr = ts_lr - decompose_lr$seasonal
adjust_rs = ts_rs - decompose_rs$seasonal
adjust_mr = ts_mr - decompose_mr$seasonal
adjust_eer = ts_eer - decompose_eer$seasonal
adjust_ak = ts_ak - decompose_ak$seasonal
adjust_hus = ts_hus - decompose_hus$seasonal
adjust_kon = ts_kon - decompose_kon$seasonal
adjust_bnp = ts_bnp - decompose_bnp$seasonal

Data_season$RUB <- as.numeric(adjust_rub)
Data_season$RUR <- as.numeric(adjust_rur)
Data_season$KR <- as.numeric(adjust_kr)
Data_season$LR <- as.numeric(adjust_lr)
Data_season$RS <- as.numeric(adjust_rs)
Data_season$MR <- as.numeric(adjust_mr)
Data_season$EER <- as.numeric(adjust_eer)
Data_season$AK <- as.numeric(adjust_ak)
Data_season$HUS <- as.numeric(adjust_hus)
Data_season$KON <- as.numeric(adjust_kon)
Data_season$BNP <- as.numeric(adjust_bnp)

PCA_cycle_season <- as.data.frame(PCA_data$Tid)
PCA_cycle_season <- rename(PCA_cycle_season, replace = c("PCA_data$Tid"="Tid"))
PCA_cycle_season$Tid <- as.Date(PCA_cycle_season$Tid)
RUB <- hpfilter(Data_season$RUB, freq = 400000)
PCA_cycle_season$RUB <- RUB$cycle
RUR <- hpfilter(Data_season$RUR, freq = 400000)
PCA_cycle_season$RUR <- RUR$cycle
KR <- hpfilter(Data_season$KR, freq = 400000)
PCA_cycle_season$KR <- KR$cycle
LR <- hpfilter(Data_season$LR, freq = 400000)
PCA_cycle_season$LR <- LR$cycle
RS <- hpfilter(Data_season$RS, freq = 400000)
PCA_cycle_season$RS <- RS$cycle
MR <- hpfilter(Data_season$MR, freq = 400000)
PCA_cycle_season$MR <- MR$cycle
EER <- hpfilter(Data_season$EER, freq = 400000)
PCA_cycle_season$EER <- EER$cycle
AK <- hpfilter(Data_season$AK, freq = 400000)
PCA_cycle_season$AK <- AK$cycle
HUS <- hpfilter(Data_season$HUS, freq = 400000)
PCA_cycle_season$HUS <- HUS$cycle
KON <- hpfilter(Data_season$KON, freq = 400000)
PCA_cycle_season$KON <- KON$cycle
BNP <- hpfilter(Data_season$BNP, freq = 400000)
PCA_cycle_season$BNP <- BNP$cycle

p1 <- ggplot(PCA_cycle_season, aes(x=Tid, y=RUB)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-100000, 200000), breaks=round(seq(min(-100000), max(200000), by = 50000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for de samlede indenlandske bankudlån til ikke-finansielle parter", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr.", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p2 <- ggplot(PCA_cycle_season, aes(x=Tid, y=RUR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-60000, 90000), breaks=round(seq(min(-60000), max(90000), by = 20000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for de samlede indenlandske realkreditudlån til ikke-finansielle parter", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr.", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p3 <- ggplot(PCA_cycle_season, aes(x=Tid, y=KR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-3, 5), breaks=round(seq(min(-3), max(5), by = 1),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for Nationalbankens diskonteringsrente", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p4 <- ggplot(PCA_cycle_season, aes(x=Tid, y=LR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-4, 10), breaks=round(seq(min(-4), max(10), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for renten på den danske 10-årige statsobligation", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p5 <- ggplot(PCA_cycle_season, aes(x=Tid, y=RS)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 7), breaks=round(seq(min(-5), max(7), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske rentespænd", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p6 <- ggplot(PCA_cycle_season, aes(x=Tid, y=MR)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1.5, 2.25), breaks=round(seq(min(-1.5), max(2.25), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for den danske marginalrente", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p7 <- ggplot(PCA_cycle_season, aes(x=Tid, y=EER)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-8, 10), breaks=round(seq(min(-8), max(10), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske EER-indeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p8 <- ggplot(PCA_cycle_season, aes(x=Tid, y=AK)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-500, 600), breaks=round(seq(min(-500), max(600), by = 200),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske aktieindeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p9 <- ggplot(PCA_cycle_season, aes(x=Tid, y=HUS)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-28, 52), breaks=round(seq(min(-30), max(50), by = 10),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske boligprisindeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p10 <- ggplot(PCA_cycle_season, aes(x=Tid, y=KON)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-0.4, 0.6), breaks=round(seq(min(-0.4), max(0.6), by = 0.2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for den danske konkursrate", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p11 <- ggplot(PCA_cycle_season, aes(x=Tid, y=BNP)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-9000, 15600), breaks=round(seq(min(-9000), max(15600), by = 3000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske BNP", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

ggsave("RUB.pdf", plot = p1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("RUR.pdf", plot = p2, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("KR.pdf", plot = p3, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("LR.pdf", plot = p4, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("RS.pdf", plot = p5, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("MR.pdf", plot = p6, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("EER.pdf", plot = p7, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("AK.pdf", plot = p8, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("HUS.pdf", plot = p9, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("KON.pdf", plot = p10, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("BNP.pdf", plot = p11, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Robusthed for HP-filter
Data_robust1 <- read_excel("Robust_data.xlsx", sheet = "Lamdba1")
Data_robust2 <- read_excel("Robust_data.xlsx", sheet = "Lamdba2")
Data_robust3 <- read_excel("Robust_data.xlsx", sheet = "Lamdba3")

p1.1 <- tibble(L1 = Data_robust1$RUB,
               L2 = Data_robust2$RUB,
               L3 = Data_robust3$RUB,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-100000, 200000), breaks=round(seq(min(-100000), max(200000), by = 50000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for de samlede indenlandske bankudlån til ikke-finansielle parter", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr.", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p2.1 <- tibble(L1 = Data_robust1$RUR,
             L2 = Data_robust2$RUR,
             L3 = Data_robust3$RUR,
             Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-60000, 90000), breaks=round(seq(min(-60000), max(90000), by = 20000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for de samlede indenlandske realkreditudlån til ikke-finansielle parter", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr.", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p3.1 <- tibble(L1 = Data_robust1$KR,
               L2 = Data_robust2$KR,
               L3 = Data_robust3$KR,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-3, 5), breaks=round(seq(min(-3), max(5), by = 1),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for Nationalbankens diskonteringsrente", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p4.1 <- tibble(L1 = Data_robust1$LR,
               L2 = Data_robust2$LR,
               L3 = Data_robust3$LR,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-4, 10), breaks=round(seq(min(-4), max(10), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for renten på den danske 10-årige statsobligation", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p5.1 <- tibble(L1 = Data_robust1$RS,
               L2 = Data_robust2$RS,
               L3 = Data_robust3$RS,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 7), breaks=round(seq(min(-5), max(7), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske rentespænd", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p6.1 <- tibble(L1 = Data_robust1$MR,
               L2 = Data_robust2$MR,
               L3 = Data_robust3$MR,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1.5, 2.25), breaks=round(seq(min(-1.5), max(2.25), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for den danske marginalrente", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p7.1 <- tibble(L1 = Data_robust1$EER,
               L2 = Data_robust2$EER,
               L3 = Data_robust3$EER,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-8, 10), breaks=round(seq(min(-8), max(10), by = 2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske EER-indeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p8.1 <- tibble(L1 = Data_robust1$AK,
               L2 = Data_robust2$AK,
               L3 = Data_robust3$AK,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-500, 600), breaks=round(seq(min(-500), max(600), by = 200),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske aktieindeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p9.1 <- tibble(L1 = Data_robust1$HUS,
               L2 = Data_robust2$HUS,
               L3 = Data_robust3$HUS,
               Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-28, 52), breaks=round(seq(min(-30), max(50), by = 10),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske boligprisindeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p10.1 <- tibble(L1 = Data_robust1$KON,
                L2 = Data_robust2$KON,
                L3 = Data_robust3$KON,
                Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-0.4, 0.6), breaks=round(seq(min(-0.4), max(0.6), by = 0.2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for den danske konkursrate", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="%-point", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

p11.1 <- tibble(L1 = Data_robust1$BNP,
                L2 = Data_robust2$BNP,
                L3 = Data_robust3$BNP,
                Tid = as.Date(Data_robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) + geom_line(show.legend = F) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-9000, 15600), breaks=round(seq(min(-9000), max(15600), by = 3000),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cyklus for det danske BNP", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="mio. kr", caption = "Kilde: (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

RUB1 <- hpfilter(log(Data_season$RUB), freq = 1600)
RUB2 <- hpfilter(log(Data_season$RUB), freq = 400000)
robust1 <- as.data.frame(log(Data$`Reale Udlån - banker`))
robust1 <- rename(robust1, replace = c("log(Data$`Reale Udlån - banker`)"="Log"))
robust1$Tid <- Data$Tid
robust1 <- robust1[1:261,]
robust1$tre1 <- as.numeric(RUB1$trend)
robust1$tre2 <- as.numeric(RUB2$trend)

p12.1 <- tibble(T1 = robust1$Log,
                T2 = robust1$tre1,
                T3 = robust1$tre2,
                Tid = as.Date(robust1$Tid)) %>% gather(variable, value, -Tid) %>% 
  ggplot(aes(x=Tid, y=value, color=variable)) +
  geom_line(show.legend = F) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Faktisk dataserie og trend ved forskellige lambdaer", subtitle = "De samlede indenlandske bankudlån til ikke-finansielle parter.", x=NULL, y="Log", caption = "Kilde: Egen beregning", color=NULL)+
  theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

ggsave("RUB.pdf", plot = p1.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("RUR.pdf", plot = p2.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("KR.pdf", plot = p3.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("LR.pdf", plot = p4.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("RS.pdf", plot = p5.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("MR.pdf", plot = p6.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("EER.pdf", plot = p7.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("AK.pdf", plot = p8.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("HUS.pdf", plot = p9.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("KON.pdf", plot = p10.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("BNP.pdf", plot = p11.1, width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")
ggsave("Robust1.pdf", plot = p12.1, width = 30, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots/Robust")

#PCA
PCA <- FactoMineR::PCA(PCA_cycle_season[,-1],ncp = 2)
PCA_korrelation <- as.data.frame(PCA$var$cor)
PCA_info <- as.data.frame(PCA$eig)

p12 <- ggplot(PCA_info, aes(x=c(1:11),y=`percentage of variance`)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(0,100), breaks=round(seq(min(0), max(100), by = 10),1)) +
  scale_x_continuous(breaks = round(seq(min(1),max(11),by=1),1)) +
  labs(x="Principal Component", y="Andel af forklaret variation", caption = "")+
  theme(panel.grid.minor.x = element_blank()) + th

p13 <- ggplot(PCA_info, aes(x=c(1:11),y=`cumulative percentage of variance`)) + geom_line() +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(0,100), breaks=round(seq(min(0), max(100), by = 10),1)) +
  scale_x_continuous(breaks = round(seq(min(1),max(11),by=1),1)) +
  labs(x="Principal Component", y="Kummuleret andel af forklaret variation", caption = "Kilde: Egen beregning, (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th

PCA_korrelation$type <- c("RUB", "RUR", "KR", "LR", "RS", "MR", "EER", "AK", "HUS", "KON", "BNP")
PCA_korrelation$Interval <- ifelse(PCA_korrelation$Dim.1>0.5,"> 0.5","< 0.5")
PCA_korrelation$Interval1 <- ifelse(PCA_korrelation$Dim.2>0.5,"> 0.5","< 0.5")

p14 <- PCA_korrelation %>% ggplot(aes(x=reorder(type,desc(Dim.1)),y=Dim.1)) + 
  geom_col(aes(fill = Interval), show.legend = F, width = 0.6) + th +  
  labs(title = "Faktor 1", x=NULL, y="Korrelation", caption = "") + scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = function(x) format(x,big.mark = ".", decimal.mark = ",",scientific = FALSE),limits = c(-1,1), breaks=round(seq(min(-1), max(1), by = 0.2),1))

p15 <- PCA_korrelation %>% ggplot(aes(x=reorder(type,desc(Dim.2)),y=Dim.2)) + 
  geom_col(aes(fill = Interval1), show.legend = F, width = 0.6) + th +  
  labs(title = "Faktor 2", x=NULL, y="Korrelation", caption = "Kilde: Egen beregning, (Abildgren, 2019)") + scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = function(x) format(x,big.mark = ".", decimal.mark = ",",scientific = FALSE),limits = c(-1,1), breaks=round(seq(min(-1), max(1), by = 0.2),1))

ggsave("PCAvarians.pdf", plot = grid.arrange(p12,p13, ncol=2, nrow=1), width = 20, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("PCAfaktor.pdf", plot = grid.arrange(p14,p15, ncol=2, nrow=1), width = 25, height = 9, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Finansiel cyklus
Finansiel_data <- as.data.frame(Data$Tid)
Finansiel_data <- rename(Finansiel_data, replace = c("Data$Tid"="Tid"))
Finansiel_data$Tid <- as.Date(Finansiel_data$Tid)
Finansiel_data$Kredit <- Data$`Reale Udlån - banker` + Data$`Reale Udlån - realkredit`
Finansiel_data$Huspris <- Data$`Reale Huspris indeks`

ts_f_kredit <- ts(Finansiel_data$Kredit, frequency = 4, start = 1951)
ts_f_hus <- ts(Finansiel_data$Huspris, frequency = 4, start = 1951)
decompose_f_kredit <- decompose(ts_f_kredit, "additive")
decompose_f_hus <- decompose(ts_f_hus, "additive")
adjust_f_kredit <- ts_f_kredit - decompose_f_kredit$seasonal
adjust_f_hus <- ts_f_hus - decompose_f_hus$seasonal

Finansiel_data$Kredit <- as.numeric(adjust_f_kredit)
Finansiel_data$Huspris <- as.numeric(adjust_f_hus)

HP_f_kredit <- hpfilter(log(Finansiel_data$Kredit), freq = 400000)
Finansiel_data$Kredit_cyklus <- HP_f_kredit$cycle
Finansiel_data$Kredit_trend <- as.numeric(HP_f_kredit$trend)
HP_f_hus <- hpfilter(log(Finansiel_data$Huspris), freq = 400000)
Finansiel_data$Huspris_cyklus <- HP_f_hus$cycle
Finansiel_data$Huspris_trend <- as.numeric(HP_f_hus$trend)

p16 <- ggplot(Finansiel_data, aes(x=Tid, y=Kredit_trend)) + geom_line() +
  geom_line(aes(y=Kredit_trend+Kredit_cyklus),color="#377eb8") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(11, 14.5), breaks=round(seq(min(11), max(14.5), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend samt cyklus for kredit", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="Log", caption = "")+
  theme(panel.grid.minor.x = element_blank()) + th 
              
p17 <- ggplot(Finansiel_data, aes(x=Tid, y=Kredit_cyklus*100)) + geom_line(color="#377eb8") +
  geom_line(aes(y=0)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-20, 25), breaks=round(seq(min(-20), max(25), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cykliske udsving for kredit", subtitle = "", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Egen beregning, (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th 

p18 <- ggplot(Finansiel_data, aes(x=Tid, y=Huspris_trend)) + geom_line() +
  geom_line(aes(y=Huspris_trend+Huspris_cyklus),color="#377eb8") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(3.7, 5.3), breaks=round(seq(min(3.7), max(5.3), by = 0.2),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend samt cyklus for husprisindeks", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="Log", caption = "")+
  theme(panel.grid.minor.x = element_blank()) + th 

p19 <- ggplot(Finansiel_data, aes(x=Tid, y=Huspris_cyklus*100)) + geom_line(color="#377eb8") +
  geom_line(aes(y=0)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-30, 35), breaks=round(seq(min(-30), max(35), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cykliske udsving for husprisindeks", subtitle = "", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Egen beregning, (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th 

ggsave("Kredit.pdf", plot = grid.arrange(p16,p17, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("Huspris.pdf", plot = grid.arrange(p18,p19, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Realøkonomisk cyklus
Real_data <- as.data.frame(Data$Tid)
Real_data <- rename(Real_data, replace = c("Data$Tid"="Tid"))
Real_data$Tid <- as.Date(Real_data$Tid)
Real_data$BNP <- Data$`Reale BNP`

ts_r_BNP <- ts(Real_data$BNP, frequency = 4, start = 1951)
decompose_r_BNP <- decompose(ts_r_BNP, "additive")
adjust_r_BNP <- ts_r_BNP - decompose_r_BNP$seasonal

Real_data$BNP <- as.numeric(adjust_r_BNP)

HP_r_BNP <- hpfilter(log(Real_data$BNP), freq = 100000)
Real_data$BNP_cyklus <- HP_r_BNP$cycle
Real_data$BNP_trend <- as.numeric(HP_r_BNP$trend)

p20 <- ggplot(Real_data, aes(x=Tid, y=BNP_trend)) + geom_line() +
  geom_line(aes(y=BNP_trend+BNP_cyklus),color="#377eb8") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(10.25, 12.25), breaks=round(seq(min(10), max(12.5), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Trend samt cyklus for BNP", subtitle = "Sæsonjusteret og deflateret med CPI.", x=NULL, y="Log", caption = "")+
  theme(panel.grid.minor.x = element_blank()) + th 

p21 <- ggplot(Real_data, aes(x=Tid, y=BNP_cyklus*100)) + geom_line(color="#377eb8") +
  geom_line(aes(y=0)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-14.5, 14.5), breaks=round(seq(min(-15), max(15), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Cykliske udsving for BNP", subtitle = "", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Egen beregning, (Abildgren, 2019)")+
  theme(panel.grid.minor.x = element_blank()) + th 

ggsave("RealBNP.pdf", plot = grid.arrange(p20,p21, ncol=2, nrow=1), width = 35, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Økonomisk cyklus
Samlet_cyklus <- as.data.frame(Data$Tid)
Samlet_cyklus <- rename(Samlet_cyklus, replace = c("Data$Tid"="Tid"))
Samlet_cyklus$Tid <- as.Date(Real_data$Tid)
Samlet_cyklus$BNP <- Real_data$BNP_cyklus*100
Samlet_cyklus$Kredit <- Finansiel_data$Kredit_cyklus*100
Samlet_cyklus$Huspris <- Finansiel_data$Huspris_cyklus*100

colors <- c("BNP" = "#377eb8", "Kredit" = "#e41a1c", "Huspriser" = "#4daf4a")
colors2 <- c("BNP" = "#377eb8", "Kredit (Højre akse)" = "#e41a1c", "Huspriser  (Højre akse)" = "#4daf4a")

p22 <- ggplot(Samlet_cyklus[81:268,], aes(x=Tid, y=BNP, color="BNP")) + geom_line() +
  geom_line(aes(y=0), color="black") +
  geom_line(aes(y=Kredit* 15 / 40,color = "Kredit (Højre akse)")) +
  geom_line(aes(y=Huspris* 15 / 40,color = "Huspriser  (Højre akse)")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-15, 15), breaks=round(seq(min(-15), max(15), by = 5),1),
                     sec.axis = sec_axis(~ .*40/15 , name = "Afvigelse fra trend i procentpoint", breaks=round(seq(min(-40), max(40), by = 10),1))) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = "Økonomiske cyklusser for Danmark", subtitle="Sæsonjusteret og deflateret med CPI.", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Egen beregning, (Abildgren, 2019)", color=NULL)+
  scale_color_manual(values = colors2) + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

ggsave("Cyklus.pdf", plot = p22, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Autokorrelationer
Auto_data <- as.data.frame(c(-5:5))
Auto_data <- rename(Auto_data, replace = c("c(-5:5)"="Lag"))

AutoBNP <- ccf(Samlet_cyklus$BNP,Samlet_cyklus$BNP, 5, plot = F)
Auto_data$auto_BNP <- as.numeric(AutoBNP$acf)

AutoKredit <- ccf(Samlet_cyklus$Kredit,Samlet_cyklus$Kredit, 5, plot = F)
Auto_data$auto_Kredit <- as.numeric(AutoKredit$acf)

AutoHuspris <- ccf(Samlet_cyklus$Huspris,Samlet_cyklus$Huspris, 5, plot = F)
Auto_data$auto_Huspris <- as.numeric(AutoHuspris$acf)

Kryds_BNP_Kredit <- ccf(Samlet_cyklus$BNP,Samlet_cyklus$Kredit, 5, plot = F)
Kryds_Hus_Kredit <- ccf(Samlet_cyklus$Huspris,Samlet_cyklus$Kredit, 5, plot = F)
Auto_data$K_BNP_Kredit <- as.numeric(Kryds_BNP_Kredit$acf)
Auto_data$K_Hus_Kredit <- as.numeric(Kryds_Hus_Kredit$acf)

Kryds_Kredit_BNP <- ccf(Samlet_cyklus$Kredit,Samlet_cyklus$BNP, 5, plot = F)
Kryds_Hus_BNP <- ccf(Samlet_cyklus$Huspris,Samlet_cyklus$BNP, 5, plot = F)
Auto_data$K_Kredit_BNP <- as.numeric(Kryds_Kredit_BNP$acf)
Auto_data$K_Hus_BNP <- as.numeric(Kryds_Hus_BNP$acf)

Kryds_BNP_Hus <- ccf(Samlet_cyklus$BNP,Samlet_cyklus$Huspris, 5, plot = F)
Kryds_Kredit_Hus <- ccf(Samlet_cyklus$Kredit,Samlet_cyklus$Huspris, 5, plot = F)
Auto_data$K_BNP_Hus <- as.numeric(Kryds_BNP_Hus$acf)
Auto_data$K_Kredit_Hus <- as.numeric(Kryds_Kredit_Hus$acf)

p23 <- ggplot(Auto_data)+
  geom_line(aes(x=Lag, y=0))+
  stat_smooth(aes(y=auto_Kredit, x=Lag, color = "Kredit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y=auto_Huspris, x=Lag, color = "Huspriser"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  geom_segment(aes(x = 0, y = 1, xend = -5, yend = 0.12, color = "BNP")) +
  geom_segment(aes(x = 0, y = 1, xend = 5, yend = 0.12, color = "BNP")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Autokorrelationer", x="Lag", y="Korrelation", caption = "", color=NULL)+
  scale_color_manual(values = colors) + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

p24 <- ggplot(Auto_data)+
  geom_line(aes(x=Lag, y=0))+
  stat_smooth(aes(y=K_BNP_Kredit, x=Lag, color = "BNP"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y=K_Hus_Kredit, x=Lag, color = "Huspriser"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Krydskorrelationer: Kredit", x="Lag", y="Korrelation", caption = "", color=NULL)+
  scale_color_manual(values = colors) + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

p25 <- ggplot(Auto_data)+
  geom_line(aes(x=Lag, y=0))+
  stat_smooth(aes(y=K_Kredit_BNP, x=Lag, color = "Kredit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y=K_Hus_BNP, x=Lag, color = "Huspriser"), formula = y ~ s(x, k = 5), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Krydskorrelationer: BNP", x="Lag", y="Korrelation", caption = "", color=NULL) +
  scale_color_manual(values = colors) + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

p26 <- ggplot(Auto_data)+
  geom_line(aes(x=Lag, y=0))+
  stat_smooth(aes(y=K_Kredit_Hus, x=Lag, color = "Kredit"), formula = y ~ s(x, k = 11), method = "gam", se = FALSE) +
  stat_smooth(aes(y=K_BNP_Hus, x=Lag, color = "BNP"), formula = y ~ s(x, k = 5), method = "gam", se = FALSE) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-1, 1), breaks=round(seq(min(-1), max(1), by = 0.5),1)) +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-5, 5), breaks=round(seq(min(-5), max(5), by = 1),1)) +
  labs(title = "Krydskorrelationer: Huspriser", x="Lag", y="Korrelation", caption = "Kilde: Egen beregning", color=NULL) +
  scale_color_manual(values = colors) + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank()) + th

ggsave("Auto.pdf", plot = grid.arrange(p23,p24,p25,p26, ncol=2, nrow=2), width = 30, height = 20, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#VAR-model
Var <- data.frame(Date = Samlet_cyklus$Tid,
                  BNP = Samlet_cyklus$BNP,
                  HUS = Samlet_cyklus$Huspris,
                  Kredit = Samlet_cyklus$Kredit)
VARselect(Var[,c(2:4)], lag.max = 13, type = c("const", "trend", "both", "none"))$selection
model1 <- VAR(Var[,c(2:4)], p =5, type = "const", ic = c("AIC", "HQ", "SC", "FPE"))

stargazer::stargazer(model1$varresult$BNP,
                     model1$varresult$HUS,
                     model1$varresult$Kredit,
                     title = "VAR(5) summary statistics",
                     no.space = T, align = T)

#Impuls-respons
or1 <- irf(VAR(Var[c(1,2,3,4)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)

or1_bnp <- as.data.frame(or1$irf$BNP)
or1_bnp$N <- c(0:48)
or1_hus <- as.data.frame(or1$irf$HUS)
or1_hus$N <- c(0:48)
or1_kredit <- as.data.frame(or1$irf$Kredit)
or1_kredit$N <- c(0:48)
upp_bnp <- as.data.frame(or1$Upper$BNP)
upp_bnp$N <- c(0:48)
upp_hus <- as.data.frame(or1$Upper$HUS)
upp_hus$N <- c(0:48)
upp_kredit <- as.data.frame(or1$Upper$Kredit)
upp_kredit$N <- c(0:48)
low_bnp <- as.data.frame(or1$Lower$BNP)
low_bnp$N <- c(0:48)
low_hus <- as.data.frame(or1$Lower$HUS)
low_hus$N <- c(0:48)
low_kredit <- as.data.frame(or1$Lower$Kredit)
low_kredit$N <- c(0:48)

p27 <- ggplot(or1_bnp, aes(x=N,y=BNP)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_bnp$BNP), linetype="dotted")+
  geom_line(aes(y=low_bnp$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(-0.5, 2.5), breaks=round(seq(min(-0.5), max(2.5), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til BNP",y = "Respons på BNP", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p28 <- ggplot(or1_bnp, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_bnp$HUS), linetype="dotted")+
  geom_line(aes(y=low_bnp$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(-0.3, 1.1), breaks=round(seq(min(-0.3), max(1.1), by = 0.3),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Huspriser", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p29 <- ggplot(or1_bnp, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_bnp$Kredit), linetype="dotted")+
  geom_line(aes(y=low_bnp$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(-0.8, 0.42), breaks=round(seq(min(-0.8), max(0.42), by = 0.4),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Kredit", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p30 <- ggplot(or1_hus, aes(x=N,y=BNP)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus$BNP), linetype="dotted")+
  geom_line(aes(y=low_hus$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(-0.25, 1.25), breaks=round(seq(min(-0.25), max(1.25), by = 0.25),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Huspriser",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p31 <- ggplot(or1_hus, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus$HUS), linetype="dotted")+
  geom_line(aes(y=low_hus$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(-1.1, 4), breaks=round(seq(min(-1), max(4), by = 1),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p32 <- ggplot(or1_hus, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus$Kredit), linetype="dotted")+
  geom_line(aes(y=low_hus$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(-0.61, 2), breaks=round(seq(min(-1), max(2), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p33 <- ggplot(or1_kredit, aes(x=N,y=BNP)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit$BNP), linetype="dotted")+
  geom_line(aes(y=low_kredit$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(-0.25, 0.68), breaks=round(seq(min(-0.25), max(0.75), by = 0.25),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Kredit",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p34 <- ggplot(or1_kredit, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit$HUS), linetype="dotted")+
  geom_line(aes(y=low_kredit$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(-1.05, 1.06), breaks=round(seq(min(-1), max(1), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p35 <- ggplot(or1_kredit, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit$Kredit), linetype="dotted")+
  geom_line(aes(y=low_kredit$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(-0.65, 2.09), breaks=round(seq(min(-1), max(2), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)", caption = "Kilde: Egen beregning") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

ggsave("IRF.pdf", plot = grid.arrange(p27,p30,p33,p28,p31,p34,p29,p32,p35, nrow =3, ncol=3), width = 30, height = 30, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#IRF-cum
or1_cum <- irf(VAR(Var[c(1,2,3,4)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66, cumulative = T)

or1_bnp_c <- as.data.frame(or1_cum$irf$BNP)
or1_bnp_c$N <- c(0:48)
or1_hus_c <- as.data.frame(or1_cum$irf$HUS)
or1_hus_c$N <- c(0:48)
or1_kredit_c <- as.data.frame(or1_cum$irf$Kredit)
or1_kredit_c$N <- c(0:48)
upp_bnp_c <- as.data.frame(or1_cum$Upper$BNP)
upp_bnp_c$N <- c(0:48)
upp_hus_c <- as.data.frame(or1_cum$Upper$HUS)
upp_hus_c$N <- c(0:48)
upp_kredit_c <- as.data.frame(or1_cum$Upper$Kredit)
upp_kredit_c$N <- c(0:48)
low_bnp_c <- as.data.frame(or1_cum$Lower$BNP)
low_bnp_c$N <- c(0:48)
low_hus_c <- as.data.frame(or1_cum$Lower$HUS)
low_hus_c$N <- c(0:48)
low_kredit_c <- as.data.frame(or1_cum$Lower$Kredit)
low_kredit_c$N <- c(0:48)

p27_c <- ggplot(or1_bnp_c, aes(x=N,y=BNP)) +
  geom_line()+
  geom_line(aes(y=upp_bnp_c$BNP), linetype="dotted")+
  geom_line(aes(y=low_bnp_c$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(2, 6.5), breaks=round(seq(min(2), max(6.5), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til BNP",y = "Respons på BNP", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p28_c <- ggplot(or1_bnp_c, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_bnp_c$HUS), linetype="dotted")+
  geom_line(aes(y=low_bnp_c$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(-1, 14.2), breaks=round(seq(min(-1), max(14), by = 1),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Huspriser", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p29_c <- ggplot(or1_bnp_c, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_bnp_c$Kredit), linetype="dotted")+
  geom_line(aes(y=low_bnp_c$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(-12, 4.5), breaks=round(seq(min(-12), max(4), by = 2),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Kredit", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p30_c <- ggplot(or1_hus_c, aes(x=N,y=BNP)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus_c$BNP), linetype="dotted")+
  geom_line(aes(y=low_hus_c$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(-0.1, 8.5), breaks=round(seq(min(-0), max(8), by =1),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Huspriser",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p31_c <- ggplot(or1_hus_c, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus_c$HUS), linetype="dotted")+
  geom_line(aes(y=low_hus_c$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(0, 50), breaks=round(seq(min(0), max(50), by = 5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p32_c <- ggplot(or1_hus_c, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_hus_c$Kredit), linetype="dotted")+
  geom_line(aes(y=low_hus_c$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(0, 42.2), breaks=round(seq(min(0), max(42), by = 5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p33_c <- ggplot(or1_kredit_c, aes(x=N,y=BNP)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit_c$BNP), linetype="dotted")+
  geom_line(aes(y=low_kredit_c$BNP), linetype="dotted") +
  scale_y_continuous(limits = c(0, 6.2), breaks=round(seq(min(0), max(6), by = 0.5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Kredit",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p34_c <- ggplot(or1_kredit_c, aes(x=N,y=HUS)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit_c$HUS), linetype="dotted")+
  geom_line(aes(y=low_kredit_c$HUS), linetype="dotted") +
  scale_y_continuous(limits = c(-13.2, 13), breaks=round(seq(min(-15), max(15), by = 3),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p35_c <- ggplot(or1_kredit_c, aes(x=N,y=Kredit)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line()+
  geom_line(aes(y=upp_kredit_c$Kredit), linetype="dotted")+
  geom_line(aes(y=low_kredit_c$Kredit), linetype="dotted") +
  scale_y_continuous(limits = c(0, 36.1), breaks=round(seq(min(0), max(36), by = 5),1))+
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)", caption = "Kilde: Egen beregning") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

ggsave("IRFcum.pdf", plot = grid.arrange(p27_c,p30_c,p33_c,p28_c,p31_c,p34_c,p29_c,p32_c,p35_c, nrow =3, ncol=3), width = 30, height = 30, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Kontrol for ordningen
or2 <- irf(VAR(Var[c(1,2,4,3)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)
or3 <- irf(VAR(Var[c(1,3,2,4)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)
or4 <- irf(VAR(Var[c(1,3,4,2)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)
or5 <- irf(VAR(Var[c(1,4,2,3)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)
or6 <- irf(VAR(Var[c(1,4,3,2)][,c(2:4)], p=5), n.ahead = 48, ci = 0.66)

Data_or1_bnp    <- as.data.frame(or1$irf$BNP)
Data_or1_hus    <- as.data.frame(or1$irf$HUS)
Data_or1_kredit <- as.data.frame(or1$irf$Kredit)
Data_or2_bnp    <- as.data.frame(or2$irf$BNP)
Data_or2_hus    <- as.data.frame(or2$irf$HUS)
Data_or2_kredit <- as.data.frame(or2$irf$Kredit)
Data_or3_bnp    <- as.data.frame(or3$irf$BNP)
Data_or3_hus    <- as.data.frame(or3$irf$HUS)
Data_or3_kredit <- as.data.frame(or3$irf$Kredit)
Data_or4_bnp    <- as.data.frame(or4$irf$BNP)
Data_or4_hus    <- as.data.frame(or4$irf$HUS)
Data_or4_kredit <- as.data.frame(or4$irf$Kredit)
Data_or5_bnp    <- as.data.frame(or5$irf$BNP)
Data_or5_hus    <- as.data.frame(or5$irf$HUS)
Data_or5_kredit <- as.data.frame(or5$irf$Kredit)
Data_or6_bnp    <- as.data.frame(or6$irf$BNP)
Data_or6_hus    <- as.data.frame(or6$irf$HUS)
Data_or6_kredit <- as.data.frame(or6$irf$Kredit)

Upper_or1_bnp    <- as.data.frame(or1$Upper$BNP)
Upper_or1_hus    <- as.data.frame(or1$Upper$HUS)
Upper_or1_kredit <- as.data.frame(or1$Upper$Kredit)
Upper_or2_bnp    <- as.data.frame(or2$Upper$BNP)
Upper_or2_hus    <- as.data.frame(or2$Upper$HUS)
Upper_or2_kredit <- as.data.frame(or2$Upper$Kredit)
Upper_or3_bnp    <- as.data.frame(or3$Upper$BNP)
Upper_or3_hus    <- as.data.frame(or3$Upper$HUS)
Upper_or3_kredit <- as.data.frame(or3$Upper$Kredit)
Upper_or4_bnp    <- as.data.frame(or4$Upper$BNP)
Upper_or4_hus    <- as.data.frame(or4$Upper$HUS)
Upper_or4_kredit <- as.data.frame(or4$Upper$Kredit)
Upper_or5_bnp    <- as.data.frame(or5$Upper$BNP)
Upper_or5_hus    <- as.data.frame(or5$Upper$HUS)
Upper_or5_kredit <- as.data.frame(or5$Upper$Kredit)
Upper_or6_bnp    <- as.data.frame(or6$Upper$BNP)
Upper_or6_hus    <- as.data.frame(or6$Upper$HUS)
Upper_or6_kredit <- as.data.frame(or6$Upper$Kredit)

Lower_or1_bnp    <- as.data.frame(or1$Lower$BNP)
Lower_or1_hus    <- as.data.frame(or1$Lower$HUS)
Lower_or1_kredit <- as.data.frame(or1$Lower$Kredit)
Lower_or2_bnp    <- as.data.frame(or2$Lower$BNP)
Lower_or2_hus    <- as.data.frame(or2$Lower$HUS)
Lower_or2_kredit <- as.data.frame(or2$Lower$Kredit)
Lower_or3_bnp    <- as.data.frame(or3$Lower$BNP)
Lower_or3_hus    <- as.data.frame(or3$Lower$HUS)
Lower_or3_kredit <- as.data.frame(or3$Lower$Kredit)
Lower_or4_bnp    <- as.data.frame(or4$Lower$BNP)
Lower_or4_hus    <- as.data.frame(or4$Lower$HUS)
Lower_or4_kredit <- as.data.frame(or4$Lower$Kredit)
Lower_or5_bnp    <- as.data.frame(or5$Lower$BNP)
Lower_or5_hus    <- as.data.frame(or5$Lower$HUS)
Lower_or5_kredit <- as.data.frame(or5$Lower$Kredit)
Lower_or6_bnp    <- as.data.frame(or6$Lower$BNP)
Lower_or6_hus    <- as.data.frame(or6$Lower$HUS)
Lower_or6_kredit <- as.data.frame(or6$Lower$Kredit)

irf_bnp_bnp <- tibble(R1 = Data_or1_bnp$BNP,
                      R2 = Data_or2_bnp$BNP,
                      R3 = Data_or3_bnp$BNP,
                      R4 = Data_or4_bnp$BNP,
                      R5 = Data_or5_bnp$BNP,
                      R6 = Data_or6_bnp$BNP,
                      N = c(0:48),
                      inte = "base") %>% gather(variable, Change, -N, -inte)
irf_bnp_hus <- tibble(R1 = Data_or1_bnp$HUS,
                      R2 = Data_or2_bnp$HUS,
                      R3 = Data_or3_bnp$HUS,
                      R4 = Data_or4_bnp$HUS,
                      R5 = Data_or5_bnp$HUS,
                      R6 = Data_or6_bnp$HUS,
                      N = c(0:48),
                      inte = "base") %>% gather(variable, Change, -N, -inte)
irf_bnp_kredit <- tibble(R1 = Data_or1_bnp$Kredit,
                         R2 = Data_or2_bnp$Kredit,
                         R3 = Data_or3_bnp$Kredit,
                         R4 = Data_or4_bnp$Kredit,
                         R5 = Data_or5_bnp$Kredit,
                         R6 = Data_or6_bnp$Kredit,
                         N = c(0:48),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_hus_bnp <- tibble(R1 = Data_or1_hus$BNP,
                      R2 = Data_or2_hus$BNP,
                      R3 = Data_or3_hus$BNP,
                      R4 = Data_or4_hus$BNP,
                      R5 = Data_or5_hus$BNP,
                      R6 = Data_or6_hus$BNP,
                      N = c(0:48),
                      inte = "base") %>% gather(variable, Change, -N, -inte)
irf_hus_hus <- tibble(R1 = Data_or1_hus$HUS,
                      R2 = Data_or2_hus$HUS,
                      R3 = Data_or3_hus$HUS,
                      R4 = Data_or4_hus$HUS,
                      R5 = Data_or5_hus$HUS,
                      R6 = Data_or6_hus$HUS,
                      N = c(0:48),
                      inte = "base") %>% gather(variable, Change, -N, -inte)
irf_hus_kredit <- tibble(R1 = Data_or1_hus$Kredit,
                         R2 = Data_or2_hus$Kredit,
                         R3 = Data_or3_hus$Kredit,
                         R4 = Data_or4_hus$Kredit,
                         R5 = Data_or5_hus$Kredit,
                         R6 = Data_or6_hus$Kredit,
                         N = c(0:48),
                         inte = "base") %>% gather(variable, Change, -N, -inte)

irf_kredit_bnp <- tibble(R1 = Data_or1_kredit$BNP,
                         R2 = Data_or2_kredit$BNP,
                         R3 = Data_or3_kredit$BNP,
                         R4 = Data_or4_kredit$BNP,
                         R5 = Data_or5_kredit$BNP,
                         R6 = Data_or6_kredit$BNP,
                         N = c(0:48),
                         inte = "base") %>% gather(variable, Change, -N, -inte)
irf_kredit_hus <- tibble(R1 = Data_or1_kredit$HUS,
                         R2 = Data_or2_kredit$HUS,
                         R3 = Data_or3_kredit$HUS,
                         R4 = Data_or4_kredit$HUS,
                         R5 = Data_or5_kredit$HUS,
                         R6 = Data_or6_kredit$HUS,
                         N = c(0:48),
                         inte = "base") %>% gather(variable, Change, -N, -inte)
irf_kredit_kredit <- tibble(R1 = Data_or1_kredit$Kredit,
                            R2 = Data_or2_kredit$Kredit,
                            R3 = Data_or3_kredit$Kredit,
                            R4 = Data_or4_kredit$Kredit,
                            R5 = Data_or5_kredit$Kredit,
                            R6 = Data_or6_kredit$Kredit,
                            N = c(0:48),
                            inte = "base") %>% gather(variable, Change, -N, -inte)

upper_bnp_bnp <- tibble(R1 = Upper_or1_bnp$BNP,
                        R2 = Upper_or2_bnp$BNP,
                        R3 = Upper_or3_bnp$BNP,
                        R4 = Upper_or4_bnp$BNP,
                        R5 = Upper_or5_bnp$BNP,
                        R6 = Upper_or6_bnp$BNP,
                        N = c(0:48),
                        inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_bnp_hus <- tibble(R1 = Upper_or1_bnp$HUS,
                        R2 = Upper_or2_bnp$HUS,
                        R3 = Upper_or3_bnp$HUS,
                        R4 = Upper_or4_bnp$HUS,
                        R5 = Upper_or5_bnp$HUS,
                        R6 = Upper_or6_bnp$HUS,
                        N = c(0:48),
                        inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_bnp_kredit <- tibble(R1 = Upper_or1_bnp$Kredit,
                           R2 = Upper_or2_bnp$Kredit,
                           R3 = Upper_or3_bnp$Kredit,
                           R4 = Upper_or4_bnp$Kredit,
                           R5 = Upper_or5_bnp$Kredit,
                           R6 = Upper_or6_bnp$Kredit,
                           N = c(0:48),
                           inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_hus_bnp <- tibble(R1 = Upper_or1_hus$BNP,
                        R2 = Upper_or2_hus$BNP,
                        R3 = Upper_or3_hus$BNP,
                        R4 = Upper_or4_hus$BNP,
                        R5 = Upper_or5_hus$BNP,
                        R6 = Upper_or6_hus$BNP,
                        N = c(0:48),
                        inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_hus_hus <- tibble(R1 = Upper_or1_hus$HUS,
                        R2 = Upper_or2_hus$HUS,
                        R3 = Upper_or3_hus$HUS,
                        R4 = Upper_or4_hus$HUS,
                        R5 = Upper_or5_hus$HUS,
                        R6 = Upper_or6_hus$HUS,
                        N = c(0:48),
                        inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_hus_kredit <- tibble(R1 = Upper_or1_hus$Kredit,
                           R2 = Upper_or2_hus$Kredit,
                           R3 = Upper_or3_hus$Kredit,
                           R4 = Upper_or4_hus$Kredit,
                           R5 = Upper_or5_hus$Kredit,
                           R6 = Upper_or6_hus$Kredit,
                           N = c(0:48),
                           inte = "upper") %>% gather(variable, Change, -N, -inte)

upper_kredit_bnp <- tibble(R1 = Upper_or1_kredit$BNP,
                           R2 = Upper_or2_kredit$BNP,
                           R3 = Upper_or3_kredit$BNP,
                           R4 = Upper_or4_kredit$BNP,
                           R5 = Upper_or5_kredit$BNP,
                           R6 = Upper_or6_kredit$BNP,
                           N = c(0:48),
                           inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_kredit_hus <- tibble(R1 = Upper_or1_kredit$HUS,
                           R2 = Upper_or2_kredit$HUS,
                           R3 = Upper_or3_kredit$HUS,
                           R4 = Upper_or4_kredit$HUS,
                           R5 = Upper_or5_kredit$HUS,
                           R6 = Upper_or6_kredit$HUS,
                           N = c(0:48),
                           inte = "upper") %>% gather(variable, Change, -N, -inte)
upper_kredit_kredit <- tibble(R1 = Upper_or1_kredit$Kredit,
                              R2 = Upper_or2_kredit$Kredit,
                              R3 = Upper_or3_kredit$Kredit,
                              R4 = Upper_or4_kredit$Kredit,
                              R5 = Upper_or5_kredit$Kredit,
                              R6 = Upper_or6_kredit$Kredit,
                              N = c(0:48),
                              inte = "upper") %>% gather(variable, Change, -N, -inte)

lower_bnp_bnp <- tibble(R1 = Lower_or1_bnp$BNP,
                        R2 = Lower_or2_bnp$BNP,
                        R3 = Lower_or3_bnp$BNP,
                        R4 = Lower_or4_bnp$BNP,
                        R5 = Lower_or5_bnp$BNP,
                        R6 = Lower_or6_bnp$BNP,
                        N = c(0:48),
                        inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_bnp_hus <- tibble(R1 = Lower_or1_bnp$HUS,
                        R2 = Lower_or2_bnp$HUS,
                        R3 = Lower_or3_bnp$HUS,
                        R4 = Lower_or4_bnp$HUS,
                        R5 = Lower_or5_bnp$HUS,
                        R6 = Lower_or6_bnp$HUS,
                        N = c(0:48),
                        inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_bnp_kredit <- tibble(R1 = Lower_or1_bnp$Kredit,
                           R2 = Lower_or2_bnp$Kredit,
                           R3 = Lower_or3_bnp$Kredit,
                           R4 = Lower_or4_bnp$Kredit,
                           R5 = Lower_or5_bnp$Kredit,
                           R6 = Lower_or6_bnp$Kredit,
                           N = c(0:48),
                           inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_hus_bnp <- tibble(R1 = Lower_or1_hus$BNP,
                        R2 = Lower_or2_hus$BNP,
                        R3 = Lower_or3_hus$BNP,
                        R4 = Lower_or4_hus$BNP,
                        R5 = Lower_or5_hus$BNP,
                        R6 = Lower_or6_hus$BNP,
                        N = c(0:48),
                        inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_hus_hus <- tibble(R1 = Lower_or1_hus$HUS,
                        R2 = Lower_or2_hus$HUS,
                        R3 = Lower_or3_hus$HUS,
                        R4 = Lower_or4_hus$HUS,
                        R5 = Lower_or5_hus$HUS,
                        R6 = Lower_or6_hus$HUS,
                        N = c(0:48),
                        inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_hus_kredit <- tibble(R1 = Lower_or1_hus$Kredit,
                           R2 = Lower_or2_hus$Kredit,
                           R3 = Lower_or3_hus$Kredit,
                           R4 = Lower_or4_hus$Kredit,
                           R5 = Lower_or5_hus$Kredit,
                           R6 = Lower_or6_hus$Kredit,
                           N = c(0:48),
                           inte = "lower") %>% gather(variable, Change, -N, -inte)

lower_kredit_bnp <- tibble(R1 = Lower_or1_kredit$BNP,
                           R2 = Lower_or2_kredit$BNP,
                           R3 = Lower_or3_kredit$BNP,
                           R4 = Lower_or4_kredit$BNP,
                           R5 = Lower_or5_kredit$BNP,
                           R6 = Lower_or6_kredit$BNP,
                           N = c(0:48),
                           inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_kredit_hus <- tibble(R1 = Lower_or1_kredit$HUS,
                           R2 = Lower_or2_kredit$HUS,
                           R3 = Lower_or3_kredit$HUS,
                           R4 = Lower_or4_kredit$HUS,
                           R5 = Lower_or5_kredit$HUS,
                           R6 = Lower_or6_kredit$HUS,
                           N = c(0:48),
                           inte = "lower") %>% gather(variable, Change, -N, -inte)
lower_kredit_kredit <- tibble(R1 = Lower_or1_kredit$Kredit,
                              R2 = Lower_or2_kredit$Kredit,
                              R3 = Lower_or3_kredit$Kredit,
                              R4 = Lower_or4_kredit$Kredit,
                              R5 = Lower_or5_kredit$Kredit,
                              R6 = Lower_or6_kredit$Kredit,
                              N = c(0:48),
                              inte = "lower") %>% gather(variable, Change, -N, -inte)

p27.1 <- rbind(irf_bnp_bnp,upper_bnp_bnp,lower_bnp_bnp) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til BNP",y = "Respons på BNP", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p28.1 <- rbind(irf_bnp_hus,upper_bnp_hus,lower_bnp_hus) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Huspriser", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p29.1 <- rbind(irf_bnp_kredit,upper_bnp_kredit,lower_bnp_kredit) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "Respons på Kredit", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p30.1 <- rbind(irf_hus_bnp,upper_hus_bnp,lower_hus_bnp) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Huspriser",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p31.1 <- rbind(irf_hus_hus,upper_hus_hus,lower_hus_hus) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p32.1 <- rbind(irf_hus_kredit,upper_hus_kredit,lower_hus_kredit) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p33.1 <- rbind(irf_kredit_bnp,upper_kredit_bnp,lower_kredit_bnp) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "Stød til Kredit",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p34.1 <- rbind(irf_kredit_hus,upper_kredit_hus,lower_kredit_hus) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)",caption = "") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

p35.1 <- rbind(irf_kredit_kredit,upper_kredit_kredit,lower_kredit_kredit) %>% 
  group_by(variable, inte) %>% 
  gather(type, value, -N, -variable, -inte) %>% 
  ggplot(aes(N,value, color = variable, linetype =inte)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_line(size=0.3) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted")) +
  scale_color_manual(values = c("black","black","black", "black", "black", "black")) +
  scale_x_continuous(breaks = seq(0,48, by = 4), limits = c(0,48))+
  labs(title = "",y = "", x ="Lags (kvartaler)", caption = "Kilde: Egen beregning") +
  th + theme(plot.title = element_text(hjust = 0.5, family = "sans", size = 12) ,legend.position = "none", axis.title.y = element_text(family = "sans", size = 12))

ggsave("IRFord.pdf", plot = grid.arrange(p27.1,p30.1,p33.1,p28.1,p31.1,p34.1,p29.1,p32.1,p35.1, nrow =3, ncol=3), width = 30, height = 30, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Varians dekomp
fevd <- fevd(model1, n.ahead = 16)
fevdBNP <- as.data.frame(fevd$BNP)
fevdBNP$Horisont <- c(1:16)
fevdBNP <- fevdBNP %>% gather(Variable, Value, - Horisont)

fevdHUS <- as.data.frame(fevd$HUS)
fevdHUS$Horisont <- c(1:16)
fevdHUS <- fevdHUS %>% gather(Variable, Value, - Horisont)

fevdKREDIT <- as.data.frame(fevd$Kredit)
fevdKREDIT$Horisont <- c(1:16)
fevdKREDIT <- fevdKREDIT %>% gather(Variable, Value, - Horisont)

fill <- c("#e41a1c", "#4daf4a","#377eb8")
fill1 <- c("#377eb8", "#e41a1c", "#4daf4a")
fill2 <- c("#377eb8", "#4daf4a","#e41a1c")

p36 <- ggplot(fevdBNP) + geom_bar(aes(y = Value, x = Horisont, fill = reorder(Variable, Value)), stat="identity", show.legend = F) + th + 
  scale_x_continuous(breaks = seq(0,16, by = 1)) +
  labs(title = "BNP",y = "", x ="Antal kvartaler", caption = "") + scale_fill_manual(values = fill) + theme(plot.title = element_text(hjust = 0.5))

p37 <- ggplot(fevdHUS) + geom_bar(aes(y = Value, x = Horisont, fill = reorder(Variable, Value)), stat="identity", show.legend = F) + th + 
  scale_x_continuous(breaks = seq(0,16, by = 1)) +
  labs(title = "Huspriser",y = "", x ="Antal kvartaler", caption = "") + scale_fill_manual(values = fill1) + theme(plot.title = element_text(hjust = 0.5))

p38 <- ggplot(fevdKREDIT) + geom_bar(aes(y = Value, x = Horisont, fill = reorder(Variable, Value)), stat="identity") + th + 
  scale_x_continuous(breaks = seq(0,16, by = 1)) +
  labs(title = "Kredit",y = "", x ="Antal kvartaler", caption = "Kilde: Egen beregning") +
  theme(legend.position="bottom", legend.title = element_blank()) + theme(panel.grid.minor.x = element_blank()) +
  guides(fill = guide_legend(reverse = F)) + scale_fill_manual(values = fill2)+ theme(plot.title = element_text(hjust = 0.5))

ggsave("FEVD1.pdf", plot = p36 , width = 30, height = 8, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("FEVD2.pdf", plot = p37, width = 30, height = 8, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")
ggsave("FEVD3.pdf", plot = p38, width = 30, height = 9.5, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Tuningpoints - Finansiel cyklus og recessioner 
LogData <- read_excel("LogData.xlsx")
LogData$Tid <- as.Date(LogData$Tid)

Hus_top <- LogData %>% dplyr::select(Tid, Hus, Toppe...4) %>% dplyr::filter(Toppe...4=="Top")
Kredit_top <- LogData %>% dplyr::select(Tid, Kredit, Toppe...7) %>% dplyr::filter(Toppe...7=="Top")

recession <- read_excel("Udtrukket data.xlsx", sheet = "Recession")
recession <- dplyr::filter(recession, Recession=="Ja")
recession$Tid

p39 <- ggplot(LogData, aes(x=Tid, y=Hus)) + geom_line(color = "#4daf4a") +
  geom_point(data=Hus_top, aes(x=Tid, y=Hus), fill="Black", shape=23, size=2, color="Black")+
  annotate("rect", xmin=as.Date("1951-02-15"), xmax=as.Date("1951-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1954-08-15"), xmax=as.Date("1954-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1957-08-15"), xmax=as.Date("1957-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1960-08-15"), xmax=as.Date("1960-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1962-08-15"), xmax=as.Date("1963-02-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1970-08-15"), xmax=as.Date("1970-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1974-08-15"), xmax=as.Date("1975-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1977-11-15"), xmax=as.Date("1978-02-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1980-05-15"), xmax=as.Date("1980-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1988-02-15"), xmax=as.Date("1988-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1989-05-15"), xmax=as.Date("1989-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1992-11-15"), xmax=as.Date("1993-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1997-08-15"), xmax=as.Date("1997-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2001-11-15"), xmax=as.Date("2002-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2006-08-15"), xmax=as.Date("2006-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-02-15"), xmax=as.Date("2009-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2017-05-15"), xmax=as.Date("2017-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(3.75,5.3), breaks=round(seq(min(3.50), max(5.5), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = "Udvikling i huspriser", subtitle="De grå felter illustrerer recessioner i Danmark og de sorte punkter er cyklustoppe.", x=NULL, y="Log", caption = "Kilde: Egen beregning, (Abildgren, 2019)") +
  theme(panel.grid.minor.x = element_blank()) + th

ggsave("Finans_recession1.pdf", plot = p39, width = 25, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p40 <- ggplot(LogData, aes(x=Tid, y=Kredit)) + geom_line(color="#e41a1c") +
  geom_point(data=Kredit_top, aes(x=Tid, y=Kredit), fill="Black", shape=23, size=2, color="Black")+
  annotate("point", x=as.Date("1974-02-15"), y=4.549, fill="Black", shape=23, size=2, color="Black")+
  annotate("point", x=as.Date("1987-11-15"), y=4.981, fill="Black", shape=23, size=2, color="Black")+
  annotate("rect", xmin=as.Date("1951-02-15"), xmax=as.Date("1951-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1954-08-15"), xmax=as.Date("1954-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1957-08-15"), xmax=as.Date("1957-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1960-08-15"), xmax=as.Date("1960-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1962-08-15"), xmax=as.Date("1963-02-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1970-08-15"), xmax=as.Date("1970-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1974-08-15"), xmax=as.Date("1975-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1977-11-15"), xmax=as.Date("1978-02-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1980-05-15"), xmax=as.Date("1980-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1988-02-15"), xmax=as.Date("1988-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1989-05-15"), xmax=as.Date("1989-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1992-11-15"), xmax=as.Date("1993-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("1997-08-15"), xmax=as.Date("1997-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2001-11-15"), xmax=as.Date("2002-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2006-08-15"), xmax=as.Date("2006-11-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-02-15"), xmax=as.Date("2009-05-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2017-05-15"), xmax=as.Date("2017-08-15"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(3.25,6), breaks=round(seq(min(3), max(6), by = 0.5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = "Udvikling i kredit", subtitle="De grå felter illustrerer recessioner i Danmark og de sorte punkter er cyklustoppe.", x=NULL, y="Log", caption = "Kilde: Egen beregning, (Abildgren, 2019)") + 
  theme(panel.grid.minor.x = element_blank()) + th

ggsave("Finans_recession2.pdf", p40, width = 25, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Recessioner er dybere
Claessen_data <- read_excel("Udtrukket data.xlsx", sheet = "Dybere recessioner")
Claessen_data <- gather(Claessen_data, key=var, value = value,`Recession med`, `Recession uden`)

p41 <- ggplot(Claessen_data, aes(x=Type, y=value, fill=reorder(var, desc(value)))) + geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recessioner associeret med finansielle forstyrrelser", subtitle="Median effekt på output.", x=NULL, y="Procent", caption = "Kilde: (Claessens et al., 2011b)", fill=NULL) + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  theme(legend.position="bottom") + scale_fill_brewer(palette = "Paired") + th 

ggsave("Finans_recession3.pdf", p41, width = 25, height = 15, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Finansielle indikatorer
TYPER <- read_excel("Finansielle indikatorer.xlsx", sheet = "Lån1")
DNRUDDKS_meta <- sdk_retrieve_metadata("DNRUDDKS")
variables <- sdk_get_variables(DNRUDDKS_meta)

RENTE <- sdk_retrieve_data("DNRUDDKS", Tid = "*", DATA = "ULTIMONOM", AFDRAG=c("*"), RENTE="*", INDSEK="1000", EJENKATNAT="ALLE", RENTEFIX1="ALLE", RENTEREF="ALLE")

RENTE <- subset(RENTE, AFDRAG!="In total")
RENTE <- subset(RENTE, RENTE!="Variable interest rate with interest rate cap - reached")
RENTE <- subset(RENTE, RENTE!="Variable or fixed interest rate in total")
RENTE <- subset(RENTE, TID!="2013M09")
RENTE <- subset(RENTE, RENTE!="Variable interest rate with interest rate cap - not reached")

RENTE$DATA = NULL
RENTE$INDSEK = NULL
RENTE$EJENKATNAT = NULL
RENTE$RENTEFIX1 = NULL
RENTE$RENTEREF = NULL

RENTE$TID2 <- substr(RENTE$TID,1,nchar(RENTE$TID)-3)
RENTE$TID3 <- substr(RENTE$TID,6,nchar(RENTE$TID))
RENTE$TID4 <- paste(RENTE$TID2, RENTE$TID3, "01",sep="-")
RENTE$TID <- as.Date(RENTE$TID4, format="%Y-%m-%d")


RENTE$TYPE <- paste(RENTE$RENTE, RENTE$AFDRAG)
RENTE$INDHOLD <- as.numeric(RENTE$INDHOLD)

RENTE$TID4 <- NULL
RENTE$TID3 <- NULL
RENTE$TID2 <- NULL
RENTE$RENTE <- NULL
RENTE$AFDRAG <- NULL

TYPER <- t(TYPER)
colnames(TYPER) = TYPER[1, ] # the first row will be the header
TYPER = TYPER[-1, ]          # removing the first row.

TYPER <- melt(TYPER, id.vars=c("typer"))

TYPER$Var1 <- as.character(TYPER$Var1)
TYPER$TID2 <- substr(TYPER$Var1,1,nchar(TYPER$Var1)-3)
TYPER$TID3 <- substr(TYPER$Var1,6,nchar(TYPER$Var1))
TYPER$TID4 <- paste(TYPER$TID2, TYPER$TID3, "01",sep="-")
TYPER$TID <- as.Date(TYPER$TID4, format="%Y-%m-%d")

TYPER$TID4 <- NULL
TYPER$TID3 <- NULL
TYPER$TID2 <- NULL
TYPER$Var1 <- NULL
colnames(TYPER) = c("TYPE", "INDHOLD", "TID")
TYPER <- TYPER[,c(3,2,1)]

TYPER$INDHOLD <- as.numeric(levels(TYPER$INDHOLD))[TYPER$INDHOLD]
TYPER$TYPE <- as.character(TYPER$TYPE)
TYPER <- subset(TYPER, TID !="2013-10-01")
REALKREDIT <- rbind(TYPER, RENTE)

REALKREDIT_prop <- ddply(REALKREDIT, "TID", transform,
                         Percent = INDHOLD / sum(INDHOLD) * 100)

REALKREDIT_prop$TYPE <- as.factor(REALKREDIT_prop$TYPE)
REALKREDIT_prop$TYPE <- factor(REALKREDIT_prop$TYPE, levels = levels(REALKREDIT_prop$TYPE))

p42 <- ggplot(REALKREDIT_prop, aes(TID, Percent)) + geom_area(aes(fill=TYPE),alpha=0.8) +
  scale_y_continuous(breaks = round(seq(min(0), max(200), by = 10),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Lånetyper for realkreditudlån", 
                y="Andel af realkreditlån",
                x=NULL,
                subtitle = "For januar 2003 - marts 2020 - Databrud i oktober 2013 - Lånetyper med renteloft blev introduceret i 2009\nog stod for ca. 10% af markedet, men faldende og udgjorde i 2020 kun få procent.",
                caption = "Kilde: Danmarks Statistik", fill=NULL) +
  scale_fill_manual(breaks=c("Fixed interest rate With instalments", "Fixed interest rate Without intalments", "Variable interest rate without interest rate cap With instalments", "Variable interest rate without interest rate cap Without intalments"),
                    labels=c("Fast rente", "Fast rente u. afdrag", "Variabel rente", "Variabel rente u. afdrag"),
                    values=c("Fixed interest rate With instalments"="#9ecae1", "Fixed interest rate Without intalments"="#6baed6", "Variable interest rate without interest rate cap With instalments"="#08519c", "Variable interest rate without interest rate cap Without intalments"="#08306b")) +
  th + theme(legend.position="bottom") + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank())

ggsave("Realkreditlån.pdf", p42, width = 25, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

NATFORM_meta <- sdk_retrieve_metadata("NATFORM")
variables <- sdk_get_variables(NATFORM_meta)

NATFORM <- sdk_retrieve_data("NATFORM", Tid = "*", FININS = "*")
NATFORM$INDHOLD <- NATFORM$INDHOLD/1000

ASSETS <- subset(NATFORM, AKTPAS=="Financial assets")

LIABILITIES <- subset(NATFORM, AKTPAS=="Liabilities")

NATFORM_ASSETS <- subset(ASSETS, FININS %in% c("F.2 Currency and deposits",
                                               "F.3 Debt securities",
                                               "F.5 Equity and investment fund shares or units",
                                               "F.6 Insurance, pension and standardised guarantee schemes",
                                               "F.7 Financial derivatives and employee stock options",
                                               "F.8 Other accounts receivable/payable"))

NATFORM_LIABILITIES <- subset(LIABILITIES, FININS %in% c("F.3 Debt securities",
                                                         "F.41 Short-term loans",
                                                         "F.42 Long-term loans",
                                                         "F.8 Other accounts receivable/payable",
                                                         "F.4 Loans"))

p43 <- ggplot(NATFORM_ASSETS, aes(TID, INDHOLD, width=0.6, fill=Aktiver)) + geom_bar(stat = "identity", aes(fill = FININS), alpha=1)+ 
  labs(title="Sammensætning af husholdningernes finansielle aktiver", 
                y="Mia. kr.",
                x=NULL,
                subtitle = NULL,
                caption = "Kilde: Danmarks Statistik")+
  scale_y_continuous(limits = c(0, 6600), breaks=round(seq(min(0), max(6600), by = 1000),1),labels = function(x) format(x, scientific =FALSE)) +
  scale_x_continuous(limits = c(1993.5, 2018.5), breaks=round(seq(min(1994), max(2018), by = 1),1)) +
  scale_fill_brewer(name="Aktivtyper",
                             breaks=c("F.2 Currency and deposits", "F.3 Debt securities", "F.5 Equity and investment fund shares or units", "F.6 Insurance, pension and standardised guarantee schemes", "F.7 Financial derivatives and employee stock options", "F.8 Other accounts receivable/payable"),
                             labels=c("F2 Kontanter og indskud", "F3 Gældsværdipapirer", "F5 Aktier", "F6 Pension og forsikring", "F7 Deriviater og aktieoptioner", "F8 Andre aktiver"), palette=1, direction=-1)+
  th + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank())

ggsave("Finansielle aktiver.pdf", p43, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
NATFORM_LIABILITIES_1 <- subset(NATFORM_LIABILITIES, FININS!="F.4 Loans" | TID %not in% c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))

p44 <- ggplot(NATFORM_LIABILITIES_1, aes(TID, INDHOLD, width=0.6, fill=Passeiver)) + geom_bar(stat = "identity", aes(fill = FININS), alpha=1) +
  scale_fill_manual(name="Passivtyper",
                             breaks=c("F.3 Debt securities", "F.4 Loans", "F.41 Short-term loans", "F.42 Long-term loans", "F.8 Other accounts receivable/payable"),
                             labels=c("F3 Gældsværdipapirer", "F4 Lån", "F4.1 Kortfristet lån", "F4.2 Langfristede lån", "F8 Andre passiver"),
                             values=c("F.3 Debt securities" = "#084594", "F.4 Loans"="#2171b5", "F.41 Short-term loans"="#4292c6", "F.42 Long-term loans"="#6baed6", "F.8 Other accounts receivable/payable"="#9ecae1")) +
  labs(title="Sammensætning af husholdningernes passiver", 
                y="Mia. kr.",
                x=NULL,
                subtitle = "Opgørelsen af kort og langfristet gæld begynder først i 2003.",
                caption = "Kilde: Danmarks Statistik") + 
  scale_y_continuous(limits = c(0, 3100), breaks=round(seq(min(0), max(3100), by = 500),1),labels = function(x) format(x, scientific =FALSE))+
  scale_x_continuous(limits = c(1993.5, 2018.5), breaks=round(seq(min(1994), max(2018), by = 1),1))+
  th + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank())

ggsave("Finansielle passiver.pdf", p44, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

NATFORM_ASSETS_TOTAL <- subset(ASSETS, FININS %in% c("F Total financial instruments"))
NATFORM_LIABILITIES_TOTAL <- subset(LIABILITIES, FININS %in% c("F Total financial instruments"))
NATFORM_FINANCIAL_WEALTH <- subset(ASSETS, FININS %in% c("Net financial assets", "Net wealth"))
NATFORM_WEALTH <- subset(ASSETS, FININS=="Net wealth")


p45 <- ggplot() + geom_bar(data = NATFORM_ASSETS_TOTAL, aes(x=TID, y=INDHOLD, fill=AKTPAS, width=0.6),stat = "identity") +
  geom_bar(data = NATFORM_LIABILITIES_TOTAL, aes(x=TID, y=-INDHOLD, fill=AKTPAS, width=0.6),stat = "identity") +
  geom_step(data = NATFORM_FINANCIAL_WEALTH, aes(x=TID, y=INDHOLD, color=FININS),size=1) +
  scale_fill_manual(name="Brutto:",
                             breaks=c("Financial assets", "Liabilities", "Finansiel netto formue"),
                             labels=c("Finansielle aktiver", "Finansielle passiver", "Finansielle netto formue"),
                             values=c("Financial assets"="#9ecae1", "Liabilities"="#084594")) +
  scale_color_manual(name = "Netto:", 
                              values = c("Net financial assets" = "#737373", "Net wealth" = "#252525"),
                              labels=c("Finansiel formue", "Formue")) +
  scale_y_continuous(limits = c(-3000, 8100), breaks=round(seq(min(-3000), max(8100), by = 1000),1),labels = function(x) format(x, scientific =FALSE)) +
  scale_x_continuous(limits = c(1993.5, 2018.5), breaks=round(seq(min(1994), max(2018), by = 1),1)) +
  labs(title="Husholdningernes nettoformue", 
                y="Mia. kr.",
                x=NULL,
                subtitle = "De reale aktiver som boliger, biler, både og fly er først af Danmarks Statistik medregnet fra 2004.",
                caption = "Kilde: Danmarks Statistik") +
  th + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank())

ggsave("Nettoformue.pdf", p45, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

Udlan <- read_excel("Finansielle indikatorer.xlsx", sheet = "Udlångab")
Udlan$Tid <- as.Date(Udlan$Tid)

p46 <- ggplot(Udlan[41:198,], aes(x=Tid, y=`Udlånsgab (procentpoint)`)) + geom_col(fill="#377eb8") +
  annotate("rect", xmin=as.Date("1987-03-31"), xmax=as.Date("1993-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-09-30"), xmax=as.Date("2013-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-33, 39), breaks=round(seq(min(-30), max(40), by = 10),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years"))+
  labs(title = "Udlånsgab", subtitle = "De grå felter illustrerer systemiske finansielle kriser.", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Det Systemiske Risikoråd")+
  theme(panel.grid.minor.x = element_blank()) + th

ggsave("Udlångab.pdf", p46, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

Bolig <- read_excel("Finansielle indikatorer.xlsx", sheet = "Boliggab")
Bolig$Dato <- as.Date(Bolig$Dato)

p47 <- ggplot(Bolig, aes(x=Dato, y=`Boligprisgab (procentpoint)`)) +
  annotate("rect", xmin=as.Date("1987-03-31"), xmax=as.Date("1993-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-09-30"), xmax=as.Date("2013-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  geom_col(fill="#377eb8") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(-20,30), breaks=round(seq(min(-20), max(30), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = "Boligprisgab", subtitle = "De grå felter illustrerer systemiske finansielle kriser.", x=NULL, y="Afvigelse fra trend i procentpoint", caption = "Kilde: Det Systemiske Risikoråd")+
  theme(panel.grid.minor.x = element_blank()) + th

ggsave("Boliggab.pdf", p47 , width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

Byrde <- read_excel("Finansielle indikatorer.xlsx", sheet = "Boligbyrde")
Byrde$Dato <- as.Date(Byrde$Dato)

p48 <- ggplot(Byrde, aes(x=Dato, y=`Stiliseret boligbyrde`)) + geom_line() +
  annotate("rect", xmin=as.Date("1987-03-31"), xmax=as.Date("1993-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-09-30"), xmax=as.Date("2013-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(18, 40), breaks=round(seq(min(15), max(40), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Boligbyrde", subtitle = "De grå felter illustrerer systemiske finansielle kriser.", x=NULL, y="Procent af disponibel indkomst", caption = "Kilde: Det Systemiske Risikoråd")+
  theme(panel.grid.minor.x = element_blank()) + th 

ggsave("Boligbyrde.pdf", p48, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

Gearing <- read_excel("Finansielle indikatorer.xlsx", sheet = "Gearing")
Gearing$Dato <- as.Date(Gearing$Dato)

p49 <- ggplot(Gearing, aes(x=Dato, y=Pengeinstitutter)) + geom_line() +
  annotate("rect", xmin=as.Date("1987-03-31"), xmax=as.Date("1993-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  annotate("rect", xmin=as.Date("2008-09-30"), xmax=as.Date("2013-12-31"), ymin=-Inf, ymax=Inf, alpha=0.2)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(13, 25), breaks=round(seq(min(12), max(30), by = 3),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  labs(title = "Gearing i pengeinstitutter", subtitle = "4 kvartalers glidende vægtet gennemsnit. De grå felter illustrerer systemiske finansielle kriser.", x=NULL, y="Aktiver/kapital", caption = "Kilde: Det Systemiske Risikoråd")+
  theme(panel.grid.minor.x = element_blank()) + th 

ggsave("Gearing.pdf", p49, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

Kredit_lån <- read_excel("Finansielle indikatorer.xlsx", sheet = "Udlån")
Kredit_lån$Tid <- as.Date(Kredit_lån$Tid)
  
RR <- Kredit_lån %>% gather(key=var, value=value, -Tid, Erhverv, Husholdning)

RR$var <- as.factor(RR$var)
RR$var <- factor(RR$var, levels = levels(RR$var))

p50 <- ggplot(RR, aes(x=Tid, y=value)) + geom_area(aes(fill=var),alpha=0.8) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",") ,limits = c(0, 3700), breaks=round(seq(min(0), max(3600), by = 500),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("4 years")) +
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Fordeling af udlån i den danske private sektor", subtitle = "Serierne er baseret på en smal kreditdefinition, det omfatter kun udlån til indenlandske sektorer\nfra danske penge- og realkreditinstitutter.", x=NULL, y="Mia. kr", caption = "Kilde: Det Systemiske Risikoråd", fill=NULL)+
  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Fordeling af udlån.pdf", p50, width = 25, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Taylor-regel
Taylor <- read_excel("Taylor data.xlsx")

Tre <- hpfilter(log(Taylor$BNP),freq = 6.25)
Taylor$BNP_afvigelse <- Tre$cycle*100

Taylor$rente <- 2+Taylor$Inflation+0.5*(Taylor$Inflation-2)+0.5*(Taylor$BNP_afvigelse)
Taylor$rente_bolig <- 2+Taylor$Inflation+0.45*(Taylor$Inflation-2)+0.45*(Taylor$BNP_afvigelse)+0.1*(Taylor$Boliggab)
Taylor$rente_udlån <- 2+Taylor$Inflation+0.45*(Taylor$Inflation-2)+0.45*(Taylor$BNP_afvigelse)+0.1*(Taylor$Udlånsgab)

colors3 <- c("Diskonto" = "#ff7f00", "Alm. Taylor" = "#377eb8", "Taylor med udlånsgab" = "#e41a1c", "Taylor med boliggab" = "#4daf4a")

p51 <- ggplot(Taylor, aes(x=År, y=Diskonto,color="Diskonto")) + geom_line() + geom_line(y=0, color="black", linetype="dashed") +
  geom_line(aes(y=rente, color="Alm. Taylor")) + geom_line(aes(y=rente_bolig, color="Taylor med boliggab")) + geom_line(aes(y=rente_udlån, color="Taylor med udlånsgab")) +
  scale_y_continuous(labels = function(value) paste0(value, "%"), limits = c(-1, 11), breaks=round(seq(min(0), max(12), by = 2),1)) +
  scale_x_continuous(limits = c(1994.5, 2019.5), breaks=round(seq(min(1995), max(2019), by = 3),1)) +
  labs(title = "Renteniveauer for tre Taylorregler", subtitle = "Diskontorenten er benyttet som det faktiske renteniveau i Danmark.", x=NULL, y=NULL, caption = "Kilde: Egne beregninger", color=NULL)+
  scale_color_manual(values = colors3) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Taylor.pdf", p51, width = 30, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Gæld/disponible indkomst
Gæld_indkomst <- read_csv("Gæld, OECD.csv", col_types = cols())
Gæld_indkomst <- Gæld_indkomst %>% dplyr::select(TIME, Value)

p52 <- ggplot(Gæld_indkomst, aes(x=TIME, y=Value)) + geom_line() +
  annotate("rect", xmin=2008.75, xmax=2014, ymin=-Inf, ymax=Inf, alpha=0.2) +
  scale_y_continuous(limits = c(150, 350), breaks=round(seq(min(150), max(350), by = 50),1), labels = function(value) paste0(value, "%")) +
  scale_x_continuous(limits = c(1995,2018), breaks=round(seq(min(1995), max(2018), by = 2),1)) +
  labs(title="Husholdningernes gæld som andel af den disponible indkomst", subtitle = "Det grå felt illustrerer en systemisk finansiel krise jf. Det systemiske risikoråd.", y="Andel af disponibel indkomst", x=NULL, caption = "Kilde: OECD") +
  th +  theme(panel.grid.minor.x = element_blank())

ggsave("Gæld som disponibel.pdf", p52, width = 30, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Stød 1
Stød1 <- read_csv("Stød_gæld.csv")
Stød1$date <- as.Date(Stød1$date)
Stød1 <- Stød1[8:22,]

plot1 <- Stød1 %>% dplyr::select(date,invh_0,invh_1,ibl_h_0,ibl_h_1,ydh_0,
                                 ydh_1,zz_i_0,zz_i_1,ch_0,ch_1,ur_0,ur_1,y_0,
                                 y_1,nlf_0,nlf_1,nlg_0,nlg_1,nlh_0,nlh_1,nlnf_0,
                                 nlnf_1,nlrow_0,nlrow_1,iba_h_0,iba_h_1,ibl_h_0,
                                 ibl_h_1,eqa_h_0,eqa_h_1,pena_h_0,pena_h_1,fnw_h_0,fnw_h_1)

plot1$invh <- (plot1$invh_1/plot1$invh_0*100)-100
plot1$ibl <- (plot1$ibl_h_1/plot1$ibl_h_0*100)-100
plot1$ydh <- (plot1$ydh_1/plot1$ydh_0*100)-100
plot1$hus <- (plot1$zz_i_1-plot1$zz_i_0)*100
plot1$ch <- (plot1$ch_1/plot1$ch_0*100)-100
plot1$ur <- (plot1$ur_1-plot1$ur_0)*100
plot1$y <- (plot1$y_1/plot1$y_0*100)-100
plot1$nlf <- (plot1$nlf_1/plot1$y_1*100)-(plot1$nlf_0/plot1$y_0*100)
plot1$nlg <- (plot1$nlg_1/plot1$y_1*100)-(plot1$nlg_0/plot1$y_0*100)
plot1$nlh <- (plot1$nlh_1/plot1$y_1*100)-(plot1$nlh_0/plot1$y_0*100)
plot1$nlnf <- (plot1$nlnf_1/plot1$y_1*100)-(plot1$nlnf_0/plot1$y_0*100)
plot1$nlrow <- (plot1$nlrow_1/plot1$y_1*100)-(plot1$nlrow_0/plot1$y_0*100)
plot1$ibah <- (plot1$iba_h_1/plot1$iba_h_0)*100-100
plot1$iblh <- (plot1$ibl_h_1/plot1$ibl_h_0)*100-100
plot1$eqah <- (plot1$eqa_h_1/plot1$eqa_h_0)*100-100
plot1$penah <- (plot1$pena_h_1/plot1$pena_h_0)*100-100
plot1$fnw <- (plot1$fnw_h_1/plot1$fnw_h_0)*100-100

p53 <- ggplot(plot1, aes(x=date, y=invh, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-36.5, 0.7), breaks=round(seq(min(-35), max(0), by = 5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i husholdningernes reale investeringer", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_investering.pdf", p53, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p54 <- ggplot(plot1, aes(x=date, y=ibl, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-17, 0.18), breaks=round(seq(min(-18), max(0), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i husholdningernes bruttogæld", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_gæld.pdf", p54, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p55 <- ggplot(plot1, aes(x=date, y=ydh, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.1, 0.4), breaks=round(seq(min(-0.15), max(0.4), by = 0.05),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i husholdningernes reale disponible indkomst", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_disponibel.pdf", p55, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p56 <- ggplot(plot1, aes(x=date, y=hus, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.07, 3.5), breaks=round(seq(min(0), max(3.5), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i boligprisindeks", subtitle = NULL, y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_huspris.pdf", p56, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p57 <- ggplot(plot1, aes(x=date, y=ch, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.23, 0.1), breaks=round(seq(min(-0.26), max(0.1), by = 0.04),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i husholdningernes reale forbrug", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_forbrug.pdf", p57, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p58 <- ggplot(plot1, aes(x=date, y=ur, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.02, 0.3), breaks=round(seq(min(-0), max(0.3), by = 0.05),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i arbejdsløsraten", subtitle = NULL, y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_arbejdsløshed.pdf", p58, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p59 <- ggplot(plot1, aes(x=date, y=y, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1.7, 0.015), breaks=round(seq(min(-1.6), max(0), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i det reale output", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød1_output.pdf", p59, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

colors4 <- c("Staten" = "#ff7f00", "Husholdningerne" = "#377eb8", "Bankerne" = "#e41a1c", "Virksomhederne" = "#4daf4a", "Resten af verden"="#984ea3")

p60 <- ggplot(plot1, aes(x=date, y=nlf, color="Bankerne")) + geom_line() + geom_line(aes(y=nlg, color="Staten")) + geom_line(aes(y=nlh, color="Husholdningerne")) + 
  geom_line(aes(y=nlnf, color="Virksomhederne")) + geom_line(aes(y=nlrow, color="Resten af verden")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1, 2.2), breaks=round(seq(min(-1), max(2.2), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i nettofordringserhvervelse", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors4) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød1_nettofordering.pdf", p60, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

colors5 <- c("Pension" = "#ff7f00", "Rentebærende aktiver" = "#377eb8", "Rentebærende passiver" = "#e41a1c", "Aktier" = "#4daf4a", "Fin. nettoformue" = "#984ea3")

p61 <- ggplot(plot1, aes(x=date, y=iblh, color="Rentebærende passiver")) + geom_line() + geom_line(aes(y=penah, color="Pension")) + 
  geom_line(aes(y=ibah, color="Rentebærende aktiver")) + geom_line(aes(y=eqah, color="Aktier")) + geom_line(aes(y=fnw, color="Fin. nettoformue")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-17, 8.2), breaks=round(seq(min(-20), max(10), by = 5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years")) +
  labs(title="Udviklingen i husholdningernes finansielle balancer", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors5) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød1_finans_balance.pdf", p61, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Stød 2
Stød2 <- read_csv("Stød_huspriser.csv")
Stød2$date <- as.Date(Stød2$date)
Stød2 <- Stød2[24:31,]

plot2 <- Stød2 %>% dplyr::select(date,r_ibl_h_0,r_ibl_h_1,invh_0,invh_1,ibl_h_0,
                                 ibl_h_1,ydh_0,ydh_1,zz_i_0,zz_i_1,ch_0,ch_1,ur_0,
                                 ur_1,y_0,y_1,nlf_0,nlf_1,nlg_0,nlg_1,nlh_0,nlh_1,
                                 nlnf_0,nlnf_1,nlrow_0,nlrow_1,iba_h_0,iba_h_1,ibl_h_0,
                                 ibl_h_1,eqa_h_0,eqa_h_1,pena_h_0,pena_h_1,fnw_h_0,fnw_h_1)

plot2$invh <- (plot2$invh_1/plot2$invh_0*100)-100
plot2$ibl <- (plot2$ibl_h_1/plot2$ibl_h_0*100)-100
plot2$ydh <- (plot2$ydh_1/plot2$ydh_0*100)-100
plot2$hus <- (plot2$zz_i_1-plot2$zz_i_0)*100
plot2$ch <- (plot2$ch_1/plot2$ch_0*100)-100
plot2$ur <- (plot2$ur_1-plot2$ur_0)*100
plot2$y <- (plot2$y_1/plot2$y_0*100)-100
plot2$nlf <- (plot2$nlf_1/plot2$y_1*100)-(plot2$nlf_0/plot2$y_0*100)
plot2$nlg <- (plot2$nlg_1/plot2$y_1*100)-(plot2$nlg_0/plot2$y_0*100)
plot2$nlh <- (plot2$nlh_1/plot2$y_1*100)-(plot2$nlh_0/plot2$y_0*100)
plot2$nlnf <- (plot2$nlnf_1/plot2$y_1*100)-(plot2$nlnf_0/plot2$y_0*100)
plot2$nlrow <- (plot2$nlrow_1/plot2$y_1*100)-(plot2$nlrow_0/plot2$y_0*100)
plot2$ibah <- (plot2$iba_h_1/plot2$iba_h_0)*100-100
plot2$iblh <- (plot2$ibl_h_1/plot2$ibl_h_0)*100-100
plot2$eqah <- (plot2$eqa_h_1/plot2$eqa_h_0)*100-100
plot2$penah <- (plot2$pena_h_1/plot2$pena_h_0)*100-100
plot2$r <- (plot2$r_ibl_h_1-plot2$r_ibl_h_0)*100
plot2$fnw <- (plot2$fnw_h_1/plot2$fnw_h_0)*100-100

p62 <- ggplot(plot2, aes(x=date, y=invh, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-3.7, 0.25), breaks=round(seq(min(-4), max(5), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale investeringer", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_investering.pdf", p62, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p63 <- ggplot(plot2, aes(x=date, y=ibl, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-5, 0.25), breaks=round(seq(min(-5), max(0), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes bruttogæld", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_gæld.pdf", p63, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p64 <- ggplot(plot2, aes(x=date, y=ydh, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-2, 0.25), breaks=round(seq(min(-2), max(0), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale disponible indkomst", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_disponibel.pdf", p64, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p65 <- ggplot(plot2, aes(x=date, y=hus, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1.25, 0), breaks=round(seq(min(-1.5), max(0), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i boligprisindeks", subtitle = NULL, y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_huspris.pdf", p65, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p66 <- ggplot(plot2, aes(x=date, y=ch, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1.75, 0), breaks=round(seq(min(-2), max(0), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale forbrug", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_forbrug.pdf", p66, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p67 <- ggplot(plot2, aes(x=date, y=ur, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(0, 0.15), breaks=round(seq(min(-0), max(0.2), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i arbejdsløsraten", subtitle = NULL, y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_arbejdsløshed.pdf", p67, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p68 <- ggplot(plot2, aes(x=date, y=y, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.7, 0), breaks=round(seq(min(-1), max(0), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i det reale output", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_output.pdf", p68, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p69 <- ggplot(plot2, aes(x=date, y=nlf, color="Bankerne")) + geom_line() + geom_line(aes(y=nlg, color="Staten")) + geom_line(aes(y=nlh, color="Husholdningerne")) + 
  geom_line(aes(y=nlnf, color="Virksomhederne")) + geom_line(aes(y=nlrow, color="Resten af verden")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1, 1.5), breaks=round(seq(min(-1), max(1.5), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i nettofordringserhvervelse", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors4) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød2_nettofordering.pdf", p69, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p70 <- ggplot(plot2, aes(x=date, y=iblh, color="Rentebærende passiver")) + geom_line() + geom_line(aes(y=penah, color="Pension")) + 
  geom_line(aes(y=ibah, color="Rentebærende aktiver")) + geom_line(aes(y=eqah, color="Aktier")) + geom_line(aes(y=fnw, color="Fin. nettoformue")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-8, 1), breaks=round(seq(min(-10), max(0), by = 2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes finansielle balancer", subtitle = NULL, y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors5) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød2_finans_balance.pdf", p70, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p71 <- ggplot(plot2, aes(x=date, y=r, color="red")) + geom_line() + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(0, 1.5), breaks=round(seq(min(0), max(2), by = 0.5),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udvikling i udlånsraten", subtitle = NULL, y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød2_rente.pdf", p71, width = 15, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Stød3
Stød3 <- read_csv("Stød_rentefradrag.csv")
Stød3$date <- as.Date(Stød3$date)
Stød3 <- Stød3[24:31,]

plot3 <- Stød3 %>% dplyr::select(date,ydh_0,ydh_1,ydh_2,ibl_h_0,ibl_h_1,ibl_h_2,invh_0,
                                 invh_1,invh_2,zz_i_0,zz_i_1,zz_i_2,ch_0,ch_1,ch_2,sh_0,
                                 sh_1,sh_2,y_0,y_1,y_2,ur_0,ur_1,ur_2,cab_0,cab_1,cab_2,
                                 iba_h_0,iba_h_1,iba_h_2,ibl_h_0,ibl_h_1,ibl_h_2,eqa_h_0,
                                 eqa_h_1,eqa_h_2,pena_h_0,pena_h_1,pena_h_2,fnw_h_0,fnw_h_1,fnw_h_2)

plot3$invh1 <- (plot3$invh_1/plot3$invh_0*100)-100
plot3$invh2 <- (plot3$invh_2/plot3$invh_0*100)-100
plot3$ibl1 <- (plot3$ibl_h_1/plot3$ibl_h_0*100)-100
plot3$ibl2 <- (plot3$ibl_h_2/plot3$ibl_h_0*100)-100
plot3$ydh1 <- (plot3$ydh_1/plot3$ydh_0*100)-100
plot3$ydh2 <- (plot3$ydh_2/plot3$ydh_0*100)-100
plot3$hus1 <- (plot3$zz_i_1-plot3$zz_i_0)*100
plot3$hus2 <- (plot3$zz_i_2-plot3$zz_i_0)*100
plot3$ch1 <- (plot3$ch_1/plot3$ch_0*100)-100
plot3$ch2 <- (plot3$ch_2/plot3$ch_0*100)-100
plot3$ur1 <- (plot3$ur_1-plot3$ur_0)*100
plot3$ur2 <- (plot3$ur_2-plot3$ur_0)*100
plot3$y1 <- (plot3$y_1/plot3$y_0*100)-100
plot3$y2 <- (plot3$y_2/plot3$y_0*100)-100
plot3$sh1 <- (plot3$sh_1/plot3$sh_0*100)-100
plot3$sh2 <- (plot3$sh_2/plot3$sh_0*100)-100
plot3$ca1 <- (plot3$cab_1/plot3$cab_0*100)-100
plot3$ca2 <- (plot3$cab_2/plot3$cab_0*100)-100
plot3$ibah1 <- (plot3$iba_h_1/plot3$iba_h_0)*100-100
plot3$iblh1 <- (plot3$ibl_h_1/plot3$ibl_h_0)*100-100
plot3$eqah1 <- (plot3$eqa_h_1/plot3$eqa_h_0)*100-100
plot3$penah1 <- (plot3$pena_h_1/plot3$pena_h_0)*100-100
plot3$ibah2 <- (plot3$iba_h_2/plot3$iba_h_0)*100-100
plot3$iblh2 <- (plot3$ibl_h_2/plot3$ibl_h_0)*100-100
plot3$eqah2 <- (plot3$eqa_h_2/plot3$eqa_h_0)*100-100
plot3$penah2 <- (plot3$pena_h_2/plot3$pena_h_0)*100-100
plot3$fnw1 <- (plot3$fnw_h_1/plot3$fnw_h_0*100)-100
plot3$fnw2 <- (plot3$fnw_h_2/plot3$fnw_h_0*100)-100

p72 <- ggplot(plot3, aes(x=date,y=ydh1)) + geom_line(color="red") + geom_line(aes(y=ydh2), color="#377eb8")  + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.5, 0.02), breaks=round(seq(min(-0.6), max(0), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale disponible indkomst", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_disponibel.pdf", p72, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p73 <- ggplot(plot3, aes(x=date, y=ibl1)) + geom_line(color="red") + geom_line(aes(y=ibl2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.3, 0.02), breaks=round(seq(min(-0.6), max(0), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes bruttogæld", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_gæld.pdf", p73, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p74 <- ggplot(plot3, aes(x=date,y=invh1)) + geom_line(color="red") + geom_line(aes(y=invh2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.9, 0.05), breaks=round(seq(min(-1), max(0), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale investeringer", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_investering.pdf", p74, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p75 <- ggplot(plot3, aes(x=date, y=hus1)) + geom_line(color="red") + geom_line(aes(y=hus2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.3, 0.02), breaks=round(seq(min(-0.3), max(0), by = 0.05),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i boligprisindeks", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_huspriser.pdf", p75, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p76 <- ggplot(plot3, aes(x=date, y=ch1)) + geom_line(color="red") + geom_line(aes(y=ch2), color="#377eb8") + geom_line(y=0, color="black")+ 
  scale_y_continuous(limits = c(-1, 0.05), breaks=round(seq(min(-1), max(0), by = 0.2),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes reale forbrug", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_forbrug.pdf", p76, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p77 <- ggplot(plot3, aes(x=date, y=ur1)) + geom_line(color="red") + geom_line(aes(y=ur2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.02, 0.2), breaks=round(seq(min(0), max(0.2), by = 0.1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i arbejdsløsraten", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y="%-point", x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_arbejdsløshed.pdf", p77, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p78 <- ggplot(plot3, aes(x=date, y=y1)) + geom_line(color="red") + geom_line(aes(y=y2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.35, 0.02), breaks=round(seq(min(-0.35), max(0), by = 0.05),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i det reale output", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_output.pdf", p78, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p79 <- ggplot(plot3, aes(x=date, y=sh1)) + geom_line(color="red") + geom_line(aes(y=sh2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-1.5, 4.0), breaks=round(seq(min(-2), max(4.0), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes opsparing", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_opsparing.pdf", p79, width = 15, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p80 <- ggplot(plot3, aes(x=date, y=ca1)) + geom_line(color="red") + geom_line(aes(y=ca2), color="#377eb8") + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-0.2, 5), breaks=round(seq(min(0), max(5), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i betalingsbalancens løbende poster", subtitle = "Isoleret rentefradrag = Rød linje\nKombineret rentefradrag og adfærdsændring = Blå linje", y=NULL, x=NULL, caption = "Kilde: Egne beregninger") +
  th +  theme(panel.grid.minor.x = element_blank()) + theme(legend.position = "none")

ggsave("Stød3_betalingsbalance.pdf", p80, width = 20, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p81 <- ggplot(plot3, aes(x=date, y=iblh1, color="Rentebærende passiver")) + geom_line() + geom_line(aes(y=penah1, color="Pension")) + 
  geom_line(aes(y=ibah1, color="Rentebærende aktiver")) + geom_line(aes(y=eqah1, color="Aktier")) + geom_line(aes(y=fnw1, color="Fin. nettoformue")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-2, 2), breaks=round(seq(min(-2), max(2.5), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes finansielle balancer", subtitle = "Stød: Isoleret rentefradrag", y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors5) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød3_finans_balance1.pdf", p81, width = 20, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

p82 <- ggplot(plot3, aes(x=date, y=iblh2, color="Rentebærende passiver")) + geom_line() + geom_line(aes(y=penah2, color="Pension")) + 
  geom_line(aes(y=ibah2, color="Rentebærende aktiver")) + geom_line(aes(y=eqah2, color="Aktier")) + geom_line(aes(y=fnw2, color="Fin. nettoformue")) + geom_line(y=0, color="black")+
  scale_y_continuous(limits = c(-2, 2), breaks=round(seq(min(-2), max(2.5), by = 1),1), labels = function(value) paste0(value, "%")) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 years")) +
  labs(title="Udviklingen i husholdningernes finansielle balancer", subtitle = "Stød: Kombineret rentefradrag og adfærdsændring", y=NULL, x=NULL, caption = "Kilde: Egne beregninger", color=NULL) +
  scale_color_manual(values = colors5) +  theme(panel.grid.minor.x = element_blank()) + th + theme(legend.position = "bottom")

ggsave("Stød3_finans_balance2.pdf", p82, width = 20, height = 12, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")

#Forside-figur
for1 <- hpfilter(Samlet_cyklus$Huspris, freq = 5000)
for2 <- hpfilter(Samlet_cyklus$Kredit, freq = 5000)

Forside <- as.data.frame(Data$Tid)
Forside <- rename(Forside, replace = c("Data$Tid"="Tid"))
Forside$Tid <- as.Date(Forside$Tid)
Forside$Huspriser <- as.numeric(for1$trend)
Forside$Kredit <- as.numeric(for2$trend)

p83 <- ggplot(Forside[81:268,], aes(x=Tid, y=Kredit, color = "Kredit (Højre akse)")) + geom_line(size=1.2) +
  geom_line(aes(y=0), color="black",linetype="dashed") +
  geom_line(aes(y=Huspriser,color = "Huspriser  (Højre akse)"), size=1.2) +
  scale_y_continuous(labels = function(value) paste0(value, "%") ,limits = c(-20, 20), breaks=round(seq(min(-40), max(40), by = 5),1)) +
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("3 years")) +
  labs(title = NULL, subtitle=NULL, x=NULL, y=NULL, caption = NULL, color=NULL)+
  scale_color_manual(values = colors2) + theme(legend.position="none") + theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank()) + th

ggsave("Forside-figur.jpeg", plot = p83, width = 20, height = 10, units = "cm", path = "C:/Users/Andreas/Dropbox/Speciale/Grafer og plots")


