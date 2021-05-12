par(mfrow=c(2,2))

plot(anscombe$x1, anscombe$y1, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x1", ylab="y1", main="anscombe1")
abline(lm(anscombe$y1~anscombe$x1), col="red")

plot(anscombe$x2, anscombe$y2, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x2", ylab="y2", main="anscombe2")
abline(lm(anscombe$y2~anscombe$x2), col="red")

plot(anscombe$x3, anscombe$y3, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x3", ylab="y3", main="anscombe3")
abline(lm(anscombe$y3~anscombe$x3), col="red")

plot(anscombe$x4, anscombe$y4, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x4", ylab="y4", main="anscombe4")
abline(lm(anscombe$y4~anscombe$x4), col="red")

########################################################################################################################################################################

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)

library(zoo)
library(ggplot2)
library(gridExtra)

covid19 <- read.csv("C:/Users/ljyjj/Documents/방송대/3학년과제/coviddata.csv", header=T)

covid19KR <- subset(covid19, geoId=="KR")
covid19CN <- subset(covid19, geoId=="CN")
covid19US <- subset(covid19, geoId=="US")
covid19FR <- subset(covid19, geoId=="FR")

c1 <- ggplot(data=covid19KR, aes(x=as.Date(dateRep, format="%d/%m/%Y"), y=cases)) + 
    geom_line(color="blue") + 
	xlab("날짜") + 
	ylab("신규확진자수") + 
	ggtitle("한국") +
	theme_bw(base_family = "NanumGothic")

c2 <- ggplot(data=covid19CN, aes(x=as.Date(dateRep, format="%d/%m/%Y"), y=cases)) + 
    geom_line(color="blue") +
	xlab("날짜") + 
	ylab("신규확진자수") + 
	ggtitle("중국") +
	theme_bw(base_family = "NanumGothic")

c3 <- ggplot(data=covid19US, aes(x=as.Date(dateRep, format="%d/%m/%Y"), y=cases)) + 
    geom_line(color="blue") +
	xlab("날짜") + 
	ylab("신규확진자수") + 
	ggtitle("미국") +
	theme_bw(base_family = "NanumGothic")

c4 <- ggplot(data=covid19FR, aes(x=as.Date(dateRep, format="%d/%m/%Y"), y=cases)) + 
    geom_line(color="blue") +
	xlab("날짜") + 
	ylab("신규확진자수") + 
	ggtitle("프랑스") +
	theme_bw(base_family = "NanumGothic")

grid.arrange(c1, c2, c3, c4, ncol=2, nrow=2)

########################################################################################################################################################################

data(UCBAdmissions)
str(UCBAdmissions)

par(mfrow=c(1,2))

mosaicplot(apply(UCBAdmissions, c(2,1), sum), color=c("red","blue"), main="전체 합격자 비율", ylab="불합격/합격", xlab="성별")
mosaicplot(~Dept+Gender+Admit, data=UCBAdmissions, color=c("red","blue"), dir=c("v","v","h"), off=1, main="과별 합격자 비율", ylab="불합격/합격", xlab="과별/성별")

########################################################################################################################################################################

install.packages("coronavirus")

library(coronavirus)
library(ggplot2)
library(scales)

data(coronavirus)
str(coronavirus)

covid19KR <- subset(coronavirus, country=="Korea, South", type = "confirmed")
datebreaks <- seq(as.Date("2020-01-22"), as.Date("2021-01-21"), "1 year")
#scale_x_date(breaks=datebreaks, labels=date_format("%Y-%m-%d"), expand=c(0.01,0.01)) +

ggplot(data=covid19KR, aes(x=date, y=cases)) + 
    geom_line(color="blue") + 
	xlab("날짜") + 
	ylab("신규확진자수") + 
	ggtitle("한국") +
	scale_x_date(breaks=datebreaks, labels=date_format("%Y-%m-%d"), expand=c(0.01,0.01)) +
	theme_bw(base_family = "NanumGothic")
