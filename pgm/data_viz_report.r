par(mfrow=c(2,2))

plot(anscombe$x1, anscombe$y1, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x1", ylab="y1", main="anscombe1")
abline(lm(anscombe$y1~anscombe$x1), col="red")

plot(anscombe$x2, anscombe$y2, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x2", ylab="y2", main="anscombe2")
abline(lm(anscombe$y2~anscombe$x2), col="red")

plot(anscombe$x3, anscombe$y3, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x3", ylab="y3", main="anscombe3")
abline(lm(anscombe$y3~anscombe$x3), col="red")

plot(anscombe$x4, anscombe$y4, pch=20, xlim=c(0, 20), ylim=c(0, 20), col="blue", xlab="x4", ylab="y4", main="anscombe4")
abline(lm(anscombe$y4~anscombe$x4), col="red")

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
