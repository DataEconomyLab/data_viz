knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)

# 프로그램 2-1
# 원그래프 그리기
blType = c("A", "B", "B", "A", "A", "O", "A", "AB", "O", "O", 
        "O", "A", "A", "B", "AB", "A", "O", "B", "A", "B", 
        "B", "A", "B", "A", "B", "AB", "B", "A", "O", "AB", 
        "O", "B", "A", "B", "A", "O", "B", "A", "A", "A",
        "A", "O", "A", "O", "O", "B", "B", "O", "AB", "A",
        "B", "AB", "B", "O", "O", "O", "AB", "O", "O", "B", 
        "A", "A", "O", "A", "B", "O", "A", "O", "B", "O",
        "A", "B", "O", "AB", "B", "B", "A", "O", "B", "A",
        "B", "B", "O", "AB", "B", "A", "AB", "A", "B", "A",
        "A", "O", "O", "A", "A", "O", "AB", "A", "A", "O")

blType

srt.blType = sort(table(blType), decreasing = T)
srt.blType

slices = c("red", "blue", "yellow", "green")
pie(srt.blType, col=slices, radius=1, main="원그래프")

# 프로그램 2-2
# 프로그램 2-3
# 원그래프 그리기
require(grDevices)
pie.vote <- c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie.vote) <- c("새누리 152명", "선진 5명", "무 3명", "진보 13명", "민주 127명")
par(mfrow=c(1,2))
pie(pie.vote, col=c("red3", "blue", "green3", "magenta", "yellow"),
    main="19대 국회의원 선거")
par(new=TRUE)
pie(c(152, 127, 13, 5, 3), radius = 0.5, col="white", label=NA, border=NA)
text(0,0,"총 300석")

# 프로그램 2-4
# 자료입력
blType = c("A", "B", "B", "A", "A", "O", "A", "AB", "O", "O", 
        "O", "A", "A", "B", "AB", "A", "O", "B", "A", "B", 
        "B", "A", "B", "A", "B", "AB", "B", "A", "O", "AB", 
        "O", "B", "A", "B", "A", "O", "B", "A", "A", "A",
        "A", "O", "A", "O", "O", "B", "B", "O", "AB", "A",
        "B", "AB", "B", "O", "O", "O", "AB", "O", "O", "B", 
        "A", "A", "O", "A", "B", "O", "A", "O", "B", "O",
        "A", "B", "O", "AB", "B", "B", "A", "O", "B", "A",
        "B", "B", "O", "AB", "B", "A", "AB", "A", "B", "A",
        "A", "O", "O", "A", "A", "O", "AB", "A", "A", "O")

blType

srt.blType = sort(table(blType), decreasing=T)
srt.blType

slices = c("red", "blue", "yellow", "green")

# 막대그래프 그리기
barplot(srt.blType, col=slices, main="혈액형별 막대그래프")

# 프로그램 2-5
# 막대그래프 그리기
require(grDevices)
pie.vote <- c(0.5067, 0.0167, 0.0100, 0.0433, 0.4233)
names(pie.vote) <- c("새누리 152명", "선진 5명", "무 3명", "진보 13명", "민주 127명")

barplot(pie.vote, col=c("red3", "blue", "green", "magenta", "yellow"), main="정당별 막대그래프")

# 프로그램 2-6 : class1 = seq(30, 150, by=10) 추가
# 외부 파일을 읽어 데이터 프레임 만들기
Gallbladder = read.csv("C:/Users/user/Documents/GitHub/data_viz/data/ch2/health.csv", header=T)
str(Gallbladder)

# 담즙과포화비율 - 자료의 크기
n = length(Gallbladder$GallbladderRatio)
n

# 담즙과포화비율 - 자료의 정렬
sort(Gallbladder$GallbladderRatio)
sort(Gallbladder$GallbladderRatio, decreasing=T)

class1 = seq(30, 150, by=10)
# 담즙과포화비율 히스토그램과 개별 자료
par(mfrow=c(2,1))
hist(Gallbladder$GallbladderRatio, breaks=class1, main=NULL)
rug(jitter(Gallbladder$GallbladderRatio))

hist(Gallbladder$GallbladderRatio, breaks=class1, right=F, main=NULL)
rug(jitter(Gallbladder$GallbladderRatio))

# 프로그램 2-7
class1 = seq(30, 150, by=10)

# 담즙과포화비율 히스토그램과 개별자료
par(mfrow=c(2,1))
hist(Gallbladder$GallbladderRatio, breaks=class1, main=NULL)
rug(jitter(Gallbladder$GallbladderRatio))
hist(Gallbladder$GallbladderRatio, breaks=class1, right=F, main=NULL)
rug(jitter(Gallbladder$GallbladderRatio))

# 상대도수밀도히스토그램, 커널밀도추정량과 개별 자료
m = matrix(c(1,3,2,3), ncol=2, byrow=T)
layout(mat=m)
hist(Gallbladder$GallbladderRatio, prob=T, breaks=class1, right=F, ylab="상대도수", main=NULL)
lines(density(Gallbladder$GallbladderRatio, bw=5), col="red")
rug(Gallbladder$GallbladderRatio, col="blue")

# 프로그램 2-8
require(stats)
data("faithful")

eruption.length <- faithful$eruptions

# 원자료의 크기
n = length(eruption.length)
n

# 원자료의 범위
range(eruption.length)

# 원자료에 대한 정렬
sort(eruption.length)

# 원자료에 대한 평균과 분산
mean(eruption.length)
var(eruption.length)

class1 = seq(1.6, 5.1, by=0.5)
class2 = seq(1.85, 4.85, by=0.5)

# 계급의 폭을 0.5로 하고 제1계급의 하한값(원점이라고도 함)을
# 1.6으로 하는 도수분포표
cat.class1 = cut(eruption.length, breaks=class1)
t1 = table(cat.class1)
t1

# 계급의 폭을 0.5로 하고 제1계급의 하한값(원점이라고도 함)을
# 1.85로 하는 도수분포표
cat.class2 = cut(eruption.length, breaks=class2)
t2 = table(cat.class2)
t2

# class1 도수분포표를 이용하여 구하는 평균과 분산
m1 = (class1 + 0.25)[-length(class1)]
f1 = as.vector(t1)

mean1 = sum(m1*f1)/sum(f1)
mean1

var1 = sum((m1 - mean1)^2*f1)/sum(f1)
var1

# class2 도수분포표를 이용하여 구하는 평균과 분산
m2 = (class2 + 0.25)[-length(class2)]
f2 = as.vector(t2)

mean2 = sum(m2*f2) / sum(f2)
mean2

var2 = sum((m2 - mean2)^2 * f2) / sum(f2)
var2

# 히스토그램
class1 = class1
class2 = c(1.35, class2, 5.35)
par(mfrow=c(1,2))
hist(eruption.length, breaks=class1, main="간헐온천 지속시간에 대한 히스토그램 \n origin : 1.6",
     xlab="간헐온천 지속시간")
hist(eruption.length, breaks=class2, main="간헐온천 지속시간에 대한 히스토그램 \n origin : 1.85", 
     xlab="간헐온천 지속시간")

# 프로그램 2-9
# 각각 제1계급의 하한값을 1.6과 1.85로 하는 히스토그램에서 계급의 폭 변동하기
hist.func2 = function(n) {
  par(mfrow=c(n,2))
  for(i in 1:n) {
    class1 = seq(1.1, 5.1, by=0.5/(2*i -1))
    class2 = seq(1.35, 5.35, by=0.5/(2*i -1))
    hist(eruption.length, breaks=class1, prob=T, main="origin:1.6", xlab=NULL)
    hist(eruption.length, breaks=class2, prob=T, main="origin:1.85", xlab=NULL)
  }
}

hist.func2(3)

# 도수분포표(계급의 폭 : 0.1, 0.2, 0.3, 0.4, 0.5)
w = c(0.1, 0.2, 0.3, 0.4, 0.5)
for(i in 1:5) {
  class1 = seq(1.1, 5.5, by=w[i])
  cat.class1 = cut(eruption.length, breaks=class1)
  table(cat.class1)
  cat("계급의 폭 = ", w[i], "\n")
  print(table(cat.class1))
}

# 프로그램 2-10
# 임의의 수 생성
runif(n, min, max)

# 균일분포로부터 난수생성 자료의 분포 히스토그램
x <- runif(400, -1, 1)
hist(x)

# 프로그램 2-11
# 정규분포로부터의 임의의 수 생성