exam <- read.table("https://raw.githubusercontent.com/data-better/data_viz/master/data/ch3/exam_scores_2012.txt", header=T)
str(exam)
attach(exam)

windows(height=5.5, width=5)
plot(exam$mid, exam$final, pch=20, xlim=c(-5, 40), ylim=c(-5, 40), col="blue", xlab="중간시험", ylab="기말시험", main="통계적 사고")
abline(lm(exam$final~exam$mid), col="red")
diff <- mean(exam$final, na.rm=T)-mean(exam$mid, na.rm=T)
abline(c(diff, 1), lty="dotted")
