#barplot 이용하기
if(!require("xlsx")) {
  install.packages("xlsx") 
  require(xlsx)
}
if(!require("reshape2")) {
  install.packages("reshape2") 
  require(reshape2)
}
if(!require("dplyr")) {
  install.packages("dplyr") 
  require(dplyr)
}
if(!require("RColorBrewer")) {
  install.packages("RColorBrewer") 
  require(RColorBrewer)
}
#버지니어 사망률 그래프
graphics.off()
VADeaths
# 누적막대그래프
barplot(VADeaths, col = heat.colors(5),
        border = "dark blue", legend = rownames(VADeaths))
title(main = list("버지니아주 사망율", font = 2))


rm(list=ls()) #dataset 지우기
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_321') #엑셀파일을 읽기 위해
file=file.choose()
data10 = read.xlsx(file, sheetIndex = 1, header=T, encoding = "UTF-8")
data20 = read.xlsx(file, sheetIndex = 2, header=T, encoding = "UTF-8")
###########java 실행이 안됨
file1 <- choose.files()
data10 <- read.csv(file1,header=T)
data20 <- read.csv(file1,header=T)
 # reshape2에서 제공하는 변환은 크게 melt(wide to long), cast(long to aggregate)
d10 <- melt(id=1, data10)

# 2020년 데이터를 구별로 long하에 데이터 변환
d20 <- melt(id=1, data20)

sum.d10 <- d10  %>%
  group_by(sido) %>%
  summarise(count10 = sum(value))
barplot(sum.d10$count10, main=paste("2010년 부산시 주요구별 과목별 병원현황"),
        ylab="병원수",ylim=c(0,400), col=brewer.pal(16,"Set3"), 
        names=sum.d10$sido, cex.names=0.6)
abline(h=seq(0,400,50),lty=3) #

#정렬해서 보여줌(직관적)
data<-sum.d10$count10
names(data)<-sum.d10$sido
sum.d20 <- d20  %>%
  group_by(sido) %>%
  summarise(count20 = sum(value))barplot(sort(data, decreasing = T), main=paste("부산시 주요구별 과목별 병원현황"),
        ylab="병원수",ylim=c(0,400), col=cm.colors(16), 
        cex.names=0.6)
abline(h=seq(0,400,50),lty=3, lwd=0.1) #

#2010년과 2020년 병원현황 비교
#2020년 데이터 


data.t <- data.frame(sum.d20$sido, sum.d10$count10, sum.d20$count20)
with(data.t, barplot(cbind(sum.d10.count10,sum.d20.count20)~sum.d20.sido, main=paste("2010과 2020년 부산시 주요구별 과목별 병원현황"),
        beside=T, ylab="병원수",xlab="구별", ylim=c(0,450), col=c("red", "blue"), 
        names=sum.d20.sido, cex.names=0.6))

graphics.off()

#모자이크 그림
#데이터 변환과정입니다.
dd10<-subset(d10, variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' )
c <- dd10$value
dim(c) <- c(16,14)
rownames(c) <- dd10$sido[1:16] #행이름 부여
colnames(c) <- unique(dd10$variable) #열 이름 부여
c
mosaicplot(c, color=rainbow(14), main="부산 지역별 진료과목 현황")
c1<-t(c)
par(mfrow=c(1, 2))
mosaicplot(c1, col=brewer.pal(14,"Set3"), cex=1, main="2010년 부산 지역별 진료과목 현황")

#2020년 모자이크
dd20<-subset(d20, variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' & variable !='직업환경의학과'& variable !='예방의학과')
d <- dd20$value
dim(d) <- c(16,14)
rownames(d) <- dd20$sido[1:16] #행이름 부여
colnames(d) <- unique(dd20$variable) #열 이름 부여
d
mosaicplot(d, color=rainbow(14), main="부산 지역별 진료과목 현황")
d1<-t(d)
mosaicplot(d1, col=brewer.pal(14,"Set3"), cex=1, main="2020년 부산 지역별 진료과목 현황")
