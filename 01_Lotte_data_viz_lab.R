### Lotte Data #####
### Loading and installing packages ###
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(dplyr)){ #
  install.packages("dplyr")
  library(dplyr)
}
if(!require(lubridate)){ #날짜 데이터를 변환
  install.packages("lubridate")
  library(lubridate)
}
if(!require(ggplot)){
  install.packages("ggplot")
  library(ggplot)
}
### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

### 테이블 조인
tb <- sqldf("select a.id, a.성별, a.연령, b.거래일자,
            b.상품대분류명, b.상품중분류명, b.구매건수,
            b.거래식별ID, b.구매금액, b.점포ID
            from customer as a, purchaseList as b
            where a.id = b.id")
head(tb, 10)


## Create date field ##
####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자)
tb$거래월 <- month(tb$거래일자)



### Data exploration
sub1 <- tb %>% 
        group_by(점포ID, 거래월, 상품대분류명) %>%
        summarise(amount=round(sum(구매금액/10000),0), cnt=sum(구매건수))


########sub1 박스플랏#######
par(mfrow=c(1,2))
with(sub1, boxplot(amount~점포ID)) #점포ID 별
with(sub1, boxplot(amount~거래월)) #거래월 별
sub1 <- subset(sub1, 거래월!=10)
with(sub1, boxplot(cnt~점포ID))
with(sub1, boxplot(cnt~거래월))
#save(sub1, file="data/lotte.rda")
##9주차
rm(list=ls())
load(file="data/s1.rda")
load(file="data/td.rda")
g<- ggplot(s1,aes(점포ID,amount,fill=점포ID)) +
  geom_boxplot()+
  stat_summary(fun.y = "mean", 
               geom = "point",shape = 23, size=3,fill="blue")+
  stat_boxplot(geom="errorbar")+
  scale_fill_brewer(palette="Set3")+
  labs(title = "롯데데이터 Box Plot", subtitle="매출액&점포명", x="점포명",
       y="매출액",caption="소스 : 롯데데이터, redPoint = 평균")
g
g <- ggplot(s2, aes(상품대분류명, amount)) + 
  geom_point(aes(col=점포ID, size=cnt))+
  labs(title = "Bubble Chart", x="품목", y="매출액")

s2 <- tb %>% 
  group_by(점포ID, 상품대분류명) %>%
  summarise(amount=round(sum(구매금액/10000),0), cnt=sum(구매건수))
