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
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
######파일 불러오기#############
file <- choose.files() #file open
pop_total<- read.csv(file,header=T) #######2021.08_지역별 인구 통계
family_type<- read.csv(file,header=T) #######2021.08_가구 유형별 인구 통계
real_estate_price<- read.csv(file,header=T) #######2021.08_부동산 지역, 거래 종류별 거래 건수 데이터(매매)
real_estate_rental<- read.csv(file,header=T) #######2021.08_부동산 평형, 종류별 거래 건수 데이터(임대)

################데이터 탐색######################
d<-dim(family_type)
smmy<-summary(family_type)
d
smmy
########구별 인구수 순위##############
sum.pop <- pop_total  %>%
  group_by(시군구명) %>%
  summarise(s.pop = sum(인구수))
########구별 인구수 barchart#########
asc.pop<-sum.pop$s.pop
names(asc.pop)<-sum.pop$시군구명
barplot(sort(asc.pop, decreasing = T), main=paste("2021년 부산시 구별 인구 수 현황"),
        ylab="인구수",ylim=c(40000,400000), col=brewer.pal(16,"Set3"), 
        cex.names=0.6)
abline(h=seq(50000,350000,50000),lty=3) #

##############구별 제곱미터 당 매매가 순위############
address_test<-strsplit(real_estate_price$행정동주소," ") ######행정동주소를 " " 를 기준으로 문자 나누기
m<-matrix(unlist(address_test),ncol=3,byrow=T) 
시군구명<-m[,2]
real_estate_price<- cbind(real_estate_price,시군구명) ######새로운 열 생성



real_sell_price <- real_estate_price  %>%
  group_by(시군구명) %>%
  summarise(sell_price = round(sum(실거래평균금액)/sum(건물면접값)))
  
asc.sell<-real_sell_price$sell_price
names(asc.sell)<-real_sell_price$시군구명
barplot(sort(asc.sell, decreasing = T), main=paste("2021년 부산시 구별 제곱미터당 매매가격 현황"),
        ylab="제곱미터 당 가격",ylim=c(100,750), col=brewer.pal(16,"Set3"), 
        cex.names=0.6)
abline(h=seq(200,700,100),lty=3)



##############구별 제곱미터 당 임대가 순위############
address_test<-strsplit(real_estate_rental$행정동주소," ") ######행정동주소를 " " 를 기준으로 문자 나누기
m<-matrix(unlist(address_test),ncol=3,byrow=T) 
시군구명<-m[,2]
real_estate_rental<- cbind(real_estate_rental,시군구명)######새로운 시군구명 열 생성


임대금추정액<-(real_estate_rental$월세평균금액*100)+real_estate_rental$보증금평균금액 ####월세 1만원 = 보증금 100만원 전제로 계산
real_estate_rental<- cbind(real_estate_rental,임대금추정액) ####df에 열 추가

real_rent_price <- real_estate_rental  %>%
  group_by(시군구명) %>%
  summarise(rent_price = round(sum(임대금추정액)/sum(건물면적값)))  #####구별 그룹바이 후 제곱미터 당 임대가격 계산


asc.sell<-real_rent_price$rent_price
names(asc.sell)<-real_rent_price$시군구명
barplot(sort(asc.sell, decreasing = T), main=paste("2021년 부산시 구별 제곱미터당 임대가격 현황"),
        ylab="제곱미터 당 가격",ylim=c(100,300), col=brewer.pal(16,"Set3"), 
        cex.names=0.6)
abline(h=seq(100,300,50),lty=3)

##############################구별 가족유 비율###############################
fmty <- family_type  %>%
  group_by(시군구명, 가구유형구분값) %>%
  summarise(f.sum = sum(인구수)) %>%
  group_by(시군구명) %>%
  summarise(f.rate = round(f.sum/sum(f.sum),2),가구유형구분값, f.sum, 구별총인구=sum(f.sum)) #구별가구유형별인구수


fmty %>% 
  ggplot(aes(x =시군구명 , y = f.rate, fill =가구유형구분값)) +
  geom_bar(stat='identity', col='black')+
  scale_fill_brewer(palette = 'Pastel1')+
  geom_col() +
  geom_text(aes(label = scales::percent(f.rate)),
            position = position_stack(vjust = 0.5))+
  labs(title = "구별 가족유형 비율", x="시군구", y="가족유형 비율")+
  coord_flip() #####100% 누적막대그래프


fmty %>% 
  ggplot(aes(x = reorder(시군구명,구별총인구), y = 구별총인구, fill = 가구유형구분값)) +
  geom_col() +
  geom_text(aes(label = scales::percent(f.rate)),
            position = position_stack(vjust = 0.5))+
  coord_flip() #####구별총인구수대비누적막대그래프
###############매매대비임대수익#########################################

inv <- left_join(real_rent_price,real_sell_price, by="시군구명" )
매매대비임대수익율<-round(inv$rent_price/inv$sell_price*100,2)
inv<- cbind(inv,매매대비임대수익율)

asc.sell<-inv$매매대비임대수익율
names(asc.sell)<-inv$시군구명
barplot(sort(asc.sell, decreasing = T), main=paste("2021년 부산시 구별 제곱미터당 매매대비임대수익율"),
        ylab="제곱미터 당 수익",ylim=c(0,100), col=brewer.pal(16,"Set3"), 
          cex.names=0.8)
abline(h=seq(0,100,10),lty=3)
  
  
  
########################산점행렬도#############################################
join <- left_join(fmty,real_sell_price, by="시군구명" )
join1 <- left_join(join,real_rent_price, by="시군구명" )
r<-join1  %>% filter(가구유형구분값=='1인가구')
select(join1 ,-시군구명, -가구유형구분값)

pairs(r[c('f.rate','sell_price','rent_price')],gap=5)
####################유의확률###########################
cor.test(r$f.rate, r$sell_price)
cor.test(r$f.rate, r$rent_price)
cor.test(join1$f.sum, join1$sell_price)
cor.test(join1$f.sum, join1$rent_price)
