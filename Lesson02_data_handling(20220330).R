#install.packages("xlsx")
#library(xlsx)
file <- choose.files() #file open
#popData <- read.xlsx(file, 1, encoding = "UTF-8")
pop <- read.csv(file,header=T)
class(pop)

pop$area <- factor(pop$area, levels= c(1:7), labels = c("수도권",
                   "동남권", "대경권", "충청권", "전라권", "강원권", 
                  "제주권"))
str(pop)
###########################################
# query 문을 지원함 ) sqldf
##########################################
#install.packages("sqldf")
library(sqldf)
data(iris)	#내장데이터 불러오기
iris.sql = sqldf("select distinct from iris")
iris.sql
sqldf("select * from iris where Species='virginica'")
sqldf('select Species, sum("Sepal.Length") as SepalLength from iris group by Species')
sqldf('select Species, avg("Petal.Length") as avg, stdev("Petal.Length") as sd from iris group by Species')

### sqldf ####
sqldf("select distinct Species from iris")

### 인구데이터 활용 ####
pop.area <- sqldf("select area, sum(pop10) as area10, 
                  sum(pop18) as area18 from pop group by area")
tot10 <- sum(pop.area$area10)
tot18 <- sum(pop.area$area18)
area10.p <- pop.area$area10/tot10 *100
area10.p <- pop.area$area18/ tot18*100
popData <- cbind(pop.area,area10.p,area18.p )
##################################
## apply(), lapply(), sapply(), tapply(), by()
##################################


## apply()

# Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
x[[1]]



row.sums <- apply(x, 1, sum)
row.sums
col.sums <- apply(x, 2, sum)
col.sums

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

# Sort the columns of a matrix
apply(x, 2, sort)


## lapply(), sapply()

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x

# compute the list mean for each list element
lapply(x, mean)

# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)

sapply(x, class)

i39 <- sapply(3:9, seq)    # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## tapply()

ages <- c(25, 26, 55, 37, 21, 42)
affils <- c("R", "D", "D", "R", "U", "D")
tapply(ages, affils, mean)

d <- data.frame(
  gender = c("M", "M", "F", "F", "M", "F"),
  age = c(23, 26, 25, 37, 55, 22),
  income = c(150, 300, 250, 350, 800, 120))
d

d$over25 <- ifelse(d$age > 25, 1, 0)
d

tapply(d$income, list(d$gender, d$over25), mean)
split(d$income, list(d$gender, d$over25))

# by()

head(warpbreaks)
levels(warpbreaks$tension)

by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1],       summary)

###########################################
# plyr  페키지 사용  split->apply->combine
###########################################
# dd? 
#install.packages("plyr")
library(plyr) #메모리를 사용함
data("baseball") #데이터 불러오기
class(baseball)
dim(baseball)
result = ddply(baseball, .(id), summarise, avg_g=mean(g, na.rm=T))
head(result, 3)
result1 = ddply(baseball, .(id), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
head(result1, 3)
result2 = ddply(baseball, .(id, team), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
# 기준이 여러개 있을 경우 .(id, team, ....), 요약
head(result2)
result2 = ddply(baseball, .(id), transform, minYear = min(year,na.rm=T )) #tranform은 요약이 아니라 뒤에 추가
result2[1, c(1,23)] 
dd<-ddply( baseball , .( id ) , subset , g == max( g ) )
dd
#id 기준으로 g의 최대값 구하기

### 인구데이터를 활용하여 권역별 인구성장과 경제성장률
pop.area <- ddply(pop, .(area), summarise, pop.area10=sum(pop10, na.rm=T),
                  pop.area18=sum(pop18, na.rm=T), 
                  grdp.area10 = sum(grdp10, na.rm=T),
                  grdp.area18 = sum(grdp18, na.rm=T))
pop.p10 <- with(pop.area, round(pop.area10/sum(pop.area10)*100, 1))
pop.p18 <- with(pop.area, round(pop.area18/sum(pop.area18)*100, 1))
grdp.p10 <- with(pop.area, round(grdp.area10/sum(grdp.area10)*100, 1))
grdp.p18 <- with(pop.area, round(grdp.area18/sum(grdp.area18)*100, 1))

lbl10 <- paste(pop.area$area, "(", pop.p10, "%)")
lbl18 <- paste(pop.area$area, "(", pop.p18, "%)")
g.lbl10 <- paste(pop.area$area, "(", grdp.p10, "%)")
g.lbl18 <- paste(pop.area$area, "(", grdp.p18, "%)")
par(mfrow=c(2,2))
with(pop.area, pie(pop.area10, labels=lbl10, 
                   col= rainbow(length(pop.area)),
                   main = "2010년 권역별 인구비율"))
with(pop.area, pie(pop.area18, labels=lbl18, 
                   col= rainbow(length(pop.area)),
                   main = "2018년 권역별 인구비율"))
with(pop.area, pie(grdp.p10, labels=g.lbl10, 
                   col= rainbow(length(pop.area)),
                   main = "2010년 권역별 grdp비율"))
with(pop.area, pie(grdp.p18, labels=g.lbl18, 
                   col= rainbow(length(pop.area)),
                   main = "2018년 권역별 grdp비율"))
###########################################
# 자료의 형태를 바꿈 / reshape2(melt, cast)
#############################################

library( reshape2 )
data(smiths)
smiths
m = melt( id =1:2 , smiths )	#데이터형태 변경하기 id=1:2 기준(1,2열)
x = dcast(m , subject + time~... ) #원래대로 전환


##################################################
#data.table (속도를 높여줌)
##################################################
library(data.table)
DF = data.frame( x = runif(520000) , 
                 y = rep(LETTERS , each =20000) )
head(DF)
system.time(x<- DF[ DF$y == "C" , ])

DT = as.data.table( DF )
setkey( DT , y )
system.time( x <- DT[ J( "C" ) , ])