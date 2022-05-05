######################
# Lesson01_R_Intro.R

#####################
# Lesson01_01. R 
#####################


dim(available.packages())

#####################
# Lesson01_02. R 기본
#####################

# Workspace

setwd("d:/r_workspace")
getwd()
dir()


#####################
# R Packages
#####################

pkgList <- installed.packages()

library()

install.packages("ggplot2")
#library(ggplot2)

# install.packages("ggplot2", depend=T)

update.packages()

#####################
# R Datatype
#####################

a <- 1
class(a)
mode(a)

?class
?mode

a <- 1L
class(a)


a <- 'a'
class(a)

a <- "a"
class(a)

a <- 1+1i
class(a)

a <- TRUE
class(a)

a <- F
class(a)

a <- as.raw(0xa3) 
a
a <- as.raw(40)
a

a <- 1
a <- as.integer(1)
class(a)

a <- 1L
typeof(a)
class(a)
mode(a)

is.integer(a)
is.numeric(a)

## 생각해보세요
### mode(), class(), typeof()의 차이에 대해 이야기 

#####################
# R Data structures
#####################

## vector

x <- c(1:20)
x
## matrix

m <- matrix(nrow = 2, ncol=3)
m

dim(m)
attributes(m)

m <- matrix(1:6, nrow=2, ncol=3)
m


?matrix

# column-wise

m <- matrix(1:6, nrow=2, ncol=3, byrow=F)
m

# row-wise

m <- matrix(1:6, 2, 3, T)
m

m <- 1:10
m

dim(m) <- c(2, 5)
m

m[1,] #1행 데이터
m[,2] #2열 데이터

rownames(m) <- c("신입", "임원")
m

# cbind (column-binding)

x <- 1:3
y <- 10:12
xy <- cbind (x, y)
class(xy)
xy<-cbind (x, y, x+y)
class(xy)

# rbind (row-binding)

rbind(x, y)
rbind(x, y, x+y)

## array

# 
x <- array(1:20, dim=c(2,2,5))
x
x[,,5]
x[1,,]

# 과제 
## vector, matrix, array 데이터 구조에 대해 설명하라


## list

#
mylist <- list(id=1001, name="song", scores=c(89, 90, 95, 99))
mylist

mylist$scores  #mylist[[3]]
mylist$scores[3] #mylist[[3]][3]

mylist[[3]]
mylist[[1]]
## data.frame

kids <- c("홍길동", "이순신", "김동아")
ages <- c(7, 5, 10)
temp <- c(1000, 2500, 3500)
children <- data.frame(kids, ages, temp, stringsAsFactors=FALSE)
children <- data.frame(kids, ages)
children 

mode(children)
class(children)
attributes(children)
class(children$kids)

summary(children)
mean(children$temp)

children[-1, c(2,3)]
#subset 데이터의 일부분만 발췌

c1 <- subset(children, ages<= 7)
c1

c1 <- subset(children, ages<= 7, select=c(kids, temp))
c1

## factor

ft <- c("f", "f", "m", "m", "f")
ft

ft <- factor(ft)
ft

table(ft)

## vertorization

x <- c(1, 3, 5, 7, 9)
x
sqrt(x)

y <- c(2, 4, 6, 8, 10)
y

x+y

x+5

x*10


# recycling rule

x<-c(1, 2, 3, 4)
x

z <- c(2, 4)
z

x+z

z <- c(2, 4, 6)
z

x+z

## indexing

x <- c(1, 3, 5, 15, 3, 2)
x

as.vector(x)
x

x[1]

x[2]

x[c(1,3,5)]

x[1:3]

x[c(1,3,5)]

x[-1]

x[x>3]

x[x>3 & x<10]

x[x<=2 | x>10]

# intexing in Matrix 

x <- matrix(1:15, 3, 5)
x
x[1,3]

x[1,]
x[-2,1]
x[1:2, ]
x[c(1,3),]
x[x[,2]>5,]

## NA, NaN, NULL 

x <- c(1, 2, NA, 10, 3)
is.na(x)
length(x)

x <- c(1, 2, NULL, NA, 4)
is.na(x)
is.nan(x)
length(x)

# NA vs NULL

x <- NULL
length(x)

y <- NA
length(y)

x <- c(88, 12, NA, 168, 13)
x

mean(x)
mean(x, na.rm=T)

x <- c(88, NULL, 12, 168, 13)
x

mean(x)

## File input/output
install.packages("xlsx")
library(xlsx)
file <- choose.files() #file open
popData <- read.xlsx(file, 1, encoding = "UTF-8")
head(popData)
class(popData)
str(popData) #데이터 구조확인


# 과제1
# area, sido, 2018년인구, 2010년 인구, 인구성장률을 계산 
# area는 factor로 변환하고, 1은 수도권, 2는 동남권 ... 라벨링을 함

# area, sido, pop10, pop18 컬럼만 선택함 
# 세종특별자치도의 row는 삭제
# 인구성장률을 계산함
# 해당 데이터셑의 이름을 popData.sido로 지정함

#과제2
#권역별 pop10과 pop18의 합을 계산함
#sqldf패키지 사용
#권역별 인구성장률 계산하고, 데이터셑 이름은 opData.area

# 데이터셑, R프로그램(반드시 주석달고), 인구성장과 관련된 데이터를 고민
## workspace 
getwd()
dir()

popData.area
popData.area1