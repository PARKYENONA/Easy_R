#3장 
#3-1 변수 이해하기
#변수만들기
a<-1 # a에 1 할당 =도 가능하지만 변수 생성시에는 <- 사용 권장
a  # a 출력

b<-2 
b

c<-3
c

d<-3.5
d

a+b

a+b+c

4/b

5*b

#여러 값으로 구성된 변수 만들기
var1 <- c(1,2,5,7,8) # 숫자 5개로 구성된 var1
var1

var2 <- c(1:5) # 1~5까지 연속 값으로 var2 생성
var2

var3 <- seq(1,5) # 1~5까지 연속 값으로 var3 생성
var3

var4 <- seq(1,10,by=2) # 1~10까지 2 간격 연속 값으로 var4 생성
var4

var5<- seq(1,10,by=3) # 1~10까지 3 간격 연속 값으로 var5 생성
var5

#문자로 된 변수 만들기 
str1<- "a"
str1

str2<- "text"
str2

str3 <- "Hello World!"
str3

str4 <- c("a","b","c")
str4

str5 <- c("Hello!","World","is","good!")
str5

str1+2 # 문자로 된 변수로는 연산 불가능

#3-2 함수 이해하기 
#숫자를 다루는 함수 이용하기 
#변수 만들기
x <- c(1,2,3)
x

#함수 적용하기
mean(x)
max(x)
min(x)

#문자를 다루는 함수 이용하기
str5
paste(str5, collapse =",") # 쉼표를 구분자로 str5의 단어들 하나로 합치기
paste(str5, collapse =" ") # collapse 같은 명령어를 파라미터 or 매개변수 옵션조정 

x_mean <- mean(x)
x_mean

str5_paste <- paste(str5, collapse = " ")
str5_paste

#3-3 패키지 이해하기
#ggplot2 패키지 설치하기
install.packages("ggplot2")
#ggplot2 패키지 로드
library(ggplot2)

#여러 문자로 구성된 변수 생성
x1<- c("a","a","b","c")
#빈도 막대 그래프 출력
qplot(x1)

#ggplot2의 mpg 데이터로 그래프 만들기

#data에 mpg, x축에 hwy 변수 지정해 그래프 생성
qplot(data = mpg, x = hwy)

# x축 cty
qplot(data = mpg , x = cty)
# x축 drv, y축 hwy
qplot(data = mpg , x = drv , y = hwy)
# x축 drv, y축 hwy, 선그래프 형태 
qplot(data = mpg , x = drv , y = hwy, geom = "line")

# x축 drv, y축 hwy, 상자그림 형태 
qplot(data = mpg , x = drv , y = hwy, geom = "boxplot")

# x축 drv, y축 hwy, 상자그림 형태 , drv별 색 표현 
qplot(data = mpg , x = drv , y = hwy, geom = "boxplot", colour = drv)

# qplot 함수 매뉴얼 출력
?qplot 

#혼자서 해보기
score <- c(80, 60, 70, 50, 90)
score
s_mean <- mean(score)
s_mean
