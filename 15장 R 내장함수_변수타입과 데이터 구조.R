# 15장 R 내장 함수, 변수 타입과 데이터 구조
# 15-1 R 내장 함수로 데이터 추출하기

exam <- read.csv("csv_exam.csv")
exam[]
exam[1,] # 1행 추출
exam[2,] # 2행 추출

# 조건을 충족하는 행 추출하기
exam[exam$class == 1,]   # class가 1인 행 추출
exam[exam$math >= 80,]   # 수학 점수가 80점 이상인 행 추출

# 1반 이면서 수학 점수가 50점 이상
exam[exam$class == 1 & exam$math >= 50,]

# 영어 점수가 90점 미만이거나 과학 점수가 50점 미만
exam[exam$english < 90 | exam$science < 50,] 

# 열 번호로 변수 추출하기
exam[,1]  # 첫 번째 열 추출
exam[,2]  # 두 번째 열 추출
exam[,3]  # 세 번째 열 추출

# 변수명으로 변수 추출하기

exam[, "class"]  # class 변수 추출
exam[, "math"]   # math 변수 추출

exam[, c("class", "math", "english")]  # class, math, english 변수 추출

# 행, 변수 동시 추출하기

# 행, 변수 모두 인덱스
exam[1,3]

# 행 인덱스, 열 변수명
exam[5, "english"]

# 행 부등호 조건, 열 변수명
exam[exam$math >= 50, c("english", "science")]


# dplyr과 내장함수의 차이

# 내장함수코드
exam$tot <- (exam$math + exam$english + exam$science) / 3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean)

# dplyr 코드
library(dplyr)
exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math+ english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean = mean(tot))

# 혼자서 해보기
# Q1

mpg <- as.data.frame(ggplot2::mpg)  
mpg$tot <- (mpg$cty + mpg$hwy) / 2
aggregate(data = mpg[mpg$class == "suv" | mpg$class == "compact",], tot~class, mean)

# 15-2 변수타입
# 변수 타입 간 차이 알아보기

var1 <- c(1,2,3,1,2)              # numeric 변수생성
var2 <- factor(c(1, 2, 3, 1, 2))  # factor 변수생성 범주의 의미를 포함
var1
var2

var1 + 2 # numeric 변수로 연산
var2 + 2 # factor 변수로 연산

# 변수 타입 확인
class(var1)
class(var2)

levels(var1)
levels(var2)

var3 <- c("a", "b", "b", "c")          # 문자 변수 생성
var4 <- factor(c("a", "b", "b", "c"))  # 문자로 된 factor 변수 생성

var3
var4

class(var3)
class(var4)
levels(var4)

mean(var1)
mean(var2)

# 변수타입 바꾸기

var2 <- as.numeric(var2)  # numeric 타입으로 변환
mean(var2)

class(var2)
levels(var2)

# 혼자서 해보기
# Q1
class(mpg$drv)
# Q2
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)
# Q3
levels(mpg$drv)

# 15-3 데이터 구조
# 벡터 만들기
a <- 1
a

b <- "hello"
b

# 데이터 구조 확인
class(a)
class(b)

# 데이터 프레임 만들기
x1 <- data.frame(var1 = c(1, 2, 3),
                 var2 = c("a", "b", "c"))
x1

# 데이터 구조 확인
class(x1)

# 매트릭스 만들기 - 1~12로 2열
x2 <- matrix(c(1:12), ncol = 2)
x2

# 데이터 구조 확인
class(x2)

# array aksemfrl - 1 ~ 20행 2행 X 5열 X 2차원
x3 <- array(1:20, dim = c(2, 5, 2))
x3

# 데이터 구조 확인
class(x3)

# 리스트 생성 - 앞에서 생성한 데이터 구조 활용
x4 <- list(f1 = a,   # 벡터
           f2 = x1,  # 데이터 프레임
           f3 = x2,  # 매트릭스
           f4 = x3)  # 어레이

x4

# 데이터 구조 확인
class(x4)

mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)
x

x$stats[,1]        # 요약 통계량 추출
x$stats[,1][3]     # 중앙값 추출
x$stats[,1][2]     # 1분위수 추출
