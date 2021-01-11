# 5장 데이터 분석 기초
# 5-1 데이터 파악하기

#exam 데이터 파악하기
exam <- read.csv("csv_exam.csv")

head(exam) # 데이터 앞부분 확인하기 앞에서부터 6행까지 출력
head(exam, 10) # 앞에서부터 10행까지 출력 

tail(exam) # 뒤에서부터 6행까지 출력
tail(exam, 10) # 뒤에서부터 10행까지 출력 

#데이터 뷰어 창에서 exam 데이터 확인
View(exam)

dim(exam) # 행, 열 출력

str(exam) # 데이터 속성 파악

summary(exam) # 요약통계량 출력

# mpg 데이터 파악하기

install.packages("ggplot2")
library("ggplot2")

# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg) # as.data.frame() 데이터 속성을 데이터 프레임 형태로 변환
                                   # :: 특정 패키지에 들어있는 함수나 데이터 지정 
head(mpg) # 데이터 앞부분 확인
tail(mpg) # 데이터 뒷부분 확인

dim(mpg) # 행, 열 출력
str(mpg) # 데이터 속성 확인

# 패키지에 들어있는 데이터 설명글 출력 -> ?mpg 

summary(mpg) # 요약통계량 출력

# 5-2 변수명 바꾸기 

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw

install.packages("dplyr") # dplyr 설치
library(dplyr) # dplyr 로드

df_new <- df_raw # 변수명 바꾸기 전 복사본 이용하는 것이 좋음
df_new

df_new <- rename(df_new, v2 = var2) # var2를 v2로 수정 
df_new

df_raw
df_new

# 혼자서 해보기 
# Q1 mpg 복사본 만들기
df_new_mpg <- as.data.frame(ggplot2 :: mpg)
str(df_new_mpg)

# Q2 변수 변경
df_new_mpg <- rename(df_new_mpg, city = cty, highway = hwy)
head(df_new_mpg)
str(df_new_mpg)


# 5-3 파생변수 만들기

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2 # var_sum 파생변수 생성
df

df$var_mean <- (df$var1 + df$var2) / 2 # var_mean 파생변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy) / 2 # 통합 연비 변수 생성
head(mpg)
mean(mpg$total)

# 조건문을 활용해 파생변수 만들기
# 기준값 정하기
summary(mpg$total) # 요약통계량 산출
hist(mpg$total) # 히스토그램 생성

# 합격 판정 변수 만들기
# 20 이상이면 pass, 그렇지 않으면 fail 부여
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20) # 데이터 확인

# 빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test)

# 막대 그래프로 빈도 표현
qplot(mpg$test) # 연비 합격 빈도 막대 그래프 생성 

# 중첩 조건문 활용하기

# total을 기준으로 A, B, C 등급 부여
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))
head(mpg,20)

# 빈도표, 막대 그래프로 연비 등급 살펴보기
table(mpg$grade) # 등급 빈도표 생성

qplot(mpg$grade) # 등급 빈도 막대 그래프 생성


# 원하는 만큼 범주 만들기 
# A, B, C, D 등급 부여 
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))
table(mpg$grade2)
qplot(mpg$grade2)

#분석 도전

#Q1 
df_midwest <- as.data.frame(ggplot2::midwest)
head(df_midwest)
tail(df_midwest)
View(df_midwest)
dim(df_midwest)
str(df_midwest)
summary(df_midwest)

#Q2
library(dplyr)
df_new_midwest <- df_midwest
str(df_new_midwest)
df_new_midwest <- rename(df_new_midwest, total = poptotal, asian = popasian)
str(df_midwest)
str(df_new_midwest)

#Q3
df_new_midwest$percentasian <- (df_new_midwest$asian / df_new_midwest$total)*100
hist(df_new_midwest$percentasian)

#Q4

mean(df_new_midwest$percentasian)
df_new_midwest$countlevel <- ifelse(df_new_midwest$percentasian > 0.4872462, "large","small")

#Q5

library(ggplot2)
qplot(df_new_midwest$countlevel)

































