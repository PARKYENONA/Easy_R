#4장 데이터프레임

# 4-1 데이터프레임 이해하기
# 데이터프레임 = 행 + 열
# 열 = 속성 컬럼 or 변수 
# 행 = 사람의 정보 로우 or 케이스
# 데이터가 크다 => 열이 많은 게 중ㅇ
# ┖ 행이 많음 -> 컴퓨터 느려짐 -> 고사양 장비 구축
# ┖ 열이 많음 -> 분석 방법의 한계 -> 고급 분석 방법

#4-2 데이터프레임 만들기 - 시험 성적 데이터
#변수 만들기 
english <- c(90, 80, 60, 70)
english

math <- c(50, 60, 100, 20)
math

#english, math로 데이터 프레임 생성 df_midterm 할당
df_midterm <- data.frame(english, math)
df_midterm

#학생의 반정보 추가
class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

#분석하기

mean(df_midterm$english) # df_midterm의 english로 평균산술

mean(df_midterm$math) # df_midterm의 math로 평균산술

# 데이터 프레임 한 번에 만들기 

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm

#혼자서 해보기 

#Q1
df_product <- data.frame(제품 = c("사과", "딸기", "수박"),
                         가격 = c(1800, 1500, 3000),
                         판매량 = c(24, 38, 13))
df_product

#Q2
mean(df_product$가격)

mean(df_product$판매량)


# 4-3 외부데이터 이용하기 

install.packages("readxl")
library(readxl)

df_exam <- read_excel("excel_exam.xlsx") # 엑셀 파일을 불러와 df_exam 할당 
df_exam                                  # 출력

df_exam <- read_excel("D:/easy_r/excel_exam.xlsx") # 파일 경로 지정해 df_exam 할당

#분석하기

mean(df_exam$english)

mean(df_exam$science)

# 엑셀 파일 첫 번째 행이 변수명 X
df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar

# 엑셀 파일에 시트가 여러개 
df_exam_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet

# csv 파일 불러오기

df_csv_exam <- read.csv("csv_exam.csv") # header= F 변수명 없을 때 col_names = F 기능 O
df_csv_exam

# 문자가 들어있는 파일을 불러올 때 
df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F)
df_csv_exam

# 데이터 프레임을 csv 파일로 저장

df_midterm <- data.frame(englsh = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm 

# csv 파일로 저장
write.csv(df_midterm, file = "df_midterm.csv")

# RDS 파일로 저장
saveRDS(df_midterm, file = "df_midterm.rds")

rm(df_midterm) # 데이터 프레임 삭제 
df_midterm

df_midterm <- readRDS("df_midterm.rds")
df_midterm




