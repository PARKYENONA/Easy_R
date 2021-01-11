# 데이터 분석 프로젝트 - '한국인의 삶을 파악하라'
# 9-1 '한국복지패널데이터' 분석 준비하기

install.packages("foreign") 

library(foreign)  # SPSS 파일 불러오기
library(dplyr)    # 전처리
library(ggplot2)  # 시각화
library(readxl)   # 엑셀 파일 불러오기

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)    # to.data.frame 데이터 프레임 형태로 변환
# 복사본 만들기
welfare <- raw_welfare

# 데이터 검토
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 변수명 바꾸기

welfare <- rename(welfare, 
                  sex = h10_g3,              # 성별
                  birth = h10_g4,            # 태어난 연도
                  marriage = h10_g10,        # 혼인 상태
                  religion = h10_g11,        # 종교
                  income = p1002_8aq1,       # 월급
                  code_job = h10_eco9,       # 직업 코드
                  code_region = h10_reg7)    # 지역 코드


# 9-2 성별에 따른 월급 차이 -'성별에 따라 월급이 다를까?"

# 성별 변수 검토 및 전처리
# 변수 검토하기

class(welfare$sex)
table(welfare$sex)

# 이상치 확인
table(welfare$sex) # 1 : 남자 2 : 여자 9 : 모름/ 무응답

# 이상치 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare)
# 결측치 확인
table(is.na(welfare$sex))

# 성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
# 변수 검토하기

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

# 전처리

# 이상치 확인
summary(welfare$income)

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income))

# 성별에 따른 월급 차이 분석하기

# 성별 월급 평균표 만들기

sex_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(sex) %>% 
              summarise(mean_income = mean(income))
sex_income

# 그래프 만들기
ggplot(data  = sex_income, aes(x = sex, y = mean_income)) + geom_col()


# 9-3 나이와 월급의 관계 - " 몇 살 때 월급을 가장 많이 받을까?"

# 나이 변수 검토 및 전처리

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

# 전처리
# 이상치 확인
summary(welfare$birth)

# 결측치 확인
table(is.na(welfare$birth))

# 이상치 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# 파생변수 만들기 - 나이
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# 나이와 월급의 관계 분석하기
# 나이에 따른 월급 평균표 만들기
age_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(age) %>% 
              summarise(mean_income = mean(income))

head(age_income)

# 그래프 만들기
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()









