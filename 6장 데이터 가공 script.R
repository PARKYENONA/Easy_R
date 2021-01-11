# 6장 데이터 가공하기

# 6-1 데이터 전처리 

library("dplyr") 

# filter() # 행추출
# select() # 열추출
# arrange() # 정렬
# mutate() # 변수추가
# summarise() # 통계치 산출
# group_by() # 집단별로 나누기
# left_join() # 데이터 합치기(열)
# bind_rows() # 데이터 합치기(행)

# 6-2 조건에 맞는 데이터만 추출

library("dplyr")

exam <- read.csv("csv_exam.csv")
exam

# exam에서 class가 1인 겨우만 추출해 출력

exam %>% filter(class == 1) # %>% 함수들을 나열하는 방식 함수들 연결

# 2반인 경우만 추출

exam %>% filter(class == 2)

exam %>% filter(class != 1) # 1반이 아닌 경우

exam %>% filter(class != 3) # 3반이 아닌 경우

exam %>% filter(math > 50) # 수학 점수가 50점을 초과한 경우 

exam %>% filter(math < 50) # 수학 점수가 50점 미만인 경우

exam %>% filter(english >= 80) # 영어 점수가 80점 이상인 경우

exam %>% filter(english <= 80) # 영어 점수가 80점 이하인 경우 

# 여러 조건을 충족하는 행 추출

# 1반이면서 수학 점수가 50점 이상인 경우 (AND)
exam %>% filter(class == 1 & math >= 50) 

# 2반이면서 영어 점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)

# 여러 조건 중 하나 이상 충족하는 행 추출 (OR)

# 수학 점수가 90점 이상이거나 영어 점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)

# 영어 점수가 90점 미만이거나 과학 점수가 50점 미만인 경우
exam %>% filter(english < 90 | science < 50)

# 목록에 해당하는 행 추출

# 1, 3, 5반에 해당하면 추출
exam %>% filter(class == 1 | class == 3 | class == 5)

exam %>% filter(class %in% c(1, 3, 5)) # %in% 매치 연산자 

#추출한 행으로 데이터 만들기
class1 <- exam %>% filter(class == 1) # class가 1인 행 추출, class1에 할당
class2 <- exam %>% filter(class == 2) # class가 2인 행 추출, class2에 할당

mean(class1$math) # 1반 수학 점수 평균 

mean(class2$math) # 2반 수학 점수 평균

# 혼자서 해보기 - mpg
library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg) #mpg 데이터 불러오기

#Q1
displ4 <- mpg %>% filter(displ <= 4)
displ5 <- mpg %>% filter(displ >= 5)

mean(displ4$hwy)
mean(displ5$hwy)

#Q2
audi <- mpg %>% filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")

mean(audi$cty)
mean(toyota$cty)

#Q3
cfh <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(cfh$hwy)


# 6-3 필요한 변수만 추출

# 변수 추출
exam %>% select(math) # math 추출

exam %>% select(english) # english 추출 

exam %>% select(class, math, english) # class, math, english 변수 추출

exam %>% select(-math) # math 제외

exam %>% select(-math, -english) # math, english

# dplyr 함수 조합하기

# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english) 

exam %>% 
  filter(class == 1) %>%  # class가 1인 행 추출 
  select(english)         # english 추출

exam %>% 
  select(id, math) %>% # id, math 추출
  head                 # 앞부분 6행까지 추출

exam %>% 
  select(id, math) %>% # id, math 추출
  head(10)             # 앞부분 10행까지 추출

# 혼자서 해보기 - mpg

library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg)

#Q1 
new_mpg <- mpg %>% select(class, cty)
head(new_mpg)

#Q2
mpg_suv <- new_mpg %>% filter(class == "suv")
mpg_com <- new_mpg %>% filter(class == "compact")
head(mpg_suv)
head(mpg_com)

mean(mpg_suv$cty)
mean(mpg_com$cty)

# 6-4 순서대로 정렬

#오름차순
exam %>% arrange(math) # math 오름차순 정렬

#내림차순
exam %>% arrange(desc(math)) # math 내림차순 정렬

exam %>% arrange(class, math) # class 및 math 오름차순 정렬

# 혼자서 해보기

library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg)

#Q1
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_audi %>% 
  arrange(desc(hwy)) %>% 
  head(5)

mpg %>% 
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

# 6-5 파생변수 추가

exam %>%  
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  head                                          # 일부 추추

exam %>%
  mutate(total = math + english + science ,           # 총합 변수 추가
         mean  = (math + english + science) / 3) %>%  # 총평균 변수 추가
  head                                                # 일부 추출
  
exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%  
  head 

exam %>% 
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  arrange(total) %>%                            # 총합 변수 기준 정렬
  head                                          # 일부 추추

# 혼자서 해보기 

library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg)

#Q1
df_mpg <- mpg
head(mpg)
head(df_mpg)

df_mpg %>% 
  mutate(total = cty + hwy) %>% 
  head

#Q2

df_mpg %>%
  mutate(total = cty + hwy) %>%
  mutate(mean = total / 2) %>% 
  head

#Q3

df_mpg %>% 
  mutate(total = cty + hwy) %>%
  mutate(mean = total / 2) %>% 
  arrange(desc(mean)) %>% 
  head(3)

#Q4

df_mpg %>% 
  mutate(total = cty + hwy,
         mean = total / 2) %>% 
  arrange(desc(mean)) %>% 
  head(3)

# 6-6 집단별로 요약하기

exam %>% 
  summarise(mean_math = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>%               # class별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>%                   # class별로 분리 
  summarise(mean_math = mean(math),     # math 평균
            sum_math = sum(math),       # math 합계
            median_math = median(math), # math 중앙값
            n = n())                    # 학생 수 빈도

mpg %>%
  group_by(manufacturer, drv) %>%       # 회사별, 구동 방식별 분리
  summarise(mean_cty = mean(cty)) %>%   # cty 평균 산출
  head(10)                              # 일부출력

mpg %>%
  group_by(manufacturer) %>%            # 회사별로 분리
  filter(class == "suv") %>%            # suv 추출
  mutate(tot = (cty+hwy) / 2) %>%       # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>%   # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%           # 내림차순 정렬
  head(5)                               # 1~5위까지 출력

# 혼자서 해보기
library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg)

# Q1
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))

#Q2
mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

#Q3
mpg %>%
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

#Q4
mpg %>% 
  filter(class == "compact") %>% 
  group_by(manufacturer) %>%
  summarise(count_com = n()) %>% 
  arrange(desc(count_com))

# 6-7 데이터 합치기

#가로로 합치기

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))
# 기말고사 데이터 생성 
test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    final = c(70, 83, 65, 95, 80))

test1
test2

total <- left_join(test1, test2, by = "id")  # id를 기준으로 합쳐 total에 할당
total

# 다른 데이터를 활용해 변수 추가
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new

# 세로로 합치기

# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))

group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))

group_a
group_b

group_all <- bind_rows(group_a, group_b)  # 데이터 합쳐서 group_all에 할당
group_all                                 # group_all 출력

# 혼자서 해보기

library("ggplot2")
mpg <- as.data.frame(ggplot2 :: mpg)

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

#Q1
mpg_all <- left_join(mpg, fuel, by = "fl")

mpg_all %>% 
  select(model, fl, price_fl) %>% 
  head(5)

# 분석 도전

library("ggplot2")
midwest <- as.data.frame(ggplot2 :: midwest)

str(midwest)

#Q1
midwest <- midwest %>% 
             mutate(popchild = (poptotal - popadults),
                    percent_child = (popchild / poptotal)*100)
head(midwest,5)

#Q2
midwest %>% 
  select(county, percent_child) %>% 
  arrange(desc(percent_child)) %>% 
  head(5)

#Q3

midwest <- midwest %>% 
              mutate(cat_child = ifelse(percent_child >= 40, "large",
                                        ifelse(percent_child >= 30, "middle", "small")))
str(midwest)

midwest %>% 
  select(cat_child) %>% 
  group_by(cat_child) %>% 
  summarise(count_child = n()) %>% 
  head

table(midwest$cat_child)

#Q4

midwest <- midwest %>%
              mutate(percent_asian = (popasian / poptotal)*100)
midwest %>% 
  select(state, county, percent_asian) %>% 
  arrange(percent_asian) %>% 
  head(10)
































































