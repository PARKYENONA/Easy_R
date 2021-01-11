# 8장 그래프 만들기
# 8-1 R로 만들 수  있는 그래프 살펴보기
# ggplot2

# 8-2 산점도 - 변수 간 관계 표현하기

# ggplot2 레이어 구조 
# 1단계 배경설정 (축)
# 2단계 그래프 추가 (점, 막대, 선)
# 3단계 설정 추가 (축 범위, 색, 표식)

# 산점도 만들기

library("ggplot2")

# 배경 설정하기
# x축은 displ, y축은 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))

# 그래프 추가하기
# 배경에 산점도 추가 
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# 배기량이 큰 자동차일수록 고속도로 연비가 낮은 경향이 있다는 것을 알 수 있음

# 축 범위를 조정하는 설정 추가하기 
# x축 범위 3~6으로  지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

# x축 범위 3~6, y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) +    # 데이터, 축
  geom_point() +                                 # 그래프 종류
  xlim(3, 6) + ylim(10, 30)                      # 세부 설정

# 그래프를 이미지 파일로 저장하기
# Export -> Save as Image or Save as PDF 

# 혼자서 해보기
# Q1

ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

# Q2
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() + 
  xlim(0, 500000) + ylim(0, 10000)


# 8-3 막대 그래프 - 집단 간 차이 표현하기

# 평균 막대 그래프 만들기
# 집단별 평균표 만들기

library("dplyr")

df_mpg <- mpg %>% 
          group_by(drv) %>% 
          summarise(mean_hwy = mean(hwy))
df_mpg

# 그래프 생성하기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()  # 요약표 이용

# 크기 순으로 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

# 빈도 막대 그래프 만들기
ggplot(data = mpg, aes(x = drv)) + geom_bar()      # 원자료 이용

ggplot(data = mpg, aes(x = hwy)) + geom_bar()

# 혼자서 해보기
# Q1
mpg <- as.data.frame(ggplot :: mpg)
df_suv <- mpg %>% 
          filter(class == "suv") %>% 
          group_by(manufacturer) %>% 
          summarise(mean_cty = mean(cty)) %>% 
          arrange(desc(mean_cty)) %>% 
          head(5)

ggplot(data = df_suv, aes(x = reorder(manufacturer, -mean_cty),
                          y = mean_cty)) + geom_col() 

# Q2
ggplot(data = mpg, aes(x = class)) + geom_bar()

# 8-4 선그래프 - 시간에 따라 달라지는 데이터 표현하기
# 시계열 그래프 만들기

ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

# 혼자서 해보기
# Q1
ggplot(data = economics, aes(x= date, y = psavert)) + geom_line()


# 8-5 상자 그림 - 집단 간 분포 차이 표현하기
# 상자그림 만들기

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

# 혼자서 해보기
# Q1

mpg <- as.data.frame(ggplot :: mpg)

df_mpg <- mpg %>% 
          filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data = df_mpg, aes(x = class, y = cty)) + geom_boxplot()
  













