# 13장 통계 분석 기법을 이용한 가설검정
# 13-1 통계적 가설 검정이란?

# 기술 통계와 추론통계 
# 기술통계(Descriptive statistics) : 데이터를 요약해 설명하는 기법
# 추론 통계(Inferential statistics) : 단순히 숫자를 요약하는 것을 넘어 어떤 값이 발생할 확률을 계산하는 통계 기법

# 통계적 가설검정
# 통계적 가설검정(Statistical hypothesis test) : 유의확률을 이용해 가설 검정하는 방법
# 유의확률(Significance probability, p-value) : 실제로는 집단 간 차이가 없는데 우연히 차이가 있는 데이터가 추출될 확률을 의미

# 13-2 t검정 - 두 집단의 평균비교

library(ggplot2)
library(dplyr)

mpg <- as.data.frame(ggplot2::mpg)
mpg_diff <- mpg %>% 
            select(class, cty) %>% 
            filter(class %in% c("compact", "suv"))

head(mpg_diff)
table(mpg_diff$class)

t.test(data = mpg_diff, cty ~ class, var.equal = T)
# compact와 suv간 평균 도시 연비 차이가 통계적으로 유의하다

# 일반 휘발유와 고급 휘발유의 도시 연비 t검정

mpg_diff2 <- mpg %>% 
             select(fl, cty) %>% 
             filter(fl %in% c("r", "p"))   # r : regular, p : premium
table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

# 일반 휘발유와 고급 휘발유를 사용하는 자동차 간 도시 연비 차이는 통계적으로 유의하지 않다.

# 13-3 상관분석

# 실업자 수와 개인 소비 지출의 상관관계
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

# 실업자 수와 개인 소비 지출의 상관이 통계적으로 유의하다.
# 상관계수 0.61 -> 정비례 관계

# 상관행렬 히트맵 만들기
head(mtcars)

car_cor <- cor(mtcars)
round(car_cor, 2)

# mpg와 cyl 상관계수 -0.85 -> 연비가 높을수록 실린더 수가 적다.
# cyl과 wt의 상관계수 0.78 -> 실린더 수가 많을수록 자동차가 무거운 경향이 있다.

install.packages("corrplot")
library(corrplot)
corrplot(car_cor)
corrplot(car_cor, method = "number")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
                          "#4477AA"))
corrplot(car_cor,
         method = "color",        # 색깔로 표현
         col = col(200),          # 색상 200개 선정
         type = "lower",          # 왼쪽 아래 행렬만 표시
         order = "hclust",        # 유사한 상관계수끼리 군집화
         addCoef.col = "black",   # 상관계수 색깔
         tl.col = "black",        # 변수명 색깔
         tl.srt = 45,             # 변수명 45도 기울임
         diag = F)                # 대각 행렬 제외
