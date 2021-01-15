# 11장 지도 시각화
# 11-1 미국 주별 강력 범죄율 단계 구분도 만들기

# 패키지 준비
install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)

# 데이터 준비
str(USArrests)
head(USArrests)

library(tibble)

# 행이름을 state변수로 변경
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)      # 소문자로  수정

str(crime)

# 미국 주 지도 데이터 준비

install.packages("maps")
library(ggplot2)

states_map <- map_data("state")
str(states_map)

# 단계구분도 만들기
ggChoropleth(data = crime,         # 지도에 표현할 데이터
             aes(fill = Murder,    # 색깔로 표현할 변수
                 map_id = state),  # 지역 기준 변수
             map = states_map)     # 지도 데이터

# 인터랙티브 단계 구분도 만드리
ggChoropleth(data = crime,         # 지도에 표현할 데이터
             aes(fill = Murder,    # 색깔로 표현할 변수
                 map_id = state),  # 지역 기준 변수
             map = states_map,     # 지도 데이터
             interactive = T)      # 인터랙티브

# 11-2 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 만들기

# 패키지 준비하기
install.packages("stringi")
install.packages("devtools")
devtools:: install_github("cardiomoon/kormaps2014")

library(kormaps2014)

str(changeCode(korpop1))

library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")

str(changeCode(kormap1))

# 단계 구분도 만들기
ggChoropleth(data = korpop1,       # 지도에 표현할 데이터
             aes(fill = pop,       # 색깔로 표현할 변수
                 map_id = code,    # 지역 기준 변수
                 tooltip = name),   # 지도 위에 표시할 지역명
             map = kormap1,        # 지도 데이터
             interactive = T)      # 인터랙티브

# 대한민국 시도별 결핵 환자 수 단계 구분도 만들기
str(changeCode(tbc))

tbc$name <- iconv(tbc$name, "UTF-8", "CP949")

ggChoropleth(data = tbc,            # 지도에 표현할 데이터
             aes(fill = NewPts,     # 색깔로 표현할 변수
                 map_id = code,     # 지역 기준 변수
                 tooltip = name),   # 지도 위에 표시할 지역명
             map = kormap1,         # 지도 데이터
             interactive = T)       # 인터랙티브






