# 12장 인터랙티브 그래프
# 12-1 plotly 패키지로 인터랙티브 그래프 만들기

# 패키지 준비하기
install.packages("plotly")
library(plotly)
library(ggplot2)

p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point() 

ggplotly(p)

p <- ggplot(data = diamonds, aes(x = cut, fil = clarity)) +
  geom_bar(position = "dodge")

ggplotly(p)

# 12-2 dygraphs패키지로 인터랙티브 시계열 그래프 만들기
install.packages("xts")
install.packages("dygraphs")

library(xts)
library(dygraphs)

economics <- ggplot2 :: economics

eco <- xts(economics$unemploy, order.by = economics$date)

dygraph(eco)  # 그래프 생성

# 날짜 범위 선택 기능
dygraph(eco) %>%  dyRangeSelector()

# 저축률 
eco_a <- xts(economics$psavert, order.by = economics$date)

# 실업자 수
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

eco2 <- cbind(eco_a, eco_b) # 데이터 결합
colnames(eco2) <- c("psavert", "unemploy") 
head(eco2)

dygraph(eco2) %>% dyRangeSelector()





