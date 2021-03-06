# ?��?��?�� 분석 ?��로젝?�� - '?���??��?�� ?��?�� ?��?��?��?��'
# 9-1 '?���?복�?�?��?��?��?��?��' 분석 준비하�?

install.packages("foreign") 

library(foreign)  # SPSS ?��?�� 불러?���?
library(dplyr)    # ?��처리
library(ggplot2)  # ?��각화
library(readxl)   # ?��??� ?��?�� 불러?���?

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)    # to.data.frame ?��?��?�� ?��?��?�� ?��?���? 변?��
# 복사�? 만들�?
welfare <- raw_welfare

# ?��?��?�� 검?��
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 변?���? 바꾸�?

welfare <- rename(welfare, 
                  sex = h10_g3,              # ?���?
                  birth = h10_g4,            # ?��?��?�� ?��?��
                  marriage = h10_g10,        # ?��?�� ?��?��
                  religion = h10_g11,        # 종교
                  income = p1002_8aq1,       # ?���?
                  code_job = h10_eco9,       # 직업 코드
                  code_region = h10_reg7)    # 지?�� 코드


# 9-2 ?��별에 ?���? ?���? 차이 -'?��별에 ?��?�� ?��급이 ?��를까?"

# ?���? 변?�� 검?�� �? ?��처리
# 변?�� 검?��?���?

class(welfare$sex)
table(welfare$sex)

# ?��?���? ?��?��
table(welfare$sex) # 1 : ?��?�� 2 : ?��?�� 9 : 모름/ 무응?��

# ?��?���? 결측 처리
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare)
# 결측�? ?��?��
table(is.na(welfare$sex))

# ?���? ?���? ?���? 부?��
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# ?���? 변?�� 검?�� �? ?��처리
# 변?�� 검?��?���?

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

# ?��처리

# ?��?���? ?��?��
summary(welfare$income)

# ?��?���? 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측�? ?��?��
table(is.na(welfare$income))

# ?��별에 ?���? ?���? 차이 분석?���?

# ?���? ?���? ?��균표 만들�?

sex_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(sex) %>% 
              summarise(mean_income = mean(income))
sex_income

# 그래?�� 만들�?
ggplot(data  = sex_income, aes(x = sex, y = mean_income)) + geom_col()


# 9-3 ?��?��??� ?��급의 관�? - " �? ?�� ?�� ?��급을 가?�� 많이 받을�??"

# ?��?�� 변?�� 검?�� �? ?��처리

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

# ?��처리
# ?��?���? ?��?��
summary(welfare$birth)

# 결측�? ?��?��
table(is.na(welfare$birth))

# ?��?���? 결측 처리
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# ?��?��변?�� 만들�? - ?��?��
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# ?��?��??� ?��급의 관�? 분석?���?
# ?��?��?�� ?���? ?���? ?��균표 만들�?
age_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(age) %>% 
              summarise(mean_income = mean(income))

head(age_income)

# 그래?�� 만들�?
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

# 9-4 ���ɴ뿡 ���� ���� ���� - "� ���ɴ��� ������ ���� ������?"
# ���ɴ� ���� ���� �� ��ó���ϱ�

welfare <- welfare %>% 
           mutate(ageg = ifelse(age < 30, "young", 
                                ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)

# ���ɴ뿡 ���� ���� ���� �м��ϱ�

ageg_income <- welfare %>% 
               filter(!is.na(income)) %>% 
               group_by(ageg) %>% 
               summarise(mean_income = mean(income))
ageg_income

# �׷��� �����
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

# 9-5 ���ɴ� �� ���� �������� - "���� ���� ���̴� ���ɴ뺰�� �ٸ���?" 

# ���ɴ� �� ���� ���� ���� �м��ϱ�

sex_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(ageg, sex) %>% 
              summarise(mean_income = mean(income))
sex_income

# �׷��� �����
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#  geom_col(position = "dodge") ���� �Ф�
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

# ���� �� ���� ���� ���� �м��ϱ�
# ���� ���ɺ� ���� ���ǥ �����
sex_age <- welfare %>% 
           filter(!is.na(income)) %>% 
           group_by(age, sex) %>% 
           summarise(mean_income = mean(income))
head(sex_age)
# �׷��� �����
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

# 9-6 ������ ���� ���� - "� ������ ������ ���� ���� ������?"

# ���� ���� ���� �� ��ó���ϱ�
class(welfare$code_job)
table(welfare$code_job)

library(readxl)

list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
              filter(!is.na(job) & !is.na(income)) %>% 
              group_by(job) %>% 
              summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
         arrange(desc(mean_income)) %>% 
         head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y= mean_income)) +
  geom_col() +
  coord_flip()    # coord_flip ���븦 90�� ȸ��

# ���� 10�� ����
bottom10 <- job_income %>% 
            arrange(mean_income) %>% 
            head(10)
bottom10

# �׷��� �����
ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

# 9-7 ���� ���� �� - " ������ � ������ ���� ������?"
# ���� ���� �ε� �м��ϱ�

# ���� ���� �� ���� 10�� ����
job_male <- welfare %>% 
            filter(!is.na(job) & sex == "male") %>% 
            group_by(job) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n)) %>% 
            head(10)
job_male

# ���� ���� �� ���� 10�� ����
job_female <- welfare %>% 
              filter(!is.na(job) & sex == "female") %>% 
              group_by(job) %>% 
              summarise( n = n()) %>% 
              arrange(desc(n)) %>% 
              head(10)
job_female

# �׷��� �����

# ���� ���� �� ���� 10�� ����
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# ���� ���� �� ���� 10�� ����
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# 9-8 ���� ������ ���� ��ȥ�� - "������ �ִ� ������� ��ȥ�� �� �ұ�?"

# ���� ���� ���� �� ��ó���ϱ�
class(welfare$religion)
table(welfare$religion)

# ���� ���� �̸� �ο�
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

# ȥ�� ���� ���� ���� �� ��ó��
class(welfare$marriage)
table(welfare$marriage)

# ��ȥ ���� ���� �����
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                          ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

# ���� ������ ���� ��ȥ�� ǥ �����
religion_marriage <- welfare %>% 
                     filter(!is.na(group_marriage)) %>% 
                     group_by(religion, group_marriage) %>% 
                     summarise( n = n()) %>% 
                     mutate(tot_group = sum(n)) %>% 
                     mutate(pct = round(n / tot_group * 100, 1))
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

religion_marriage

# ��ȥ ����
divorce <- religion_marriage %>% 
           filter(group_marriage == "divorce") %>% 
           select(religion, pct)
divorce

# �׷��� �����
ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

# ���ɴ� �� ���� ������ ���� ��ȥ�� �м��ϱ�
ageg_marriage <- welfare %>% 
                 filter(!is.na(group_marriage)) %>% 
                 group_by(ageg, group_marriage) %>% 
                 summarise(n = n()) %>% 
                 mutate(tot_group = sum(n)) %>% 
                 mutate(pct = round(n/tot_group*100, 1))

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

ageg_marriage

# ���ɴ뺰 ��ȥ�� �׷��� �����
# �ʳ� ����, ��ȥ ����
ageg_divorce <- ageg_marriage %>% 
                filter(ageg != "young" & group_marriage == "divorce") %>% 
                select(ageg, pct)

ageg_divorce

# �׷��� �����
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()

# ���ɴ� �� ���� ������ ���� ��ȥ�� ǥ �����
# ���ɴ�, �� ����, ��ȥ ���º� ����ǥ �����
ageg_religion_marriage <- welfare %>% 
                          filter(!is.na(group_marriage) & ageg != "young") %>%
                          group_by(ageg, religion, group_marriage) %>% 
                          summarise(n = n()) %>% 
                          mutate(tot_group = sum(n)) %>% 
                          mutate(pct = round(n/tot_group*100, 1))

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>%
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))


ageg_religion_marriage

# ���ɴ� �� ���� ������ ��ȥ�� ǥ �����
df_divorce <- ageg_religion_marriage %>% 
              filter(group_marriage == "divorce") %>% 
              select(ageg, religion, pct)
df_divorce

# �׷��� �����
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = "dodge")

# 9-9 ������ ���ɴ� ���� - "������� ���� ������ ����ϱ�?"
# �������� ���� �� ��ó���ϱ�
class(welfare$code_region)
table(welfare$code_region)
# ���� �ڵ� ��� �����
list_region <- data.frame(code_region = c(1:7),
                          region = c("����",
                                     "������(��õ/���)",
                                     "�λ�/�泲/���",
                                     "�뱸/���",
                                     "����/�泲",
                                     "����/���",
                                     "����/����/����/���ֵ�"))
list_region

# ������ ���� �߰�
welfare <- left_join(welfare,list_region, id = "code_region")
welfare %>%
  select(code_region, region) %>% 
  head

# ������ ���ɴ� ����ǥ �����
region_ageg <- welfare %>% 
               group_by(region, ageg) %>% 
               summarise(n = n()) %>% 
               mutate(tot_group = sum(n)) %>% 
               mutate(pct = round(n/tot_group*100,2))
region_ageg <- welfare %>% 
               count(region, ageg) %>% 
               group_by(region) %>% 
               mutate(pct = round(n/sum(n)*100, 2))
region_ageg

# �׷��� �����
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# ����� ���� ���� ������ ���� �����ϱ�
# ����� ���� �������� ����
list_order_old <- region_ageg %>% 
                  filter(ageg == "old") %>% 
                  arrange(pct)
list_order_old

# ������ ���� ���� �����
order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

# ���ɴ� ������ ���� ���� �����ϱ�
class(region_ageg$ageg)
levels(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old", "middle", "young"))


