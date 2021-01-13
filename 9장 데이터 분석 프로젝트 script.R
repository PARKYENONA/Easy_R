# ?°?´?„° ë¶„ì„ ?”„ë¡œì ?Š¸ - '?•œêµ??¸?˜ ?‚¶?„ ?ŒŒ?•…?•˜?¼'
# 9-1 '?•œêµ?ë³µì?€?Œ¨?„?°?´?„°' ë¶„ì„ ì¤€ë¹„í•˜ê¸?

install.packages("foreign") 

library(foreign)  # SPSS ?ŒŒ?¼ ë¶ˆëŸ¬?˜¤ê¸?
library(dplyr)    # ? „ì²˜ë¦¬
library(ggplot2)  # ?‹œê°í™”
library(readxl)   # ?—‘??€ ?ŒŒ?¼ ë¶ˆëŸ¬?˜¤ê¸?

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)    # to.data.frame ?°?´?„° ?”„? ˆ?„ ?˜•?ƒœë¡? ë³€?™˜
# ë³µì‚¬ë³? ë§Œë“¤ê¸?
welfare <- raw_welfare

# ?°?´?„° ê²€?† 
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# ë³€?ˆ˜ëª? ë°”ê¾¸ê¸?

welfare <- rename(welfare, 
                  sex = h10_g3,              # ?„±ë³?
                  birth = h10_g4,            # ?ƒœ?–´?‚œ ?—°?„
                  marriage = h10_g10,        # ?˜¼?¸ ?ƒ?ƒœ
                  religion = h10_g11,        # ì¢…êµ
                  income = p1002_8aq1,       # ?›”ê¸?
                  code_job = h10_eco9,       # ì§ì—… ì½”ë“œ
                  code_region = h10_reg7)    # ì§€?—­ ì½”ë“œ


# 9-2 ?„±ë³„ì— ?”°ë¥? ?›”ê¸? ì°¨ì´ -'?„±ë³„ì— ?”°?¼ ?›”ê¸‰ì´ ?‹¤ë¥¼ê¹Œ?"

# ?„±ë³? ë³€?ˆ˜ ê²€?†  ë°? ? „ì²˜ë¦¬
# ë³€?ˆ˜ ê²€?† ?•˜ê¸?

class(welfare$sex)
table(welfare$sex)

# ?´?ƒì¹? ?™•?¸
table(welfare$sex) # 1 : ?‚¨? 2 : ?—¬? 9 : ëª¨ë¦„/ ë¬´ì‘?‹µ

# ?´?ƒì¹? ê²°ì¸¡ ì²˜ë¦¬
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare)
# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(welfare$sex))

# ?„±ë³? ?•­ëª? ?´ë¦? ë¶€?—¬
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# ?›”ê¸? ë³€?ˆ˜ ê²€?†  ë°? ? „ì²˜ë¦¬
# ë³€?ˆ˜ ê²€?† ?•˜ê¸?

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

# ? „ì²˜ë¦¬

# ?´?ƒì¹? ?™•?¸
summary(welfare$income)

# ?´?ƒì¹? ê²°ì¸¡ ì²˜ë¦¬
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(welfare$income))

# ?„±ë³„ì— ?”°ë¥? ?›”ê¸? ì°¨ì´ ë¶„ì„?•˜ê¸?

# ?„±ë³? ?›”ê¸? ?‰ê· í‘œ ë§Œë“¤ê¸?

sex_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(sex) %>% 
              summarise(mean_income = mean(income))
sex_income

# ê·¸ë˜?”„ ë§Œë“¤ê¸?
ggplot(data  = sex_income, aes(x = sex, y = mean_income)) + geom_col()


# 9-3 ?‚˜?´??€ ?›”ê¸‰ì˜ ê´€ê³? - " ëª? ?‚´ ?•Œ ?›”ê¸‰ì„ ê°€?¥ ë§ì´ ë°›ì„ê¹??"

# ?‚˜?´ ë³€?ˆ˜ ê²€?†  ë°? ? „ì²˜ë¦¬

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

# ? „ì²˜ë¦¬
# ?´?ƒì¹? ?™•?¸
summary(welfare$birth)

# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(welfare$birth))

# ?´?ƒì¹? ê²°ì¸¡ ì²˜ë¦¬
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

# ?ŒŒ?ƒë³€?ˆ˜ ë§Œë“¤ê¸? - ?‚˜?´
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

# ?‚˜?´??€ ?›”ê¸‰ì˜ ê´€ê³? ë¶„ì„?•˜ê¸?
# ?‚˜?´?— ?”°ë¥? ?›”ê¸? ?‰ê· í‘œ ë§Œë“¤ê¸?
age_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(age) %>% 
              summarise(mean_income = mean(income))

head(age_income)

# ê·¸ë˜?”„ ë§Œë“¤ê¸?
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

# 9-4 ¿¬·É´ë¿¡ µû¸¥ ¿ù±Ş Â÷ÀÌ - "¾î¶² ¿¬·É´ëÀÇ ¿ù±ŞÀÌ °¡Àå ¸¹À»±î?"
# ¿¬·É´ë º¯¼ö °ËÅä ¹× ÀüÃ³¸®ÇÏ±â

welfare <- welfare %>% 
           mutate(ageg = ifelse(age < 30, "young", 
                                ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)

# ¿¬·É´ë¿¡ µû¸¥ ¿ù±Ş Â÷ÀÌ ºĞ¼®ÇÏ±â

ageg_income <- welfare %>% 
               filter(!is.na(income)) %>% 
               group_by(ageg) %>% 
               summarise(mean_income = mean(income))
ageg_income

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

# 9-5 ¿¬·É´ë ¹× ¼ºº° ¿ù±ŞÂ÷ÀÌ - "¼ºº° ¿ù±Ş Â÷ÀÌ´Â ¿¬·É´ëº°·Î ´Ù¸¦±î?" 

# ¿¬·É´ë ¹× ¼ºº° ¿ù±Ş Â÷ÀÌ ºĞ¼®ÇÏ±â

sex_income <- welfare %>% 
              filter(!is.na(income)) %>% 
              group_by(ageg, sex) %>% 
              summarise(mean_income = mean(income))
sex_income

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#  geom_col(position = "dodge") ¸·´ë ºĞ¤©
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

# ³ªÀÌ ¹× ¼ºº° ¿ù±Ş Â÷ÀÌ ºĞ¼®ÇÏ±â
# ¼ºº° ¿¬·Éº° ¿ù±Ş Æò±ÕÇ¥ ¸¸µé±â
sex_age <- welfare %>% 
           filter(!is.na(income)) %>% 
           group_by(age, sex) %>% 
           summarise(mean_income = mean(income))
head(sex_age)
# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

# 9-6 Á÷¾÷º° ¿ù±Ş Â÷ÀÌ - "¾î¶² Á÷¾÷ÀÌ ¿ù±ŞÀ» °¡Àå ¸¹ÀÌ ¹ŞÀ»±î?"

# Á÷¾÷ º¯¼ö °ËÅä ¹× ÀüÃ³¸®ÇÏ±â
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
  coord_flip()    # coord_flip ¸·´ë¸¦ 90µµ È¸Àü

# ÇÏÀ§ 10À§ ÃßÃâ
bottom10 <- job_income %>% 
            arrange(mean_income) %>% 
            head(10)
bottom10

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

# 9-7 ¼ºº° Á÷¾÷ ºóµµ - " ¼ºº°·Î ¾î¶² Á÷¾÷ÀÌ °¡Àå ¸¹À»±î?"
# ¼ºº° Á÷¾÷ ¹Îµµ ºĞ¼®ÇÏ±â

# ³²¼º Á÷¾÷ ºóµµ »óÀ§ 10°³ ÃßÃâ
job_male <- welfare %>% 
            filter(!is.na(job) & sex == "male") %>% 
            group_by(job) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n)) %>% 
            head(10)
job_male

# ¿©¼º Á÷¾÷ ºóµµ »óÀ§ 10°³ ÃßÃâ
job_female <- welfare %>% 
              filter(!is.na(job) & sex == "female") %>% 
              group_by(job) %>% 
              summarise( n = n()) %>% 
              arrange(desc(n)) %>% 
              head(10)
job_female

# ±×·¡ÇÁ ¸¸µé±â

# ³²¼º Á÷¾÷ ºóµµ »óÀ§ 10°³ Á÷¾÷
ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# ¿©¼º Á÷¾÷ ºóµµ »óÀ§ 10°³ Á÷¾÷
ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

# 9-8 Á¾±³ À¯¹«¿¡ µû¸¥ ÀÌÈ¥À² - "Á¾±³°¡ ÀÖ´Â »ç¶÷µéÀÌ ÀÌÈ¥À» ´ú ÇÒ±î?"

# Á¾±³ º¯¼ö °ËÅä ¹× ÀüÃ³¸®ÇÏ±â
class(welfare$religion)
table(welfare$religion)

# Á¾±³ À¯¹« ÀÌ¸§ ºÎ¿©
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

# È¥ÀÎ »óÅÂ º¯¼ö °ËÅä ¹× ÀüÃ³¸®
class(welfare$marriage)
table(welfare$marriage)

# ÀÌÈ¥ ¿©ºÎ º¯¼ö ¸¸µé±â
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                          ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

# Á¾±³ À¯¹«¿¡ µû¸¥ ÀÌÈ¥À² Ç¥ ¸¸µé±â
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

# ÀÌÈ¥ ÃßÃâ
divorce <- religion_marriage %>% 
           filter(group_marriage == "divorce") %>% 
           select(religion, pct)
divorce

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

# ¿¬·É´ë ¹× Á¾±³ À¯¹«¿¡ µû¸¥ ÀÌÈ¥À² ºĞ¼®ÇÏ±â
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

# ¿¬·É´ëº° ÀÌÈ¥À² ±×·¡ÇÁ ¸¸µé±â
# ÃÊ³â Á¦¿Ü, ÀÌÈ¥ ÃßÃâ
ageg_divorce <- ageg_marriage %>% 
                filter(ageg != "young" & group_marriage == "divorce") %>% 
                select(ageg, pct)

ageg_divorce

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) + geom_col()

# ¿¬·É´ë ¹× Á¾±³ À¯¹«¿¡ µû¸¥ ÀÌÈ¥À² Ç¥ ¸¸µé±â
# ¿¬·É´ë, Á¾ À¯¹«, °áÈ¥ »óÅÂº° ºñÀ²Ç¥ ¸¸µé±â
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

# ¿¬·É´ë ¹× Á¾±³ À¯¹«º° ÀÌÈ¥À² Ç¥ ¸¸µé±â
df_divorce <- ageg_religion_marriage %>% 
              filter(group_marriage == "divorce") %>% 
              select(ageg, religion, pct)
df_divorce

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = "dodge")

# 9-9 Áö¿ªº° ¿¬·É´ë ºñÀ² - "³ë³âÃşÀÌ ¸¹Àº Áö¿ªÀº ¾îµğÀÏ±î?"
# Áö¿ªº¯¼ö °ËÅä ¹× ÀüÃ³¸®ÇÏ±â
class(welfare$code_region)
table(welfare$code_region)
# Áö¿ª ÄÚµå ¸ñ·Ï ¸¸µé±â
list_region <- data.frame(code_region = c(1:7),
                          region = c("¼­¿ï",
                                     "¼öµµ±Ç(ÀÎÃµ/°æ±â)",
                                     "ºÎ»ê/°æ³²/¿ï»ê",
                                     "´ë±¸/°æºÏ",
                                     "´ëÀü/Ãæ³²",
                                     "°­¿ø/ÃæºÏ",
                                     "±¤ÁÖ/Àü³²/ÀüºÏ/Á¦ÁÖµµ"))
list_region

# Áö¿ª¸í º¯¼ö Ãß°¡
welfare <- left_join(welfare,list_region, id = "code_region")
welfare %>%
  select(code_region, region) %>% 
  head

# Áö¿ªº° ¿¬·É´ë ºñÀ²Ç¥ ¸¸µé±â
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

# ±×·¡ÇÁ ¸¸µé±â
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# ³ë³âÃş ºñÀ² ³ôÀº ¼øÀ¸·Î ¸·´ë Á¤·ÄÇÏ±â
# ³ë³âÃş ºñÀ² ³»¸²Â÷¼ø Á¤·Ä
list_order_old <- region_ageg %>% 
                  filter(ageg == "old") %>% 
                  arrange(pct)
list_order_old

# Áö¿ª¸í ¼ø¼­ º¯¼ö ¸¸µé±â
order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

# ¿¬·É´ë ¼øÀ¸·Î ¸·´ë »ö±ò ³ª¿­ÇÏ±â
class(region_ageg$ageg)
levels(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old", "middle", "young"))


