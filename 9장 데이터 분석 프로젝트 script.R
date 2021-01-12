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











