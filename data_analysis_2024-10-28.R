rm(list=ls())

setwd("//192.168.188.121/부산행/개인/이동준/논문/최양신 교수님/MMSE/R")

DAT1 <- read.csv("oef_data_final.csv", header = TRUE)
colnames(DAT2)

library(tidyr)
library(car)
library(dplyr)
library(ppcor)

# remove NAs in mmse
DAT1 <- DAT1 %>% drop_na(mmse)
DAT1 <- DAT1 %>% filter(diag_cat!=7)

DAT2 <- DAT1 %>% dplyr::select(eTIV, WMHvol, OEF_Mean, WB_OEF, CMRO2_Mean, WB_CMRO2, sex, age, htn, dm, dl, sm, bmi, mmse, diag_cat, inf_cat)


# new sex variable
DAT2 <- DAT2 %>%
  mutate(sex2 = dplyr::recode(sex, M = '1', F = '2')
         )
DAT2$sex2 <- as.integer(DAT2$sex2)
DAT2$inf_cat <- as.integer(DAT2$inf_cat)

# 연속형 범수를 범주형으로
str(DAT2)
median(DAT2$bmi, na.rm=TRUE)
DAT3 <- DAT2 %>% mutate(bmi2=ifelse(bmi>24.1,"medianhigh", "medianlow"),
                        age2=ifelse(age>=65, "old", "young"),
                        inf_cat2=ifelse(inf_cat==4,"no","yes"),
                        inf_cat3 = ifelse(inf_cat %in% c(1, 2, 3), "old", 
                                          ifelse(inf_cat == 4, "no", 
                                                 ifelse(inf_cat == 5, "recent", NA))))
DAT3 <- DAT3 %>% mutate(WMHvol2=WMHvol/1000)


baseline2 <- baseline2 %>% mutate(bmi2=ifelse(bmi>24.39,"medianhigh", "medianlow"), age2=ifelse(age>=65, "old", "young"))


# 분포확인
hist(DAT2$WB_OEF)
shapiro.test(DAT2$WB_OEF)
shapiro.test(log(DAT2$WB_OEF))
hist(DAT2$WB_CMRO2)
shapiro.test(DAT3$CMRO2)
shapiro.test(log(DAT2$CMRO2_Mean))
hist(DAT2$WMHvol)
shapiro.test(DAT2$log.WMHvol)
hist(log(DAT2$WB_OEF))


# Make log WMHvol and CMRO2  # 데이터가 한쪽으로 몰려있으므로 log 변환을 해준다.
hist(DAT2$WMHvol)

library(dplyr)
DAT3 <- DAT3 %>%
  mutate(log.WMHvol=log(WMHvol), log.CMRO2=log(CMRO2_Mean), log.WB_OEF=log(WB_OEF))



colnames(DAT2)

age_mmse <- lm(mmse~age, data = DAT7)
summary(age_mmse)



lm(mmse~age+OEF_Mean, data = DAT7) %>% summary()
lm(mmse~age+CMRO2_Mean, data = DAT7) %>% summary()
lm(mmse~age+CMRO2_Mean+OEF_Mean, data = DAT7) %>% summary()
lm(mmse~age+CMRO2_Mean+OEF_Mean+WMHvol, data = DAT7) %>% summary()
lm(mmse~age+CMRO2_Mean+OEF_Mean+WMHvol+sex, data = DAT7) %>% summary()


sum(is.na(DAT6$sm))


attach(DAT7)
cor.test(mmse, CMRO2_Mean)
cor.test(mmse, OEF_Mean)


# remove NAs in WMHvol
DAT7 <- DAT7 %>% drop_na(WMHvol)

attach(DAT7)
pcor.test(mmse, WMHvol, DAT7[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, OEF_Mean, DAT7[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, CMRO2_Mean, DAT7[,c("eTIV","age","sex2")], method="kendall")


# NA 제외한 개수 세기
non_na_count <- sum(!is.na(DAT3$bmi))
print(non_na_count)

# Make table 1
install.packages("tableone")
install.packages("plyr")
library(tableone)
library(plyr)

summary(DAT2)
DAT2$MRdate <- as.Date(DAT2$MRdate, format="%Y-%m-%d")
DAT2 <- DAT2 %>% mutate(across(c(sex, htn, dm, dl, sm), as.factor))
summary(DAT2)

var1<-c("eTIV", "WMHvol", "log.WMHvol", "OEF_Mean", "WB_OEF", "CMRO2_Mean", "log.CMRO2", "WB_CMRO2", "sex", "age", "htn", "dm", "dl", "sm", "bmi", "mmse", "diag_cat", "inf_cat")
var2<-c("WMHvol", "log.WMHvol", "OEF_Mean", "WB_OEF", "CMRO2_Mean", "WB_CMRO2")

tobj<-CreateTableOne(vars=var1, data=DAT2)
summary(tobj, digits=2)
print(tobj, showAllLevels = T)


# Make table 2
install.packages("tableone")
library(tableone)

tobj2<-CreateTableOne(vars=var1, data=DAT3, strata="inf_cat2")
summary(tobj2)
print(tobj2, showAllLevels = TRUE, 
      formatOptions = list(digits = 3, digits.p = 3, scientific = FALSE))

var2<-c("eTIV", "WMHvol", "OEF_Mean", "WB_OEF", "CMRO2_Mean", "WB_CMRO2", "sex", "age", "htn", "dm", "dl", "sm", "bmi", "mmse")
tobj3<-CreateTableOne(vars=var2, data=DAT9, strata="age2")
print(tobj3, showAllLevels = T)


# Make scatterplot
install.packages("ggplot2")
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

ggplot(DAT3,aes(x=mmse,y=WMHvol)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="WMH volume (mm3)")
ggplot(DAT3,aes(x=mmse,y=OEF_Mean)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="OEF")
ggplot(DAT3,aes(x=mmse,y=CMRO2_Mean)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="CMRO2 (μmol/100 g/min)")
ggplot(DAT3,aes(x=mmse,y=age)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="Age") + theme(axis.title = element_text(size=20)) + stat_cor(method = "pearson", label.x = 3, label.y = 2)

ggplot(DAT3,aes(x=mmse,y=log.WMHvol)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="log.WMH volume")
ggplot(DAT3,aes(x=mmse,y=log.CMRO2)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="log.CMRO2")

ggplot(DAT3,aes(x=mmse,y=WB_OEF)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="WB_OEF")
ggplot(DAT3,aes(x=mmse,y=WB_CMRO2)) + geom_point() + stat_smooth(method="lm", se=F) + theme_light() + labs(x="MMSE", y="WB_CMRO2")


ggplot(DAT3, aes(x = mmse, y = WB_CMRO2)) +
  geom_point() +  # 산점도
  geom_smooth(method = "lm", se = FALSE) +  # 회귀선 추가
  stat_cor(method = "pearson", cor.coef.name = "r", label.x = 3, label.y = 1.2, size = 6) +  # 상관계수와 p-value 표시
  labs(x="MMSE", y=expression("WB CMRO"["2"])) +
  theme_light() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
  


# partial correlation test with log
attach(DAT8)
pcor.test(mmse, log.WMHvol, DAT8[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, OEF_Mean, DAT8[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, log.CMRO2, DAT8[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, WB_OEF, DAT8[,c("eTIV","age","sex2")], method="kendall")
pcor.test(mmse, WB_CMRO2, DAT8[,c("eTIV","age","sex2")], method="kendall")



# 세 그룹 이상의 평균 비교
#   --- 설명변수 범주형             : ANOVA (분산분석)
#   --- 설명변수 범주형 + 범주형    : Two-way ANOVA
#   --- 설명변수 범주형 + 연속형    : ANCOVA (공분산분석)

# Adjust group comparison for age and sex
typeof(DAT8$dm)
summary(DAT8)
DAT8$dm <- as.factor(DAT8$dm)
DAT8$htn <- as.factor(DAT8$htn)
DAT8$sm <- as.factor(DAT8$sm)
DAT8$sex <- as.factor(DAT8$sex)
DAT3$diag_cat <- as.integer(DAT3$diag_cat)

# 연속형 범수를 범주형으로
DAT9 <- DAT8 %>% mutate(bmi2=ifelse(bmi>24.39,"medianhigh", "medianlow"), age2=ifelse(age>=65, "old", "young"))


# Multivariable regression analysis -- lm() 혹은 aov() 사용 가능
MRA <- lm(formula = mmse ~ log.WMHvol + WB_OEF + WB_CMRO2 + sex2 + age, data = DAT3)
summary(MRA)

MAOV <- aov(log.WMHvol ~ sm + age + sex, data=DAT9)
MAOV <- aov(OEF_Mean ~ sm + age + sex, data=DAT9)
MAOV <- aov(log.CMRO2 ~ sm + age + sex, data=DAT9)
MAOV <- aov(WB_OEF ~ age2 + sex, data=DAT9)
MAOV <- aov(WB_CMRO2 ~ age2 + sex, data=DAT9)
summary(MAOV)


# Univariable regression analysis -- lm() 혹은 aov() 사용 가능
URA <- lm(formula = mmse ~ WB_CMRO2, data = DAT8)
summary(URA)

#gtsummary
install.packages("gtsummary")
install.packages("glue")
library(gtsummary)

tbl_regression(MRA, 
               conf.level = 0.95, 
               pvalue_fun = ~style_pvalue(., digits=3)) %>% 
  modify_column_merge(pattern = "{estimate} [{ci}]", row=!is.na(estimate)) %>% 
  modify_header(label = "**Variable**", 
                estimate = "**OR (95% CI)**", 
                p.value = "**P value**")


tbl_summary(data=DAT3,
           by=age2)
summary(DAT3)


#stargazer
install.packages("stargazer")
library(stargazer)

stargazer(MRA, type='text', title='Table 3.', out='table3.txt', single.row=TRUE, ci=TRUE, ci.level=0.95)



# Table 3

# Kruskal-Wallis test for OEF
kruskal_OEF <- kruskal.test(OEF_Mean ~ inf_cat3, data = DAT3)
print(kruskal_OEF)

# Kruskal-Wallis test for CMRO2
kruskal_CMRO2 <- kruskal.test(CMRO2_Mean ~ inf_cat3, data = DAT3)
print(kruskal_CMRO2)

# Kruskal-Wallis test for WMH volume
kruskal_WMH <- kruskal.test(WMHvol ~ inf_cat3, data = DAT3)
print(kruskal_WMH)

# Kruskal-Wallis test for age
kruskal_age <- kruskal.test(age ~ inf_cat3, data = DAT3)
print(kruskal_age)

# Kruskal-Wallis test for sex2
kruskal_sex <- kruskal.test(sex2 ~ inf_cat3, data = DAT3)
print(kruskal_sex)


# 사후분석 Dunn's test - 패키지 설치
install.packages("FSA")
library(FSA)

# Dunn's test for age
dunn_age <- dunnTest(age ~ inf_cat3, data = DAT3, method = "bonferroni")
print(dunn_age)

# Dunn's test for sex2
dunn_sex <- dunnTest(sex2 ~ inf_cat3, data = DAT3, method = "bonferroni")
print(dunn_sex)

# Dunn's test for WMHvol
dunn_WMH <- dunnTest(WMHvol ~ inf_cat3, data = DAT3, method = "bonferroni")
print(dunn_WMH)

# Dunn's test for OEF_Mean
dunn_OEF <- dunnTest(OEF_Mean ~ inf_cat3, data = DAT3, method = "bonferroni")
print(dunn_OEF)

# Dunn's test for CMRO2_Mean
dunn_CMRO2 <- dunnTest(CMRO2_Mean ~ inf_cat3, data = DAT3, method = "bonferroni")
print(dunn_CMRO2)



# Age의 평균 및 표준편차 계산
age_summary <- DAT3 %>%
  group_by(inf_cat3) %>%
  summarise(
    Mean_Age = mean(age, na.rm = TRUE),
    SD_Age = sd(age, na.rm = TRUE)
  )
print(age_summary)

# WMH volume의 평균 및 표준편차 계산
WMH_summary <- DAT3 %>%
  group_by(inf_cat3) %>%
  summarise(
    Mean_WMH = mean(WMHvol, na.rm = TRUE),
    SD_WMH = sd(WMHvol, na.rm = TRUE)
  )
print(WMH_summary)

# OEF의 평균 및 표준편차 계산
OEF_summary <- DAT3 %>%
  group_by(inf_cat3) %>%
  summarise(
    Mean_OEF = mean(OEF_Mean, na.rm = TRUE),
    SD_OEF = sd(OEF_Mean, na.rm = TRUE)
  )
print(OEF_summary)

# CMRO2의 평균 및 표준편차 계산
CMRO2_summary <- DAT3 %>%
  group_by(inf_cat3) %>%
  summarise(
    Mean_CMRO2 = mean(CMRO2_Mean, na.rm = TRUE),
    SD_CMRO2 = sd(CMRO2_Mean, na.rm = TRUE)
  )
print(CMRO2_summary)

# Sex 비율 계산
sex_summary <- DAT3 %>%
  group_by(inf_cat3) %>%
  summarise(
    Male_Count = sum(sex == "M", na.rm = TRUE),    # 남성의 수 계산
    Female_Count = sum(sex == "F", na.rm = TRUE), # 여성의 수 계산
    Total = n(),                                        # 총 인원 수
    Male_Percentage = (Male_Count / Total) * 100,       # 남성 비율 계산
    Female_Percentage = (Female_Count / Total) * 100    # 여성 비율 계산
  )
print(sex_summary)

# 숫자 세기
count <- sum(DAT3$inf_cat3 == "recent", na.rm = TRUE)
print(count)


#Fig3 CVD hx에 따라라

# ggboxplot 생성 for WMH volume
a <- ggboxplot(DAT3, x = "inf_cat3", y = "WMHvol2",
               color = "inf_cat3",
               add = "jitter", shape = "inf_cat3", legend.title = "CVD") +
  xlab("CVD history") + labs(y = "WMH volume")

# Dunn's test 결과 추가 for WMH volume
a2 <- a +
  geom_text(aes(x = 1.5, y = 180, label = "no vs old: p < 0.001"), size = 5) +
  geom_text(aes(x = 2, y = 160, label = "no vs recent: p = 0.004"), size = 5) +
  geom_text(aes(x = 2.5, y = 140, label = "old vs recent: p = 0.286"), size = 5) +
  theme(text = element_text(size = 14))

# ggboxplot 생성 for CMRO2
b <- ggboxplot(DAT3, x = "inf_cat3", y = "CMRO2_Mean",
               color = "inf_cat3",
               add = "jitter", shape = "inf_cat3", legend.title = "CVD") +
  xlab("CVD history") + labs(y = expression(CMRO[2]))

# Dunn's test 결과 추가 for CMRO2 (p-value 수정)
b2 <- b +
  geom_text(aes(x = 1.5, y = 25, label = "no vs old: p < 0.001"), size = 5) +
  geom_text(aes(x = 2, y = 23, label = "no vs recent: p = 0.052"), size = 5) +
  geom_text(aes(x = 2.5, y = 21, label = "old vs recent: p = 1"), size = 5) +
  theme(text = element_text(size = 14))

# 그래프 합치기
grid.arrange(a2, b2, ncol = 2)



# Table5

# diag_cat에서 1,2,3,4만 남기고 5,6은 지우기
DAT4 <- DAT3 %>% filter(!diag_cat %in% c(5, 6))

# Kruskal-Wallis test for OEF in diag_cat
kruskal2_OEF <- kruskal.test(OEF_Mean ~ diag_cat, data = DAT4)
print(kruskal2_OEF)

# Kruskal-Wallis test for CMRO2 in diag_cat
kruskal2_CMRO2 <- kruskal.test(CMRO2_Mean ~ diag_cat, data = DAT4)
print(kruskal2_CMRO2)

# Kruskal-Wallis test for WMH volume in diag_cat
kruskal2_WMH <- kruskal.test(WMHvol ~ diag_cat, data = DAT4)
print(kruskal2_WMH)

# Kruskal-Wallis test for age in diag_cat
kruskal2_age <- kruskal.test(age ~ diag_cat, data = DAT4)
print(kruskal2_age)

# Kruskal-Wallis test for sex2 in diag_cat
kruskal2_sex <- kruskal.test(sex2 ~ diag_cat, data = DAT4)
print(kruskal2_sex)


# Dunn's test for age in diag_cat
dunn2_age <- dunnTest(age ~ diag_cat, data = DAT4, method = "bonferroni")
print(dunn2_age)

# Dunn's test for sex2 in diag_cat
dunn2_sex <- dunnTest(sex2 ~ diag_cat, data = DAT4, method = "bonferroni")
print(dunn2_sex)

# Dunn's test for WMHvol in diag_cat
dunn2_WMH <- dunnTest(WMHvol ~ diag_cat, data = DAT4, method = "bonferroni")
print(dunn2_WMH)

# Dunn's test for OEF_Mean in diag_cat
dunn2_OEF <- dunnTest(OEF_Mean ~ diag_cat, data = DAT4, method = "bonferroni")
print(dunn2_OEF)

# Dunn's test for CMRO2_Mean in diag_cat
dunn2_CMRO2 <- dunnTest(CMRO2_Mean ~ diag_cat, data = DAT4, method = "bonferroni")
print(dunn2_CMRO2)


# Age의 평균 및 표준편차 계산
age_summary2 <- DAT3 %>%
  group_by(diag_cat) %>%
  summarise(
    Mean_Age2 = mean(age, na.rm = TRUE),
    SD_Age2 = sd(age, na.rm = TRUE)
  )
print(age_summary2)

# WMH volume의 평균 및 표준편차 계산
WMH_summary2 <- DAT3 %>%
  group_by(diag_cat) %>%
  summarise(
    Mean_WMH2 = mean(WMHvol, na.rm = TRUE),
    SD_WMH2 = sd(WMHvol, na.rm = TRUE)
  )
print(WMH_summary2)

# OEF의 평균 및 표준편차 계산
OEF_summary2 <- DAT3 %>%
  group_by(diag_cat) %>%
  summarise(
    Mean_OEF2 = mean(OEF_Mean, na.rm = TRUE),
    SD_OEF2 = sd(OEF_Mean, na.rm = TRUE)
  )
print(OEF_summary2)

# CMRO2의 평균 및 표준편차 계산
CMRO2_summary2 <- DAT3 %>%
  group_by(diag_cat) %>%
  summarise(
    Mean_CMRO22 = mean(CMRO2_Mean, na.rm = TRUE),
    SD_CMRO22 = sd(CMRO2_Mean, na.rm = TRUE)
  )
print(CMRO2_summary2)

# Sex 비율 계산
sex_summary2 <- DAT3 %>%
  group_by(diag_cat) %>%
  summarise(
    Male_Count2 = sum(sex == "M", na.rm = TRUE),    # 남성의 수 계산
    Female_Count2 = sum(sex == "F", na.rm = TRUE), # 여성의 수 계산
    Total = n(),                                        # 총 인원 수
    Male_Percentage2 = (Male_Count2 / Total) * 100,       # 남성 비율 계산
    Female_Percentage2 = (Female_Count2 / Total) * 100    # 여성 비율 계산
  )
print(sex_summary2)



#Fig4 dementia dx에 따라라

# diag_cat 변수를 factor로 변환하고 수준 이름 변경
DAT4$diag_cat <- factor(DAT4$diag_cat, 
                        levels = c("1", "2", "3", "4"), 
                        labels = c("SCI", "MCI", "AD", "VD"))

# ggboxplot 생성 for WMH volume
c <- ggboxplot(DAT4, x = "diag_cat", y = "WMHvol2",
               color = "diag_cat",
               add = "jitter", shape = "diag_cat", legend.title = "Dementia") +
  xlab("Types of dementia") + labs(y = "WMH volume")

c
my_comparisons <- list( c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))

c2 <- c + stat_compare_means(comparisons = my_comparisons, size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 250, size =5) +  # Add global p-value
  theme(text = element_text(size = 14))

# ggboxplot 생성 for CMRO2
d <- ggboxplot(DAT4, x = "diag_cat", y = "CMRO2_Mean",
               color = "diag_cat",
               add = "jitter", shape = "diag_cat", legend.title = "Dementia") +
  xlab("Types of dementia") + labs(y = expression(CMRO[2]))

d
my_comparisons <- list( c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))

d2 <- d + stat_compare_means(comparisons = my_comparisons, size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 34, size =5) +  # Add global p-value
  theme(text = element_text(size = 14))

# 그래프 합치기
grid.arrange(c2, d2, ncol = 2)