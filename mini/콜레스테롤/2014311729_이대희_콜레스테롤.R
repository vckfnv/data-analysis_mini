chol <- read.csv("NHIS_OPEN_GJ_2017.csv")
library(dplyr)
library(ggplot2)

##상관관계
#결측치제거
chol<-chol %>% select(-치아우식증유무,-결손치유무,-치아마모증유무,-사랑니이상,-치석)
chol_noNA<- na.omit(chol)
chol_cor <- cor(chol_noNA)
round(chol_cor)
library(corrplot)
corrplot(chol_cor)

##수축기혈압
chol1<- chol %>% 
  filter(!is.na(수축기혈압)) %>% 
  mutate(bloodpressure수축 = ifelse(수축기혈압>=140,"수축 140이상", "수축 140미만")) %>%
  select(bloodpressure수축, LDL콜레스테롤)
chol1_1<- chol1 %>%
  filter(bloodpressure수축 == "수축 140이상")
chol1_2<- chol1 %>%
  filter(bloodpressure수축 == "수축 140미만")
#boxplot stat보기
boxplot(chol1_1$LDL콜레스테롤)$stats#204
boxplot(chol1_2$LDL콜레스테롤)$stats#201기
#이상치 제거
chol1$LDL콜레스테롤 <- ifelse(chol1$LDL콜레스테롤>201 & chol1$bloodpressure수축 == "수축 140미만",
                         NA, chol1$LDL콜레스테롤) 
chol1$LDL콜레스테롤 <- ifelse(chol1$LDL콜레스테롤>204 & chol1$bloodpressure수축 == "수축 140이상",
                         NA, chol1$LDL콜레스테롤) 

chol1<- chol1 %>% 
  filter(!is.na(LDL콜레스테롤)) 
#박스플롯 생성
ggplot(data = chol1, aes(x=bloodpressure수축, y = LDL콜레스테롤)) + geom_boxplot()

##이완기 혈압
chol2<- chol %>% 
  filter(!is.na(이완기혈압)) %>% 
  mutate(bloodpressure이완 = ifelse(이완기혈압>=90,"이완 90이상", "이완 90미만")) %>%
  select(bloodpressure이완, LDL콜레스테롤)
chol2_1<- chol2 %>%
  filter(bloodpressure이완 == "이완 90이상")
chol2_2<- chol2 %>%
  filter(bloodpressure이완 == "이완 90미만")
#boxplot stat보기
boxplot(chol2_1$LDL콜레스테롤)$stats#211
boxplot(chol2_2$LDL콜레스테롤)$stats#201
#이상치 제거
chol2$LDL콜레스테롤 <- ifelse(chol2$LDL콜레스테롤>211 & chol2$bloodpressure이완 == "이완 90이상",
                         NA, chol2$LDL콜레스테롤) 
chol2$LDL콜레스테롤 <- ifelse(chol2$LDL콜레스테롤>201 & chol2$bloodpressure이완 == "이완 90미만",
                         NA, chol2$LDL콜레스테롤) 

chol2<- chol2 %>% 
  filter(!is.na(LDL콜레스테롤)) 

#박스플롯 생성
ggplot(data = chol2, aes(x=bloodpressure이완, y = LDL콜레스테롤)) + geom_boxplot()

##남자45세
chol3<- chol %>% 
  filter(!is.na(연령대코드)) %>%
  filter(성별코드 ==1) %>% 
  mutate(men45 = ifelse(연령대코드>=6,"45세 이상", "45세 미만")) %>%
  select(men45, LDL콜레스테롤)
chol3_1<- chol3 %>%
  filter(men45 == "45세 이상")
chol3_2<- chol3 %>%
  filter(men45 == "45세 미만")
#boxplot stat보기
boxplot(chol3_1$LDL콜레스테롤)$stats#204
기boxplot(chol3_2$LDL콜레스테롤)$stats#164
#이상치 제거
chol3$LDL콜레스테롤 <- ifelse(chol3$LDL콜레스테롤>204 & chol3$men45 == "45세 이상",
                         NA, chol3$LDL콜레스테롤) 
chol3$LDL콜레스테롤 <- ifelse(chol3$LDL콜레스테롤>201 & chol3$men45 == "45세 미만",
                         NA, chol3$LDL콜레스테롤) 

chol3<- chol3 %>% 
  filter(!is.na(LDL콜레스테롤)) 
#박스플롯 생성
ggplot(data = chol3, aes(x=men45, y = LDL콜레스테롤)) + geom_boxplot()
###########################################

#여자55세
chol4<- chol %>% 
  filter(!is.na(연령대코드)) %>%
  filter(성별코드 ==2) %>% 
  mutate(women55 = ifelse(연령대코드>=8,"55세 이상", "55세 미만")) %>%
  select(women55, LDL콜레스테롤)
chol4_1<- chol4 %>%
  filter(women55 == "55세 이상")
chol4_2<- chol4 %>%
  filter(women55 == "55세 미만")
#boxplot stat 보기
boxplot(chol4_1$LDL콜레스테롤)$stats#208
boxplot(chol4_2$LDL콜레스테롤)$stats#167
#이상제 젝거
chol4$LDL콜레스테롤 <- ifelse(chol4$LDL콜레스테롤>208 & chol4$women55 == "55세 이상",
                         NA, chol4$LDL콜레스테롤) 
chol4$LDL콜레스테롤 <- ifelse(chol4$LDL콜레스테롤>167 & chol4$women55 == "55세 미만",
                         NA, chol4$LDL콜레스테롤) 

chol4<- chol4 %>% 
  filter(!is.na(LDL콜레스테롤)) 
#박스플롯 생성
ggplot(data = chol4, aes(x=women55, y = LDL콜레스테롤)) + geom_boxplot()

##BMI
#BMI추가
chol5 <-chol %>% 
  select(신장,체중,LDL콜레스테롤) %>% 
  mutate(BMI = 체중/((신장*0.01)^2))
chol5$BMI <- ifelse(chol5$BMI>=40,"고도 비만",
                    ifelse(chol5$BMI>30,"비만",
                           ifelse(chol5$BMI>25, "과체중",
                                  ifelse(chol5$BMI>18.5,"정상","저체중"))))
chol5<-na.omit(chol5)
#BMI별 그룹화
chol5<-chol5 %>% 
  group_by(BMI) %>% 
  summarise(mean_ldl = mean(LDL콜레스테롤))
#geom_col 생성
ggplot(data = chol5, aes(x=BMI, y = mean_ldl)) + geom_col() +
  scale_x_discrete(limits = c("저체중","정상","과체중","비만","고도 비만"))
