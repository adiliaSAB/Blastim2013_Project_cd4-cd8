library(tidyverse)
Tcell <- read.csv2("data/TCell.csv")
library(readxl)
codebook <- read_xlsx("data/codebook_EXHALE.xlsx")

Tcell

Tcell_range <- Tcell %>%
  mutate(cd4_cd8_ratio_range = case_when( 
    cd4_cd8_ratio < 0.4 ~ "<0.4", 
    cd4_cd8_ratio > 1 ~ ">1",
    .default = "0.4-1" )) %>% 
  relocate(cd4_cd8_ratio_range, .after = cd4_cd8_ratio) %>%

  mutate(emphysema_severity_group = case_when(   
    emphysema_severity == c(2:5) ~ ">10%",
    .default = "≤10% ",
    is.na(emphysema_severity) ~ NA_character_)) %>% 
  relocate(emphysema_severity_group, .after = emphysema_severity)

Tcell_range

# Эксплоративный анализ данных по группам cd4/cd8
library(ggplot2)

mean(cd4_cd8_ratio)
median(cd4_cd8_ratio)
mean(cd4_cd8_ratio, trim = 0.1)

range(cd4_cd8_ratio)
diff(range(cd4_cd8_ratio))

var(cd4_cd8_ratio)
sd(cd4_cd8_ratio) 

z_score <- function(x) (x - mean(x))/sd(x)
z_score(cd4_cd8_ratio)

mad(cd4_cd8_ratio)
IQR(cd4_cd8_ratio)

library("psych")
skew(cd4_cd8_ratio)
kurtosi(cd4_cd8_ratio)

psych::describe(cd4_cd8_ratio)

Tcell_range %>% reframe(describe(cd4_cd8_ratio))

Tcell_range %>% 
  group_by(cd4_cd8_ratio_range) %>% 
  reframe(describe(cd4_cd8_ratio))
  
  
library("skimr") 

Tcell_stat <- Tcell_range %>% 
  group_by(cd4_cd8_ratio_range) %>% 
  skimr::skim()


# GGPlot :: графики -------------------------------------------------------

library(ggplot2)
library(RColorBrewer)

#DONE--cd4/cd8 groups
ggplot() +
  geom_bar(data = Tcell_range,
           aes(x = "", 
               fill = factor(cd4_cd8_ratio_range, 
                             levels = c("<0.4", "0.4-1", ">1")))) +
  coord_polar(theta = "y") +
  labs(title = "cd4/cd8 values") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")

#DONE--age_at_enrollment
ggplot(data = Tcell_range,
       aes(x = cd4_cd8_ratio_range,
           y = age_at_enrollment,
           fill = factor(cd4_cd8_ratio_range,
                         levels = c("<0.4", "0.4-1", ">1")))) +
  
  geom_violin(scale = "count", 
              adjust = 0.5) +
  geom_boxplot(alpha = 0,
               show.legend = FALSE) +
  
  labs(title = "Age of participants, by cd4/cd8") +
  xlab("cd4/cd8") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_classic() +
  scale_x_discrete(limits=c("<0.4", "0.4-1", ">1")) +
  scale_fill_brewer(palette = "Dark2")


#DONE--Sex
ggplot(data = Tcell_range) +   
  geom_bar(aes(y = sex, fill = factor(cd4_cd8_ratio_range,
                                      levels = c("<0.4", "0.4-1", ">1"))),
           position = "fill") +
  labs(title = "Gender of participant, by cd4/cd8",
       x = NULL, y = "Gender") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_classic()  +
  theme(legend.position = "bottom") +
  scale_x_continuous() +
  scale_y_continuous(breaks = c(0, 1), label = c("female", "male")) +
  scale_fill_brewer(palette = "Dark2")

#DONE--race_ethnicity
ggplot(data = Tcell_range) +
  geom_bar(aes(x = cd4_cd8_ratio_range, fill = race_ethnicity),
           position = "dodge")  +
  labs(title = "Race ethnicity, by cd4/cd8",
       x = "cd4/cd8", y = "Race ethnicity") +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2")

#cd4_cd8_ratio: BMI
#ggplot(data = Tcell_range, aes(x = cd4_cd8_ratio_range, y = BMI)) +
 # geom_density(alpha=0.3)() +
  #labs(title = "BMI by cd4/cd8",
       #x = "cd4/cd8", y = "BMI") +
  #theme_classic()

#Tobacco

#DONE--Inhalation
ggplot(data = Tcell_range) +   
geom_bar(aes(y = inhalational_drugs, fill = factor(cd4_cd8_ratio_range,
                                    levels = c("<0.4", "0.4-1", ">1"))),
         position = "fill") +
  labs(title = "Inhalation, by cd4/cd8",
       x = NULL, y = "Inhalation") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_classic()  +
  theme(legend.position = "bottom") +
  scale_x_continuous() +
  scale_y_continuous(breaks = c(0, 1), label = c("no", "yes")) +
  scale_fill_brewer(palette = "Dark2")

#DONE--iv drug use
ggplot(data = Tcell_range) +   
  geom_bar(aes(y = ivdu, fill = factor(cd4_cd8_ratio_range,
                                       levels = c("<0.4", "0.4-1", ">1"))),
           position = "fill") +
  labs(title = "iv drug use, by cd4/cd8",
       x = NULL, y = "ivdu") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_classic()  +
  theme(legend.position = "bottom") +
  scale_x_continuous() +
  scale_y_continuous(breaks = c(0, 1), label = c("no", "yes")) +
  scale_fill_brewer(palette = "Dark2")


# ЗАДАНИЕ  1 --------------------------------------------------------------

#Корреляция cd4/cd8--Emph

Tcell %>% 
  select(cd4_cd8_ratio, emphysema_severity) %>% 
  drop_na() %>% 
  cor()

cor.test(cd4_cd8_ratio, emphysema_severity)


# Линейная регрессия cd4/cd8--Emph

Tcell_lm <- lm(cd4_cd8_ratio ~ emphysema_severity, data = Tcell_range)  

summary(Tcell_lm)

#Tcell_lm$residuals <-  residuals(Tcell_lm) #R-test
#Tcell_lm$fitted <-  fitted(Tcell_lm)

#Tcell_range %>% 
 #transmute(cd4_cd8_ratio - (Tcell_lm$residuals + Tcell_lm$fitted))

#rss <- sum(Tcell_lm$residuals^2) #Необъясненная регрессия
#rss
#tss <- sum(Tcell_range$cd4_cd8_ratio - mean(Tcell_range$cd4_cd8_ratio))^2
#tss
#1-rss/tss

ggplot(data = Tcell_range,
       aes(x=cd4_cd8_ratio, y=emphysema_severity)) +
  geom_point(aes(colour = cd4_cd8_ratio_range),
             position = "jitter",
             alpha = .8) +
  geom_smooth(method = "lm",
              colour = "#BA1825",
              size = .5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3)) +
  theme_classic() +
  ggtitle("Emphysema severity change")
  

#Корреляция Emph--multiple factors

#Dummy coding
Tcell_ANOVA <-  Tcell_range %>% 
  mutate(former_smoker = ifelse(smoking_status == "former smoker", 1, 0),
         never_smoker = ifelse(smoking_status == "never smoker", 1, 0),
         current_smoker = ifelse(smoking_status == "current smoker", 1, 0),
         
         black = ifelse(race_ethnicity == "Black", 1, 0),
         hispanic = ifelse(race_ethnicity == "Hispanic", 1, 0),
         white = ifelse(race_ethnicity == "White", 1, 0),
         other = ifelse(race_ethnicity == "Other", 1, 0)) %>% 
  relocate(c(black, white, hispanic, other), 
           .after = race_ethnicity) %>%      
  relocate(c(current_smoker, former_smoker,never_smoker), 
           .after = smoking_status)  

Tcell_ANOVA

Cor_matrix <- Tcell_ANOVA %>% 
  select(emphysema_severity,  age_at_enrollment, sex, 
         black, hispanic, white, other, BMI,
         former_smoker, never_smoker, current_smoker, pack_years,
         dm, htn, anemia, chronic_heart_disease, tb, pcp, pneu,
         inhalational_drugs, ivdu, cd4, hivrna, nadir_cd4, s_cd14) %>% 
  drop_na()

cor(Cor_matrix)











