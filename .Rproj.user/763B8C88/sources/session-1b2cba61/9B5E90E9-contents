dnorm(0, mean = 0, sd = 1) #dencity - непрерывное распределение

drop_na() %>% 
  na.omit()

geom_abline

#emphysema_severity[!is.na(emphysema_severity)]
linelist %>% drop_na(outcome

# -------------------------------------------------------------------------

# Эксплоративный графический анализ данных по группам CD4/CD8 -------------


hist(cd4_cd8_ratio, breaks = 30) 
#plot(cd4_cd8_ratio, emphysema_severity)  #Позже
#boxplot(cd4_cd8_ratio_range ~ emphysema_severity, Tcell_range)

library(ggplot2)
library(RColorBrewer)

#DONEcd4/cd8 по группам 
ggplot() +
  geom_bar(data = Tcell_range,
           aes(x = "", 
               fill = factor(cd4_cd8_ratio_range, 
                             levels = c("<0.4", "0.4-1", ">1")))) +
  coord_polar(theta = "y") +
  labs(title = "Распределение по группам cd4/cd8") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_void() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2")


#DONEage_at_enrollment
ggplot(data = Tcell_range,
       aes(x = cd4_cd8_ratio_range,
           y = age_at_enrollment,
           fill = factor(cd4_cd8_ratio_range,
                         levels = c("<0.4", "0.4-1", ">1")))) +
  
  geom_violin(scale = "count", 
              adjust = 0.5) +
  geom_boxplot(alpha = 0,
               show.legend = FALSE) +
  
  labs(title = "Возраст добровольцев",
       subtitle = "по группам cd4/cd8") +
  xlab("cd4/cd8") +
  guides(fill = guide_legend(title = "cd4/cd8")) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2")


#Sex
ggplot(data = Tcell_range) +
  geom_bar(aes(y = sex, fill = cd4_cd8_ratio_range),
           position = "fill") +
  theme_classic()  +
  scale_fill_brewer(palette = "Dark2")

#DONEcd4_cd8_ratio: race_ethnicity

ggplot() +
  geom_bar(data = Tcell_range,
           aes(x = cd4_cd8_ratio_range, fill = race_ethnicity),
           position = "dodge")  +
  labs(title = "Этнические группы",
       subtitle = "Распределение по уровням cd4/cd8") +
  xlab("cd4/cd8") +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

#cd4_cd8_ratio:BMI
ggplot()


#cd4_cd8_ratio:smoking_status & pack-years ???
ggplot() +
  geom_bar(data = Tcell_range,
           aes(x = cd4_cd8_ratio_range, fill = smoking_status),
           position = "dodge") +
  labs(title = "Статус курильщика",
       subtitle = "Распределение уровней cd4/cd8") +
  #guides(fill = guide_legend(title = "cd4/cd8")) +      #как побписать НА в легенде?
  xlab("cd4/cd8") +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

#cd4_cd8_ratio:chronic_heart_disease
ggplot() +
  geom_bar(data = Tcell_range,
           aes(x = chronic_heart_disease,fill = cd4_cd8_ratio_range),
           position = "dodge") +
  labs(title = "Хронические болезни сердца",
       subtitle = "Распределение уровней cd4/cd8") +
  guides(fill = guide_legend(title = "cd4/cd8")) + 
  #xlab("cd4/cd8") +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")

install.packages("installr")
library(installr)
updateR()

as.numeric(cd4_cd8_ratio_range, c("<0.4", "0.4-1", ">1"))
