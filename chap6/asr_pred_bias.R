library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#Modifier le working directory 
#setwd()

# Expérience sur l'existence de biais prédicitfs genrés dans les systèmes d'ASR
# Chapitre 6 de la thèse de Mahault GARNERIN

##################################################### Imports des données


spk_train <- read_delim("./data/spk_all_info_train.csv", 
                        "\t",
                        escape_double = FALSE,
                        col_types = cols(gender = col_factor(levels = c("female", "male")),
                                         id_show = col_factor(levels = c("2","8", "9", "10", "13", "19", "22", "26")),
                                         spk_class = col_factor(levels = c("4","1", "3", "2"))),
                        trim_ws = TRUE)

spk_test <- read_delim("./data/spk_all_info_test.csv",
                       ";",
                       escape_double = FALSE, 
                       col_types = 
                         cols(gender = col_factor(levels = c("femme", "homme")),
                              id_show = col_factor(levels = seq(1:26)),
                              spk_class = col_factor(levels = c("4","1","3", "2"))),
                       trim_ws = TRUE)

showid2name <- read_delim("./data/mapping_files/showid2name.csv", 
                          ";", 
                          escape_double = FALSE, 
                          col_types = cols(id_show = col_character()), 
                          trim_ws = TRUE)

eval_data <- read_delim("./data/wer_by_episode.csv",
                        "\t", escape_double = FALSE,
                        col_types = cols(
                          gender = col_factor(levels = c("femme", "homme")),
                          id_show = col_factor(levels = seq(1:26)), 
                          spk_class = col_factor(levels = c("4","1", "3", "2"))), 
                        trim_ws = TRUE)

# Matching des index de show avec les noms d'émission 
eval_data <-left_join(eval_data,showid2name, by = "id_show")

# Fusion des catégories de rôles 2 & 3 (catégorie Autres)
spk_train$spk_class[spk_train$spk_class==3]<-2

#N.B Les différents niveau de rôles du locuteur ou de la locutrice sont :
# 1 : Ponctuel·les
# 4 : Ancres
# 2 & 3 : avaient été classé comme Autres et Experts d'après le travail de master (voir Garnerin, 2018) mais ces catégories 
# n'ont pas été utilisées ni analysées dans le présent travail et sont réunis sous la catégorie Autres.


############################# DESCRIPTION DU CORPUS D'APPRENTISSAGE (TRAIN) 

### Calcul des taux de présence :

#Par catégorie de genre
table(spk_train$gender, useNA="always")

#Par rôle :
table(spk_train$spk_class)

#Par catégorie de genre et rôle
table(spk_train$gender,spk_train$spk_class, useNA="always")


### Calcul des temps de parole (et taux d'expression) dans le corpus d'apprentissage 

# Par catégorie de genre
train_speech_time_by_gender <- spk_train   %>%   group_by(gender)   %>%   summarise(length = sum(total_length),
                                                                          period = seconds_to_period(length))
train_speech_time_by_gender$percent <- train_speech_time_by_gender$length*100/sum(train_speech_time_by_gender$length)

#Par rôle
train_speech_time_by_class <- spk_train %>% group_by(spk_class) %>% summarise(length = sum(total_length),
                                                                              period = seconds_to_period(length))
train_speech_time_by_class$percent <- train_speech_time_by_class$length*100/sum(train_speech_time_by_class$length)

#Par catégorie de genre et rôle
train_speech_time_by_class_gender <- spk_train %>% group_by(spk_class,gender) %>% summarise(length = sum(total_length),
                                                                              period = seconds_to_period(length))

## Visualisation graphique : 

#NB loc
ggplot(spk_train, aes(x=spk_class, fill=gender)) + 
  geom_bar(stat="count",position="dodge") +
  labs(x="", y = "Nombre de loc.", fill="Genre") +
  scale_y_continuous()+
  scale_x_discrete(labels = c("Ancres","Ponctuel·les","Autres"))+
  scale_fill_manual(labels = c("F","H","NA"),values=c("indianred2","tomato4"),na.value="darksalmon")


#Temps de parole
ggplot(train_speech_time_by_class_gender, aes(x=spk_class)) + 
  geom_bar(aes(x=spk_class, y=length/3600, fill = gender), stat="identity", position ="dodge") +
  labs(x="",y="Heures de parole",fill="Genre") +
  scale_x_discrete(labels = c("Ancres","Ponctuel·les","Autres"))+
  scale_fill_manual(labels = c("F","H","NA"),values=c("indianred2","tomato4"),na.value="darksalmon")


#Durée de parole pour chaque émission (correspond à la Table 5.2 du manuscrit)
train_show_lengths <- spk_train   %>%   group_by(id_show)   %>%   summarise(length = sum(total_length),
                                                                            period = seconds_to_period(length))
# Fusion avec les noms d'émissions (au lieu des index)
train_full<-left_join(train_show_lengths, showid2name, by = "id_show")


#Visualisation des quantités de données (par ordre décroissant)
ggplot(train_full,aes(x=reorder(name,-length),y=length))+
  geom_bar(stat="identity")+
  labs(x="Shows", y = "Total length (in hours)")+
  scale_y_continuous(breaks=seq(0,133200,18000),labels=seq(0,37,5))+
  theme(panel.background = element_rect(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype=3),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1))


############################# DESCRIPTION DU CORPUS DE TEST 

### Calcul des taux de présence :

#Par catégorie de genre
table(spk_test$gender, useNA="always")

#Par rôle :
table(spk_test$spk_class)

#Par catégorie de genre et rôle
table(spk_test$gender,spk_test$spk_class, useNA="always")


#Calcul des temps de parole dans le corpus de test
test_speech_time <- spk_test   %>%   group_by(gender)   %>%   summarise(length = sum(total_length))
test_speech_time$percent <- test_speech_time$length/sum(test_speech_time$length)

test_speech_time_by_class <- spk_test %>% group_by(spk_class) %>% summarise(length = sum(total_length))
test_speech_time_by_class$period=seconds_to_period(test_speech_time_by_class$length)
test_speech_time_by_class$percent <- test_speech_time_by_class$length/sum(test_speech_time_by_class$length)


test_speech_time_by_class_gender <- spk_test %>% group_by(gender,spk_class) %>% summarise(length = sum(total_length))
test_speech_time_by_class_gender$period=seconds_to_period(test_speech_time_by_class_gender$length)
test_speech_time_by_class_gender$percent <- test_speech_time_by_class_gender$length/sum(test_speech_time_by_class_gender$length)


#### Description par émission 


#Temps de parole (Table 5.2 du manuscrit)
test_show_lengths <- spk_test   %>%   group_by(id_show)   %>%   summarise(length = sum(total_length),
                                                                               period = seconds_to_period(length))

# Fusion avec les noms d'émissions (au lieu des index)
test_full<-left_join(test_show_lengths, showid2name, by = "id_show")


#Visualisation des quantités de données (par ordre décroissant)
ggplot(test_full,aes(x=reorder(name,-length),y=length))+
  geom_bar(stat="identity")+
  labs(x="Shows", y = "Total length (in hours)")+
  scale_y_continuous(breaks=seq(0,133200,18000),labels=seq(0,37,5))+
  theme(panel.background = element_rect(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype=3),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1))


######################################## Analyse des performances

#Comme indiqué dans notre manuscrit nous n'analysons les résultats que pour les catégories Ancres et Ponctuel·les
eval_data_trim <- subset(eval_data, eval_data$spk_class %in% c('1','4'))

#Performances globales
wer_summary <- eval_data_trim %>% summarise(n = n(),
                                            meanWER= mean(WER),
                                            medWER = median(WER))

#Performances globales par genre
wer_summ_gender <- eval_data_trim %>% group_by(gender) %>% summarise(n = n(),
                                                                     meanWER= mean(WER),
                                                                     medWER = median(WER))

#Performances globales par rôle
wer_summ_class <- eval_data_trim %>% group_by(spk_class) %>% summarise(n = n(),
                                                                     meanWER= mean(WER),
                                                                     medWER = median(WER))

#Performances globales par émissions (Tableau 6.5)
wer_summ_show <- eval_data_trim %>% group_by(name) %>% summarise(n = n(),
                                                                 meanWER= mean(WER),
                                                                 medWER = median(WER))

#Performances par émissions et rôle
wer_summ_show_class <- eval_data_trim %>% group_by(name,spk_class) %>% summarise(n = n(),
                                                                                 meanWER= mean(WER),
                                                                                 medWER = median(WER))

#Performances par émissions et genre
wer_summ_show_gender <- eval_data_trim %>% group_by(name,gender) %>% summarise(n = n(),
                                                                              meanWER= mean(WER),
                                                                              medWER = median(WER))

#Performances par émissions, rôle, genre (Table B.1)
wer_summary_show_class_gender <- eval_data_trim %>% group_by(name,spk_class,gender) %>% summarize(n = n(),
                                                                                                  meanWER = mean(WER), 
                                                                                                  medWER = median(WER))


wer_summ_class_gender <- eval_data_trim %>% group_by(spk_class,gender) %>% summarise(n = n(),
                                                                     meanWER= mean(WER),
                                                                     medWER = median(WER))


## Test statistiques

#WER en fonction du genre
wilcox.test(WER~gender,data=eval_data_trim,paired=F)

#Wer en fonction du rôle
wilcox.test(WER~spk_class,data=eval_data_trim,paired=F)

eval_punct <- subset(eval_data_trim, eval_data_trim$spk_class == '1')
wilcox.test(WER~gender,data=eval_punct,paired=F)

eval_anch <- subset(eval_data_trim, eval_data_trim$spk_class == '4')
wilcox.test(WER~gender,data=eval_anch,paired=F)


#Visualisation graphique

#Distribution des WER par émission
ggplot(eval_data_trim,aes(x=reorder(name,WER),y=WER*100))+
  geom_boxplot()+
  labs(title="",
       y="WER",
       x="Émissions")+
  theme(text = element_text(size=18))+
  scale_y_continuous(lim=c(0,100))+
  theme(panel.background = element_rect(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype=3),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1))


# Distributions des WER par émissions par catégorie de genre
ggplot(eval_data_trim,aes(x=reorder(name,WER),y=WER*100, fill=factor(gender,labels=c("Femmes","Hommes"))))+
  geom_boxplot()+
  labs(title="",
       y="WER",
       x="",
       fill="Genre")+
  theme(text = element_text(size=18))+
  scale_y_continuous(lim=c(0,100))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  theme(panel.background = element_rect(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype=3),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1))


# Distributions des WER par émissions par rôle
ggplot(eval_data_trim,aes(x=reorder(name,WER),y=WER*100, fill = factor(spk_class,labels=c("Ancres","Ponctuel·les"))))+
  geom_boxplot()+
  labs(title="",
       y="WER",
       x="",
       fill="Rôle")+
  theme(text = element_text(size=18))+
  scale_y_continuous(lim=c(0,100))+
  scale_fill_manual(values=c("skyblue","royalblue"))+
  theme(panel.background = element_rect(), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype=3),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1))


##### Analyse en fonction du type de parole (préparée vs spontanée)


#La classification des émissions en préparée/spontanée est reprise du travail d'Elloumi et al., 2018.
show_level<-as.data.frame(
  cbind(c("Africa1 Infos","Comme On Nous Parle","Culture et Vous","La Place du Village","Le Masque et la Plume","Pile et Face","PlanĂ¨te Showbiz","RFI Infos","RTM Infos","Service Public","TVME Infos","Un Temps de Pauchon"),
        c(1,5,6,11,15,17,18,19,20,21,23,24),
        c("P","S","S","S","S","P","S","P","P","S","P","S")))
colnames(show_level) <- c("show","id_show","level")
show_level$level<-as.factor(show_level$level)
show_level$id_show<-as.factor(show_level$id_show)

eval_data_speech_type<-(left_join(eval_data_trim,show_level, by = "id_show"))

spont_eval<-subset(eval_data_speech_type,level=="S")
prep_eval<-subset(eval_data_speech_type,level=="P")

#Performances par type de parole 
wer_style <- eval_data_speech_type %>% group_by(level) %>% summarise(n = n(),
                                                                     meanWER= mean(WER),
                                                                     medWER = median(WER),
                                                                     sd = sd(WER))
#Performances par type de parole et genre
wer_style_gender <- eval_data_speech_type %>% group_by(gender,level) %>% summarise(n = n(),
                                                                             meanWER= mean(WER),
                                                                             medWER = median(WER),
                                                                             sd = sd(WER))
#Performances par catégorie de genre et rôle pour la parole spontanée 
spont_wer <- spont_eval   %>%   group_by(gender,spk_class)   %>%   summarise(n = n(),
                                                                             meanWER= mean(WER),
                                                                             medWER = median(WER),
                                                                             sd = sd(WER))

#Performances par catégorie de genre et rôle pour la parole préparée 
prep_wer <- prep_eval   %>%   group_by(gender,spk_class)   %>%   summarise(n = n(),
                                                                           meanWER= mean(WER),
                                                                           medWER = median(WER),
                                                                           sd = sd(WER))


#Test statistiques

wilcox.test(WER ~ gender, data = spont_eval, paired = F)
wilcox.test(WER ~ gender, data = prep_eval, paired = F)

#Visualisation graphique

ggplot(eval_data_speech_type, aes(x=factor(level,labels=c("Préparée","Spontanée")), y=WER*100)) +
  geom_boxplot(aes(fill=factor(gender,labels=c("Femmes","Hommes")))) + 
  theme(text = element_text(size=18))+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Type de parole",
       fill="Genre")+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))







