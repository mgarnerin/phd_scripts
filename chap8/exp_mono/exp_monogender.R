library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

#Modifier le working directory 
#setwd()

# Expérience sur la comparaison des WER par catégorie de genre dans le cas de systèmes monogenre.
# Chapitre 8 de la thèse de Mahault GARNERIN

##################################################### Préparation des données

#Import data sets

clean_male <- read_delim("./data/wer_utt_test_clean_male_only.csv", 
                          "\t", 
                          escape_double = FALSE, 
                          col_types = cols(`#C` = col_integer(),
                                           `#D` = col_integer(),
                                           `#I` = col_integer(), 
                                           `#S` = col_integer(),
                                           book = col_character(),
                                           err = col_integer(),
                                           gender = col_factor(levels = c("F","M")),
                                           spk = col_character(),
                                           wc = col_integer()),
                          trim_ws = TRUE)


other_male <- read_delim("./data/wer_utt_test_other_male_only.csv", 
                          "\t", 
                          escape_double = FALSE, 
                          col_types = cols(`#C` = col_integer(),
                                           `#D` = col_integer(),
                                           `#I` = col_integer(), 
                                           `#S` = col_integer(),
                                           book = col_character(),
                                           err = col_integer(),
                                           gender = col_factor(levels = c("F","M")),
                                           spk = col_character(),
                                           wc = col_integer()),
                          trim_ws = TRUE)


clean_female <- read_delim("./data/wer_utt_test_clean_female_only.csv", 
                            "\t", 
                            escape_double = FALSE, 
                            col_types = cols(`#C` = col_integer(),
                                             `#D` = col_integer(),
                                             `#I` = col_integer(), 
                                             `#S` = col_integer(),
                                             book = col_character(),
                                             err = col_integer(),
                                             gender = col_factor(levels = c("F","M")),
                                             spk = col_character(),
                                             wc = col_integer()),
                            trim_ws = TRUE)


other_female <- read_delim("./data/wer_utt_test_other_female_only.csv", 
                            "\t", 
                            escape_double = FALSE, 
                            col_types = cols(`#C` = col_integer(),
                                             `#D` = col_integer(),
                                             `#I` = col_integer(), 
                                             `#S` = col_integer(),
                                             book = col_character(),
                                             err = col_integer(),
                                             gender = col_factor(levels = c("F","M")),
                                             spk = col_character(),
                                             wc = col_integer()),
                            trim_ws = TRUE)


# Data preprocessing (reconstruction des WER à l'échelle du livre)

book_clean_male <- clean_male %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                             words = sum(wc),
                                                                             WER = (errors/words)*100)

book_other_male <- other_male %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                             words = sum(wc),
                                                                             WER = (errors/words)*100)

book_clean_female <- clean_female %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                                 words = sum(wc),
                                                                                 WER = (errors/words)*100)

book_other_female <- other_female %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                                 words = sum(wc),
                                                                                 WER = (errors/words)*100)


# Création de dataframe pour les analyses & la visualisation 

book_clean_female$set="clean"
book_other_female$set="other"
book_female<-bind_rows(book_clean_female,book_other_female)


book_clean_male$set="clean"
book_other_male$set="other"
book_male<-bind_rows(book_clean_male,book_other_male)

book_clean_female$sys_monogender="F"
book_clean_male$sys_monogender="M"
book_clean_mono<-bind_rows(book_clean_female,book_clean_male)

book_other_female$sys_monogender="F"
book_other_male$sys_monogender="M"
book_other_mono<-bind_rows(book_other_female,book_other_male)


#Si besoin de sauvegarde des données : 
#write_csv2(book_clean_mono,path="wer_book_clean_mono.csv")
#write_csv2(book_other_mono,path="wer_book_other_mono.csv")

##################################################### Statistiques générales : 

wer_clean_female <- book_clean_female %>% group_by(gender) %>% summarize(n= n(), 
                                                                         err=sum(errors), 
                                                                         wrd=sum(words), 
                                                                         med = median(WER),
                                                                         WER_avg=mean(WER), 
                                                                         sd = sd(WER))

wer_other_female <- book_other_female %>% group_by(gender) %>% summarize(n= n(), 
                                                                         err=sum(errors), 
                                                                         wrd=sum(words), 
                                                                         med = median(WER),
                                                                         WER_avg=mean(WER), 
                                                                         sd = sd(WER))

wer_clean_male <- book_clean_male %>% group_by(gender) %>% summarize(n= n(), 
                                                                     err=sum(errors), 
                                                                     wrd=sum(words), 
                                                                     med = median(WER),
                                                                     WER_avg=mean(WER), 
                                                                     sd = sd(WER))

wer_other_male <- book_other_male %>% group_by(gender) %>% summarize(n= n(), 
                                                                     err=sum(errors), 
                                                                     wrd=sum(words), 
                                                                     med = median(WER),
                                                                     WER_avg=mean(WER), 
                                                                     sd = sd(WER))




######################################################### Visualisation et analyses stat :

## Comparaison des modèles sur le test clean et le test other :

ggplot(book_clean_mono,aes(x=sys_monogender,y=WER,fill=sys_monogender,color=sys_monogender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle mono-genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3,size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))

ggplot(book_other_mono,aes(x=sys_monogender,y=WER,fill=sys_monogender,color=sys_monogender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle mono-genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3,size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))

# Test des rangs signés de Wilcoxon (données appariées): 
wilcox.test(WER~sys_monogender,data=book_clean_mono)
wilcox.test(WER~sys_monogender,data=book_other_mono)


## Visualisation des résultats par catégorie de genre (comparaison des modèles)

ggplot(book_clean_mono,aes(x=sys_monogender,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle mono-genre",
       fill="Genre")+
  theme(text = element_text(size=18))+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_x_discrete(labels=c("F","H"))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))

ggplot(book_other_mono,aes(x=sys_monogender,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle mono-genre",
       fill="Genre")+
  theme(text = element_text(size=18))+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_x_discrete(labels=c("F","H"))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))

# Test de Mann-Withney (ou somme des rangs de Wilcoxon)

wilcox.test(WER~gender,data=book_clean_female,paired=F)
wilcox.test(WER~gender,data=book_other_female,paired=F)

wilcox.test(WER~gender,data=book_clean_male,paired=F)
wilcox.test(WER~gender,data=book_other_male,paired=F)

###################



