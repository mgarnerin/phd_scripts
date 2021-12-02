library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

#Modifier le working directory 
#setwd()

# Expérience sur la comparaison des WER en fonction de la variabilité des individus représentés dans le corpus de train (dseed).
# Chapitre 8 de la thèse de Mahault GARNERIN.
# NB : l'ensemble des modèles comparés ici ont été appris sur données équilibrées H/F

##################################################### Préparation des données


clean_m1 <- read_delim("./data/wer_utt_test_clean_wp50.csv", 
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


other_m1 <- read_delim("./data/wer_utt_test_other_wp50.csv", 
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


clean_d2 <- read_delim("./data/wer_utt_test_clean_wp50_dseed26.csv", 
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


other_d2 <- read_delim("./data/wer_utt_test_other_wp50_dseed26.csv", 
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

clean_d3 <- read_delim("./data/wer_utt_test_clean_wp50_dseed42.csv", 
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


other_d3 <- read_delim("./data/wer_utt_test_other_wp50_dseed42.csv", 
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

book_clean_m1 <- clean_m1 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_other_m1 <- other_m1 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_clean_d2 <- clean_d2 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_other_d2 <- other_d2 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_clean_d3 <- clean_d3 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_other_d3 <- other_d3 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)


# Data prep/merging for graphic display

book_clean_m1$m_seed="1"
book_clean_m1$d_seed="67"
book_clean_m1$model="m1"
book_clean_d2$m_seed="1"
book_clean_d2$d_seed="26"
book_clean_d2$model="d2"
book_clean_d3$m_seed="1"
book_clean_d3$d_seed="42"
book_clean_d3$model="d3"

book_clean<-bind_rows(book_clean_m1,book_clean_d2)
book_clean<-bind_rows(book_clean,book_clean_d3)
book_clean$d_seed<-as.factor(book_clean$d_seed)
book_clean$d_seed<-fct_relevel(book_clean$d_seed, c("1","26","42"))
book_clean$model<-as.factor(book_clean$model)
book_clean$model<-fct_relevel(book_clean$model, c("m1","d2","d3"))


book_other_m1$m_seed="1"
book_other_m1$d_seed="67"
book_other_m1$model="m1"
book_other_d2$m_seed="1"
book_other_d2$d_seed="26"
book_other_d2$model="d2"
book_other_d3$m_seed="1"
book_other_d3$d_seed="42"
book_other_d3$model="d3"

book_other<-bind_rows(book_other_m1,book_other_d2)
book_other<-bind_rows(book_other,book_other_d3)
book_other$d_seed<-as.factor(book_other$d_seed)
book_other$d_seed<-fct_relevel(book_other$d_seed, c("1","26","42"))
book_other$model<-as.factor(book_other$model)
book_other$model<-fct_relevel(book_other$model, c("m1","d2","d3"))


#Sauvegarde générale si besoin 
#write_csv2(book_clean,path="wer_book_clean_dseed.csv")
#write_csv2(book_other,path="wer_book_other_dseed.csv")

####################################################### Statistiques générales :

# WER tables for test-clean

wer_clean_dseed <- book_clean %>% group_by(model) %>% summarize(n= n(), 
                                                                 err=sum(errors), 
                                                                 wrd=sum(words), 
                                                                 WER_glob=err/wrd*100,
                                                                 med=median(WER),
                                                                 WER_avg=mean(WER), 
                                                                 sd = sd(WER))

wer_clean_dseed_gdrd <- book_clean %>% group_by(model,gender) %>% summarize(n= n(), 
                                                                             err=sum(errors), 
                                                                             wrd=sum(words), 
                                                                             WER_glob=err/wrd*100,
                                                                             med=median(WER),
                                                                             WER_avg=mean(WER), 
                                                                             sd = sd(WER))


# WER tables for test-other

wer_other_dseed <- book_other %>% group_by(model) %>% summarize(n= n(), 
                                                                 err=sum(errors), 
                                                                 wrd=sum(words), 
                                                                 WER_glob=err/wrd*100,
                                                                 med=median(WER),
                                                                 WER_avg=mean(WER), 
                                                                 sd = sd(WER))

wer_other_dseed_gdrd <- book_other %>% group_by(model,gender) %>% summarize(n= n(), 
                                                                             err=sum(errors), 
                                                                             wrd=sum(words), 
                                                                             WER_glob=err/wrd*100,
                                                                             med=median(WER),
                                                                             WER_avg=mean(WER), 
                                                                             sd = sd(WER))




####################################################### Visualisation & analyses


#########################" Comparaison des modèles :

ggplot(book_clean,aes(x=model,y=WER,fill=model,color=model))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))


ggplot(book_other,aes(x=model,y=WER,fill=model,color=model))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))



### Analyses stat :

#Test de Kruskall-Wallis pour comparaison des 3 modèles :
kruskal.test(WER~d_seed,data=book_clean)
kruskal.test(WER~d_seed,data=book_other)

#Comparaison 2 à 2 (équivalent au test de Wilcoxon) :

clean_m1d2<-bind_rows(book_clean_m1,book_clean_d2)
kruskal.test(WER~d_seed,data=clean_m1d2)
other_m1d2<-bind_rows(book_other_m1,book_other_d2)
kruskal.test(WER~d_seed,data=other_m1d2)

clean_m1d3<-bind_rows(book_clean_m1,book_clean_d3)
kruskal.test(WER~d_seed,data=clean_m1d3)
other_m1d3<-bind_rows(book_other_m1,book_other_d3)
kruskal.test(WER~d_seed,data=other_m1d3)

n_d2d3<-bind_rows(book_clean_d2,book_clean_d3)
kruskal.test(WER~d_seed,data=clean_d2d3)
other_d2d3<-bind_rows(book_other_d2,book_other_d3)
kruskal.test(WER~d_seed,data=other_d2d3)


########################## Comparaison des modèles par catégorie de genre

ggplot(book_clean,aes(x=model,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle",
       fill="Genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank())+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))

ggplot(book_other,aes(x=model,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle",
       fill="Genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank())+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))


### Analyses stats :

#Test de la somme des rangs de Wilcoxon (ou Mann-Withney)

wilcox.test(WER~gender,data=book_clean_m1,paired=F)
wilcox.test(WER~gender,data=book_clean_d2,paired=F)
wilcox.test(WER~gender,data=book_clean_d3,paired=F)

wilcox.test(WER~gender,data=book_other_m1,paired=F)
wilcox.test(WER~gender,data=book_other_d2,paired=F)
wilcox.test(WER~gender,data=book_other_d3,paired=F)

################### Calcul moyennes/écart types & intervalles de confiance

mean(book_clean_m1$WER)
sd(book_clean_m1$WER)
mean(book_clean_m1$WER)-(1.96*sd(book_clean_m1$WER)/sqrt(87))
mean(book_clean_m1$WER)+(1.96*sd(book_clean_m1$WER)/sqrt(87))
(max(book_clean_m1$WER)-min(book_clean_m1$WER))/87

mean(book_clean_d2$WER)
sd(book_clean_d2$WER)
mean(book_clean_d2$WER)-(1.96*sd(book_clean_d2$WER)/sqrt(87))
mean(book_clean_d2$WER)+(1.96*sd(book_clean_d2$WER)/sqrt(87))
(max(book_clean_d2$WER)-min(book_clean_d2$WER))/87

mean(book_clean_d3$WER)
sd(book_clean_d3$WER)
mean(book_clean_d3$WER)-(1.96*sd(book_clean_d3$WER)/sqrt(87))
mean(book_clean_d3$WER)+(1.96*sd(book_clean_d3$WER)/sqrt(87))
(max(book_clean_d3$WER)-min(book_clean_d3$WER))/87


mean(book_other_m1$WER)
sd(book_other_m1$WER)
mean(book_other_m1$WER)-(1.96*sd(book_other_m1$WER)/sqrt(90))
mean(book_other_m1$WER)+(1.96*sd(book_other_m1$WER)/sqrt(90))
(max(book_other_m1$WER)-min(book_other_m1$WER))/90

mean(book_other_d2$WER)
sd(book_other_d2$WER)
mean(book_other_d2$WER)-(1.96*sd(book_other_d2$WER)/sqrt(90))
mean(book_other_d2$WER)+(1.96*sd(book_other_d2$WER)/sqrt(90))
(max(book_other_d2$WER)-min(book_other_d2$WER))/90

mean(book_other_d3$WER)
sd(book_other_d3$WER)
mean(book_other_d3$WER)-(1.96*sd(book_other_d3$WER)/sqrt(90))
mean(book_other_d3$WER)+(1.96*sd(book_other_d3$WER)/sqrt(90))
(max(book_other_d3$WER)-min(book_other_d3$WER))/90


########################################## Clarification expérimentale 

#Les résultats ci-dessous portent sur deux modèles appris suite à une confusion expérimentale (voir section 8.4.1 du manuscrit)
#Ces résultats ne sont donc pas comparables aux résultats obtenus ci-dessus et doivent être pris avec précaution.

#Imports des données
clean_d2_5342 <- read_delim("./data/wer_utt_test_clean_wp50_dseed26_5342.csv", 
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


other_d2_5342 <- read_delim("./data/wer_utt_test_other_wp50_dseed26_5342.csv", 
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

clean_d3_5342 <- read_delim("./data/wer_utt_test_clean_wp50_dseed42_5342.csv", 
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


other_d3_5342 <- read_delim("./data/wer_utt_test_other_wp50_dseed42_5342.csv", 
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


#Prep/merging data
book_clean_d2_5342 <- clean_d2_5342 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_other_d2_5342 <- other_d2_5342 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_clean_d3_5342 <- clean_d3_5342 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_other_d3_5342 <- other_d3_5342 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                        words = sum(wc),
                                                                        WER = (errors/words)*100)

book_clean_d2_5342$m_seed="1"
book_clean_d2_5342$d_seed="26"
book_clean_d2_5342$model="d2_5342"
book_clean_d3_5342$m_seed="1"
book_clean_d3_5342$d_seed="42"
book_clean_d3_5342$model="d3_5342"

book_clean_hickup <- bind_rows(book_clean_d2_5342,book_clean_d3_5342)

book_other_d2_5342$m_seed="1"
book_other_d2_5342$d_seed="26"
book_other_d2_5342$model="d2_5342"
book_other_d3_5342$m_seed="1"
book_other_d3_5342$d_seed="42"
book_other_d3_5342$model="d3_5342"

book_other_hickup <- bind_rows(book_other_d2_5342,book_other_d3_5342)


#Calcul des tables de WER
wer_clean_dseed_hickup <- book_clean_hickup %>% group_by(model) %>% summarize(n= n(), 
                                                                err=sum(errors), 
                                                                wrd=sum(words), 
                                                                WER_glob=err/wrd*100,
                                                                med=median(WER),
                                                                WER_avg=mean(WER), 
                                                                sd = sd(WER))

wer_clean_dseed_hickup_gdrd <- book_clean_hickup %>% group_by(model,gender) %>% summarize(n= n(), 
                                                                            err=sum(errors), 
                                                                            wrd=sum(words), 
                                                                            WER_glob=err/wrd*100,
                                                                            med=median(WER),
                                                                            WER_avg=mean(WER), 
                                                                            sd = sd(WER))


wer_other_dseed_hickup <- book_other_hickup %>% group_by(model) %>% summarize(n= n(), 
                                                                err=sum(errors), 
                                                                wrd=sum(words), 
                                                                WER_glob=err/wrd*100,
                                                                med=median(WER),
                                                                WER_avg=mean(WER), 
                                                                sd = sd(WER))

wer_other_dseed_hickup_gdrd <- book_other_hickup %>% group_by(model,gender) %>% summarize(n= n(), 
                                                                            err=sum(errors), 
                                                                            wrd=sum(words), 
                                                                            WER_glob=err/wrd*100,
                                                                            med=median(WER),
                                                                            WER_avg=mean(WER), 
                                                                            sd = sd(WER))

#Analyse stat. : comparaison des modèles avec le test de Kruskall-Wallis
clean_hickup<-bind_rows(book_clean_d2_5342,book_clean_d3_5342)
kruskal.test(WER~d_seed,data=clean_hickup)
other_hickup<-bind_rows(book_other_d2_5342,book_other_d3_5342)
kruskal.test(WER~d_seed,data=other_hickup)

#Visualisation graphique

ggplot(book_clean_hickup,aes(x=model,y=WER,fill=model,color=model))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))


ggplot(book_other_hickup,aes(x=model,y=WER,fill=model,color=model))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))


# Analyse en fonction du genre :

#Test statistiques 
wilcox.test(WER~gender,data=book_clean_d2_5342,paired=F)
wilcox.test(WER~gender,data=book_clean_d3_5342,paired=F)

wilcox.test(WER~gender,data=book_other_d2_5342,paired=F)
wilcox.test(WER~gender,data=book_other_d3_5342,paired=F)


# Visualisation graphique

ggplot(book_clean_hickup,aes(x=model,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Modèle",
       fill="Genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank())+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))

ggplot(book_other_hickup,aes(x=model,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Modèle",
       fill="Genre")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank())+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))
