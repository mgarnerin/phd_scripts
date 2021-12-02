library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)

#Modifier le working directory 
#setwd()

# Expérience sur la comparaison des WER en fonction de la représentation des catégories de genre dans 
# les données d'apprentissage. (Répartitions H/F : 30/70 - 50/50 - 70/30).
# Chapitre 8 de la thèse de Mahault GARNERIN.

##################################################### Préparation des données

test_clean70 <- read_delim("./data/wer_utt_test_clean_wp70.csv", 
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


test_other70 <- read_delim("./data/wer_utt_test_other_wp70.csv", 
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

test_clean50 <- read_delim("./data/wer_utt_test_clean_wp50.csv", 
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


test_other50 <- read_delim("./data/wer_utt_test_other_wp50.csv", 
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



test_clean30 <- read_delim("./data/wer_utt_test_clean_wp30.csv", 
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


test_other30 <- read_delim("./data/wer_utt_test_other_wp30.csv", 
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

book_clean70 <- test_clean70 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)

book_other70 <- test_other70 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)

book_clean50 <- test_clean50 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)

book_other50 <- test_other50 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)

book_clean30 <- test_clean30 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)

book_other30 <- test_other30 %>% group_by(book, spk, gender) %>% summarize(errors = sum(err),
                                                                           words = sum(wc),
                                                                           WER = (errors/words)*100)


book_clean30$wper="30%"
book_clean50$wper="50%"
book_clean70$wper="70%"

book_clean_wper<-bind_rows(book_clean30,book_clean50)
book_clean_wper<-bind_rows(book_clean_wper,book_clean70)
book_clean_wper$wper<-as.factor(book_clean_wper$wper)

book_other30$wper="30%"
book_other50$wper="50%"
book_other70$wper="70%"

book_other_wper<-bind_rows(book_other30,book_other50)
book_other_wper<-bind_rows(book_other_wper,book_other70)
book_other_wper$wper<-as.factor(book_other_wper$wper)


#Sauvegarde si besoin :
#write.csv2(book_clean_wper,file="wer_book_clean_wper.csv")
#write.csv2(book_other_wper,file="wer_book_other_wper.csv")


###############################################


############################################## Visualisation et analyses

########### Comparaison des performances par modèles :


# Visualisation graphique : 

ggplot(book_clean_wper,aes(x=wper,y=WER,fill=wper,color=wper))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Pourcentage de femmes dans le corpus d'apprentissage")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))

ggplot(book_other_wper,aes(x=wper,y=WER,fill=wper,color=wper))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Pourcentage de femmes dans le corpus d'apprentissage")+
  theme(text = element_text(size=18),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  stat_summary(fun=mean ,geom="point",color="white", shape=3, size=8, mapping=aes(group=1))+
  scale_fill_manual(values=c("skyblue","royalblue","navy"))+
  scale_color_manual(values=c("skyblue","royalblue","navy"))

# Analyse stat :
# Test de Kruskall-Wallis (comparaison des 3 modèles, puis comparaison 2 à à 2
# équivalentes au test des rangs signés de Wilcoxon)

kruskal.test(WER~wper,data=book_clean_wper)
kruskal.test(WER~wper,data=book_other_wper)

clean3050<-bind_rows(book_clean30,book_clean50)
kruskal.test(WER~wper,data=clean3050)
other3050<-bind_rows(book_other30,book_other50)
kruskal.test(WER~wper,data=other3050)

clean3070<-bind_rows(book_clean30,book_clean70)
kruskal.test(WER~wper,data=clean3070)
other3070<-bind_rows(book_other30,book_other70)
kruskal.test(WER~wper,data=other3070)

clean5070<-bind_rows(book_clean50,book_clean70)
kruskal.test(WER~wper,data=clean5070)
other5070<-bind_rows(book_other50,book_other70)
kruskal.test(WER~wper,data=other5070)


######### Comparaison des performances par catégories de genre et par modèles :


## Visualisation :

ggplot(book_clean_wper,aes(x=wper,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,40))+
  labs(title="",
       y="WER",
       x="Pourcentage de femmes dans le corpus d'apprentissage",
       fill="Genre")+
  theme(text = element_text(size=18))+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))

ggplot(book_other_wper,aes(x=wper,y=WER,fill=factor(gender,labels=c("F","H")),color=gender))+
  geom_violin()+
  scale_y_continuous(lim=c(0,100))+
  labs(title="",
       y="WER",
       x="Pourcentage de femmes dans le corpus d'apprentissage",
       fill="Genre")+
  theme(text = element_text(size=18))+
  stat_summary(fun=mean ,geom="point", mapping=aes(group=1))+
  scale_fill_manual(values=c("indianred2","tomato4"))+
  scale_color_manual(guide=FALSE,values=c("indianred2","tomato4"))



### Analyses stats : 
# Test de Mann-Whitney (ou somme des rangs de Wilcoxon)

wilcox.test(WER~gender,data=book_clean30, paired = F)
wilcox.test(WER~gender,data=book_other30, paired = F)

wilcox.test(WER~gender,data=book_clean50, paired = F)
wilcox.test(WER~gender,data=book_other50, paired = F)

wilcox.test(WER~gender,data=book_clean70, paired = F)
wilcox.test(WER~gender,data=book_other70, paired = F)

wilcox.test(book_clean70$WER[book_clean70$gender=="F"],book_clean70$WER[book_clean70$gender=="M"],paired=F)
wilcox.test(book_clean70$WER[book_clean70$gender=="M"],book_clean70$WER[book_clean70$gender=="F"],paired=F)



################### Calcul moyennes/écart types & intervalles de confiance

mean(book_other30$WER)
sd(book_clean30$WER)
mean(book_clean30$WER)-(1.96*sd(book_clean30$WER)/sqrt(87))
mean(book_clean30$WER)+(1.96*sd(book_clean30$WER)/sqrt(87))
(max(book_clean30$WER)-min(book_clean30$WER))/87

mean(book_clean50$WER)
sd(book_clean50$WER)
mean(book_clean50$WER)-(1.96*sd(book_clean50$WER)/sqrt(87))
mean(book_clean50$WER)+(1.96*sd(book_clean50$WER)/sqrt(87))
(max(book_clean50$WER)-min(book_clean50$WER))/87

mean(book_clean70$WER)
sd(book_clean70$WER)
mean(book_clean70$WER)-(1.96*sd(book_clean70$WER)/sqrt(87))
mean(book_clean70$WER)+(1.96*sd(book_clean70$WER)/sqrt(87))
(max(book_clean70$WER)-min(book_clean70$WER))/87

mean(book_other30$WER)
sd(book_other30$WER)
mean(book_other30$WER)-(1.96*sd(book_other30$WER)/sqrt(90))
mean(book_other30$WER)+(1.96*sd(book_other30$WER)/sqrt(90))
(max(book_other30$WER)-min(book_other30$WER))/90

mean(book_other50$WER)
sd(book_other50$WER)
mean(book_other50$WER)-(1.96*sd(book_other50$WER)/sqrt(90))
mean(book_other50$WER)+(1.96*sd(book_other50$WER)/sqrt(90))
(max(book_other50$WER)-min(book_other50$WER))/90

mean(book_other70$WER)
sd(book_other70$WER)
mean(book_other70$WER)-(1.96*sd(book_other70$WER)/sqrt(90))
mean(book_other70$WER)+(1.96*sd(book_other70$WER)/sqrt(90))
(max(book_other70$WER)-min(book_other70$WER))/90

###

mean(book_clean30$WER[book_clean30$gender=="F"])
sd(book_clean30$WER[book_clean30$gender=="F"])

mean(book_clean50$WER[book_clean50$gender=="F"])
sd(book_clean50$WER[book_clean50$gender=="F"])

mean(book_clean70$WER[book_clean70$gender=="F"])
sd(book_clean70$WER[book_clean70$gender=="F"])

mean(book_clean30$WER[book_clean30$gender=="M"])
sd(book_clean30$WER[book_clean30$gender=="M"])

mean(book_clean50$WER[book_clean50$gender=="M"])
sd(book_clean50$WER[book_clean50$gender=="M"])

mean(book_clean70$WER[book_clean70$gender=="M"])
sd(book_clean70$WER[book_clean70$gender=="M"])


mean(book_other30$WER[book_other30$gender=="F"])
sd(book_other30$WER[book_other30$gender=="F"])

mean(book_other50$WER[book_other50$gender=="F"])
sd(book_other50$WER[book_other50$gender=="F"])

mean(book_other70$WER[book_other70$gender=="F"])
sd(book_other70$WER[book_other70$gender=="F"])

mean(book_other30$WER[book_other30$gender=="M"])
sd(book_other30$WER[book_other30$gender=="M"])

mean(book_other50$WER[book_other50$gender=="M"])
sd(book_other50$WER[book_other50$gender=="M"])

mean(book_other70$WER[book_other70$gender=="M"])
sd(book_other70$WER[book_other70$gender=="M"])

###############

median(book_clean30$WER[book_clean30$gender=="F"])
median(book_clean50$WER[book_clean50$gender=="F"])
median(book_clean70$WER[book_clean70$gender=="F"])

median(book_clean30$WER[book_clean30$gender=="M"])
median(book_clean50$WER[book_clean50$gender=="M"])
median(book_clean70$WER[book_clean70$gender=="M"])

median(book_clean30$WER)
median(book_clean50$WER)
median(book_clean70$WER)

median(book_other30$WER[book_other30$gender=="F"])
median(book_other50$WER[book_other50$gender=="F"])
median(book_other70$WER[book_other70$gender=="F"])

median(book_other30$WER[book_other30$gender=="M"])
median(book_other50$WER[book_other50$gender=="M"])
median(book_other70$WER[book_other70$gender=="M"])

median(book_other30$WER)
median(book_other50$WER)
median(book_other70$WER)
