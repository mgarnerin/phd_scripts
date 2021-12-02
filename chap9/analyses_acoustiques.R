library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(reshape2)

# Modifier le working directory où ce trouve ce script
# setwd() 

# Expérience sur la comparaison acoustique train/test 
# Chapitre 9 de la thèse de Mahault GARNERIN

# ================================= EXPE PARAM ACOUSTIQUES

# Imports des paramètres acoustiques
param_test_clean <- read_delim("./data/acoustic_feats/feat_test_clean.csv", 
                               "\t", 
                               escape_double = FALSE, 
                               col_types = cols(idLivre = col_character(), 
                                                idLoc = col_character(), 
                                                npause = col_integer(), 
                                                nsyll = col_integer()), 
                               trim_ws = TRUE)

param_test_other <- read_delim("./data/acoustic_feats/feat_test_other.csv", 
                               "\t", 
                               escape_double = FALSE, 
                               col_types = cols(idLivre = col_character(), 
                                                idLoc = col_character(), 
                                                npause = col_integer(), 
                                                nsyll = col_integer()), 
                               trim_ws = TRUE)


param_train_full <- read_delim("./data/acoustic_feats/feat_train.csv", 
                               "\t", 
                               escape_double = FALSE, 
                               col_types = cols(idLivre = col_character(), 
                                                idLoc = col_character(), 
                                                npause = col_integer(), 
                                                nsyll = col_integer()), 
                               trim_ws = TRUE)

# On reconstruit les mesures à la granularité du livre

param_book_clean <- param_test_clean %>% group_by(idLoc,idLivre) %>% summarize(nsyll = sum(nsyll),
                                                                               npauses = sum(npause),
                                                                               dur = sum(`dur (s)`),
                                                                               speech_dur = sum(`durParole(s)`),
                                                                               speechrate = mean(`debitArt (nsyll / durParole)`),
                                                                               speechrate_sd = sd(`debitArt (nsyll / durParole)`),
                                                                               syll_dur = mean(`durSyll (durParole/nsyll)`),
                                                                               f0med = median(`f0med (Hz)`),
                                                                               f0quant = quantile(`f0med (Hz)`, probs = 0.75) - quantile(`f0med (Hz)`, probs = 0.25),
                                                                               f0delta = f0quant / f0med,
                                                                               f0med_tones = median(`f0med (semitones 100Hz)`),
                                                                               f0quant_tones = quantile(`f0med (semitones 100Hz)`, probs = 0.75) - quantile(`f0med (semitones 100Hz)`, probs = 0.25),
                                                                               f0delta_tones = f0quant_tones/f0med_tones
)



param_book_other <- param_test_other %>% group_by(idLoc,idLivre) %>% summarize(nsyll = sum(nsyll),
                                                                               npauses = sum(npause),
                                                                               dur = sum(`dur (s)`),
                                                                               speech_dur = sum(`durParole(s)`),
                                                                               speechrate = mean(`debitArt (nsyll / durParole)`),
                                                                               speechrate_sd = sd(`debitArt (nsyll / durParole)`),
                                                                               syll_dur = mean(`durSyll (durParole/nsyll)`),
                                                                               f0med = median(`f0med (Hz)`),
                                                                               f0quant = quantile(`f0med (Hz)`, probs = 0.75) - quantile(`f0med (Hz)`, probs = 0.25),
                                                                               f0delta = f0quant / f0med,
                                                                               f0med_tones = median(`f0med (semitones 100Hz)`),
                                                                               f0quant_tones = quantile(`f0med (semitones 100Hz)`, probs = 0.75) - quantile(`f0med (semitones 100Hz)`, probs = 0.25),
                                                                               f0delta_tones = f0quant_tones/f0med_tones)



param_book_train <- param_train_full %>% group_by(idLoc,idLivre) %>% summarize(nsyll = sum(nsyll),
                                                                               npauses = sum(npause),
                                                                               dur = sum(`dur (s)`),
                                                                               speech_dur = sum(`durParole(s)`),
                                                                               speechrate = mean(`debitArt (nsyll / durParole)`),
                                                                               speechrate_sd = sd(`debitArt (nsyll / durParole)`),
                                                                               syll_dur = mean(`durSyll (durParole/nsyll)`),
                                                                               f0med = median(`f0med (Hz)`),
                                                                               f0quant = quantile(`f0med (Hz)`, probs = 0.75) - quantile(`f0med (Hz)`, probs = 0.25),
                                                                               f0delta = f0quant / f0med,
                                                                               f0med_tones = median(`f0med (semitones 100Hz)`),
                                                                               f0quant_tones = quantile(`f0med (semitones 100Hz)`, probs = 0.75) - quantile(`f0med (semitones 100Hz)`, probs = 0.25),
                                                                               f0delta_tones = f0quant_tones/f0med_tones
)


# Harmonisation des intitulés de colonnes
colnames(param_book_clean)[colnames(param_book_clean) == "idLoc"] <- "spk"
colnames(param_book_clean)[colnames(param_book_clean) == "idLivre"] <- "book"

colnames(param_book_other)[colnames(param_book_other) == "idLoc"] <- "spk"
colnames(param_book_other)[colnames(param_book_other) == "idLivre"] <- "book"

colnames(param_book_train)[colnames(param_book_train) == "idLoc"] <- "spk"
colnames(param_book_train)[colnames(param_book_train) == "idLivre"] <- "book"

#####  Fusion paramètres et ensembles de train pour nos modèles 

# TRAIN - Expérience de variation de la proportion
spk2gender_wper30 <- read_table2("./data/gendermaps/spk2gender_wper30.csv", 
                                 col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_wper30 <- separate(spk2gender_wper30,id, c("spk","book"),sep ="-")
spk2gender_wper30 <- as.data.frame(apply(spk2gender_wper30,2,toupper))
spk2gender_wper30$gender <- as.factor(spk2gender_wper30$gender)

spk2gender_wper50 <- read_table2("./data/gendermaps/spk2gender_wper50.csv", 
                                 col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_wper50 <- separate(spk2gender_wper50,id, c("spk","book"),sep ="-")
spk2gender_wper50 <- as.data.frame(apply(spk2gender_wper50,2,toupper))
spk2gender_wper50$gender <- as.factor(spk2gender_wper50$gender)

spk2gender_wper70 <- read_table2("./data/gendermaps/spk2gender_wper70.csv", 
                                 col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_wper70 <- separate(spk2gender_wper70,id, c("spk","book"),sep ="-")
spk2gender_wper70 <- as.data.frame(apply(spk2gender_wper70,2,toupper))
spk2gender_wper70$gender <- as.factor(spk2gender_wper70$gender)

param_book_train_wper30 <- left_join(spk2gender_wper30,param_book_train, by= c("spk","book"))

param_book_train_wper50 <- left_join(spk2gender_wper50,param_book_train, by= c("spk","book"))

param_book_train_wper70 <- left_join(spk2gender_wper70,param_book_train, by= c("spk","book"))

# TRAIN - Expérience de la variation individuelle (dseed)

spk2gender_d2 <- read_table2("./data/gendermaps/spk2gender_d2.csv", 
                             col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_d2 <- separate(spk2gender_d2,id, c("spk","book"),sep ="-")
spk2gender_d2 <- as.data.frame(apply(spk2gender_d2,2,toupper))
spk2gender_d2$gender <- as.factor(spk2gender_d2$gender)

spk2gender_d3 <- read_table2("./data/gendermaps/spk2gender_d3.csv", 
                             col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_d3 <- separate(spk2gender_d3,id, c("spk","book"),sep ="-")
spk2gender_d3 <- as.data.frame(apply(spk2gender_d3,2,toupper))
spk2gender_d3$gender <- as.factor(spk2gender_d3$gender)

param_book_train_d2 <- left_join(spk2gender_d2,param_book_train, by= c("spk","book"))

param_book_train_d3 <- left_join(spk2gender_d3,param_book_train, by= c("spk","book"))


# TRAIN - Expérience de la variation individuelle (dseed 5342)

spk2gender_d2_5342 <- read_table2("./data/gendermaps/spk2gender_d2_5342.csv", 
                                   col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_d2_5342 <- separate(spk2gender_d2_5342,id, c("spk","book"),sep ="-")
spk2gender_d2_5342 <- as.data.frame(apply(spk2gender_d2_5342,2,toupper))
spk2gender_d2_5342$gender <- as.factor(spk2gender_d2_5342$gender)

spk2gender_d3_5342 <- read_table2("./data/gendermaps/spk2gender_d3_5342.csv", 
                                   col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_d3_5342 <- separate(spk2gender_d3_5342,id, c("spk","book"),sep ="-")
spk2gender_d3_5342 <- as.data.frame(apply(spk2gender_d3_5342,2,toupper))
spk2gender_d3_5342$gender <- as.factor(spk2gender_d3_5342$gender)

param_book_train_d2_5342 <- left_join(spk2gender_d2_5342,param_book_train, by= c("spk","book"))
param_book_train_d3_5342 <- left_join(spk2gender_d3_5342,param_book_train, by= c("spk","book"))


# TRAIN - Expérience systèmes monogenre

spk2gender_female <- read_table2("./data/gendermaps/spk2gender_female.csv", 
                                 col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_female <- separate(spk2gender_female,id, c("spk","book"),sep ="-")
spk2gender_female <- as.data.frame(apply(spk2gender_female,2,toupper))
spk2gender_female$gender <- as.factor(spk2gender_female$gender)

spk2gender_male <- read_table2("./data/gendermaps/spk2gender_male.csv", 
                               col_types = cols(gender = col_factor(levels = c("m","f"))))
spk2gender_male <- separate(spk2gender_male,id, c("spk","book"),sep ="-")
spk2gender_male <- as.data.frame(apply(spk2gender_male,2,toupper))
spk2gender_male$gender <- as.factor(spk2gender_male$gender)

param_book_train_female <- left_join(spk2gender_female,param_book_train, by= c("spk","book"))
param_book_train_male <- left_join(spk2gender_male,param_book_train, by= c("spk","book"))


# ============================================== EXPE VARIATION REPRENSENTATION (WPER)

# Import des données de test

wer_book_clean_wper <- read_delim("./data/wer_book/wer_book_clean_wper.csv", 
                                  ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                               book = col_character(), errors = col_integer(), 
                                                                               gender = col_factor(levels = c("F",  "M")),
                                                                               spk = col_character(), 
                                                                               wper = col_factor(levels = c("30%", "50%", "70%"))),
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = ""),
                                  trim_ws = TRUE)

wer_book_other_wper <- read_delim("./data/wer_book/wer_book_other_wper.csv", 
                                  ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                               book = col_character(), errors = col_integer(), 
                                                                               gender = col_factor(levels = c("F",  "M")),
                                                                               spk = col_character(), 
                                                                               wper = col_factor(levels = c("30%", "50%", "70%"))),
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = ""), 
                                  trim_ws = TRUE)


param_book_clean_wper <- left_join(wer_book_clean_wper,param_book_clean, by = c("spk","book"))
param_book_other_wper <- left_join(wer_book_other_wper,param_book_other, by = c("spk","book"))


#### F0 med ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=f0med,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none(title="Pourcentage de femmes dans le corpus d'apprentissage"))

#Calcul de corrélation
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="30%"],param_book_clean_wper$f0med[param_book_clean_wper$wper=="30%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="50%"],param_book_clean_wper$f0med[param_book_clean_wper$wper=="50%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="70%"],param_book_clean_wper$f0med[param_book_clean_wper$wper=="70%"])

#### F0 écart interquartile ### 

#Visualisation
ggplot(param_book_clean_wper,aes(x=f0quant,y=WER))+geom_point(aes(color=gender))+facet_grid(. ~ wper)+geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))

#Après visualisation, suppression des outliers
temp <- subset(param_book_clean_wper, f0quant<50)
ggplot(temp,aes(x=f0quant,y=WER))+geom_point(aes(color=gender))+facet_grid(. ~ wper)+geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))

ggplot(temp,aes(x=f0quant,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 - écart interquartile",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))

#Calcul corrélations
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="30%"],param_book_clean_wper$f0quant[param_book_clean_wper$wper=="30%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="50%"],param_book_clean_wper$f0quant[param_book_clean_wper$wper=="50%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="70%"],param_book_clean_wper$f0quant[param_book_clean_wper$wper=="70%"])


### F0  delta ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=f0delta,y=WER))+geom_point(aes(color=gender))+facet_grid(. ~ wper)+geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))

#Suppression des outliers
temp2 <- subset(param_book_clean_wper, f0delta<0.3)
ggplot(temp2,aes(x=f0delta,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 - delta",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  facet_grid(. ~ wper)+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))


### F0 mediane en semi-tons ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=f0med_tones,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane (en semi-tons)",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))

#Calcul de correlations
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="30%"],param_book_clean_wper$f0med_tones[param_book_clean_wper$wper=="30%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="50%"],param_book_clean_wper$f0med_tones[param_book_clean_wper$wper=="50%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="70%"],param_book_clean_wper$f0med_tones[param_book_clean_wper$wper=="70%"])

### F0  écart interquartile en semi-tons ###

#Suppression outliers
ggplot(param_book_clean_wper,aes(x=f0quant_tones,y=WER))+
  geom_point(aes(color=gender))+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))
temp <- subset(param_book_clean_wper, f0quant_tones<5)

#Visualisation
ggplot(temp,aes(x=f0quant_tones,y=WER))+  
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 - écart interquartile (en semi-tons)",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))

#Calcul des correlations
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="30%"],param_book_clean_wper$f0quant_tones[param_book_clean_wper$wper=="30%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="50%"],param_book_clean_wper$f0quant_tones[param_book_clean_wper$wper=="50%"])
cor.test(param_book_clean_wper$WER[param_book_clean_wper$wper=="70%"],param_book_clean_wper$f0quant_tones[param_book_clean_wper$wper=="70%"])

#Visualisation sur le test other
ggplot(param_book_other_wper,aes(x=f0med,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))


### Débit articulatoire ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=speechrate,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit moyen",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))

### Débit articulatoire - écart-type  ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=speechrate_sd,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit - écart-type",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))



### Durée de parole ###

#Visualisation
ggplot(param_book_clean_wper,aes(x=speech_dur,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Durée",
       color="Genre")+
  facet_grid(. ~ wper)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Pourcentage de femmes dans le corpus d'apprentissage"))


#====================================================================== EXPE VARIATION INDIVIDUELLE (DSEED)

wer_book_clean_dseed <- read_delim("./data/wer_book/wer_book_clean_dseed.csv", 
                                   ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                                book = col_character(), errors = col_integer(), 
                                                                                gender = col_factor(levels = c("F",  "M")),
                                                                                spk = col_character(), 
                                                                                model = col_factor(levels = c("m1", "d2", "d3"))),
                                   locale = locale(decimal_mark = ",",
                                                   grouping_mark = ""),
                                   trim_ws = TRUE)

param_book_clean_dseed <- left_join(wer_book_clean_dseed,param_book_clean, by = c("spk","book"))

### F0 médiane ###

#Visualisation
ggplot(param_book_clean_dseed,aes(x=f0med,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane",
       color="Genre")+
  facet_grid(. ~ model)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Modèles"))

#Calcul corrélations
cor.test(param_book_clean_dseed$WER[param_book_clean_dseed$model=="m1"],param_book_clean_dseed$f0med[param_book_clean_dseed$model=="m1"])
cor.test(param_book_clean_dseed$WER[param_book_clean_dseed$model=="d2"],param_book_clean_dseed$f0med[param_book_clean_dseed$model=="d2"])
cor.test(param_book_clean_dseed$WER[param_book_clean_dseed$model=="d3"],param_book_clean_dseed$f0med[param_book_clean_dseed$model=="d3"])


# ======================================================== EXPE SYSTEMES MONOGENRE (MONO)

wer_book_clean_mono <- read_delim("./data/wer_book/wer_book_clean_mono.csv", 
                                  ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                               book = col_character(), errors = col_integer(), 
                                                                               gender = col_factor(levels = c("F",  "M")),
                                                                               spk = col_character(), 
                                                                               sys_monogender = col_factor(levels = c("F", "M"))),
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = ""),
                                  trim_ws = TRUE)

wer_book_other_mono <- read_delim("./data/wer_book/wer_book_other_mono.csv", 
                                  ";", escape_double = FALSE, col_types = cols(X1 = col_skip(), 
                                                                               book = col_character(), errors = col_integer(), 
                                                                               gender = col_factor(levels = c("F",  "M")),
                                                                               spk = col_character(), 
                                                                               sys_monogender = col_factor(levels = c("F", "M"))),
                                  locale = locale(decimal_mark = ",",
                                                  grouping_mark = ""),
                                  trim_ws = TRUE)

param_book_clean_mono <- left_join(wer_book_clean_mono,param_book_clean, by = c("spk","book"))
param_book_other_mono <- left_join(wer_book_other_mono,param_book_other, by = c("spk","book"))

### F0 médiane ###

#Visualisation (test-clean)
ggplot(param_book_clean_mono,aes(x=f0med,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))

#Visualisation (test-other)
ggplot(param_book_other_mono,aes(x=f0med,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="F0 médiane",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))


#Calcul de corrélations
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="F"],param_book_clean_mono$f0med[param_book_clean_mono$sys_monogender=="F"])
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="M"],param_book_clean_mono$f0med[param_book_clean_mono$sys_monogender=="M"])

cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="F"],param_book_other_mono$f0med[param_book_other_mono$sys_monogender=="F"])
cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="M"],param_book_other_mono$f0med[param_book_other_mono$sys_monogender=="M"])



### Débit articulatoire moyen ###

#Visualisation (test-clean)
ggplot(param_book_clean_mono,aes(x=speechrate,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit moyen",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))

#Visualisation (test-other)
ggplot(param_book_other_mono,aes(x=speechrate,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit moyen",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))


#Calcul de corrélations
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="F"],param_book_clean_mono$speechrate[param_book_clean_mono$sys_monogender=="F"])
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="M"],param_book_clean_mono$speechrate[param_book_clean_mono$sys_monogender=="M"])

cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="F"],param_book_other_mono$speechrate[param_book_other_mono$sys_monogender=="F"])
cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="M"],param_book_other_mono$speechrate[param_book_other_mono$sys_monogender=="M"])


### Débit articulatoire - écart-type ###

#Visualisation (test-clean)
ggplot(param_book_clean_mono,aes(x=speechrate_sd,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit - écart-type",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))

#Visualisation (test-other)
ggplot(param_book_other_mono,aes(x=speechrate_sd,y=WER))+
  geom_point(aes(color=factor(gender,labels=c("F","H"))))+
  labs(title="",
       y="WER",
       x="Débit - écart-type",
       color="Genre")+
  facet_grid(. ~ sys_monogender)+
  theme(text = element_text(size=18))+
  geom_smooth(method=lm,color="darkorange3")+
  scale_color_manual(values=c("indianred2","aquamarine3"))+
  guides(x.sec = guide_none("Système mono-genre"))


#Calcul corrélations
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="F"],param_book_clean_mono$speechrate_sd[param_book_clean_mono$sys_monogender=="F"])
cor.test(param_book_clean_mono$WER[param_book_clean_mono$sys_monogender=="M"],param_book_clean_mono$speechrate_sd[param_book_clean_mono$sys_monogender=="M"])

cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="F"],param_book_other_mono$speechrate_sd[param_book_other_mono$sys_monogender=="F"])
cor.test(param_book_other_mono$WER[param_book_other_mono$sys_monogender=="M"],param_book_other_mono$speechrate_sd[param_book_other_mono$sys_monogender=="M"])


#======================================================== EXPLORATION NOTION COUVERTURE VOCALE

wilcox.test(param_book_clean$f0med,param_book_train_wper30$f0med,paired=F)
wilcox.test(param_book_clean$f0med,param_book_train_wper50$f0med,paired=F)
wilcox.test(param_book_clean$f0med,param_book_train_wper70$f0med,paired=F)

wilcox.test(param_book_clean$f0quant,param_book_train_wper30$f0quant,paired=F)
wilcox.test(param_book_clean$f0quant,param_book_train_wper50$f0quant,paired=F)
wilcox.test(param_book_clean$f0quant,param_book_train_wper70$f0quant,paired=F)

wilcox.test(param_book_clean$f0quant,param_book_train_female$f0quant,paired=F)
wilcox.test(param_book_clean$f0quant,param_book_train_male$f0quant,paired=F)

wilcox.test(param_book_other$f0quant,param_book_train_female$f0quant,paired=F)
wilcox.test(param_book_other$f0quant,param_book_train_male$f0quant,paired=F)

wilcox.test(param_book_train_d2$f0med,param_book_clean$f0med,paired=F)
wilcox.test(param_book_train_d3$f0med,param_book_clean$f0med,paired=F)

