#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readr")


library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#setwd()
#Change accordingly with path to your directory containing the csv file 


summup_oslr <- read_delim("summup_oslr.csv", ";",
                          escape_double = FALSE,
                          col_types = cols(`#spk` = col_integer(), 
                                           `#spk_f` = col_integer(), 
                                           `#spk_m` = col_integer(), 
                                           elicited = col_factor(levels = c("yes","no")), 
                                           found_in = col_factor(levels = c("metadata",
                                                                            "indexed", 
                                                                            "paper", 
                                                                            "manually")),
                                           lang_status = col_factor(levels = c("Low-resource","High-resource")),
                                           provided = col_factor(levels = c("yes","no")), 
                                           sampling = col_factor(levels = c("8000",
                                                                            "16000", 
                                                                            "22050", 
                                                                            "44100", 
                                                                            "48000")),
                                           lang_status = col_factor(levels = c("Low-resource","High-resource")),
                                           task = col_factor(levels = c("ASR","TTS"))),
                          locale = locale(decimal_mark = ",", grouping_mark = ""), 
                          trim_ws = TRUE)


# Global statistics,  #resources, languages, with and without accent/dialect variation
corpora <- summup_oslr %>% group_by(id) %>% summarize() # 53 speech resources
lang <- summup_oslr %>% group_by(lang) %>% summarize() # 33 different languages 
lang_dial <- summup_oslr %>% group_by(lang,dial) %>% summarize() #51 dial/accent variations

# In total 66 resources: if we divide resources containing different languages and/or accents/dialects
summary(summup_oslr$provided)
# 24/66 = 36,4% of the corpora without gender info
nprov<-subset(summup_oslr,provided=="no")
summary(nprov$`#spk`)
# in which 12 with no info about speakers at all


#Creating a subset with all corpora with gender info provided
prov <- subset(summup_oslr,provided=="yes")
summary(prov)

#Count where info was found
prov_stat <- prov %>% group_by(found_in) %>% summarize(count=n())

#Create a subset containing only corpora with gender info 
#(different from the one above as we sometimes looked for the info manually)
summup_full <- subset(summup_oslr,!is.na(`#spk_f`))
gender <- summup_full %>% summarise(spk_f = sum(`#spk_f`),spk_m = sum(`#spk_m`))
#3050 women et 3022 men - total : 6072 speakers

#Examining gender difference regarding elicitation status
gender_elicited <- summup_full %>% group_by(elicited) %>% summarise(count = n(),
                                                                    spk_f = sum(`#spk_f`),
                                                                    spk_m = sum(`#spk_m`),
                                                                    spk_total = spk_m+spk_f,
                                                                    per_f = spk_f/spk_total*100,
                                                                    per_m =spk_m/spk_total*100)

#Creating a subset without Librispeech
summup_woLS <- subset(summup_full, id!="SLR12")

gender_elicited_woLS_s <- summup_woLS %>% group_by(elicited,size) %>% summarise(count = n(),
                                                                         spk_f = sum(`#spk_f`),
                                                                         spk_m = sum(`#spk_m`),
                                                                         spk_total = spk_m+spk_f,
                                                                         per_f = spk_f/spk_total*100,
                                                                         per_m =spk_m/spk_total*100)

# We observe that after removing Librispeech, gender balance in terms of number of speakers is achieved in elicited corpora 



#Examining gender difference regarding language status in elicited corpora
summup_eli <- subset(summup_full,elicited=="yes")
gender_lang_eli <- summup_eli %>% group_by(lang_status) %>% summarise(count = n(),
                                                                         spk_f = sum(`#spk_f`),
                                                                         spk_m = sum(`#spk_m`),
                                                                         spk_total = spk_m+spk_f,
                                                                         per_f = spk_f/spk_total*100,
                                                                         per_m =spk_m/spk_total*100)


#Examining gender difference regarding the task
gender_task<- summup_full %>% group_by(task) %>% summarise(count = n(),
                                                           spk_f = sum(`#spk_f`),
                                                           spk_m = sum(`#spk_m`),
                                                           spk_total = spk_m+spk_f,
                                                           per_f = spk_f/spk_total*100,
                                                           per_m =spk_m/spk_total*100)

#More women present for TTS task


#Crossing parameters : task, elicitation status and language status
gender_task_stat <- summup_full %>% group_by(task,elicited,lang_status) %>% summarise(count = n(),
                                                                            spk_f = sum(`#spk_f`),
                                                                            spk_m = sum(`#spk_m`),
                                                                            spk_total = spk_m+spk_f,
                                                                            per_f = spk_f/spk_total*100,
                                                                            per_m =spk_m/spk_total*100)


#Examining gender difference at the utterance level
utt<-subset(summup_oslr,`#utt_m`!="NA") # We create a subset containing corpora with gendered utterance info provided
gender_utt <- utt %>% summarize(corpora=n(),
                                nb_m=sum(utt$`#spk_m`,na.rm=T),
                                nb_f=sum(utt$`#spk_f`,na.rm=T),
                                sum_utt_m=sum(utt$`#utt_m`),
                                sum_utt_f=sum(utt$`#utt_f`))

gender_utt2 <- utt %>% group_by(elicited) %>% summarize(corpora=n(),nb_m=sum(`#spk_m`,na.rm=T),
                                                        nb_f=sum(`#spk_f`,na.rm=T),
                                                        sum_utt_m=sum(`#utt_m`),
                                                        sum_utt_f=sum(`#utt_f`))

#Caution: 3 outliers, corpora mono-genre containing only male speakers, that are distorting the total number of utterances.
#If we look at the means, no difference between man and women in non elicited corpora



#Evolution in time
ggplot(summup_oslr,aes(x=year,fill=factor(provided,labels=c('Oui','Non'))))+geom_bar()+
  labs(x="",y="Nombre de corpus",fill="Informations\nsur le genre")+
  scale_x_continuous(lim=c(2010,2020), breaks = c(2010, 2012, 2014, 2016, 2018, 2020))+
  scale_fill_manual(values=c("aquamarine3","aquamarine4"))+
  theme(axis.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        panel.grid.major.y = element_line(linetype=3))

                     
