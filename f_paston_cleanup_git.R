#
# TRAVELERS LAB
# Clean redo of all Paston entries
#
# Last Edit: 10.25.2017
#
#---------------------------------------------------------------------------------------
# packages
library(stringr)
library(ggplot2)
library(mosaic)
library(dplyr)
library(tidyr)
#-----------------------
setwd("C:/Users/Liza/Desktop")
data <- read.csv("pastons_full_try.csv")
subset1 <- data.frame(letters = data$to_from,
                    location = data$location,
                    mentions = data$contacts,
                    date1 = data$date1,
                    date2 = data$date2)

subset1$letters<-gsub("Clement Pastonto John Paston", "Clement Paston to John Paston", 
                      subset1$letters)
subset1$fixed.letter<-str_replace_all(subset1$letters, "[[:punct:]]", " ")

subset1 <- filter(subset1, grepl("to ", fixed.letter))
subset1 <- filter(subset1,!grepl("Deed|Memorandum|Passport|
                                 misinformed|Indenture|Introductory|Letter|
                                 manuscript|etition|Defeasance|Copy|Errands|
                                 Memorial|Message|Schedule|copy|Corporation|
                                 manuscript|Writ|Collections|relating|ardon|will|
                                 emorial",
                                 subset1$fixed.letter))
subset1$date2 <- str_match(subset1$date2, ".{4}")
subset1$date2 <- as.numeric(subset1$date2)

test<-str_split_fixed(subset1$fixed.letter, " to ", 2)

#--------------------------------------------------

# clean up names

subset1$from<-gsub(" to .*$", "", subset1$fixed.letter)
subset1$to<-gsub(".* to","", subset1$fixed.letter)

# split an "and" entry

subset1<-subset1 %>%
  mutate(from = strsplit(subset1$from, " and "))%>%
  unnest(from)

#________________________

subset1$from <- str_trim(subset1$from)
subset1$to <- str_trim(subset1$to)

# from entries

subset1$from<-gsub("\\s{2,}", " ", subset1$from)
subset1$from<-gsub("Sir J ohn Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Eliz C lere", "Elizabeth Clere", subset1$from)
subset1$from<-gsub("R obert Clere", "Robert Clere", subset1$from)
subset1$from<-gsub("Fragment of a letter from Edmund Paston", "Edmund Paston", subset1$from)
subset1$from<-gsub("Grant by Margaret Paston", "Margaret Paston", subset1$from)
subset1$from<-gsub("Paston was misinformed at", "Paston", subset1$from)
subset1$from<-gsub("J P John Paston", "John Paston", subset1$from)
subset1$from<-gsub("The Earl of Oxford s Steward", "The Earl of Oxford's Steward", subset1$from)

# Spelling fixes
subset1$from<-gsub("Sir Jo Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Sir J Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Sir John Hevenyngham", "Sir John Heveningham", subset1$from)
subset1$from<-gsub("John Gyne", "John Gyney", subset1$from)
subset1$from<-gsub("John Osbbern", "John Osbern", subset1$from)
subset1$from<-gsub("John John Pampyng", "John Pampyng", subset1$from)
subset1$from<-gsub("John Pampynge", "John Pampyng", subset1$from)
subset1$from<-gsub("Richard Call", "Richard Calle", subset1$from)
subset1$from<-gsub("Richard Callee", "Richard Calle", subset1$from)
subset1$from<-gsub("Richard Suthwell", "Richard Southwell", subset1$from)
subset1$from<-gsub("Thomas Playters", "Thomas Playter", subset1$from)
subset1$from<-gsub("Thomas Platter", "Thomas Playter", subset1$from)
subset1$from<-gsub("Thomas Plaiter", "Thomas Playter", subset1$from)
subset1$from<-gsub("W C", "William Cotyng", subset1$from)
subset1$from<-gsub("W Coting", "William Cotyng", subset1$from)
subset1$from<-gsub("W Cotyng", "William Cotyng", subset1$from)
subset1$from<-gsub("William Botober", "William Worcester", subset1$from)
subset1$from<-gsub("William Botoner", "William Worcester", subset1$from)
subset1$from<-gsub("W Lomner", "William Lomner", subset1$from)
subset1$from<-gsub("William Lomnor", "William Lomner", subset1$from)
subset1$from<-gsub("Hugh A Fenne", "Hugh Fenn", subset1$from)
subset1$from<-gsub("John Bocking", "John Bockyng", subset1$from)
subset1$from<-gsub("Osbert Mundeford", "Osbert Mundford", subset1$from)
subset1$from<-gsub("Sir Thomas Howes", "Thomas Howes", subset1$from)
subset1$from<-gsub("Thomas Howys", "Thomas Howes", subset1$from)

# Spelling fixes, less sure
subset1$from<-gsub("John Wyke", "John Wykes", subset1$from)
subset1$from<-gsub("William Pekoc", "William Peroc", subset1$from)
subset1$from<-gsub("Caster Halle", "Caister", subset1$from)

##########################################
### The John Paston Recode ###
##########################################

# John Paston II
subset1$from<-gsub("Sir John Paston", "John Paston II", subset1$from)
subset1$from<-gsub("John Paston The Eldest Son", "John Paston II", subset1$from)
subset1$from<-gsub("John Paston The Elder Son", "John Paston II", subset1$from)
subset1$from<-gsub("John Paston Junior", "John Paston II", subset1$from)

# John Paston III
subset1$from<-gsub("John Paston the Younger", "John Paston III", subset1$from)
subset1$from<-gsub("John Paston the Youngest", "John Paston III", subset1$from)
subset1$from<-gsub("John Paston of Gelston", "John Paston III", subset1$from)
subset1$from<-gsub("J Paston of Gelston", "John Paston III", subset1$from)

# John Paston I
subset1$from<-gsub("John Paston", "John Paston I", subset1$from)
subset1$from<-gsub("John Paston the Elder", "John Paston I", subset1$from)

#------------------------------------------
# To entries
#------------------------------------------

subset1$from<-gsub("\\s{2,}", " ", subset1$from)
subset1$from<-gsub("Sir J ohn Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Eliz C lere", "Elizabeth Clere", subset1$from)
subset1$from<-gsub("R obert Clere", "Robert Clere", subset1$from)
subset1$from<-gsub("Fragment of a letter from Edmund Paston", "Edmund Paston", subset1$from)
subset1$from<-gsub("Grant by Margaret Paston", "Margaret Paston", subset1$from)
subset1$from<-gsub("Paston was misinformed at", "Paston", subset1$from)
subset1$from<-gsub("J P John Paston", "John Paston", subset1$from)
subset1$from<-gsub("The Earl of Oxford s Steward", "The Earl of Oxford's Steward", subset1$from)

# Spelling fixes
subset1$from<-gsub("Sir Jo Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Sir J Fastolf", "Sir John Fastolf", subset1$from)
subset1$from<-gsub("Sir John Hevenyngham", "Sir John Heveningham", subset1$from)
subset1$from<-gsub("John Gyne", "John Gyney", subset1$from)
subset1$from<-gsub("John Osbbern", "John Osbern", subset1$from)
subset1$from<-gsub("John John Pampyng", "John Pampyng", subset1$from)
subset1$from<-gsub("John Pampynge", "John Pampyng", subset1$from)
subset1$from<-gsub("Richard Call", "Richard Calle", subset1$from)
subset1$from<-gsub("Richard Callee", "Richard Calle", subset1$from)
subset1$from<-gsub("Richard Suthwell", "Richard Southwell", subset1$from)
subset1$from<-gsub("Thomas Playters", "Thomas Playter", subset1$from)
subset1$from<-gsub("Thomas Platter", "Thomas Playter", subset1$from)
subset1$from<-gsub("Thomas Plaiter", "Thomas Playter", subset1$from)
subset1$from<-gsub("W C", "William Cotyng", subset1$from)
subset1$from<-gsub("W Coting", "William Cotyng", subset1$from)
subset1$from<-gsub("W Cotyng", "William Cotyng", subset1$from)
subset1$from<-gsub("William Botober", "William Worcester", subset1$from)
subset1$from<-gsub("William Botoner", "William Worcester", subset1$from)
subset1$from<-gsub("W Lomner", "William Lomner", subset1$from)
subset1$from<-gsub("William Lomnor", "William Lomner", subset1$from)
subset1$from<-gsub("Hugh A Fenne", "Hugh Fenn", subset1$from)
subset1$from<-gsub("John Bocking", "John Bockyng", subset1$from)
subset1$from<-gsub("Osbert Mundeford", "Osbert Mundford", subset1$from)
subset1$from<-gsub("Sir Thomas Howes", "Thomas Howes", subset1$from)
subset1$from<-gsub("Thomas Howys", "Thomas Howes", subset1$from)

# Spelling fixes, less sure
subset1$from<-gsub("John Wyke", "John Wykes", subset1$from)
subset1$from<-gsub("William Pekoc", "William Peroc", subset1$from)
subset1$from<-gsub("Caster Halle", "Caister", subset1$from)

##########################################
### The John Paston Recode ###
##########################################

# John Paston II
subset1$from<-gsub("Sir John Paston", "John Paston II", subset1$from)
subset1$from<-gsub("John Paston The Eldest Son", "John Paston II", subset1$from)

subset1$from<-gsub("Sir Johnny Paston", "John Paston II", subset1$from)

subset1$from<-gsub("John Paston The Elder Son", "John Paston II", subset1$from)
subset1$from<-gsub("John Paston Junior", "John Paston II", subset1$from)

# John Paston III
subset1$from<-gsub("John Paston the Younger", "John Paston III", subset1$from)
subset1$from<-gsub("John Paston the Youngest", "John Paston III", subset1$from)
subset1$from<-gsub("John Paston of Gelston", "John Paston III", subset1$from)
subset1$from<-gsub("J Paston of Gelston", "John Paston III", subset1$from)

# John Paston I
subset1$from<-gsub("John Paston", "John Paston I", subset1$from)
subset1$from<-gsub("John Paston the Elder", "John Paston I", subset1$from)



save(subset1, file="fall_paston.csv")
