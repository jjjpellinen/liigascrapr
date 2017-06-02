source("funktiot.r")

# install.packages("rvest")
# install.packages("stringr")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("magrittr")

library(rvest)
library(stringr)
library(readr)
library(dplyr)
library(magrittr)


## n. 20-30 min.

pelaajadata <- pelaajahaku()


pelaajat <- pelaajadata$pelaajadata
maalivahdit <- pelaajadata$maalivahtidata


## csv-tiedostot 
write_csv(pelaajat, "pelaajat.csv")
write_csv(maalivahdit, "maalivahdit.csv")

save(pelaajat, maalivahdit, file="pelaajadata.RData")


########################
### datan käsittelyä ###
########################

pelaajat %>% 
  mutate(
    paikka = if_else(PP %in% c("7. P", "OP", "VP"), "P", 
                     if_else(PP %in% c("VH","OH","KH","13. H","14. H"), "H", ""))
  ) %>% 
  group_by(pelaajaID) %>% 
  #group_by(pelaajaID, kausi) %>% 
  summarise(
    nimi = unique(Nimi),
    ottelut = n(),
    maalit = sum(M),
    syotot = sum(S),
    pisteet = sum(P),
    laukaukset = sum(L),
    TOI = sum(Aika),
    TOIper = mean(Aika),
    rm = sum(R),
    maalit60 = maalit / TOI * 60,
    syotot60 = syotot / TOI * 60,
    pisteet60 = pisteet / TOI * 60,
    L. = maalit / laukaukset
  ) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  arrange(desc(pisteet60)) -> pelaajatilastoja


pt_yli200 <- pelaajatilastoja %>% dplyr::filter(TOI > 200)


maalivahdit %>% 
  mutate(
    SAG = TO + PM
  ) %>% 
  group_by(pelaajaID) %>% 
  summarise(
    nimi = unique(Nimi),
    GP = n(),
    SAG = sum(SAG),
    GA = sum(PM),
    SV. = 1 - GA / SAG,
    maalit = sum(M),
    syotot = sum(S),
    pisteet = sum(P),
    rm = sum(R),
    TOI = sum(Aika),
    SAG60 = SAG / TOI * 60
  ) %>% mutate_if(is.numeric, round, digits=3) -> mvtilastoja
  
mv_yli1000 <- mvtilastoja %>% dplyr::filter(TOI > 1000)

View(pelaajatilastoja)
View(pt_yli200)

View(mvtilastoja)
View(mv_yli1000)
