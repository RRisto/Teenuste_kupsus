#küpsustaseme määramine ministeeriumite ja allasutuste lõikes
#laiasid andmeid vaja, hoiame alles vajalikud muutujad
var=c("ministeerium", "allasutus", "omanikunimi", "omanikuamet" ,
      "omanikutelefon", "omanikuemail")
andmedLaiSub=andmedLai[,var]

#teeme uue colmni, kuhu paneme väärtuse 1 kui on omanik (e-mail või telefon või
#nimi)
andmedLaiSub$OnOmanik=ifelse(andmedLaiSub$omanikunimi==""&
                               andmedLaiSub$omanikuamet==""&
                               andmedLaiSub$omanikuemail==""&
                               andmedLaiSub$omanikutelefon=="", 0, 1)
#ministeeriumite lõikes osakaal
library(dplyr)
OmanikMinLoikes=andmedLaiSub%>%
  group_by(ministeerium)%>%
  summarise(OmanikeOsakaal = round(mean(OnOmanik)*100),
            teenusteArv=n())%>%
  mutate(kupsus=ifelse(OmanikeOsakaal==100, 3, 2)
  )

##allasutuste loikes
OmanikAsutusLoikes=andmedLaiSub%>%
  group_by(ministeerium, allasutus)%>%
  summarise(omanikOsakaal = round(mean(OnOmanik)*100),
            teenusteArv=length(OnOmanik))%>%
  mutate(kupsus=ifelse(omanikOsakaal==100, 3, 2)
  )

##LEIAME ASUTUSED, KUS ON tase 3
tase3=OmanikAsutusLoikes$allasutus[OmanikAsutusLoikes$kupsus==3]

##andmedPikk võtame välja subseti, kus need asutused
andmedPikkSub=andmedPikk[andmedPikk$allasutus%in% tase3,]

##kui paljudel kanalitel on olemas mõõdikud
moodikAsutusLoikes=andmedPikkSub %>%
  group_by(ministeerium,allasutus) %>%
  summarise(onMoodik=sum(!is.na(value)),
            kokku=n(),
            moodikOsakaal=onMoodik/kokku)%>%
  mutate(kupsus=ifelse(moodikOsakaal>=0.8, 4, 3))

#tulemused
taseOmanik=OmanikAsutusLoikes[OmanikAsutusLoikes$kupsus==2,c("ministeerium",
                                                  "allasutus", "kupsus")]
taseMoodik=moodikAsutusLoikes[,c("ministeerium","allasutus","kupsus")]
#paneme need kokku
kupsustasemed=rbind(taseOmanik, taseMoodik)
kupsustasemed=kupsustasemed[order(kupsustasemed$ministeerium),]

#######teeme funktsiooni asutuse küpsuse arvutamiseks
KupsusTaseAsutus=function(andmedPikk, andmedLai) {
  #muutujad, mida vajame
  var=c("ministeerium", "allasutus", "omanikunimi", "omanikuamet" ,
        "omanikutelefon", "omanikuemail")
  andmedLaiSub=andmedLai[,var]
  #teeme uue colmni, kuhu paneme väärtuse 1 kui on omanik (e-mail või 
  #telefon või nimi)
  andmedLaiSub$OnOmanik=ifelse(andmedLaiSub$omanikunimi==""&
                                 andmedLaiSub$omanikuamet==""&
                                 andmedLaiSub$omanikuemail==""&
                                 andmedLaiSub$omanikutelefon=="", 0, 1)
  #kupsus teenuste omanike olemasolu mõistes
  library(dplyr)
  OmanikAsutusLoikes=andmedLaiSub%>%
    group_by(ministeerium, allasutus)%>%
    summarise(omanikOsakaal = round(mean(OnOmanik)*100),
              teenusteArv=length(OnOmanik))%>%
    mutate(kupsus=ifelse(omanikOsakaal==100, 3, 2)) 
  #võtame välja, kus tase 3 olemas
  tase3=OmanikAsutusLoikes$allasutus[OmanikAsutusLoikes$kupsus==3]
  #äkki on neil kõrgem tase e 80% teenuste igal kanalil on mõõdik olemas
  andmedPikkSub=andmedPikk[andmedPikk$allasutus %in% tase3, ]
  moodikAsutusLoikes=andmedPikkSub %>%
    group_by(ministeerium,allasutus) %>%
    summarise(onMoodik=sum(!is.na(value)),
              moodikArv=n(),
              moodikOsakaal=onMoodik/moodikArv)%>%
    mutate(kupsus=ifelse(moodikOsakaal>=0.8, 4, 3))
  #paneme andmed kokku ja näitame välja
  OmanikAsutusLoikes[OmanikAsutusLoikes$kupsus==3,]$kupsus=
    moodikAsutusLoikes$kupsus
  #paneme ministeeriumite kaupa järjekorda
  OmanikAsutusLoikes[order(OmanikAsutusLoikes$ministeerium, 
                           OmanikAsutusLoikes$allasutus),
                c("ministeerium", "allasutus", "teenusteArv","kupsus")]
}

#näide
proov=KupsusTaseAsutus(andmedPikk, andmedLai)

####funktsioon ministeeruimi küpsustaseme arvutamiseks
KupsusTaseMin=function(andmedPikk, andmedLai) {
  #muutujad, mida vajame
  var=c("ministeerium", "omanikunimi", "omanikuamet" ,
        "omanikutelefon", "omanikuemail")
  andmedLaiSub=andmedLai[,var]
  #teeme uue colmni, kuhu paneme väärtuse 1 kui on omanik (e-mail või 
  #telefon või nimi)
  andmedLaiSub$OnOmanik=ifelse(andmedLaiSub$omanikunimi==""&
                                 andmedLaiSub$omanikuamet==""&
                                 andmedLaiSub$omanikuemail==""&
                                 andmedLaiSub$omanikutelefon=="", 0, 1)
  #kupsus teenuste omanike olemasolu mõistes
  library(dplyr)
  OmanikMinLoikes=andmedLaiSub%>%
    group_by(ministeerium)%>%
    summarise(omanikOsakaal = round(mean(OnOmanik)*100),
              teenusteArv=length(OnOmanik))%>%
    mutate(kupsus=ifelse(omanikOsakaal==100, 3, 2)) 
  #võtame välja, kus tase 3 olemas
  tase3=OmanikMinLoikes$ministeerium[OmanikMinLoikes$kupsus==3]
  andmedPikkSub=andmedPikk[andmedPikk$ministeerium %in% tase3, ]
  #äkki on neil kõrgem tase e 80% teenuste igal kanalil on mõõdik olemas
  moodikMinLoikes=andmedPikkSub %>%
    group_by(ministeerium) %>%
    summarise(onMoodik=sum(!is.na(value)),
              moodikArv=n(),
              moodikOsakaal=onMoodik/moodikArv)%>%
    mutate(kupsus=ifelse(moodikOsakaal>=0.8, 4, 3))
  #paneme andmed kokku ja näitame välja
  OmanikMinLoikes[OmanikMinLoikes$kupsus==3,]$kupsus=
    moodikMinLoikes$kupsus
  #paneme ministeeriumite kaupa järjekorda
  OmanikMinLoikes[order(OmanikMinLoikes$ministeerium, 
                           OmanikMinLoikes$ministeerium),
                     c("ministeerium", "teenusteArv","kupsus")]
}

##proovime
proov=KupsusTaseMin(andmedPikk, andmedLai)





