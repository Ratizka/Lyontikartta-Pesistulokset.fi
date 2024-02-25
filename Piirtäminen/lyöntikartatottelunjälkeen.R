library(tidyverse)
library(ggforce)

luoliitonkenttä <- function () {
  ggplot() +
    
    #3-pesä ja 2-pesä
    geom_arc(aes(x0 = 2, y0 = 44.5, start = 0, end = pi / 1, r = 3 )) + #3-pesä
    geom_arc(aes(x0 = 40, y0 = 44.5, start = -pi / 1, end = 0, r = 3 )) + #2-pesä
    
    # 1-pesä
    geom_arc(aes(x0 = 10, y0 = 24.5, start = -pi/5, end = pi/3, r = 3 )) + #1-pesä
    
    #Takaraja
    geom_line(aes(x = c(2, 40), y = c(96, 96))) + #Takaraja
    
    #3-jatke ja 2-jatke
    geom_line(aes(x = c(2, 2), y = c(29, 96))) + #3 pitkä viiva/sivuraja
    geom_line(aes(x = c(40, 40), y = c(38, 96)))  + # 2 pitkä viiva
    
    # 3-pesän ja 2-pesän viivat
    geom_line(aes(x = c(-5,2), y = c(41.5, 41.5))) + #3 viiva 
    geom_line(aes(x = c(40,47), y = c(41.5, 41.5))) + #2 viiva 
    
    # 1-2 juoksuviiva
    geom_line(aes(x = c(6,37.5), y = c(21.5,43)), lineend = 'round')  + # 1-2 juoksuviiva
    
    # 2-3 juoksuviiva
    geom_line(aes(x = c(5, 37), y = c(44.5,44.5)))  + # 2-3 juoksuviiva
    
    # 3-raja ja 2-raja
    geom_line(aes(x = c(20, 2), y = c(6,38)),lineend = 'round') + # 3-raja
    geom_line(aes(x = c(22, 40), y = c(6,38))) + # 2-raja
    
    # kotijuoksuviiva
    geom_line(aes(x = c(15, 2), y = c(6,29)))  + 
    
    # kotipesäviiva
    geom_line(aes(x = c(14, 28), y = c(6,6))) +
    
    #kotipesän kaaret
    geom_arc(aes(x0 = 21, y0 = 6, start = -pi/2, end = -pi/1, r = 5 )) + #sisempikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 6, start = pi/1, end = pi/2, r = 5 )) + #sisempikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 6, start = pi/1, end = pi/2, r = 7 )) +  #ulompikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 6, start = pi/-1, end = pi/-2, r = 7 )) +  #ulompikaari 1/4
    
    #Muut
    coord_fixed() + geom_smooth(se = F) + theme_void()
  
  # Kenttä on neljä metriä kapeampi
}

#-----------------Ottelun tiedot------------------------------------------------

otteluntiedot <- function(ottelu, kotijoukkue, vierasjoukkue) {
  
  ottelunData <- read.csv(ottelu, header = T, sep = ",")
  
  ottelunTaulukko <- data.frame(ottelunData)
  
  colnames(ottelunTaulukko)[colnames(ottelunTaulukko) == 'X'] <-
    'tapahtumaid'
  
  ottelunTaulukko$team <- as.integer(ottelunTaulukko$team)
  
  ottelunTaulukko$batter <- as.integer(ottelunTaulukko$batter)
  
  ottelunTaulukko$hit.id <- as.integer(ottelunTaulukko$hit.id)
  
  ottelunTaulukko$hit.team_id <- as.integer(ottelunTaulukko$hit.team_id)
  
  ottelunTaulukko$hit.x <- as.numeric(ottelunTaulukko$hit.x)
  
  ottelunTaulukko$hit.y <- as.numeric(ottelunTaulukko$hit.y)
  
  ottelunTaulukko$hit.batter_player_id <- as.integer(ottelunTaulukko$hit.batter_player_id)
  
  ottelunTaulukko$lyöjä <- as.numeric(ottelunTaulukko$lyöjä)
  
  ottelunTaulukko$ykkösellä <- as.numeric(ottelunTaulukko$ykkösellä)
  
  ottelunTaulukko$kakkosella <-
    as.numeric(ottelunTaulukko$kakkosella)
  
  ottelunTaulukko$kolmosella <-
    as.numeric(ottelunTaulukko$kolmosella)
  
  ottelunTaulukko$kotipesässä <-
    as.numeric(ottelunTaulukko$kotipesässä)
  
  ottelunTaulukko$xakseli <- ottelunTaulukko$hit.x / 2.5
  
  ottelunTaulukko$yakseli <- abs(ottelunTaulukko$hit.y - 100)
  
  ottelunTaulukko$hit.caught <-
    as.factor(ottelunTaulukko$hit.caught)
  
  ottelunTaulukko$hit.out <- as.factor(ottelunTaulukko$hit.out)
  
  ottelunTaulukko$etenemisentapahtuma <- as.factor(ottelunTaulukko$etenemisentapahtuma)
  
  ottelunTaulukko$etenemisentapahtumanpesä <- as.factor(ottelunTaulukko$etenemisentapahtumanpesä)
  
  ottelunTaulukko$lyönninlopputulos <-
    as.factor(paste(ottelunTaulukko$hit.caught, ottelunTaulukko$hit.out, ottelunTaulukko$etenemisentapahtuma))
  
  kotijoukkuedata <<-
    data.frame(ottelunTaulukko %>% filter(hit.team_id == kotijoukkue, period != 3))
  
  vierasjoukkuedata <<-
    data.frame(ottelunTaulukko %>% filter(hit.team_id == vierasjoukkue, period != 3))
  
}

otteluntiedot("tiedostonimi", 12500, 12502)


#------------------------Kotikartat---------------------------------------------

kuvaaja_kotijoukkue <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata ,
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia'
    )
  
      return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkue("Kuvaajan otsikko")

# Nollatilanne

kuvaaja_kotijoukkueen_nollatilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_nollatilanne("Kuvaajan otsikko")

# Ykköstilanne

kuvaaja_kotijoukkueen_ykköstilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_ykköstilanne("Kuvaajan otsikko")

# Kakkostilanne

kuvaaja_kotijoukkueen_kakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_kakkostilanne("Kuvaajan otsikko")


kuvaaja_kotijoukkueen_ykköskakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_ykköskakkostilanne("Kuvaajan otsikko")


kuvaaja_kotijoukkueen_nollakakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_nollakakkostilanne("Kuvaajan otsikko")

# Kolmostilanne

kuvaaja_kotijoukkueen_kolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}


kuvaaja_kotijoukkueen_kolmostilanne("Kuvaajan otsikko")

kuvaaja_kotijoukkueen_kakkoskolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}


kuvaaja_kotijoukkueen_kakkoskolmostilanne("Kuvaajan otsikko")

kuvaaja_kotijoukkueen_ykköskolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella) ,ykkösellä != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}


kuvaaja_kotijoukkueen_ykköskolmostilanne("Kuvaajan otsikko")

kuvaaja_kotijoukkueen_ajolähtö <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}


kuvaaja_kotijoukkueen_ajolähtö("Kuvaajan otsikko")

#----------------------Vieraskartat---------------------------------------------

kuvaaja_vierasjoukkue <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata ,
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia'
    )
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkue("Kuvaajan otsikko")

# Nollatilanne

kuvaaja_vierasjoukkueen_nollatilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_nollatilanne("Kuvaajan otsikko")

# Ykköstilanne

kuvaaja_vierasjoukkueen_ykköstilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ykköstilanne("Kuvaajan otsikko")

# Kakkostilanne

kuvaaja_vierasjoukkueen_kakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_kakkostilanne("Kuvaajan otsikko")


kuvaaja_vierasjoukkueen_ykköskakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ykköskakkostilanne("Kuvaajan otsikko")


kuvaaja_vierasjoukkueen_nollakakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
                                   is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_nollakakkostilanne("Kuvaajan otsikko")

# Kolmostilanne

kuvaaja_vierasjoukkueen_kolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_kolmostilanne("Kuvaajan otsikko")

kuvaaja_vierasjoukkueen_kakkoskolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}


kuvaaja_vierasjoukkueen_kakkoskolmostilanne("Kuvaajan otsikko")

kuvaaja_vierasjoukkueen_ykköskolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella) ,ykkösellä != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}


kuvaaja_vierasjoukkueen_ykköskolmostilanne("Kuvaajan otsikko")

kuvaaja_vierasjoukkueen_ajolähtö <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}


kuvaaja_vierasjoukkueen_ajolähtö("Kuvaajan otsikko")

#------------------------Kotijoukkueen pelaajan kartta---------------------------

kuvaaja_kotijoukkueen_pelaaja <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia'
    )
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaaja("Kuvaajan otsikko ja id tulee toisena argumenttina", 7132)

# Nollatilanne

kuvaaja_kotijoukkueen_pelaajan_nollatilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella), hit.batter_player_id == {{id}}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_nollatilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 8496)

# Ykköstilanne

kuvaaja_kotijoukkueen_pelaajan_ykköstilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella), hit.batter_player_id == {{id}}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ykköstilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 8868)

# Kakkostilanne

kuvaaja_kotijoukkueen_pelaajan_kakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_kakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_kotijoukkueen_pelaajan_ykköskakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ykköskakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_kotijoukkueen_pelaajan_nollakakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_nollakakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina",7134)

# Kolmostilanne

kuvaaja_kotijoukkueen_pelaajan_kolmostilanne <- function(otsikko,id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0,hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_kolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_kotijoukkueen_pelaajan_kakkoskolmostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä),hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}

kuvaaja_kotijoukkueen_pelaajan_kakkoskolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_kotijoukkueen_pelaajan_ykköskolmostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella) ,ykkösellä != 0, hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}

kuvaaja_kotijoukkueen_pelaajan_ykköskolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7132)


kuvaaja_kotijoukkueen_pelaajan_ajolähtö <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0, hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(kotijoukkueenlyönnit)
  
}

kuvaaja_kotijoukkueen_pelaajan_ajolähtö("Kuvaajan otsikko ja id tulee toisena argumenttina", 7132)


#------------------------vierasjoukkueen pelaajan kartta------------------------

kuvaaja_vierasjoukkueen_pelaaja <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia'
    )
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaaja("Kuvaajan otsikko ja id tulee toisena argumenttina", 10028)

# Nollatilanne

kuvaaja_vierasjoukkueen_pelaajan_nollatilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella), hit.batter_player_id == {{id}}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_nollatilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 8496)

# Ykköstilanne

kuvaaja_vierasjoukkueen_pelaajan_ykköstilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella), hit.batter_player_id == {{id}}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ykköstilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 8868)

# Kakkostilanne

kuvaaja_vierasjoukkueen_pelaajan_kakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_kakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_vierasjoukkueen_pelaajan_ykköskakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ykköskakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_vierasjoukkueen_pelaajan_nollakakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
                                   is.na(kolmosella), hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    )+ ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_nollakakkostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina",7134)

# Kolmostilanne

kuvaaja_vierasjoukkueen_pelaajan_kolmostilanne <- function(otsikko,id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0,hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_kolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_vierasjoukkueen_pelaajan_kakkoskolmostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä),hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}

kuvaaja_vierasjoukkueen_pelaajan_kakkoskolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7134)


kuvaaja_vierasjoukkueen_pelaajan_ykköskolmostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella) ,ykkösellä != 0, hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}

kuvaaja_vierasjoukkueen_pelaajan_ykköskolmostilanne("Kuvaajan otsikko ja id tulee toisena argumenttina", 7132)


kuvaaja_vierasjoukkueen_pelaajan_ajolähtö <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0, hit.batter_player_id == {{id}}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = 'black',
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks =  c(
        'False False ',
        'False False eteni',
        'False False eteni harhaheitolla',
        'False False karkasi',
        'False False sai vapaataipaleen väärien syöttöjen johdosta',
        'False False sai vapaataipaleen ulkopelaajan estäessä',
        'False False paloi' ,
        'False False paloi kärpäsenä',
        'False False paloi kärpäsenä lukkarin toimesta',
        'False True ',
        'False True paloi',
        'False True paloi kärpäsenä',
        'True False ',
        'True False eteni',
        'True False haavoittui',
        'False False haavoittui',
        'True False paloi',
        'True False paloi kärpäsenä',
        'True True '
      ),
      values = c(
        'white',
        'green' ,
        'olivedrab1',
        'lightgreen',
        'darkgreen',
        'seagreen1',
        'red',
        'pink',
        'pink',
        'grey',
        'cyan',
        'magenta',
        'orange',
        'green',
        'yellow',
        'yellow',
        'deeppink2',
        'brown',
        'black'
      ),
      labels = c(
        'Sisällä',
        'Vaihto',
        'Sisällä ja vaihto harhaheittolla',
        'Sisällä ja Vaihto karkauksella',
        'Sisällä ja vapaataival seuraavalla syötöllä',
        'Sisällä ja vapaataival estämisestä',
        'Palo',
        'Sisällä ja lyönnin jälkeen kärpänen',
        'Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta',
        "Laiton",
        'Viimeisellä lyönnillä laiton',
        'Laittoman jälkeen kärpänen',
        "Koppi",
        'Vaihto',
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = 'Lyönnin lopputulos ja tapahtumia')
  
  return(vierasjoukkueenlyönnit)
  
}

kuvaaja_vierasjoukkueen_pelaajan_ajolähtö("Kuvaajan otsikko ja id tulee toisena argumenttina", 7132)
