library(tidyverse)
library(ggforce)

luoliitonkenttä <- function() {
  ggplot() +

    # 3-pesä ja 2-pesä
    geom_arc(aes(x0 = 2, y0 = 44.5, start = 0, end = pi / 1, r = 3)) + # 3-pesä
    geom_arc(aes(x0 = 38, y0 = 44.5, start = -pi / 1, end = 0, r = 3)) + # 2-pesä

    # 1-pesä
    geom_arc(aes(x0 = 10.5, y0 = 25, start = -pi / 5, end = pi / 3, r = 3)) + # 1-pesä

    # Takaraja
    geom_line(aes(x = c(2, 38), y = c(96, 96))) + # Takaraja

    # 3-jatke ja 2-jatke
    geom_line(aes(x = c(2, 2), y = c(29, 96))) + # 3 pitkä viiva/sivuraja
    geom_line(aes(x = c(38, 38), y = c(38, 96))) + # 2 pitkä viiva

    # 3-pesän ja 2-pesän viivat
    geom_line(aes(x = c(-5, 2), y = c(41.5, 41.5))) + # 3 viiva
    geom_line(aes(x = c(38, 45), y = c(41.5, 41.5))) + # 2 viiva

    # 1-2 juoksuviiva
    geom_line(aes(x = c(7, 35.5), y = c(22, 43)), lineend = "round") + # 1-2 juoksuviiva

    # 2-3 juoksuviiva
    geom_line(aes(x = c(5, 35), y = c(44.5, 44.5))) + # 2-3 juoksuviiva

    # 3-raja ja 2-raja
    geom_line(aes(x = c(20, 2), y = c(10, 38)), lineend = "round") + # 3-raja
    geom_line(aes(x = c(22, 38), y = c(10, 38))) + # 2-raja

    # kotijuoksuviiva
    geom_line(aes(x = c(15, 2), y = c(10, 29))) +

    # kotipesäviiva
    geom_line(aes(x = c(14, 28), y = c(10, 10))) +

    # kotipesän kaaret
    geom_arc(aes(x0 = 21, y0 = 10, start = -pi / 2, end = -pi / 1, r = 5)) + # sisempikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 10, start = pi / 1, end = pi / 2, r = 5)) + # sisempikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 10, start = pi / 1, end = pi / 2, r = 7)) + # ulompikaari 1/4
    geom_arc(aes(x0 = 21, y0 = 10, start = pi / -1, end = pi / -2, r = 7)) + # ulompikaari 1/4

    geom_arc(aes(x0 = 21, y0 = 10, start = -pi / 3, end = pi / 3, r = 2)) + # etukaari

    # Muut
    coord_fixed() +
    geom_smooth(se = T) +
    theme_void()
}

otteluntiedot <- function(ottelu, kotijoukkue, vierasjoukkue) {
  ottelunData <- read.csv(ottelu, header = T, sep = ",")

  ottelunTaulukko <- data.frame(ottelunData)

  colnames(ottelunTaulukko)[colnames(ottelunTaulukko) == "X"] <-
    "tapahtumaid"

  ottelunTaulukko$team <- as.integer(ottelunTaulukko$team)

  ottelunTaulukko$batter <- as.integer(ottelunTaulukko$batter)

  ottelunTaulukko$hit.x <- as.numeric(ottelunTaulukko$hit.x)

  ottelunTaulukko$hit.y <- as.numeric(ottelunTaulukko$hit.y)

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
    data.frame(ottelunTaulukko %>% filter(team == kotijoukkue, period != 3, hitNumber != is.na(hitNumber)))

  vierasjoukkuedata <<-
    data.frame(ottelunTaulukko %>% filter(team == vierasjoukkue, period != 3, hitNumber != is.na(hitNumber)))
}

otteluntiedot("joma_pupe_kesken_kakkonen.csv", 12485, 12513)

#------------------------Kotikartat---------------------------------------------

kuvaaja_kotijoukkue <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata,
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkue("Virkiän lyönnit")

# Nollatilanne

kuvaaja_kotijoukkueen_nollatilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_nollatilanne("Virkiän nollatilanne")

# Ykköstilanne

kuvaaja_kotijoukkueen_ykköstilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_ykköstilanne("Virkiän ykköstilanne")

# Kakkostilanne

kuvaaja_kotijoukkueen_kakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_kakkostilanne("Virkiän Kakkostilanne")


kuvaaja_kotijoukkueen_ykköskakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_ykköskakkostilanne("Virkiän ykkös-kakkostilanne")


kuvaaja_kotijoukkueen_nollakakkostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_nollakakkostilanne("Virkiän nollakakkostilanne")

# Kolmostilanne

kuvaaja_kotijoukkueen_kolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}


kuvaaja_kotijoukkueen_kolmostilanne("Virkiän kolmostilanne")

kuvaaja_kotijoukkueen_kakkoskolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}


kuvaaja_kotijoukkueen_kakkoskolmostilanne("Virkiän takapesät täynnä tilanne")

kuvaaja_kotijoukkueen_ykköskolmostilanne <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella), ykkösellä != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}


kuvaaja_kotijoukkueen_ykköskolmostilanne("Virkiän 1-3 tilanne")

kuvaaja_kotijoukkueen_ajolähtö <- function(otsikko) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}


kuvaaja_kotijoukkueen_ajolähtö("Virkiän ajolähtötilanne")

#----------------------Vieraskartat---------------------------------------------

kuvaaja_vierasjoukkue <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata,
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkue("Mailattarien lyönnit")

# Nollatilanne

kuvaaja_vierasjoukkueen_nollatilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_nollatilanne("Virkiän nollatilanne")

# Ykköstilanne

kuvaaja_vierasjoukkueen_ykköstilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella)
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ykköstilanne("Virkiän ykköstilanne")

# Kakkostilanne

kuvaaja_vierasjoukkueen_kakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_kakkostilanne("Virkiän Kakkostilanne")


kuvaaja_vierasjoukkueen_ykköskakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ykköskakkostilanne("Virkiän ykkös-kakkostilanne")


kuvaaja_vierasjoukkueen_nollakakkostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
        is.na(kolmosella)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_nollakakkostilanne("Virkiän nollakakkostilanne")

# Kolmostilanne

kuvaaja_vierasjoukkueen_kolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_kolmostilanne("Virkiän kolmostilanne")

kuvaaja_vierasjoukkueen_kakkoskolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä)),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_kakkoskolmostilanne("Virkiän takapesät täynnä tilanne")

kuvaaja_vierasjoukkueen_ykköskolmostilanne <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella), ykkösellä != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ykköskolmostilanne("Virkiän 1-3 tilanne")

kuvaaja_vierasjoukkueen_ajolähtö <- function(otsikko) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}


kuvaaja_vierasjoukkueen_ajolähtö("Mailattarien ajolähtötilanne")

#------------------------Kotijoukkueen pelaajan kartta---------------------------

kuvaaja_kotijoukkueen_pelaaja <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaaja("Lonkaisen lyönnit", 8)

# Nollatilanne

kuvaaja_kotijoukkueen_pelaajan_nollatilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella), batter == {{ id }}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_nollatilanne("Virkiän nollatilanne", 1)

# Ykköstilanne

kuvaaja_kotijoukkueen_pelaajan_ykköstilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella), batter == {{ id }}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ykköstilanne("Virkiän ykköstilanne", 8868)

# Kakkostilanne

kuvaaja_kotijoukkueen_pelaajan_kakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_kakkostilanne("Virkiän Kakkostilanne", 7134)


kuvaaja_kotijoukkueen_pelaajan_ykköskakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ykköskakkostilanne("Iina Lehtisen ykkös-kakkostilanne", 7134)


kuvaaja_kotijoukkueen_pelaajan_nollakakkostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_nollakakkostilanne("Virkiän nollakakkostilanne", 7134)

# Kolmostilanne

kuvaaja_kotijoukkueen_pelaajan_kolmostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_kolmostilanne("Virkiän kolmostilanne", 7134)


kuvaaja_kotijoukkueen_pelaajan_kakkoskolmostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_kakkoskolmostilanne("Virkiän takapesät täynnä tilanne", 7134)


kuvaaja_kotijoukkueen_pelaajan_ykköskolmostilanne <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella), ykkösellä != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ykköskolmostilanne("Virkiän 1-3 tilanne", 7132)


kuvaaja_kotijoukkueen_pelaajan_ajolähtö <- function(otsikko, id) {
  kotijoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      kotijoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(kotijoukkueenlyönnit)
}

kuvaaja_kotijoukkueen_pelaajan_ajolähtö("Virkiän ajolähtötilanne", 7132)


#------------------------vierasjoukkueen pelaajan kartta------------------------

kuvaaja_vierasjoukkueen_pelaaja <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaaja("Tuikan lyönnit", 3)

# Nollatilanne

kuvaaja_vierasjoukkueen_pelaajan_nollatilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        is.na(ykkösellä) &
          is.na(kakkosella) & is.na(kolmosella), batter == {{ id }}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_nollatilanne("Virkiän nollatilanne", 8496)

# Ykköstilanne

kuvaaja_vierasjoukkueen_pelaajan_ykköstilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(
        ykkösellä != 0 &
          is.na(kakkosella) & is.na(kolmosella), batter == {{ id }}
      ),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ykköstilanne("Virkiän ykköstilanne", 8868)

# Kakkostilanne

kuvaaja_vierasjoukkueen_pelaajan_kakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_kakkostilanne("Virkiän Kakkostilanne", 7134)


kuvaaja_vierasjoukkueen_pelaajan_ykköskakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ykköskakkostilanne("Iina Lehtisen ykkös-kakkostilanne", 7134)


kuvaaja_vierasjoukkueen_pelaajan_nollakakkostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(is.na(ykkösellä) & kakkosella != 0 &
        is.na(kolmosella), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_nollakakkostilanne("Virkiän nollakakkostilanne", 7134)

# Kolmostilanne

kuvaaja_vierasjoukkueen_pelaajan_kolmostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_kolmostilanne("Virkiän kolmostilanne", 7134)


kuvaaja_vierasjoukkueen_pelaajan_kakkoskolmostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, kakkosella != 0, is.na(ykkösellä), batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_kakkoskolmostilanne("Virkiän takapesät täynnä tilanne", 7134)


kuvaaja_vierasjoukkueen_pelaajan_ykköskolmostilanne <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(kolmosella != 0, is.na(kakkosella), ykkösellä != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ykköskolmostilanne("Virkiän 1-3 tilanne", 7132)


kuvaaja_vierasjoukkueen_pelaajan_ajolähtö <- function(otsikko, id) {
  vierasjoukkueenlyönnit <-
    luoliitonkenttä() + geom_point(
      vierasjoukkuedata %>% filter(ykkösellä != 0 & kakkosella != 0 & kolmosella != 0, batter == {{ id }}),
      mapping = aes(x = xakseli, y = yakseli, fill = lyönninlopputulos),
      color = "black",
      pch = 21,
      size = 2
    ) + ggtitle(otsikko) + theme(plot.title = element_text(hjust = .5)) +
    scale_fill_manual(
      breaks = c(
        "False False ",
        "False False eteni",
        "False False eteni harhaheitolla",
        "False False karkasi",
        "False False sai vapaataipaleen väärien syöttöjen johdosta",
        "False False sai vapaataipaleen ulkopelaajan estäessä",
        "False False paloi",
        "False False paloi kärpäsenä",
        "False False paloi kärpäsenä lukkarin toimesta",
        "False True ",
        "False True paloi",
        "False True paloi kärpäsenä",
        "True False ",
        "True False eteni",
        "True False haavoittui",
        "False False haavoittui",
        "True False paloi",
        "True False paloi kärpäsenä",
        "True True "
      ),
      values = c(
        "white",
        "green",
        "olivedrab1",
        "lightgreen",
        "darkgreen",
        "seagreen1",
        "red",
        "pink",
        "pink",
        "grey",
        "cyan",
        "magenta",
        "orange",
        "green",
        "yellow",
        "yellow",
        "deeppink2",
        "brown",
        "black"
      ),
      labels = c(
        "Sisällä",
        "Vaihto",
        "Sisällä ja vaihto harhaheittolla",
        "Sisällä ja Vaihto karkauksella",
        "Sisällä ja vapaataival seuraavalla syötöllä",
        "Sisällä ja vapaataival estämisestä",
        "Palo",
        "Sisällä ja lyönnin jälkeen kärpänen",
        "Sisällä ja lyönnin jälkeen kärpänen lukkarin toimesta",
        "Laiton",
        "Viimeisellä lyönnillä laiton",
        "Laittoman jälkeen kärpänen",
        "Koppi",
        "Vaihto",
        "Haava",
        "Haava",
        "Koppi ja palo",
        "Koppilyönnin jälkeen kärpänen",
        "Laiton, koppi tai Laiton lyönti haettu kopiksi"
      ),
      name = "Lyönnin lopputulos ja tapahtumia"
    )

  return(vierasjoukkueenlyönnit)
}

kuvaaja_vierasjoukkueen_pelaajan_ajolähtö("Virkiän ajolähtötilanne", 7132)
