# Lyontikartta-Pesistulokset.fi

Hakee pesistulokset.fi ottelun tiedot ja piirtää lyöntikartan R-kielellä

# Vaatimukset

Asenna Python osoitteesta (https://www.python.org/downloads/) 

Asenna requests ja pandas kirjastot käyttäen komentokehotetta:

`pip install requests`

`pip install pandas`

Asenna R osoitteesta (https://www.r-project.org/)

Asenna R-Studio osoitteesta (https://posit.co/download/rstudio-desktop/)

Asenna R-kirjastot tidyverse ja ggforce

`install.packages("tidyverse")`

`install.packages("ggforce")`

# Käyttö

Muuta aluksi otteluntiedot.py tiedostopolut haluamaasi kohteeseen.

Sen jälkeen mene (https://www.pesistulokset.fi) ja klikkaa haluamaasi ottelu auki. Ottelun id löytyy (https://www.pesistulokset.fi/ottelut/58626) ottelut/tästä kohdalta.

Suorita pesistuloksetotteluntiedot.py ja anna komentokehotteessa id. Esimerkiksi 58626, jonka jälkeen hakee pesistulokset.fi API:sta tiedot.

Anna tiedostonimet ilman tiedostopäätteitä.

Sen jälkeen ohjelma kysyy haluatko suorittaa ohjelman uudestaan. Painamalla k-kirjainta suorittaa uudestaan ja e-kirjaimella sammuttaa. Väärällä kirjaimella kysyy uudestaan valinnan.

Seuraavaksi avaa R-tiedosto R-studiolla ja valitse hakemisto, jossa ottelusta tehty csv-tiedosto on. Anna otteluntiedot funktioon tiedostonimi ja joukkueiden id:t. Esimerkiksi `otteluntiedot("virkiä_mailattaret.csv", 12500, 12502)`. Joukkueen id löytää esimerkisi (https://www.pesistulokset.fi/joukkue/joma/8778), jossa numerosarja on joukkueen kaudenid.

Sitten voit piirtää eri tilanteet käyttämällä eri funktioita. Joukkuefunktiot ottavat parametrinä kuvaajan otsikon. Pelaajien ottaa sen lisäksi pelaajan id, joka löytyy pelaajan sivulta esimerkiksi (https://www.pesistulokset.fi/pelaaja/8440).

# Tietoa

Vaihdot ovat merkitty niiden lopputulosten mukaan, eikä erottele kärki tai kärjentakana tapahtuvia vaihtoja. Tilanteissa kuten 1-2 jne. tilanteessa ovat todennäköisesti kärjenvaihtoja. Aivan varmaksi ei voi sanoa, kun on mahdollisesti virheitä taulukon muodostamisessa. Osa tilanteista ei liity lyöntisuoritukseen, mutta tulevat samalla, kun siivoaa events listaa json-tiedostosta. 

Lisäämällä kuvaajissa `filter(hit.batter_player_id == {{id}}), hit.hit_numeber == {{lyöntinumero}}` saa eri piiretty joko ensimmäisen, toisen tai kolmannen lyöntikartan. Period, inning ja batTurn saa vaihdettua jakson, vuoroparin tai aloittava ja lopettavan vuoroparin välillä. Ei sisällytä kotiutuslyöntikilpailua karttoihin. Kotiutuslyöntikilpailun saa mukaan `period != 3` poistamalla tuon kohdan. 

# Lopuksi

Jos on ongelmia, laittakaa Issues pyyntöä. Korjaan mahdolliset ongelmat, jos pystyn. Kehitysehdotukset voi laittaa Pull Requests kohdalle, jonka jälkeen laitan käytäntöön. Kehitysehdotuksia voi laittaa erityisesti datan_muokkaus funktioon. Kesken ottelun toimivat koodit tulevat jossain vaiheessa, kun saan kirjoittua R-koodin. 
