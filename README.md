# Lyontikartta-Pesistulokset.fi

Hakee pesistulokset.fi ottelun tiedot Python kielellä ja piirtää lyöntikartan R-kielellä.

## Vaatimukset

Asenna Python osoitteesta (https://www.python.org/downloads/)

Asenna requests, pandas ja numpy kirjastot käyttäen komentokehotetta:

`pip install requests`

`pip install pandas`

`pip install numpy`

Asenna R osoitteesta (https://www.r-project.org/)

Asenna R-Studio osoitteesta (https://posit.co/download/rstudio-desktop/)

Asenna R-kirjastot tidyverse ja ggforce

`install.packages("tidyverse")`

`install.packages("ggforce")`

## Käyttö

Muuta aluksi pesistuloksetotteluuusihaku.py csv-tiedoston polku haluamakseen. Muita hakuja ei tueta enää ja poistuvat, kun saan piirtämisen korjattua.

Sen jälkeen mene (https://www.pesistulokset.fi) ja klikkaa haluamaasi ottelu auki. Ottelun id löytyy (https://www.pesistulokset.fi/ottelut/58626) ottelut/{586262} kohdalta. Esimerkiksi kyseisen ottelun id on 58626.

Suorita pesistuloksetotteluntiedot.py ja anna komentokehotteessa id. Esimerkiksi 58626, jonka jälkeen hakee pesistulokset.fi API:sta tiedot. Anna tiedostonimet ilman tiedostopäätteitä.

Sen jälkeen ohjelma kysyy haluatko suorittaa ohjelman uudestaan. Painamalla k-kirjainta suorittaa uudestaan ja e-kirjaimella sammuttaa. Väärällä kirjaimella kysyy uudestaan valinnan.

Seuraavaksi avaa R-tiedosto R-studiolla ja valitse hakemisto, jossa ottelusta tehty csv-tiedosto on. Anna otteluntiedot funktioon tiedostonimi ja joukkueiden id:t. Esimerkiksi `otteluntiedot("ottelunid.csv", 12500, 12502)`. Joukkueen id löytää esimerkisi (https://www.pesistulokset.fi/joukkue/joma/8778), jossa numerosarja on joukkueen kauden id.

Sitten voit piirtää eri tilanteet käyttämällä eri funktioita. Joukkuefunktiot ottavat parametrinä kuvaajan otsikon. Pelaajien kodalta parametrina on ottelun lisäksi pelaajan id, joka löytyy pelaajan sivulta esimerkiksi (https://www.pesistulokset.fi/pelaaja/8440).

## Tietoa

Päivityksen myötä on lisätty kunnareiden, saattojen ja muita tilastoja, joita voi käyttää, jos sarjassa käytetään suurta pöytäkirjaa. Toimivat, vaikka lyöntien koordinaatteja ei merkata. Sen lisäksi on lisätty lyönnin alkutilanne ja lopputilanne, josta voidaan tarkastella esimerkiksi, miten paljon lyödään tiettyyn tilanteeseen. Piirtminen päivitetään jossain vaiheessa, kun jaksan tehdä sen.

Lisäämällä kuvaajissa `filter(hit.batter_player_id == {{id}}, hit.hit_number == {{lyöntinumero}})` saa piiretty joko ensimmäisen, toisen tai kolmannen lyönnin lyöntikartan. Period, inning ja batTurn saa vaihdettua jakson, vuoroparin tai aloittava ja lopettavan vuoroparin välillä. Ei sisällytä kotiutuslyöntikilpailua karttoihin. Kotiutuslyöntikilpailun saa mukaan `period != 3` poistamalla.

Kirjausohjelman dokumentointi löytyy [kirjausohjelma](https://docs.google.com/document/d/1fxeVdm7g9F1dQ3o3n5QWrRvtDppo1BuLj0SJgn5FSlE/). Sieltä löytyy lisää tietoa, miten lyönnit kirjataan ja muuta vastaavaa.

## Lopuksi

Piirtäminen on tehty R-kielellä, koska saa helpommin tehtyä kuvaajat. Jos on ongelmia, laittakaa Issues pyyntöä. Korjaan mahdolliset ongelmat, kun on aikaa. Kehitysehdotukset voi laittaa Pull Requests kohdalle, jonka jälkeen laitan käytäntöön.
