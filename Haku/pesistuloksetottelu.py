import json
import sys
import requests
import pandas as pd

def hae_data(url):
    try:
        vastaus = requests.get(url, timeout=1000)
        vastaus.raise_for_status()
        return vastaus.content
    except requests.RequestException as e:
        print(f"Virhe datan haussa: {e}")
        sys.exit(1)


def tallenna_json(data, nimi):
    try:
        with open("f:/Ohjelmointitiedostot/Rtiedostot/Pesäpallo/Lyöntikartat/Pelit/" + nimi, "w", encoding="utf-8") as tiedosto:
            tiedosto.write(data.decode("utf-8"))
        print("JSON-tiedosto tallennettu onnistuneesti!")
    except FileNotFoundError:
        print(f"Tiedostoa ei löydy: {nimi}")
        sys.exit(1)


def lue_json(polku):
    try:
        with open("f:/Ohjelmointitiedostot/Rtiedostot/Pesäpallo/Lyöntikartat/Pelit/" + polku, "r", encoding="utf-8") as tiedosto:
            print("JSON-tiedosto luettu onnistuneesti!")
            return json.load(tiedosto)
    except FileNotFoundError:
        print(f"Tiedostoa ei löydy: {polku}")
        sys.exit(1)


def data_muokkaus(taulukko):
    try:
        taulukko["pesät"] = taulukko["events"].astype(str)
        taulukko["pesät"] = taulukko["pesät"].str.split(']').str[1].str.strip()
        taulukko["pesät"] = taulukko["pesät"].apply(lambda x: str(x).strip(", '"))
        taulukko["pesät"] = taulukko["pesät"].str.split('[').str[1].str.strip()
        taulukko[["lyöjä", 'ykkösellä', 'kakkosella', 'kolmosella','kotipesässä']] = taulukko['pesät'].str.split(', ', expand=True)
        
        # Jos lyöntijärjestys muuttuu kesken ottelun ja tulee virheitä. Ei pitäisi tulla, mutta joskus on käynyt.
        # taulukko[["lyöjä", 'ykkösellä', 'kakkosella', 'kolmosella','kotipesässä', 'kutonen', 'seiska', 'kasi', 'ysi','jokeri10', 'jokeri11', 'jokeri12']] = taulukko['pesät'].str.split(', ', expand=True)
        
        taulukko["tilastot"] = taulukko["events"].astype(str)
        taulukko["tilastot"] = taulukko["tilastot"].str.split('[').str[4].str.strip()
        taulukko["tilastot"] = taulukko["tilastot"].str.split(']').str[0].str.strip()
        
        taulukko["lisäätilastoja"] = taulukko["tilastot"].str.split('{').str[3].str.strip()
        taulukko["aputilasto"] = taulukko["tilastot"].str.split(', {').str[1].str.strip()
        
        taulukko["apusarake"] = taulukko["aputilasto"].str.split("', '").str[1].str.strip()
        taulukko["apusarake"] = taulukko["apusarake"].apply(lambda x: str(x).strip(", '"))
        
        taulukko["pesälle"] = taulukko["apusarake"]
        taulukko["pesälle"] = taulukko["pesälle"].str.split("'").str[4].str.strip()
        
        taulukko['etenijä'] = taulukko["apusarake"].str.split("}").str[0].str.strip()
        taulukko['etenijä'] = taulukko["etenijä"].str.split("': '").str[1].str.strip()
        taulukko['etenijä'] = taulukko["etenijä"].apply(lambda x: str(x).strip("'"))
        taulukko['etenijä'] = taulukko["etenijä"].str.lstrip("nan")
        
        taulukko["takaetenijä"] = taulukko["lisäätilastoja"]
        taulukko["takaetenijä"] = taulukko["takaetenijä"].str.split("}").str[0].str.strip()
        taulukko["takaetenijä"] = taulukko["takaetenijä"].str.split("'text':").str[1].str.strip()
        taulukko["takaetenijä"] = taulukko["takaetenijä"].apply(lambda x: str(x).strip(" ,'"))
        taulukko["takaetenijä"] = taulukko["takaetenijä"].str.lstrip("nan")
        
        taulukko["takaetenemistä"] = taulukko["lisäätilastoja"]
        taulukko["takaetenemistä"] = taulukko["takaetenemistä"].str.split("}").str[1].str.lstrip()
        taulukko["takaetenemistä"] = taulukko["takaetenemistä"].apply(lambda x: str(x).strip(" ,'"))
        taulukko["takaetenemistä"] = taulukko["takaetenemistä"].str.lstrip("nan")
        
        print('Pesälläolijat, etenijät ja lyöntitapahtumat on muokattu!')
    except (UnboundLocalError, ValueError, KeyError, AttributeError, TypeError) as e:
        print(F'Virhemuokkausesssa: {e}')

def taulukon_teko(dataframe):
    try:
        taulukko = pd.DataFrame(dataframe)
        taulukko["etenemisentapahtumanpesä"] = taulukko["pesälle"].astype(str) + taulukko["takaetenemistä"]
        taulukko["etenemisentapahtumanpesä"] = taulukko["etenemisentapahtumanpesä"].str.lstrip("nan")
        taulukko["etenemisentapahtuma"] = taulukko['etenijä'].astype(str) + taulukko["takaetenijä"] 
        taulukko["etenemisentapahtuma"] = taulukko["etenemisentapahtuma"].str.lstrip("nan")
        print('Yhdistämiset ja tietotyypin muokkaus')
        return(taulukko)
    except (UnboundLocalError, KeyError, AttributeError, TypeError, ValueError) as e:
        print(f"Virhe Taulukon:n muodostamisessa: {e}")

def tallenna_csv(dataframe, polku):
    try:
        dataframe.to_csv(
            "f:/Ohjelmointitiedostot/Rtiedostot/Pesäpallo/Lyöntikartat/Pelit/" + polku)
        print('CSV-tiedosto luotu onnistuneesti')
    except (FileNotFoundError, PermissionError, ValueError, AttributeError) as e:
        print(f"Virhe CSV:n tallentamisessa: {e}")
        sys.exit(1)


def main():
    
    ottelunId = input("Anna ottelun id: ")

    ottelunUrl = ("https://www.pesistulokset.fi/api/v1/online/" + ottelunId + "/events")

    ottelut_json_data = hae_data(ottelunUrl)

    ottelunJsonTiedosto = input(
        "Anna ottelun json-tiedoston nimi ilman .json-päätettä: ")

    ottelunCsvTiedosto = input(
        "Anna ottelun json-tiedostosta luotavan csv-tiedoston nimi ilman .csv päätettä: ")

    tallenna_json(ottelut_json_data, ottelunJsonTiedosto + ".json")

    json_tiedostosta = lue_json(ottelunJsonTiedosto + ".json")

    pandas_data = pd.json_normalize(json_tiedostosta, record_path='events')
    
    data_muokkaus(pandas_data)
    
    data = taulukon_teko(pandas_data)

    tallenna_csv(data, ottelunCsvTiedosto + ".csv")
    
    while True:
        suoritaUudestaan = input(
            "Suorita ohjelma uudestaan kyllä/ei (k/e): ")
        if suoritaUudestaan.lower() == "k":
            main()
        if suoritaUudestaan.lower() == "e":
            print("Ohjelma suljetaan")
            sys.exit(1)
        else:
            print('Painoit väärää kirjainta')
            
main()
