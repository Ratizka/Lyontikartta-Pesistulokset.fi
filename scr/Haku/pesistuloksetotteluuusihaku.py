import sys
import pandas as pd
import numpy as np
import requests


def hae_data(url):
    """
    Funktio hakee tiedot rajapinnasta. Katkaisee yhteyden 60 sekunnin kuluttua.

    Args:
        url (string): ottelun rajapinnan url

    Returns:
        json: Palauttaa json-tiedot
    """
    try:
        vastaus = requests.get(url, timeout=60)
        vastaus.raise_for_status()
        return vastaus.json()
    except requests.RequestException as e:
        print(f"Datan hakuvirhe {e}")
        sys.exit(1)


def perus_data(data):
    """
    Funktio saa json tiedot ja muokkaa ne taulukko muotoon
    Args:
        data (json): ottelun json data

    Returns:
        DataFrame: palauttaa taulukon json datasta
    """
    events_data = data["events"]

    normalisoitu_data = pd.json_normalize(events_data)

    df_normalisoitu_data = pd.DataFrame(normalisoitu_data)

    return df_normalisoitu_data


def lisa_data(data):
    """
    Funktio saa json tiedot ja silmukoiden avulla hakee tiedot listaan, josta tekee taulukon
    ja palauttaa sen
    Args:
        data (json): _description_

    Returns:
        DataFrame: palauttaa taulukon json datasta
    """
    eventsdata = data["events"]

    pointhits_to_key = {
        0: "kärkilyöntiykköselle",
        1: "kärkilyöntikakkoselle",
        2: "kärkilyöntikolmoselle",
        3: "kärkilyöntikotia"
    }

    tailhits_to_key = {
        0: "takaeteneminenykköselle",
        1: "saattokakkoselle",
        2: "saattokolmoselle",
    }

    pointhitf_to_key = {
        0: "epäonnistuminenykköselle",
        1: "epäonnistuminenkakkoselle",
        2: "epäonnistuminenkolmoselle",
        3: "epäonnistuminenkotia"
    }

    pointout_to_key = {
        1: "palo",
    }

    score_to_key = {
        3: "juoksu",
    }
    walkscore_to_key = {
        3: "juoksu",
    }

    wtscore_to_key = {
        3: "harhaheittojuoksu",
    }

    karpanen_to_key = {
        "paloi kärpäsenä": "karpanen",
    }

    vapaataival_to_key = {
        "sai vapaataipaleen väärien syöttöjen johdosta": "vapaataival",
    }

    estaminen_to_key = {
        "sai vapaataipaleen ulkopelaajan estäessä": "estäminen",
    }

    homerun_to_key = {
        2: "kunnari",
    }

    data_json = []

    for i in eventsdata:
        row = {}
        row['id'] = i.get('id')
        for j in i["events"]:
            for k in j["texts"]:
                if isinstance(k, dict):

                    pointhits = k.get("pointhits")
                    out = k.get("out")
                    pointhitf = k.get("pointhitf")
                    score = k.get("score")
                    walkscore = k.get("walkscore")
                    homerun = k.get("homerun")
                    wtscore = k.get("wtscore")
                    tailhits = k.get("tailhits")
                    text = k.get("text")

                    if pointhits in pointhits_to_key:
                        row[pointhits_to_key[pointhits]] = 1
                    if out in pointout_to_key:
                        row[pointout_to_key[out]] = 1
                    if pointhitf in pointhitf_to_key:
                        row[pointhitf_to_key[pointhitf]] = 1
                    if score in score_to_key:
                        row[score_to_key[score]] = 1
                    if walkscore in walkscore_to_key:
                        row[walkscore_to_key[walkscore]] = 1
                    if wtscore in wtscore_to_key:
                        row[wtscore_to_key[wtscore]] = 1
                    if tailhits in tailhits_to_key:
                        row[tailhits_to_key[tailhits]] = 1
                    if text in karpanen_to_key:
                        row[karpanen_to_key[text]] = 1
                    if text in vapaataival_to_key:
                        row[vapaataival_to_key[text]] = 1
                    if text in estaminen_to_key:
                        row[estaminen_to_key[text]] = 1
                    if homerun in homerun_to_key:
                        row[homerun_to_key[homerun]] = 1

                    if "hit" in k:
                        row["hit"] = k.get("hit")
                        row["lyoja"] = j.get("runnersAtBases")[0]
                        row["ykkospesa"] = j.get("runnersAtBases")[1]
                        row["kakkospesa"] = j.get("runnersAtBases")[2]
                        row["kolmospesa"] = j.get("runnersAtBases")[3]
                        row["kotipesa"] = j.get("runnersAtBases")[4]

            row["lyojajalkeen"] = j.get("runnersAtBases")[0]
            row["ykkospesajalkeen"] = j.get("runnersAtBases")[1]
            row["kakkospesajalkeen"] = j.get("runnersAtBases")[2]
            row["kolmospesajalkeen"] = j.get("runnersAtBases")[3]
            row["kotipesajalkeen"] = j.get("runnersAtBases")[4]
        data_json.append(row)

    norm_data = pd.json_normalize(data_json)

    df_norm_data = pd.DataFrame(norm_data)

    return df_norm_data


def yhdistys(dataperus, lisadata):
    """
    Funktio yhdistää argumentti saadut taulukot

    Args:
        dataperus (Dataframe): Funktion perusdata taulukko
        lisadata (Dataframe): Funktion lisa_data taulukko

    Returns:
        Dataframe: data taulukon
    """
    data = dataperus.merge(lisadata, on='id', how='left', suffixes=("", "_"))
    
    data['lahtotilanne'] =  np.where(data['lyoja'].isna() & data['ykkospesa'].isna() & data['kakkospesa'].isna() & data['kolmospesa'].isna(),
                                    '', 
                            np.where(data['ykkospesa'].isna() & data['kakkospesa'].isna() & data['kolmospesa'].isna(),
                                    '0',
                            np.where(data['ykkospesa'].notna() & data['kakkospesa'].isna() & data['kolmospesa'].isna(),
                                    '1',
                            np.where(data['ykkospesa'].notna() & data['kakkospesa'].notna() & data['kolmospesa'].isna(),
                                    '1-2', 
                            np.where(data['ykkospesa'].isna() & data['kakkospesa'].notna() & data['kolmospesa'].isna(),
                                    '0-2',
                            np.where(data['ykkospesa'].isna() & data['kakkospesa'].isna() & data['kolmospesa'].notna(),
                                    '0-3',
                            np.where(data['ykkospesa'].notna() & data['kakkospesa'].isna() & data['kolmospesa'].notna(),
                                    '1-3',
                            np.where(data['ykkospesa'].isna() & data['kakkospesa'].notna() & data['kolmospesa'].notna(),
                                    '2-3', 
                            np.where(data['ykkospesa'].notna() & data['kakkospesa'].notna() & data['kolmospesa'].notna(),
                                     'ajo', '')
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
    
    data['lopputilanne'] =  np.where(data['ykkospesajalkeen'].isna() & data['kakkospesajalkeen'].isna() & data['kolmospesajalkeen'].isna(),
                                    '0',
                            np.where(data['ykkospesajalkeen'].notna() & data['kakkospesajalkeen'].isna() & data['kolmospesajalkeen'].isna(),
                                    '1',
                            np.where(data['ykkospesajalkeen'].notna() & data['kakkospesajalkeen'].notna() & data['kolmospesajalkeen'].isna(),
                                    '1-2', 
                            np.where(data['ykkospesajalkeen'].isna() & data['kakkospesajalkeen'].notna() & data['kolmospesajalkeen'].isna(),
                                    '0-2',
                            np.where(data['ykkospesajalkeen'].isna() & data['kakkospesajalkeen'].isna() & data['kolmospesajalkeen'].notna(),
                                    '0-3',
                            np.where(data['ykkospesajalkeen'].notna() & data['kakkospesajalkeen'].isna() & data['kolmospesajalkeen'].notna(),
                                    '1-3',
                            np.where(data['ykkospesajalkeen'].isna() & data['kakkospesajalkeen'].notna() & data['kolmospesajalkeen'].notna(),
                                    '2-3', 
                            np.where(data['ykkospesajalkeen'].notna() & data['kakkospesajalkeen'].notna() & data['kolmospesajalkeen'].notna(),
                                    'ajo', '')
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
    return data


def tallennus(data, tiedostonimi):
    """

    Funktio saa taulukon ja tallentaa sen csv-tiedostoon

    Args:
        data (Dataframe): Taulukko
        tiedostonimi (string): csv-tiedoston nimi
    """
    data.to_csv(f"d:/Users/1060/Documents/{tiedostonimi}.csv", index=False)

    print("Tallennus onnistui!")


def main():
    ottelunid = input("Anna ottelun id: ")

    url = (
        f"https://api.pesistulokset.fi/api/v1/online/{ottelunid}/events")

    json_data = hae_data(url)

    data_perus = perus_data(json_data)

    data_lisa = lisa_data(json_data)

    yhdistaminen = yhdistys(data_perus, data_lisa)

    tallennus(yhdistaminen, ottelunid)

    while True:
        suorita_uudestaan = input(
            "Suorita ohjelma uudestaan kyllä/ei (k/e): ")
        if suorita_uudestaan.lower() == "k":
            main()
        if suorita_uudestaan.lower() == "e":
            print("Ohjelma suljetaan")
            sys.exit(1)
        else:
            print('Painoit väärää kirjainta')


main()
