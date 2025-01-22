import sys
import requests
import pandas as pd


def hae_data(url):
    try:
        vastaus = requests.get(url, timeout=240)
        vastaus.raise_for_status()
        return vastaus.json()
    except requests.RequestException as e:
        print(f"Datan hakuvirhe {e}")
        sys.exit(1)


def perusdata(data):
    events_data = data["events"]

    normalisoitu_data = pd.json_normalize(events_data)

    return normalisoitu_data


def muokattu_data(data):
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

    data_json = []

    for i in eventsdata:
        row = {}
        for j in i["events"]:
            for text in j["texts"]:
                if isinstance(text, dict):

                    pointhits = text.get("pointhits")
                    out = text.get("out")
                    pointhitf = text.get("pointhitf")
                    score = text.get("score")
                    walkscore = text.get("walkscore")
                    wtscore = text.get("wtscore")
                    tailhits = text.get("tailhits")

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

                    if "hit" in text:
                        row["hit"] = text.get("hit")
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

    return norm_data


def yhdistys(dataperus, lisadata):
    data = dataperus.join(lisadata, on="hit.id", lsuffix="", rsuffix="testi")
    return data


def lisatallennus(data, polku):
    df_data = pd.DataFrame(data)

    df_data.to_csv(f"d:/Users/1060/Documents/{polku}lisa.csv")
    
    print("tallennuonnistui")


def main():

    ottelunid = input("Anna ottelun id: ")

    url = (
        f"https://api.pesistulokset.fi/api/v1/online/{ottelunid}/events")

    json_data = hae_data(url)

    data_perus = perusdata(json_data)

    data_lisa = muokattu_data(json_data)

    yhdistaminen = yhdistys(data_perus, data_lisa)

    lisatallennus(yhdistaminen, ottelunid)
    
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
