"""Genera docs/data.js a partir de sismos.csv y clustering.results.table.csv.

Corrige dos problemas del CSV original que el script en R no maneja:
  1. Fechas en dos formatos: 'dd-mm-aaaa H:M' (may 2016 a 2018) y 'm-dd-aaaa H:M:S' (abril 2016).
  2. Codificación ISO-8859-1 del símbolo de grado.

Las filas de clustering.results.table.csv conservan el número de fila original de sismos.csv
(nombres de fila de R), así que se unen por esa posición para recuperar la fecha de cada evento.

Uso: python3 scripts/prepare_web_data.py
"""
import json
import re
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
# La hora local se interpreta como si fuera UTC y luego se desplaza +5 h (Ecuador continental, UTC-5).
LOCAL_TO_UTC = timedelta(hours=5)


def parse_local(s):
    s = s.strip()
    for fmt in ("%d-%m-%Y %H:%M", "%m-%d-%Y %H:%M:%S"):
        try:
            return datetime.strptime(s, fmt).replace(tzinfo=timezone.utc)
        except ValueError:
            pass
    raise ValueError(f"Fecha no reconocida: {s!r}")


def coord(s):
    m = re.match(r"\s*([\d.]+)\D*([NSEW])", s)
    v = float(m.group(1))
    return -v if m.group(2) in "SW" else v


def city(s):
    if s.strip() == "nas":
        return None
    s = re.sub(r"^(a\s+)?[\d.]+\s*km\s+(de\s+)?", "", s.strip())
    return s.strip()


def main():
    raw = pd.read_csv(ROOT / "sismos.csv", encoding="latin1")
    res = pd.read_csv(ROOT / "clustering.results.table.csv")  # índice = fila original (base 1)
    cluster_by_row = dict(zip(res.index.astype(int), res["cluster"].astype(int)))

    events = []
    for i, r in raw.iterrows():
        row = i + 1
        local = parse_local(r["LocalHour"])
        depth = None if str(r["Depth"]).strip() == "-" else float(r["Depth"])
        events.append([
            int((local + LOCAL_TO_UTC).timestamp() // 60),  # minutos UTC desde epoch
            round(float(r["Mag"]), 2),
            depth,
            coord(r["Lat"]),
            coord(r["Long"]),
            r["Region"].replace("Ecuador - ", "").replace("Cañar", "Canar"),
            city(r["CloserCity"]),
            cluster_by_row.get(row),
            row,
        ])
    events.sort(key=lambda e: e[0])
    payload = {
        "fields": ["tmin", "mag", "depth", "lat", "lon", "region", "city", "cluster", "row"],
        "events": events,
        "mainshock": {"tmin": int((datetime(2016, 4, 16, 18, 58, tzinfo=timezone.utc) + LOCAL_TO_UTC).timestamp() // 60),
                      "mag": 7.8, "depth": 20.6, "lat": 0.382, "lon": -79.922},
    }
    out = ROOT / "docs" / "data.js"
    out.write_text("window.QUAKES=" + json.dumps(payload, separators=(",", ":"), ensure_ascii=False) + ";\n",
                   encoding="utf-8")
    n_cl = sum(e[7] is not None for e in events)
    print(f"{out}: {len(events)} eventos, {n_cl} con cluster MST-kNN")


if __name__ == "__main__":
    main()
