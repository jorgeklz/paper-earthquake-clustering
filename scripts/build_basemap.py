"""Construye el mapa base vectorial (docs/basemap.js) para la web de visualización.

Fuentes:
  - Provincias de Ecuador: geoBoundaries gbOpen ECU ADM1 (CC BY 4.0), versión simplificada.
  - Países vecinos: Natural Earth 10m admin 0 (dominio público).

Uso:
  python3 scripts/build_basemap.py <geoBoundaries-ECU-ADM1_simplified.geojson> <ne_10m_admin_0_countries.geojson>
Requiere shapely.
"""
import json
import sys
from pathlib import Path

from shapely.geometry import box, mapping, shape

BBOX = box(-84.5, -5.5, -74.5, 2.8)  # Ecuador continental y zona marítima
TOL = 0.004  # tolerancia de simplificación en grados


def rounded(geom):
    def rnd(c):
        if isinstance(c[0], (int, float)):
            return [round(c[0], 3), round(c[1], 3)]
        return [rnd(x) for x in c]
    g = mapping(geom)
    return {"type": g["type"], "coordinates": rnd(g["coordinates"])}


def main(adm1_path, ne_path):
    feats = []
    adm1 = json.loads(Path(adm1_path).read_text(encoding="utf-8"))
    for f in adm1["features"]:
        g = shape(f["geometry"]).intersection(BBOX)
        if g.is_empty:
            continue
        g = g.simplify(TOL, preserve_topology=True)
        feats.append({"type": "Feature", "properties": {"name": f["properties"]["shapeName"], "kind": "province"},
                      "geometry": rounded(g)})
    ne = json.loads(Path(ne_path).read_text(encoding="utf-8"))
    for f in ne["features"]:
        if f["properties"]["ADM0_A3"] not in ("COL", "PER"):
            continue
        g = shape(f["geometry"]).intersection(BBOX).simplify(TOL, preserve_topology=True)
        if g.is_empty:
            continue
        feats.append({"type": "Feature", "properties": {"name": f["properties"]["NAME_ES"], "kind": "neighbor"},
                      "geometry": rounded(g)})
    out = Path(__file__).resolve().parent.parent / "docs" / "basemap.js"
    out.write_text("window.BASEMAP=" + json.dumps({"type": "FeatureCollection", "features": feats},
                                                  separators=(",", ":"), ensure_ascii=False) + ";\n", encoding="utf-8")
    print(f"{out}: {len(feats)} features, {out.stat().st_size/1024:.0f} KB")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
