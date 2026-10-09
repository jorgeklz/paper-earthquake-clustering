# Unsupervised Pattern Recognition for Geographical Clustering of Seismic Events Post Mw 7.8 Ecuador Earthquake
Jorge Parraga-Alava, Gustavo Molina, Roberth Alcivar and Mario Inostroza-Ponta

## Visualización interactiva

La carpeta `docs/` tiene una web estática con el mapa de réplicas, los grupos MST-kNN, una línea de tiempo animada, gráficos de Omori y Gutenberg-Richter, y una comparación con DBSCAN. Para verla localmente:

```bash
python3 -m http.server -d docs
```

Los datos de la web se regeneran con `python3 scripts/prepare_web_data.py`. El análisis de los datos y las mejoras propuestas están en [MEJORAS.md](MEJORAS.md).
