# Datos COVID19 Chile

En este respositorio encontrarás versiones .csv de datos de COVID19 en Chile, la mayor parte de ellos obtenidos desde [esta planilla](https://docs.google.com/spreadsheets/d/1mLx2L8nMaRZu0Sy4lyFniDewl6jDcgnxB_d0lHG-boc). Los datos en la planilla se actualizan con cada nuevo informe en pdf entregado por el [Minsal](https://www.minsal.cl/) (sí, **en pdf**🤦‍♂️). Los datos en este repositorio los actualizaremos tan frecuemtemente como podamos (idealmente una vez al día) y dependiendo de si el Minsal sigue entregando informes y no cambia demasiado los formatos.

¡Pueden usar los datos para lo que quieran! Pero por favor🙏 sean responsables si hacen predicciones con ellos. Cualquiera que quiera ayudar es muy bienvenid@. Pueden hacer un *Pull Request*, abrir un *Issue* o escribir a jperez_arroba_dcc.uchile.cl.

## Datos

Los datos disponibles son los siguientes:
* [confirmados.csv](csv/confirmados.csv): Total de pacientes confirmados como positivos de COVID19 por región desde 07 de marzo de 2020
* [confirmados_comunas.csv](csv/confirmados_comunas.csv): Total de pacientes confirmados como positivos de COVID19 por comuna para datos reportados los días 30 de marzo y 1, 3, 6 y 8 de abril (no hay más informes detallados por comuna)
* [pacientes_en_uci.csv](csv/pacientes_en_uci.csv): Total de pacientes hospitalizados en la Unidad de Cuidados Intensivos (UCI) por región desde el 1 de abril de 2020
* [muertes.csv](csv/muertes.csv): Total de muertes por COVID19 por región desde el 1 de abril de 2020
* [notificaciones.csv](csv/notificaciones.csv): Total de casos notificados (tests) por tipo de institución (ISP, Hospital, Privado) desde el 1 de abril de 2020

## Fuentes

Todos los datos de este repositorio han sido extraidos desde fuentes oficiales a partir del 10 de marzo de 2020, principalmente desde los siguientes sitios:
* [Informes COVID 19 (Depto. Epidemiología Minsal)](http://epi.minsal.cl/informes-covid-19/) 
* [Reportes COVID 19 Diarios (Gobierno de Chile)](https://www.gob.cl/coronavirus/cifrasoficiales/#reportes)
* [Resumen diario COVID 19 (Minsal)](https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/)

La cantidad de confirmados en días previos al 10 de marzo, las obtuvimos desde informaciones varias de prensa. No reportamos los pacientes de COVID19 recuperados pues hay serias dudas de que este número sea fidedigno y tenga algún sentido práctico.

## Cómo usarlos

Si sabes cómo usar un archivo .csv puedes simplemente descargar el que quieras y listo. Si no estás acostumbrad@ a usar archivos .csv puedes importarlo directamente por ejemplo en una planilla de [Google Sheets](https://docs.google.com/spreadsheets/) y usar la función `IMPORTDATA` de esta forma (en cualquier celda)
```
=IMPORTDATA("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados.csv")
```
Lo anterior copia los datos de [confirmados.csv](csv/confirmados.csv) pero puedes copiar datos de cualquier otro archivo cambiando el nombre. En Excel seguro hay formas similares para descargar y usar los datos.

## Agradecimientos

Hasta ahora han aportado 🥰:
* [pbecerra](https://github.com/pabecerra)
* [Daniel Gomez M.](https://github.com/danielgomezm)
