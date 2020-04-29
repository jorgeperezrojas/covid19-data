# Datos COVID19 Chile

En este respositorio encontrarás versiones .csv de datos de COVID19 en Chile, la mayor parte de ellos obtenidos desde [esta planilla](https://docs.google.com/spreadsheets/d/1mLx2L8nMaRZu0Sy4lyFniDewl6jDcgnxB_d0lHG-boc). Los datos en la planilla se actualizan con cada nuevo informe en pdf entregado por el [Minsal](https://www.minsal.cl/) (sí, **en pdf**🤦‍♂️). Los datos en este repositorio los actualizaremos tan frecuemtemente como podamos (idealmente una vez al día) y dependiendo de si el Minsal sigue entregando informes y no cambia demasiado los formatos.

¡Pueden usar los datos para lo que quieran! Pero por favor🙏 sean responsables si hacen predicciones con ellos. Cualquiera que quiera ayudar es muy bienvenid@. Pueden hacer un *Pull Request*, abrir un *Issue* o escribir a jperez_arroba_dcc.uchile.cl.


## Datos

Los datos disponibles son los siguientes:
* [confirmados.csv](csv/confirmados.csv): Total de pacientes confirmados como positivos de COVID19 por región desde 07 de marzo de 2020
* [confirmados_comunas.csv](csv/confirmados_comunas.csv): Total de pacientes confirmados como positivos de COVID19 por comuna para datos reportados desde el lunes 30 de marzo (según el Minsal estos datos se liberarán los lunes, miércoles y viernes de cada semana)
* [pacientes_en_uci.csv](csv/pacientes_en_uci.csv): Total de pacientes hospitalizados en la Unidad de Cuidados Intensivos (UCI) por región desde el 1 de abril de 2020
* [muertes.csv](csv/muertes.csv): Total de muertes por COVID19 por región desde el 1 de abril de 2020
* [notificaciones.csv](csv/notificaciones.csv): Total de casos notificados (tests) por tipo de institución (ISP, Hospital, Privado) desde el 1 de abril de 2020 (Este es un dato un tanto ambiguo. El Minsal lo reporta como "tests notificados" pero parece estar ligado mas a un "reporte de sospecha" en el sistema Epivigila.)
* [resumen_nacional.csv](csv/resumen_nacional.csv): Cantidad total de contagiados y muertos en todo el país ordenados por día. No reportamos los pacientes "recuperados" pues la estadística reportada por el Minsal es nada más que una fórmula (`recuperados[t] = confirmados[t-14] - muertos[t]`).

## Fuentes

Los datos de este repositorio han sido extraidos desde fuentes oficiales a partir del 10 de marzo de 2020, principalmente desde los siguientes sitios:
* [Informes COVID 19 (Depto. Epidemiología Minsal)](http://epi.minsal.cl/informes-covid-19/) 
* [Reportes COVID 19 Diarios (Gobierno de Chile)](https://www.gob.cl/coronavirus/cifrasoficiales/#reportes)
* [Resumen diario COVID 19 (Minsal)](https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/)

La cantidad de confirmados en días previos al 10 de marzo, las obtuvimos desde informaciones varias de prensa.

**DISCLAIMER**: Hay que tener mucho ojo con los datos presentados en este repositorio, pues en su mayoría provienen de información oficial que ha esta en entredicho en varias ocasiones. Por esta misa razón, nosotros no reportamos a los *pacientes recuperados* pues de ninguna manera corresponden a datos sobre pacientes que se haya comprobado estén curados y no hayan recaído. Las cifras y todos los datos aquí alojados son de responsabilidad exclusiva del gobierno y del MINSAL y el uso que les des depende complemente de ti.

## Cómo usarlos

Puedes acceder a algunos de los datasets más importantes que aquí almacenamos por medio de la API del proyecto hermano [``chile-coronapi``](https://github.com/sanguineti/chile-coronapi). Dicha API consume los datos directamente desde este repositorio. Puedes consultar la documentación de [``chile-coronapi``](https://github.com/sanguineti/chile-coronapi) directamente desde su repositorio, en donde encontrarás todos los endpoints disponibles de momento.

Por otra parte, si sabes cómo usar un archivo .csv puedes simplemente descargar el que quieras y listo. Si no estás acostumbrad@ a usar archivos .csv puedes importarlo directamente por ejemplo en una planilla de [Google Sheets](https://docs.google.com/spreadsheets/) y usar la función `IMPORTDATA` de esta forma (en cualquier celda)
```
=IMPORTDATA("https://raw.githubusercontent.com/jorgeperezrojas/covid19-data/master/csv/confirmados.csv")
```
Lo anterior copia los datos de [confirmados.csv](csv/confirmados.csv) pero puedes copiar datos de cualquier otro archivo cambiando el nombre. En Excel seguro hay formas similares para descargar y usar los datos.

## Agradecimientos

Gente que ha aportado🥰 con datos, chequeos, código, reportando errores, etc.:
* [pbecerra](https://github.com/pabecerra)
* [Daniel Gomez M.](https://github.com/danielgomezm)
* [SebastiánR](https://twitter.com/SebastinR14)