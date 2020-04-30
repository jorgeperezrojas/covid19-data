# RECUPERADO

Este repositorio replica los cálculos de la nota ["RECUPERADO: La estadística del Minsal que se burla de los chilenos más afectados por el Coronavirus"](https://users.dcc.uchile.cl/~jperez/covid/recuperado.html) (publicada también por [CIPER](https://ciperchile.cl/2020/04/30/academico-acusa-que-estadistica-del-minsal-considera-recuperados-a-personas-que-estan-en-riesgo-de-morir/)). Para poder replicarlos se necesita solo tener instalado [`sqlite`](https://sqlite.org/index.html)

### Datos de México y Colombia

Después de clonar el repositorio (`git clone https://github.com/jorgeperezrojas/covid19-data.git`) desde la raíz, correr el script 

```
bash src/recuperado/get_data.sh
```

Esto generará dos archivos `mexico.csv` y `colombia.csv` en la carpeta `csv` con datos desagregados de ambos países descargados desde sus respectivos sitios de datos abiertos (links en referencias).

Luego de descargar los datos, para obtener la información de muertos en México y sus tiempos desde el inicio de síntomas, debes hacer:
```
sqlite3 < src/recuperado/mexico.sql
```
Esto generará un archivo .csv en la raíz del repositorio con nombre `tiempo_mexico.csv`. 

Similarmente para los datos de Colombia, debes hacer:
```
sqlite3 < src/recuperado/colombia.sql
```
lo que generará el archivo `tiempo_colombia.csv`.

Al día 28 de abril de 2020, los datos obtenidos son los siguientes:

* `tiempo_mexico.csv`:

|muertos|tpo_medio_sintomas_muerte|mas_de_10_desde_sintomas|mas_de_14_desde_sintomas|
|-------|--------|-------|-------|
|1569|10.146|741|383|

* `tiempo_colombia.csv`

|recuperados|mas_de_14_desde_diagnostico|
|-----|-----|
|1269|353|

Por favor nota que si ejecutas este código un dís distinto al 28 de abril de 2020, posiblemente obtengas datos distintos. Si quieres replicar exactamente los datos acá presentados, usa los archivos
`src/recuperado/mexico_20200429.sql` y `src/recuperado/colombia_20200429.sql` cuando ejecutar las líneas del `sqlite3`.


### Referencias

* [Datos abiertos covid México](https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico)
* [Datos abiertos covid Colombia](https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr/data)


