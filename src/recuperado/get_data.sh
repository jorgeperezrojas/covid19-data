wget http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip -O mexico.zip
unzip mexico.zip
mv *.csv mexico.csv
rm mexico.zip
wget https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=DOWNLOAD -O colombia.csv
mv mexico.csv csv/.
mv colombia.csv csv/.

# diccionario de datos para datos mexicanos: http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip
