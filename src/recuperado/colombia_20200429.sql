.headers on
.mode csv
.import csv/colombia_20200429.csv covid_colombia
.output tiempo_colombia.csv

SELECT * FROM
(
    SELECT 
        COUNT(*) as recuperados
    FROM
        covid_colombia
    WHERE
        "Fecha recuperado" <> "-   -"
) AS T0
JOIN
(
    SELECT
        SUM(CASE WHEN 
                julianday("Fecha recuperado") - julianday("Fecha diagnostico") >= 14 
            THEN 1 
            ELSE 0 END) as mas_de_14_desde_diagnostico
    FROM 
        covid_colombia
    WHERE
        "Fecha recuperado" <> "-   -"
) AS T1;