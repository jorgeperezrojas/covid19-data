.headers on
.mode csv
.import csv/mexico.csv covid_mexico
.output tiempo_mexico.csv

SELECT * FROM
(
    SELECT
        COUNT(*) as muertos
    FROM 
        covid_mexico
    WHERE
        RESULTADO = 1 AND
        FECHA_DEF <> '9999-99-99'
) AS T0
JOIN
(
    SELECT
        AVG(julianday(FECHA_DEF) - julianday(FECHA_SINTOMAS)) as tpo_medio_sintomas_muerte
    FROM 
        covid_mexico
    WHERE
        RESULTADO = 1 AND
        FECHA_DEF <> '9999-99-99' AND
        julianday(FECHA_DEF) >= julianday(FECHA_SINTOMAS)
) AS T1 
JOIN
(
    SELECT
        SUM(CASE WHEN 
                julianday(FECHA_DEF) - julianday(FECHA_SINTOMAS) >= 10 
            THEN 1 
            ELSE 0 END) as mas_de_10_desde_sintomas,
        SUM(CASE WHEN 
                julianday(FECHA_DEF) - julianday(FECHA_SINTOMAS) >= 14 
            THEN 1 
            ELSE 0 END) as mas_de_14_desde_sintomas
    FROM 
        covid_mexico
    WHERE
        RESULTADO = 1 AND
        FECHA_DEF <> '9999-99-99'
) AS T2;   
