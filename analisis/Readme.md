Análisis de datos COVID-19 Chile - Escuela de Salud Pública UChile
==================================================================

En este repositorio encontrarás resultados de algunos análisis que
realizamos como parte de los reportes que un equipo de académicos y
académicas de la [Escuela de Salud Pública de la Universidad de
Chile](http://www.saludpublica.uchile.cl) realiza semana a semana. Los
informes semanales que están publicados directamente en la página web de
la Escuela, no obstante este repositorio tiene por objetivo dejar
disponible para la comunidad las bases de datos procesadas, gráficos y
código utilizado. La mayor parte de los datos utilizados para estos
analizados son obtenidos directamente del repositorio de a [Jorge
Pérez](https://github.com/jorgeperezrojas/covid19-data) quien
gentilmente aloja también nuestros resultados.

¡Pueden usar los datos para lo que quieran! Agréguenos en sus
agradecimientos sí lo usan para publicar algo 🥰. Pueden escribir a
ccuadrado\_arroba\_uchile.cl si notan cualquier error o tienen alguna
pregunta de los datos.

Nota metodológica
-----------------

-   **Estimaciones por día del número de reproducción efectivo a nivel
    de región, servicio de salud y comuna**. El número de reproducción
    efectivo para cada tiempo (día) se calcula utilizando el método
    desarrollado por Cori [(Cori et
    al 2020)](https://academic.oup.com/aje/article/178/9/1505/89262)
    utilizando el paquete
    [R0](https://www.rdocumentation.org/packages/R0/versions/1.2-6). Se
    utiliza un intervalo serial τ = 5 días [(Nishiura et
    al 2020)](https://www.ijidonline.com/article/S1201-9712(20)30119-3/fulltext)
    con la variabilidad habitual entre 3 y 7 días [(Sanche et
    al 2020)](https://wwwnc.cdc.gov/eid/article/26/7/20-0282_article) con una
    ventana de 14 días para la estimación. El detalle del seteo utilizado
    para reproducibilidad puede encontrarse en el script  en la
    sección [Estimaciones del número de reproducción (Re)](Re). 
    La interpretación más sencilla de este valor es el número de nuevos
    contagiados que produce cada caso (casos secundarios) en un
    intervalo serial en un contexto en la que no toda la población es
    susceptible y donde hay medidas epidemiológicas de control
    instaladas. Si Re=1 el numero de casos es estable (endemia). Si
    Re&gt;1 la epidemia esta en crecimiento. Si Re&lt;1 la epidemia esta
    siendo controlada con una reducción del número de casos activos.
    Estimamos el Re para regiones, servicios de salud y comunas de Chile
    con sus respectivos intervalos de credibilidad. Debe tenerse
    precaución que para áreas pequeñas o con pocos casos (comunas
    pequeñas o regiones bajamente pobladas como Aysén) las estimaciones
    son inestables, por lo que las estimaciones deben ser analizadas con
    particular precaución.

-   **Estimaciones de subreporte de casos sintomáticos a nivel nacional
    y regional**: Calcular el subreporte no es una tarea trivial. Más
    aún hacerlo durante una epidemia en curso. Russel y colaboradores
    [(Russel et al,
    2020a)](https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html?fbclid=IwAR31V4DbTkUDkJJKpfJMI1M7sYxt16EMQ9yRH5Y-lV0lAIH2mbkfkFZ5zeE)
    han propuesto una metodológica que puede entregar una razonable
    aproximación al cálculo. La lógica es bastante sencilla y permite
    una aproximación rápida a la pregunta sobre que tan eficaz esta
    siendo un sistema de salud en identificar los casos. En términos
    conceptuales, la propuesta de Russel utiliza como referencia la tasa
    de letalidad ajustada calculada para una epidemia ya concluida
    (Wuhan) en la que se ya se han realizado ajustes para dar cuenta del
    subreporte de casos. De esta manera, desviaciones de la letalidad
    observada en una epidemia en curso de la cifra de letalidad de una
    epidemia ya concluida pueden atribuirse principalmente al subreporte
    de casos. Esta aproximación tiene limitaciones sin duda, una de las
    más importantes es que asume que los datos de casos fallecidos son
    confiables, pero permite una primera aproximación al fenómeno de
    interés de manera oportuna. Para poder realizar la estimación, el
    primer paso es calcular una tasa de letalidad corregida por el
    retardo entre el desenlace y la exposición (ver abajo)

-   **Estimaciones de letalidad de casos cruda y ajustada por retraso a
    nivel nacional y regional** Una tasa de letalidad cruda “ingenua”
    (naive) es aquella que se calcula dividiendo los fallecidos
    acumulados por los casos confirmados acumulados. Uno de los
    principales problemas de una tasa de letalidad cruda naive es que
    esta forma de cálculo subestima la tasa de letalidad de una epidemia
    en curso, ya que el desenlace (ya sea sobrevivir o morir por
    COVID-19) no es conocido para todos los casos en un momento dado al
    no haber podido observar durante el tiempo suficiente a los
    individuos como para conocer el curso completo de la enfermedad para
    cada uno. Una forma de resolver este problema es ajustar el
    denominador por el tiempo promedio de demora entre la confirmación
    de un caso hasta el desenlace de interés, en este caso la muerte.
    Esta es una tasa de letalidad de caso corregida por retraso (TLCc o
    cCFR por su sigla en ingles), para lo cuál utilizamos la metodología
    propuesta por Russel y colaboradores [(Russel et al,
    2020b)](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.12.2000256).

El código utilizado para las estimaciones de subreporte y letalidad
ajustada son adaptaciones para Chile del desarrollado por [Tim
Russel](https://github.com/thimotei/CFR_calculation) disponible en su
repositorio.

Los datos
---------

Los datos disponibles son los siguientes:

1.  [Estimaciones del número de reproducción (Re)](Re): Estimaciones por
    día del número de reproducción efectivo a nivel de región, servicio
    de salud y comuna. Se incorporan además algunas visualizaciones
    básicas de estos datos. Las estimaciones de Re a nivel regional se
    construyen a partir de los reportes diarios del MINSAL. Las
    estimaciones de Re a nivel de servicios de salud y comuna utilizan
    los reportes bisemanales del MINSAL con desagregación a nivel
    comunal. Una nota de cautela adicional sobre los datos a nivel comunal: los 
    reportes han tenido variaciones en su frecuencia y los días en que se reportan.
    A modo de ejemplo, el último reporte presento una distancia de 5 días entre
    los datos reportados entre reporte y reporte, lo que hace que los resultados
    de las interpolaciones de casos entre fechas sea mucho más incierta.
    Mientras no tengamos datos diarios de casos por comuna nuestras estimaciones
    a nivel de comuna y servicio de salud están sujetas a alta incertidumbre.

2.  [Estimaciones de subreporte de casos](Subreporte): Estimaciones de
    subreporte de casos sintomáticos por día a nivel nacional y el
    acumulado para cada región. Se incorporan además algunas
    visualizaciones básicas de estos datos. Una nota de cautela: los datos
    de nivel regional para fallecidos no han sido ajustados retrospectivamente
    por la autoridad. Esto es evidente con el salto de casi 600 fallecidos a inicios
    de Junio, mayoritariamente en la RM. Esos fallecidos deben ser incorporados
    a la fecha respectiva de ocurrencia del fallecimiento, lo que aún no ocurre
    en las series oficiales que publica el Ministerio de Ciencias. Esto hace que
    el subreporte y la letalidad estén muy subestimados durante Mayo (y quizás Abril).

3.  [Estimaciones de letalidad de caso](Letalidad): Estimaciones de
    letalidad de casos cruda y ajustada por subreporte por día a nivel
    nacional y el acumulado para cada región. Se incorporan además
    algunas visualizaciones básicas de estos datos. Ninguna de estas dos
    métricas es adecuada para realizar comparaciones internacionales con
    otros países. Aquí pueden encontrar una [explicación en
    simple](https://twitter.com/ccuadradon/status/1247693886195195905)
    de por qué es una mala idea intentar hacer esas comparaciones. Si
    pueden ser útiles para comparar al interior del mismo país, así como
    la trayectoria observada por Chile en el tiempo.

**DISCLAIMER**: Hay que tener mucho ojo con los datos presentados en
este repositorio, pues en su mayoría provienen de información oficial
que ha esta en entredicho en varias ocasiones. Las estimaciones son tan
buenas como la calidad de los datos. No obstante, tanto los datos de
casos confirmados, como las estadísticas de fallecidos COVID-19 pueden
estar sujetos a importantes errores de medición. A principios de Junio la autoridad ha reconocido la existencia de un [número mayor de fallecidos](https://www.latercera.com/nacional/noticia/manalich-anuncia-correccion-en-cifra-de-fallecidos-con-covid-19-incorpora-a-653-personas-y-eleva-cifra-total-a-2290/VR2N2AWOIZAGJHDTDRWA7AON3M/) lo que aún no esta incorporado en las cifras retrospectivas, con lo cuál se subestima tanto la letalidad como el subreporte de casos.
La interpretación juiciosa de estos resultados, a la luz de las limitaciones de los datos,
es esencial, por lo que llamamos a la prudencia en los usos que se le
den.

Equipo
------

-   [Mauricio
    Canals](http://www.saludpublica.uchile.cl/academicos/salud-ambiental/138634/mauricio-canals-lambarri)
-   [Andrea
    Canals](http://www.saludpublica.uchile.cl/academicos/bioestadistica/136193/maria-andrea-canals-cifuentes)
-   [Cristóbal Cuadrado](https://github.com/ccuadradon)

Agradecimientos 🥰 en especial a [María Paz
Bertoglia](http://www.saludpublica.uchile.cl/academicos/nutricion-de-poblaciones/103392/maria-paz-bertoglia-arredondo)
y [Karla
Yohannessen](http://www.saludpublica.uchile.cl/academicos/salud-ambiental/104656/karla-yohannessen-vasquez)
por sus aportes como revisoras de nuestros reportes semanales, además de
[Jorge Pérez](https://github.com/jorgeperezrojas/covid19-data) por su
apoyo para montar este repositorio.
