import csv
import subprocess
import pyautogui
import pyperclip
import time
import re
import ipdb

# TODO: hacerle un main para que se pueda correr desde la linea de comando

# Estos números son 1 + la cantidad de comunas de cada región
# Se usan para chequear que se hayan obtenidos todos los datos
cantidad_comunas = [5, 8, 10, 10, 16, 39, 53, 34, 31, 22, 34, 33, 13, 31, 11, 12]


def copia_texto_desde_archivo(archivo, s_time=3, app="Google Chrome"):
    # Copia el texto (Cmd+C) desde una ventana a un texto (Cmd+V)
    # No es la mejor forma, pero el Minsal no nos da mucha opción :-(
    x, y = 500, 300

    subprocess.Popen(["open", "-na", app, archivo])
    time.sleep(2)

    pyautogui.moveTo(x, y)
    pyautogui.click()
    time.sleep(2)
    pyautogui.scroll(-100, x, y)
    time.sleep(2)

    pyautogui.hotkey("command", "a")
    time.sleep(s_time)

    pyautogui.hotkey("command", "c")
    time.sleep(s_time)

    pyautogui.hotkey("command", "w")
    time.sleep(1)

    text = pyperclip.paste()
    return text


def lee_datos_csv_para_heurística(
    archivo="data_for_preprocessing.csv",
    columna_region=0,
    columna_comuna=2,
    columna_habitantes=1,
):
    # Asume un archivo de datos de comunas ordenado por region (dato columna 0)
    # y el nombre de las comunas con datos de habitantes
    with open(archivo) as infile:
        reader = csv.reader(infile)
        pre_datos_de_regiones = []
        for row in reader:
            posible_region = row[columna_region]
            if posible_region != "":
                # comienza una nueva región
                nueva_region = []
                pre_datos_de_regiones.append(nueva_region)
            comuna = row[columna_comuna]
            habitantes = int(row[columna_habitantes])
            nueva_region.append((comuna, habitantes))

    # Chequea cierta consistencia en los datos
    assert len(pre_datos_de_regiones) == 16
    for c, pre_datos_region in zip(cantidad_comunas, pre_datos_de_regiones):
        assert len(pre_datos_region) == c

    return pre_datos_de_regiones


def formatea_numero_como_re_y_string_numero(cantidad):
    cantidad_str = str(int(cantidad))
    out_re, out = "", ""
    for i, c in enumerate(cantidad_str[::-1]):
        if i != 0 and i % 3 == 0:
            out_re = r"\." + out_re
            out = "." + out
        out_re = c + out_re
        out = c + out
    return (out_re, out)


def busca_primer_numero_en_texto(sub_texto, texto="", __re=""):
    # extrae el primer número válido
    re_numero = r"(-|[.0-9]+)"
    mo = re.search(re_numero, sub_texto)
    if not mo:
        out = "***"
    else:
        out = mo.expand(r"\1")
    return out


def busca_primer_float_en_texto(sub_texto, texto="", __re=""):
    # extrae el primer string de la forma \d,\d
    re_float = r"([0-9]+,[0-9]+)"
    mo = re.search(re_float, sub_texto)
    if not mo:
        out = "***"
    else:
        out = mo.expand(r"\1")
    return out


def extrae_casos_como_string_desde_texto_region(texto, datos_habitantes_region):
    casos = []
    tasas = []
    for h1, h2 in zip(datos_habitantes_region[:-1], datos_habitantes_region[1:]):
        re_1, _ = formatea_numero_como_re_y_string_numero(h1)
        re_2, _ = formatea_numero_como_re_y_string_numero(h2)
        re_search = re_1 + r"((.|\n)*)" + re_2
        mo = re.search(re_search, texto)
        if not mo:
            caso = "***"
            tasa = "***"
        else:
            sub_texto = mo.expand(r"\1")
            caso = busca_primer_numero_en_texto(sub_texto, texto, re_search)
            tasa = busca_primer_float_en_texto(sub_texto, texto, re_search)
        casos.append(caso)
        tasas.append(tasa)
    re_total_hab, _ = formatea_numero_como_re_y_string_numero(
        datos_habitantes_region[-1]
    )
    re_final = re_total_hab + r"((.|\n)*)"
    mo = re.search(re_final, texto)
    if not mo:
        total = "***"
        tasa_total = "***"
    else:
        sub_texto = mo.expand(r"\1")
        total = busca_primer_numero_en_texto(sub_texto, texto, re_final)
        tasa_total = busca_primer_float_en_texto(sub_texto, texto, re_final)
    casos.append(total)
    tasas.append(tasa_total)
    return casos, tasas


def extrae_tasas_como_string(texto):
    # busca todos los strings con el formato "\d+,\d+"
    re_tasas = r"([0-9]+,[0-9]+)"
    textos_tasas = re.findall(re_tasas, texto)
    return textos_tasas


def chequea_consistencia(
    comunas, textos_tasas, textos_casos, habitantes, permisivo=False
):
    # acá debería chequear que todos los datos concuerdan
    if not permisivo:
        if len(textos_tasas) != len(comunas):
            ipdb.set_trace()
        # assert len(textos_tasas) == len(comunas)
    else:
        if len(textos_tasas) != len(comunas):
            # TODO: Decidir qué hacer acá
            pass


def extrae_tasas_y_casos_por_region(
    textos_tablas_de_regiones, pre_datos_de_regiones, permisivo=False
):
    # heurística para extraer todas las tasas de las comunas de cada region
    tasas_comunas_por_region = []
    casos_comunas_por_region = []

    for texto_tabla_region, datos_region in zip(
        textos_tablas_de_regiones, pre_datos_de_regiones
    ):
        comunas = [c for c, _ in datos_region]
        habitantes = [h for _, h in datos_region]

        textos_casos, textos_tasas = extrae_casos_como_string_desde_texto_region(
            texto_tabla_region, habitantes
        )
        # textos_tasas = extrae_tasas_como_string(texto_tabla_region)

        chequea_consistencia(comunas, textos_tasas, textos_casos, habitantes)

        tasas_comunas_por_region.append(textos_tasas)
        casos_comunas_por_region.append(textos_casos)

    return casos_comunas_por_region, tasas_comunas_por_region


def extrae_texto_para_cada_region(texto):
    # Extrae el texto correspondiente a las tablas con las comunas por región
    # Supone que cada de cada región comienza con un encabezado y termina
    # con la primera aparición del string `Total`
    re_tablas_de_comunas = r"(Poblaci[^T]*Confirmado(T[^o]|To[^t]|[^T])*Total[\n ]*[.0-9]*[\n ]*[.0-9]+[\n ]*[,0-9]+)"
    textos_tablas_de_regiones = re.findall(re_tablas_de_comunas, texto)

    # Si se extrayeron menos de 16 regiones lanza un error
    assert len(textos_tablas_de_regiones) == 16

    return [x[0] for x in textos_tablas_de_regiones]


def genera_datos_salida(comuna, habitantes, casos, tasa):
    # primero limpia casos y tasa
    casos = re.sub(r"\.", "", casos)
    tasa = re.sub(r",", ".", tasa)
    # chequea posibles erroes
    ERR = False
    TC = False

    if casos == "***":
        ERR = True
    elif casos != "-":
        casos = int(casos)

    if tasa == "***":
        ERR = True
    else:
        tasa = float(tasa)

    # chequea que tasa y casos concuerden
    if casos != "-" and casos != "***" and tasa != "***":
        if round(tasa * habitantes / 100000) != casos:
            ERR = True

    if casos == "-" and tasa > 0:
        casos = round(tasa * habitantes / 100000)
        if casos > 5:
            ERR = True
        TC = True

    out = [habitantes, comuna, casos, tasa]
    if ERR:
        out = out + ["CHEQUEAR!"]
    if TC:
        out = out + ["(calculo desde tasa)"]

    return out


def genera_archivos_de_salida(
    pre_datos_de_regiones,
    casos_por_region,
    tasas_por_region,
    nombre_archivo="out_gs.csv",
):
    with open(nombre_archivo, "w") as outfile_gs:
        writer = csv.writer(outfile_gs)
        for datos_region, casos_region, tasas_region in zip(
            pre_datos_de_regiones, casos_por_region, tasas_por_region
        ):
            for dr, casos, tasa in zip(datos_region, casos_region, tasas_region):
                comuna, habitantes = dr
                datos = genera_datos_salida(comuna, habitantes, casos, tasa)
                writer.writerow(datos)
            writer.writerow([])

def main():
    # NO FUNCIONA DATOS AL REVES archivo = '../../../reports/Informe_EPI_03_04_2020.pdf'
    #archivo = '../../../reports/Reporte_COVID_19_06_04_2020.pdf'
    #archivo = '../../../reports/INFORME_EP_COVID19_20200408.pdf'
    #archivo = '../../../reports/Informe_EPI_10_04_2020.pdf'
    #archivo = '../../../reports/INFORME_EPI_COVID19_20200313.pdf'
    #archivo = '../../../reports/Informe_EPI_15_04_2020.pdf'
    #archivo = '../../../reports/Informe-Epidemiológico-17_04_2020_Corregido-V2.pdf'
    #archivo = '../../../reports/Informe_EPI_PUB_20042020.pdf'
    #archivo = '../../../reports/Informe_EPI_24042020.pdf'
    archivo = '../../../reports/Informe-EPI-27042020.pdf'
    texto = copia_texto_desde_archivo(archivo, 5)
    textos_tablas_de_regiones = extrae_texto_para_cada_region(texto)
    pre_datos_de_regiones = lee_datos_csv_para_heurística()
    casos_por_region, tasas_por_region = extrae_tasas_y_casos_por_region(textos_tablas_de_regiones,pre_datos_de_regiones)
    genera_archivos_de_salida(pre_datos_de_regiones, casos_por_region, tasas_por_region)

if __name__ == '__main__':
    main()
