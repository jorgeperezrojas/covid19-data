import subprocess
import pyautogui
import pyperclip
import time
import re
import ipdb

# TODO: hacerle un main para que se pueda correr desde la linea de comando

def copia_texto_desde_archivo(archivo, app='Preview'):
    # Copia el texto (Cmd+C) desde una ventana a un texto (Cmd+V)
    # No es la mejor forma, pero el Minsal no nos da mucha opción :-(
    x,y  = 50, 50

    subprocess.Popen(['open', '-na', app, archivo])
    time.sleep(2)
    
    pyautogui.moveTo(x, y)
    pyautogui.click()
    
    pyautogui.hotkey('command', 'a')
    time.sleep(5)
    
    pyautogui.hotkey('command', 'c')
    time.sleep(5)
    
    pyautogui.hotkey('command', 'w')
    time.sleep(1)

    text = pyperclip.paste()
    return text

def extrae_texto_para_cada_region(texto):

    # Extrae el texto correspondiente a las tablas con las comunas por región
    # Supone que cada de cada región comienza con un encabezado y termina 
    # con la primera aparición del string `Total`
    re_tablas_de_comunas = r'(Poblaci.*Confirmados(T[^o]|To[^t]|[^T])*Total\n.*\n *([0-9]+,[0-9]+)?)'
    textos_tablas_de_regiones = re.findall(re_tablas_de_comunas, texto)
    
    # Si se extrayeron menos de 16 regiones lanza un error
    assert len(textos_tablas_de_regiones) == 16

    return [x[0] for x in textos_tablas_de_regiones]


def deja_solo_datos(texto_tabla_region):
    # Supone que el texto de entrada tiene todos los datos de comunas correspondientes a una región.
    texto_out = texto_tabla_region

    # Primero busca si existe un "Por determinar" y lo elimina (junto con el número)
    re_por_determinar = '[^\n0-9]+determinar[^0-9]*([0-9]+)[^\n0-9]*\n'
    texto_out = re.sub(re_por_determinar, '', texto_out)

    # Si una línea contiene al menos un número la deja, de otra forma la elimina
    re_linea_no_datos = r'[^\-\n0-9]+\n'
    texto_out = re.sub(re_linea_no_datos, '\n', texto_out)

    # Elimina las lineas en blanco
    re_multiples_saltos = r'\n+'
    texto_out = re.sub(re_multiples_saltos, '\n', texto_out)    

    # Elimina posible linea inicial en blanco
    lineas = texto_out.split('\n')
    if lineas[0].strip() == "":
        texto_out = '\n'.join(lineas[1:])

    return texto_out

def intenta_extraer_datos_consistentes_de_lineas_seguidas(lineas, i):
    re_dato_1 = r' *([0-9]+) *'
    re_dato_2 = r' *([-0-9]+) *'
    re_dato_3 = r' *([0-9]+\.[0-9]+) *'
    re_espacio = r' +'

    re_dato_1_2 = re_dato_1 + re_espacio + re_dato_2
    re_dato_2_3 = re_dato_2 + re_espacio + re_dato_3

    #ipdb.set_trace()

    # intenta el caso en que hay dos datos en la primera linea y un dato en la siguiente
    if re.fullmatch(re_dato_1_2, lineas[i]) and re.fullmatch(re_dato_3, lineas[i+1]):
        linea_datos_1_2 = re.sub(re_dato_1_2, r'\1,\2', lineas[i])
        linea_datos_3 = re.sub(re_dato_3, r'\1', lineas[i+1])
        linea_datos = f'{linea_datos_1_2},{linea_datos_3}'
        inc = 1
    # intenta el caso en que hay un dato en la primera linea y dos datos en la siguiente
    elif re.fullmatch(re_dato_1, lineas[i]) and re.fullmatch(re_dato_2_3, lineas[i+1]):
        linea_datos_1 = re.sub(re_dato_1, r'\1', lineas[i])
        linea_datos_2_3 = re.sub(re_dato_2_3, r'\1\2', lineas[i+1])
        linea_datos = f'{linea_datos_1},{linea_datos_2_3}'
        inc = 1
    # intenta el caso en que hay un dato en cada una de las tres lineas siguientes
    elif re.fullmatch(re_dato_1, lineas[i]) and re.fullmatch(re_dato_2, lineas[i+1]) and re.fullmatch(re_dato_3, lineas[i+2]):
        linea_datos_1 = re.sub(re_dato_1, r'\1', lineas[i])
        linea_datos_2 = re.sub(re_dato_2, r'\1', lineas[i+1])
        linea_datos_3 = re.sub(re_dato_3, r'\1', lineas[i+2])
        linea_datos = f'{linea_datos_1},{linea_datos_2},{linea_datos_3}'
        inc = 2
    else:
        return (None,0)

    out = linea_datos.split(',')
    return (out, inc)

def extrae_datos_comunas(texto_tabla_solo_datos):
    # Primero cambia puntos por nada
    texto = re.sub(r'\.', '', texto_tabla_solo_datos)
    # Ahora cambia comas por puntos
    texto = re.sub(',', '.', texto)

    # Procesa linea a linea
    lineas = texto.split('\n')

    # Datos para guardar
    datos = []

    # Iteracion tipo C para tener más control
    i = 0
    while i < len(lineas):
        # chequea si cumple el formato
        re_dato_limpio = r' *([0-9]+) +([-0-9]+) +([0-9]+.[0-9]+) *'
        if re.match(re_dato_limpio, lineas[i]):
            # extrae dato
            linea_datos = re.sub(re_dato_limpio, r'\1,\2,\3', lineas[i])
            datos.append(linea_datos.split(','))
        else:
            # si no cumple con el formato limpio aplica heurísticas simples (por ahora)
            if re.match('[^0-9]', lineas[i].strip()):
                # si el primer caracter es no dígito, ignora la línea
                pass
            else:
                # si no, trata de extraer datos de varias líneas seguidas
                d, inc = intenta_extraer_datos_consistentes_de_lineas_seguidas(lineas, i)
                if d != None:
                    datos.append(d)
                    i = i + inc
                else:
                    # No se pudo hacer match (tal vez habría que reportar el error?)
                    pass
        i = i + 1
    return datos

def extrae_comunas_por_region(textos_tablas_de_regiones, permisivo=False):
    # Estos números son 1 + la cantidad de comunas de cada región
    # Se usan para chequear que se hayan obtenidos todos los datos
    cantidad_comunas = [5,8,10,10,16,39,53,34,31,22,34,33,13,31,11,12]
    datos_comunas_por_region = []

    for c,texto_tabla_region in zip(cantidad_comunas,textos_tablas_de_regiones):
        # Intenta dejar solo datos
        texto_tabla_solo_datos = deja_solo_datos(texto_tabla_region)

        # Procesa por linea para extraer datos
        datos_comunas = extrae_datos_comunas(texto_tabla_solo_datos)

        if not permisivo:
            assert len(datos_comunas) == c
        else:
            if len(datos_comunas) != c:
                info = "Hay inconsistencia en datos extraidos para una región. Debería revisar...\n"
                info += "\nTEXTO\n" + "-"*20 + "\n" + texto_tabla_region 
                info += "\nDATOS EXTRAIDOS\n" + "-"*20 + "\n" + str(datos_comunas)
                print(info)

                print(c)

        datos_comunas_por_region.append(datos_comunas)

    return datos_comunas_por_region



archivo = '../../informes/Reporte_COVID_19_06_04_2020.pdf'
#archivo = '../../informes/INFORME_EP_COVID19_20200408.pdf'
texto = copia_texto_desde_archivo(archivo)
textos_tablas_de_regiones = extrae_texto_para_cada_region(texto)
datos_comunas_por_region = extrae_comunas_por_region(textos_tablas_de_regiones, permisivo=True)

