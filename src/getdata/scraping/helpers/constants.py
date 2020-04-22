import pathlib
import os

BASE_PATH = (
    pathlib.Path(__file__).resolve().parent.parent.parent.parent.parent.absolute()
)
VIRTUAL_ENV = os.getenv("COVID19_VENV")
VIRTUAL_ENV_AUTO_CSV = "/usr/bin/env python3"
GOV_PAGE_URL = "https://www.gob.cl/coronavirus/cifrasoficiales/"
MINSAL_PAGE_URL = "https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/"

REGIONS_JSON_FILE = "{}/src//getdata/scraping/helpers/regions.json".format(BASE_PATH)
SPREADSHEET_JSON_FILE = "{}/src/getdata/scraping/helpers/covid-chile.json".format(BASE_PATH)
CONFIRMED_CSV_PATH = "{}/csv/confirmados.csv".format(BASE_PATH)
DEATHS_CSV_PATH = "{}/csv/muertes.csv".format(BASE_PATH)
NOTIFICATIONS_CSV_PATH = "{}/csv/notificaciones.csv".format(BASE_PATH)
CONFIRMED_COMUNAS_CSV_PATH = "{}/csv/confirmados_comunas.csv".format(BASE_PATH)
PATIENTS_ICU_CSV_PATH = "{}/csv/pacientes_en_uci.csv".format(BASE_PATH)
PCRS_REGION = "{}/csv/pcrs_region.csv".format(BASE_PATH)
CRON_EXECUTABLE = "{}/src/update_files.py".format(BASE_PATH)
CRON_EXECUTABLE_CSV = "{}/src/auto_update_csv.py".format(BASE_PATH)
NATIONAL_REPORT_PATH = "{}/csv/resumen_nacional.csv".format(BASE_PATH)
INFOGRAM_LINK = "https://infogram.com/nuevo-mapa-coronavirus-1hkv2n1q3w9z6x3"

SCOPE_FEEDS = "https://spreadsheets.google.com/feeds"
SCOPE_SPREADSHEET = "https://www.googleapis.com/auth/spreadsheets"
GOOGLEAPIS_DRIVE_FILE = "https://www.googleapis.com/auth/drive.file"
GOOGLEAPIS_DRIVE = "https://www.googleapis.com/auth/drive"