import pathlib
import os

BASE_PATH = (
    pathlib.Path(__file__).resolve().parent.parent.parent.parent.parent.absolute()
)
VIRTUAL_ENV = os.getenv("COVID19_VENV")
GOV_PAGE_URL = "https://www.gob.cl/coronavirus/cifrasoficiales/"
MINSAL_PAGE_URL = "https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/"
REGIONS_JSON_FILE = "{}/src//getdata/scraping/helpers/regions.json".format(BASE_PATH)
CONFIRMED_CSV_PATH = "{}/csv/confirmados.csv".format(BASE_PATH)
DEATHS_CSV_PATH = "{}/csv/muertes.csv".format(BASE_PATH)
CRON_EXECUTABLE = "{}/src/update_files.py".format(BASE_PATH)
NATIONAL_REPORT_PATH = "{}/csv/resumen_nacional.csv".format(BASE_PATH)
