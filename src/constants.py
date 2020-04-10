import pathlib

BASE_PATH = pathlib.Path(__file__).resolve().parent.parent.absolute()
VIRTUAL_ENV = "route/to/venv"
GOV_PAGE_URL = "https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/"
REGIONS_JSON_FILE = "{}/src/regions.json".format(BASE_PATH)
CONFIRMED_CSV_PATH = "{}/csv/confirmados.csv".format(BASE_PATH)
DEATHS_CSV_PATH = "{}/csv/muertes.csv".format(BASE_PATH)
CRON_EXECUTABLE = "{}/src/update.py".format(BASE_PATH)
