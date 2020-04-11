import pathlib

BASE_PATH = (
    pathlib.Path(__file__).resolve().parent.parent.parent.parent.parent.absolute()
)
VIRTUAL_ENV = "/Users/sayhello/.pyenv/versions/covid19-data/bin/python"
GOV_PAGE_URL = "https://www.gob.cl/coronavirus/cifrasoficiales/"
REGIONS_JSON_FILE = "{}/src//getdata/scraping/helpers/regions.json".format(BASE_PATH)
CONFIRMED_CSV_PATH = "{}/csv/confirmados.csv".format(BASE_PATH)
DEATHS_CSV_PATH = "{}/csv/muertes.csv".format(BASE_PATH)
CRON_EXECUTABLE = "{}/src/update_files.py".format(BASE_PATH)
