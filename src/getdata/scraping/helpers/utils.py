import json
import subprocess as cmd

from .constants import REGIONS_JSON_FILE, BASE_PATH


def undotter(string_number):
    return int(string_number.replace(".", "").replace(",", ""))


def format_date_last_update(
    date_string, to_replace="actualizadas al ", normalize_day=0,
):
    date_str = date_string.replace(to_replace, "").replace(".", "")
    months = [
        "enero",
        "febrero",
        "marzo",
        "abril",
        "mayo",
        "junio",
        "julio",
        "agosto",
        "septiembre",
        "octubre",
        "noviembre",
        "diciembre",
    ]
    name_month = date_str.split()[2].lower()
    day = "{:0>2d}".format(int(date_str.split()[0]) + normalize_day)
    number_month = "{:0>2d}".format(months.index(name_month) + 1)
    year = "2020"
    return "{}/{}/{}".format(number_month, day, year)


def get_regions_info():

    regions = []
    with open(REGIONS_JSON_FILE) as json_file:
        data = json.load(json_file)
        for key in data:
            regions.append(key)

    return json.dumps(regions, ensure_ascii=False)


def git_commit_and_push(message):
    cmd.run("git pull", shell=True, cwd=BASE_PATH)
    cmd.run("git add .", shell=True, cwd=BASE_PATH)
    cmd.run("git commit -m '{}'".format(message), shell=True, cwd=BASE_PATH)
    cmd.run(["git push -u origin master -f"], shell=True, cwd=BASE_PATH)
