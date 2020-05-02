import csv
import json
import datetime


from ..scraping.helpers.utils import (
    undotter,
    get_regions_info,
    git_commit_and_push,
)
from .scraper import get_gov_page
from ..scraping.helpers.constants import (
    CONFIRMED_CSV_PATH,
    DEATHS_CSV_PATH,
    NATIONAL_REPORT_PATH,
)
from ..processors.generate_consolidated_data import generate


def update_files():
    regions_data = json.loads(get_regions_info())
    rows = get_gov_page()
    date = datetime.date.today().strftime("%m/%d/%y") + "20"

    with open(NATIONAL_REPORT_PATH) as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=",")
        national_header = csv_reader.fieldnames
        national_data = list(csv_reader)
        if rows[-1][1] == national_data[-1]["confirmados"]:
            time = datetime.datetime.now()
            print(
                "[{}:{}:{}] The data is already up-to-date".format(
                    time.hour, time.minute, time.second
                )
            )
            return

    # open confirmed file, get data amd check if is updated
    with open(CONFIRMED_CSV_PATH) as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=",")
        confirmed_header = csv_reader.fieldnames
        confirmed_header.append(date)
        confirmed_data = list(csv_reader)

    # open deaths file and get data
    with open(DEATHS_CSV_PATH) as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=",")
        deaths_header = csv_reader.fieldnames
        deaths_header.append(date)
        deaths_data = list(csv_reader)

    dict_per_region = dict()

    # prepare the new gov data

    for row in rows:
        for name_region in regions_data:
            if row[0] == name_region["region"]:
                dict_per_region[regions_data.index(name_region)] = {
                    "region": row[0],
                    "region_id": regions_data.index(name_region) + 1,
                    "new_daily_cases": undotter(row[2]),
                    "confirmed": undotter(row[1]),
                    "deaths": undotter(row[5]),
                }

    # add latest gov confirmed data to csv
    for row in confirmed_data:
        row[date] = str(dict_per_region[int(row["codigo"]) - 1]["confirmed"])
    with open(CONFIRMED_CSV_PATH, "w", newline="") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=confirmed_header)
        writer.writeheader()
        writer.writerows(confirmed_data)
    # add latest gov death data to csv
    for row in deaths_data:
        row[date] = str(dict_per_region[int(row["codigo"]) - 1]["deaths"])
    with open(DEATHS_CSV_PATH, "w", newline="") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=deaths_header)
        writer.writeheader()
        writer.writerows(deaths_data)

    if date != national_data[-1]["dia"]:
        national_dict = {
            "confirmados": rows[-1][1],
            "dia": date,
            "muertes": rows[-1][5],
        }
        national_data.append(national_dict)

        with open(NATIONAL_REPORT_PATH, "w", newline="") as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=national_header)
            writer.writeheader()
            writer.writerows(national_data)
    try:
        generate()
    except Exception as e:
        print("Sorry, can't generate. The reason is: {}".format(e))

    message = "contagios y muertes al {}".format(date)
    git_commit_and_push(message)


if __name__ == "__main__":
    a = update_files()
