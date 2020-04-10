import json
import csv

from utils import (
    undotter,
    format_date_last_update,
    get_regions_info,
    get_minsal_page,
    git_commit_and_push,
)
from constants import CONFIRMED_CSV_PATH, DEATHS_CSV_PATH


def update_files():
    regions_data = json.loads(get_regions_info())
    gov_data = get_minsal_page()
    rows, date = map(gov_data.get, ("rows", "date"))
    date = format_date_last_update(date.text)

    # open confirmed file, get data amd check if is updated
    with open(CONFIRMED_CSV_PATH) as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=",")
        confirmed_header = csv_reader.fieldnames
        if confirmed_header[-1] == date:
            print("El documento ya se encuentra actualizado")
            return
        confirmed_header.append(date)
        confirmed_data = list(csv_reader)

    # open deaths file and get data
    with open(DEATHS_CSV_PATH) as csv_file:
        csv_reader = csv.DictReader(csv_file, delimiter=",")
        deaths_header = csv_reader.fieldnames
        deaths_header.append(date)
        deaths_data = list(csv_reader)

    index_data = 0
    dict_per_region = dict()

    # prepare the new minsal data
    for row in rows:
        index_data += 1
        if 3 < index_data < 20:
            for name_region in regions_data:
                if row.findAll("td")[0].text == name_region["region"]:
                    dict_per_region[regions_data.index(name_region)] = {
                        "region": row.findAll("td")[0].text,
                        "region_id": regions_data.index(name_region) + 1,
                        "new_daily_cases": undotter(row.findAll("td")[1].text),
                        "confirmed": undotter(row.findAll("td")[2].text),
                        "deaths": undotter(row.findAll("td")[4].text),
                    }

    # add latest minsal confirmed data to csv
    for row in confirmed_data:
        row[date] = str(dict_per_region[int(row["codigo"]) - 1]["confirmed"])
    with open(CONFIRMED_CSV_PATH, "w", newline="") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=confirmed_header)
        writer.writeheader()
        writer.writerows(confirmed_data)

    # add latest minsal death data to csv
    for row in deaths_data:
        row[date] = str(dict_per_region[int(row["codigo"]) - 1]["deaths"])
    with open(DEATHS_CSV_PATH, "w", newline="") as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=deaths_header)
        writer.writeheader()
        writer.writerows(deaths_data)

    git_commit_and_push()


if __name__ == "__main__":
    a = update_files()
