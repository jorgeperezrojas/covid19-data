import gspread
from oauth2client.service_account import ServiceAccountCredentials 
import pandas as pd

from getdata.scraping.helpers.constants import ( 
    SPREADSHEET_JSON_FILE,
    SCOPE_FEEDS,
    SCOPE_SPREADSHEET,
    GOOGLEAPIS_DRIVE,
    GOOGLEAPIS_DRIVE_FILE
)

scope = [SCOPE_FEEDS, SCOPE_SPREADSHEET, GOOGLEAPIS_DRIVE_FILE, GOOGLEAPIS_DRIVE]
creds = ServiceAccountCredentials.from_json_keyfile_name(SPREADSHEET_JSON_FILE, scope)
client = gspread.authorize(creds)

def update_csv(old_csv_name, csv_delimiter, sheet_number):

    sheet=client.open("covid_chile_csv").get_worksheet(sheet_number)
    first_row_new = sheet.row_values(1)
    source_sheet = sheet.get_all_values()
    data_sheet = pd.DataFrame(source_sheet)
    data = pd.read_csv(old_csv_name)
    data_bl = data.loc[:, ~data.columns.str.contains('^Unnamed')] 
    first_row_old = list(data_bl.columns)

    if first_row_new != first_row_old:
        data_sheet.to_csv(old_csv_name, header= False, index=False)
        data_sheet = pd.read_csv(old_csv_name)
        data_sheet = data_sheet.loc[:, ~data_sheet.columns.str.contains('^Unnamed')]
        data_sheet.to_csv(old_csv_name, index=False)


def update_csv_all(csvList, sheetList): 
    size = len(csvList)
    for i in range (0, size):
        update_csv(csvList[i], ",",sheetList[i])


