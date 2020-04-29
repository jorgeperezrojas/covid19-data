#-----------------------"covid_chile_csv"---------------------------
#indexes: numer of sheet -1.
# 0: confirmados
# 1: test PCR notificados
# 2: pacientes en UCI
# 3: muertes
# 4: confirmados acumulados por comuna
# 5: PCRs por region
#-------------------------------------------------------------------

import subprocess as cmd
import update_csv
import datetime
from getdata.scraping.helpers.utils import git_commit_and_push
from getdata.scraping.helpers.constants import (
    NOTIFICATIONS_CSV_PATH,
    PATIENTS_ICU_CSV_PATH,
    CONFIRMED_COMUNAS_CSV_PATH,
    PCRS_REGION
)

update_csv.update_csv_all([NOTIFICATIONS_CSV_PATH, PATIENTS_ICU_CSV_PATH, CONFIRMED_COMUNAS_CSV_PATH, PCRS_REGION], [1, 2, 4, 5])
timestamp = datetime.datetime.now()
message = "Actualizacion de csv a las " + str(timestamp)
git_commit_and_push(message)
