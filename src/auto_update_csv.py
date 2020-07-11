#-----------------------"covid_chile_csv"---------------------------
#indexes: numer of sheet -1.
# 0: confirmados
# 1: test PCR notificados
# 2: pacientes en UCI
# 3: muertes
# 4: confirmados acumulados por comuna
# 5: PCRs por region

#Only adds files modified by update_csv
#-------------------------------------------------------------------

import subprocess as cmd
import update_csv
import datetime
from getdata.scraping.helpers.utils import git_commit_and_push
from getdata.scraping.helpers.constants import (
    BASE_PATH,
    NOTIFICATIONS_CSV_PATH,
    PATIENTS_ICU_CSV_PATH,
    CONFIRMED_COMUNAS_CSV_PATH,
    PCRS_REGION
)
cmd.run("git pull --force", shell=True, cwd=BASE_PATH) 
update_csv.update_csv_all([NOTIFICATIONS_CSV_PATH, PATIENTS_ICU_CSV_PATH, CONFIRMED_COMUNAS_CSV_PATH, PCRS_REGION], [1, 2, 4, 5])
timestamp = datetime.datetime.now()
message = "Actualizacion de csv a las " + str(timestamp)
cmd.run("git pull --force", shell=True, cwd=BASE_PATH) 
cmd.run("git add {}".format(NOTIFICATIONS_CSV_PATH), shell=True, cwd=BASE_PATH)
cmd.run("git add {}".format(PATIENTS_ICU_CSV_PATH), shell=True, cwd=BASE_PATH)
cmd.run("git add {}".format(CONFIRMED_COMUNAS_CSV_PATH), shell=True, cwd=BASE_PATH)
cmd.run("git add {}".format(PCRS_REGION), shell=True, cwd=BASE_PATH)
cmd.run("git commit -m '{}'".format(message), shell=True, cwd=BASE_PATH)
cmd.run("git push", shell=True, cwd=BASE_PATH)
