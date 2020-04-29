import os
from crontab import CronTab
from getpass import getuser
import subprocess as cmd


from getdata.scraping.helpers.constants import CRON_EXECUTABLE_CSV, VIRTUAL_ENV_AUTO_CSV

user = getuser()
HOME = os.getenv("HOME")
cron_csv = CronTab(user=user)
job_csv = cron_csv.new(
    command="{} {} >> ~/cron_csv.log 2>&1".format(VIRTUAL_ENV_AUTO_CSV, CRON_EXECUTABLE_CSV)
)
job_csv.minute.every(30)
cmd.run("rm -f cron_csv.log", shell=True, cwd=HOME)
cron_csv.write()
