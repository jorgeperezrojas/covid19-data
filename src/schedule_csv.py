#-----------------------WARNING: NEEDS TO BE TESTED ----------------------------------
import os
from crontab import CronTab
from getpass import getuser
import subprocess as cmd


from getdata.scraping.helpers.constants import CRON_EXECUTABLE_CSV, VIRTUAL_ENV

user = getuser()
HOME = os.getenv("HOME")
cron = CronTab(user=user)
job_csv = cron.new(command="{} {} >> ~/cron.log 2>&1".format(VIRTUAL_ENV, CRON_EXECUTABLE_CSV))
job_csv.minute.every(5)
cmd.run("rm -f cron.log", shell=True, cwd=HOME)
cron.write()
