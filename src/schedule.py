import os
from crontab import CronTab
from getpass import getuser
import subprocess as cmd


from getdata.scraping.helpers.constants import CRON_EXECUTABLE, VIRTUAL_ENV

user = getuser()
HOME = os.getenv("HOME")
cron = CronTab(user=user)
job = cron.new(command="{} {} >> ~/cron.log 2>&1".format(VIRTUAL_ENV, CRON_EXECUTABLE))
job.setall("0,15,30,45 9-15 * * *")
cmd.run("rm -f cron.log", shell=True, cwd=HOME)
cron.write()
