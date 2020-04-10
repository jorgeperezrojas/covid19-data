from crontab import CronTab
from getpass import getuser

from constants import CRON_EXECUTABLE, VIRTUAL_ENV

user = getuser()

cron = CronTab(user=user)
job = cron.new(command="{} {} >> ~/cron.log 2>&1".format(VIRTUAL_ENV, CRON_EXECUTABLE))
job.setall("00,15,30,45 8-13 * * *")
cron.write()
