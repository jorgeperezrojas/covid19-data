from crontab import CronTab
from getpass import getuser

from constants import CRON_EXECUTABLE, VIRTUAL_ENV

user = getuser()

cron = CronTab(user=user)
job = cron.new(command="{} {} >> ~/cron.log 2>&1".format(VIRTUAL_ENV, CRON_EXECUTABLE))
job.setall("55,57,59,01 00-04 * * *")
cron.write()
