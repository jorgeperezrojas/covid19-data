import re
import ast
import ssl
from bs4 import BeautifulSoup as soup
from urllib.request import urlopen as ureq


from ..scraping.helpers.utils import format_date_last_update, undotter
from .helpers.constants import MINSAL_PAGE_URL, INFOGRAM_LINK


def get_gov_page():
    uclient = ureq(INFOGRAM_LINK)
    page_html = uclient.read()
    uclient.close()
    page_soup = soup(page_html, "html.parser")
    scripts = page_soup.find_all("script")[7].string
    data_string = re.search(r'"data":\[\[\[(.*)[(0-9)]"\]\]\]', scripts).group()
    data = ast.literal_eval(data_string.replace('"data":', ""))[0]
    date = re.search(r'"text":"[^"]*abril de 2020', scripts).group()[8:]
    date = format_date_last_update(date)

    return {"rows": data, "date": date}


def get_minsal_recovered():
    ssl._create_default_https_context = ssl._create_unverified_context
    uclient = ureq(MINSAL_PAGE_URL)
    page_html = uclient.read()
    uclient.close()
    page_soup = soup(page_html, "html.parser")
    container = page_soup.find("div", {"class": "contenido"})
    date = format_date_last_update(
        container.p.strong.text, to_replace="*Informe corresponde al ",
    )
    recovered = undotter(page_soup.find_all("table")[1].findAll("td")[1].text)

    return {"date": date, "recovered": recovered}
