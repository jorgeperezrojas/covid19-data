import zlib
import re
import ast
from bs4 import BeautifulSoup as soup
from urllib.request import urlopen as ureq
from urllib.request import Request, urlopen
import ssl

from ..scraping.helpers.utils import format_date_last_update, undotter

from .helpers.constants import GOV_PAGE_URL, MINSAL_PAGE_URL


def get_gov_page():
    # scraper
    url_req = Request(GOV_PAGE_URL)
    url_req.add_header("Connection", "keep-alive")
    url_req.add_header("Accept-Encoding", "gzip, deflate, br")
    url_req.add_header(
        "User-Agent",
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:62.0) Gecko/20100101 Firefox/62.0",
    )
    resp = urlopen(url_req)
    compressed_data = resp.read()
    data = zlib.decompress(compressed_data, 16 + zlib.MAX_WBITS)
    resp.close()
    page_soup = soup(data, "html.parser")
    url_container = page_soup.find("div", {"id": "cifras"})
    infogram_text = url_container.find("a", href=True)["href"]

    uclient = ureq(infogram_text)
    page_html = uclient.read()
    uclient.close()
    page_soup = soup(page_html, "html.parser")
    scripts = page_soup.find_all("script")[7].string
    data_string = re.search(r'"data":\[\[\[(.*)\]\]\]', scripts).group()
    data = ast.literal_eval(data_string.replace('"data":', ""))[0]
    date = re.search(r"actualiza(.*?)21:", scripts).group()
    date = format_date_last_update(date, normalize_day=1)

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
