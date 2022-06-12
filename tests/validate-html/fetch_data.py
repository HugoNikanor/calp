#!/usr/bin/env python

import subprocess
import urllib.request

from selenium import webdriver
from selenium.webdriver.firefox.options import Options

def fetch_rendered(url, port):
    options = Options()
    options.add_argument('--headless')
    driver = webdriver.Firefox(options=options)

    driver.get(url)
    page_source = driver.page_source

    # TODO check encoding from driver
    page_encoded = page_source.encode('utf-8')

    cmd = subprocess.run(['xmllint', '--format', '-'],
            input=page_encoded,
            capture_output=True)

    if cmd.returncode == 0:
        port.write(cmd.stdout)
    else:
        port.write(page_encoded)

def fetch_raw(url, port):
    response = urllib.request.urlopen(url)
    data = response.read()
    port.write(data)
    
url = 'http://localhost:8080/week/2022-03-31.html'

with open('raw.xhtml', 'wb') as f:
    fetch_raw(url, f)

# with open('raw.html', 'wb') as f:
#     fetch_raw(f'{url}?html', f)

with open('selenium.xhtml', 'wb') as f:
    fetch_rendered(url, f)

# with open('selenium.html', 'wb') as f:
#     fetch_rendered(f'{url}?html', f)
