import requests
import urllib
import re
from bs4 import BeautifulSoup


post_params = {
    'getButton' : '',
    'fromDt' : '01-01-2009',
    'toDt' : '30-01-2009'
        }
        
post_args = urllib.urlencode(post_params)


s = requests.Session() 
s.proxies = {'https': 'https://10.65.1.33:8080'}

url=r'https://www.mytransport.sg//etc/designs/mytransport/js/bus_arrival_time/bus_arrival_time.js'

r = s.get(url)

data = r.text

print(data)

<script type="text/javascript" src="/etc/designs/mytransport/js/bus_arrival_time/bus_arrival_time.js"></script>

html_proc = BeautifulSoup(data)

d = {e['name']: e.get('value', '') for e in html_proc.find_all('input', {'name': True})}
print(d)


response_post=s.post(url,data=post_args)

print response_post.text

regex = r"1.2.3.45"  # Proxy IP
regex2 = r"6.7.8.99"  # My IP

matches = re.findall(regex, data)
matches2 = re.findall(regex2, data)

print(matches)
print(matches2)  # Always prints out my current IP and not my proxy
