#!/usr/bin/python

from lxml import etree
from sys import stdin, stdout
import subprocess
from re import search
import HTMLParser
import codecs
import magic

h = HTMLParser.HTMLParser()

while True:
    line = raw_input()
    res = search('https?://[^ ]*', line.strip())
    if res != None:
        url = res.group(0)
        if subprocess.call(['wget', '-T', '5', '-t', '1', '-O','doc', url], stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0:
            docfile = codecs.open('doc','r', 'utf-8')
            if magic.from_file("doc", mime=True) == 'text/html':
                content = docfile.read()
                docfile.close()
                res = search('<title>(.*)</title>', content)
                if res != None:
                    title = h.unescape(res.group(1))
                    print title.encode('utf8')
                    stdout.flush()
     

