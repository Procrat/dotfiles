#!/usr/bin/env python2
# encoding: utf-8
from autopy import mouse
from sys import argv
import time

while True:
    mouse.click()
    sleep_interval = float(argv[1]) if len(argv) > 1 else .02
    time.sleep(sleep_interval)
