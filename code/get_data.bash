#!/usr/bin/env bash

rm data/raw/sf_incidents_new.csv

wget -P data/raw/sf_incidents_new.csv "https://data.sfgov.org/api/views/wg3w-h783/rows.csv?date=20231211&accessType=DOWNLOAD"