#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 27 13:58:29 2019

@author: xtai
This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.

"""
import csv
import numpy as np
import helpers as eh
import sys
import os
os.chdir("/home/xtai/Desktop/tmp_9-5/1-27pythonInventories")  

############### run on all
csv_file = open('items.csv', 'r')
db_result = csv.reader(csv_file, delimiter=',')
new_rows_list = []

for r in db_result:
    hash_str = r[0]

    vol, qty = [0,0]
    vol, qty = eh.extract_weight_qty(r[1])
    
    if hash_str in eh.manual_qty: 
        qty = eh.manual_qty[hash_str] # manual override
        
    new_row = [r[0], r[1], vol, qty]
    new_rows_list.append(new_row)

csv_file.close()   # <---IMPORTANT

# Do the writing
file2 = open('itemsOut.csv', 'w')
writer = csv.writer(file2)
writer.writerows(new_rows_list)
file2.close()
