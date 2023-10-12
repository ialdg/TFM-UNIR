# -*- coding: utf-8 -*-
"""
Modified on Fri Oct 2023

@author: ivanlorenzo
"""

#!/usr/bin/python
 
###########################################################
#
# This python script is used for mysql database backup
# using mysqldump and tar utility.
#
# Written by : Rahul Kumar
# Website: http://tecadmin.net
# Created date: Dec 03, 2013
# Last modified: Aug 17, 2018 
# Tested with : Python 2.7.15 & Python 3.5
# Script Revision: 1.4
#
##########################################################
 
# Import required python libraries
 
import random

generador = "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ123456789"
print(''.join(random.sample(generador,k = 8)))
