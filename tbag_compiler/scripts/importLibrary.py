#!/usr/bin/env python

# looks for instances of "#import libraryName.tbag" in command line arg and copies library files into another file called prog_w_stdlib.tbag
# Author: Julie Chien
# 11/20/2015

import re
import fileinput
import shutil
import sys

tbagFileName = sys.argv[1]
tempFileName = 'prog_w_stdlib.tbag'

#copy file to temp file
shutil.copyfile(tbagFileName, tempFileName)

# search for lines starting with #import
linePattern = re.compile(r'#import (\w+)')

#funcPattern = re.compile(r'func ')

tbagFile = open(tbagFileName, 'r')

libraries = []

for line in tbagFile:
    matches = linePattern.findall(line)
    for libName in matches:
    	libraries.append(libName)
        #print libName
#print libraries
'''
for line in tbagFile:
	if "func " in line:
'''


if len(libraries) > 0:
	# if func exists replace
	lineToReplace = "#import " + libraries[0]
	#print lineToReplace

	libTxtToPasteIn = ""

	for libName in libraries:
		libFileName = "lib/" + libName + ".tbag"
		#print libFileName
		with open(libFileName, 'r') as myfile:
			data=myfile.read()
		libTxtToPasteIn += data

		#print data
	funcfound = False
	for line in fileinput.input(tempFileName, inplace=True):
		if funcfound == False:
			if "func " in line:
				#print("func found")
				line = line.replace("func ", libTxtToPasteIn + "func ")
				funcfound = True
		print(line)


	if funcfound == False:
		with open(tempFileName, 'a') as f:
			f.write(libTxtToPasteIn)

	'''
	for line in fileinput.input(tempFileName, inplace=True):
		line = line.replace(lineToReplace, libTxtToPasteIn)
		#if not re.search(linePattern, line):
		print(line)
	'''

	for line in fileinput.input(tempFileName, inplace=True):
		#line = line.replace(lineToReplace, libTxtToPasteIn)
		if not re.search(linePattern, line):
			print(line)
