#!/usr/bin/env python

import re
import os
import fileinput
import shutil

shutil.copyfile('test.tbag', 'prog_w_stdlib.tbag')

linePattern = re.compile(r'#import (\w+)')
#libNamePattern = re.compile(r'#import (\w+);')

path = '../lib/'
files = os.listdir(path)

for filed in files:
	print filed

coolstring = "asjfal;sjfa;ejofiawefw"

tbagFile = open('test.tbag', 'r')
'''
for i, line in enumerate(open('test.tbag')):
    for match in re.finditer(pattern, line):
        print 'Found on line %s: %s' % (i+1, match.groups())
        print "lolol"
        print match
'''
libraries = []

for line in tbagFile:
    matches = linePattern.findall(line)
    for libName in matches:
    	#matchinglib = libNamePattern.match(word)
    	#print matchinglib.group()
    	#line = re.sub(libName, 'fuckkkkk', line.rstrip())
    	libraries.append(libName)
        print libName


print libraries


'''
for line in fileinput.input(inplace=1, backup='.bak'):
	matches = linePattern.findall(line)
	for libName in matches:
		line = re.sub(libName,'rofl', line.rstrip())
		print(line)
'''
#re.findall(r'#import (\d+);',open('test.tbag'))