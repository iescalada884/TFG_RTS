#!/usr/bin/python
#
#   author:   Alvaro Garcia Cuesta
#   email:    alvaro@binarynonsense.com
#   website:  www.binarynonsense.com
#
#   last update: 10 Apr 2009
#
# LICENSE:
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License    
# version 2 as published by the Free Software Foundation.
# See COPYING file for more info about the license
#
# ABOUT:
#
# Creates an array from a ppm image file using a gimp
# palette file as reference for the color indexes
#
# TODO: 
# - right now it only works with ppm ascii files, I have to 
#   implement the option for binary files (shouldn't be difficult)
# - implement the option to choose array format: C or Ada as I 
#   did with the original c++ version of this tool
# - choose output file name
#
# USAGE:
#   ./ppm2array.py ppmImageFile gimpPaletteFile

import os, sys, re;

########################################
#GENERATE 'RGB TO INDEX' Dictionary: 

if len(sys.argv)<3:
    gplFilePath="marte.gpl";
else:
    gplFilePath = os.path.abspath(sys.argv[2]);#gimp palette, i.e marte.gpl
gplFile = open(gplFilePath);

"""
example of a Gimp palette file:
GIMP Palette
Name: MaRTE OS VGA
Columns: 16
0   0   0   Index 0
31  23  11  Index 1
23  15  7   Index 2
75  75  75  Index 3
...
"""
rgbDict={};
index=0;
while True:
    
    line = gplFile.readline();
    if len(line) == 0:
        break;
    else:
        m = re.search(r'^\s*(\d+)\s+(\d+)\s+(\d+)\s+',line);
        if m != None:
            rgbKey=m.group(1)+"-"+m.group(2)+"-"+m.group(3);
            rgbDict[rgbKey] = index;
            index+=1;

gplFile.close();

########################################
#GENERATE ARRAY FILE:

ppmFilePath=os.path.abspath(sys.argv[1]);#ppm-ascii image file
ppmFile = open(ppmFilePath);

imageName = re.search(r'^(.*)\.ppm$',ppmFilePath);
arrayFilePath=imageName.group(1)+".c";
arrayString="";
arrayFile = open(arrayFilePath, 'w');

"""
example of ppm file header:
P3
# CREATOR: GIMP PNM Filter Version 1.1
50 55
255
"""

ROWS=0;
COLUMNS=0;

while True:
    ppmLine = ppmFile.readline();
    if len(ppmLine) == 0:
        break;
    else:
        rowsColumns = re.search(r'^(\d+)\s+(\d+)$',ppmLine);
        if rowsColumns != None:
            ROWS=rowsColumns.group(1);
            COLUMNS=rowsColumns.group(2);
            break;
        
ppmFile.readline();#skip one line (number of colors)

#///////////ONE DIMENSION C ARRAY://////////////        
arrayString+="array["+COLUMNS+"*"+ROWS+"]=\n{";

for y in range(0,int(ROWS)):#from 0 to int(ROWS)-1
    for x in range(0,int(COLUMNS)):
        red = ppmFile.readline().rstrip();#rstrip() gets rid of the \n
        green = ppmFile.readline().rstrip();
        blue = ppmFile.readline().rstrip();
        if (len(red) == 0) or (len(green) == 0) or (len(blue) == 0):
            break;#we should never be here
        else:#we have the pixel's rgb value
            rgbKey=red+"-"+green+"-"+blue;
            arrayString+=str(rgbDict[rgbKey]);
            if (y!=int(ROWS)-1) or (x!=int(COLUMNS)-1):                
                arrayString+=",";
                
arrayString+="};";
#///////////////////////////////////////////////

arrayFile.write(arrayString);
arrayFile.close();
ppmFile.close();
