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
# creates a file containing the instructions to set the 
# vga palette colors in MaRTE OS to those included in
# a gimp palette file
#
# USAGE:
#   ./gpl2vga.py gimpPaletteFile

import os, sys, re;

if len(sys.argv)<2:
    inputFilePath="marte.gpl";
else:
    inputFilePath = os.path.abspath(sys.argv[1]);#gimp palette, i.e marte.gpl    
inputFile = open(inputFilePath);

vgaOutputFilePath="vgapalette.txt";
vgaOutputString="";

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

index=0;
while True:
    
    line = inputFile.readline();
    if len(line) == 0:
        break;
    else:
        m = re.search(r'^\s*(\d+)\s+(\d+)\s+(\d+)\s+',line);
        if m != None:
            # rgb values in 320x200x256 mode seem to go from 0 to 63 (6 bits)
            # instead of from 0 to 255 (8 bits), so:
            vgaOutputString+= "vga_setpalette("+ str(index) + ", " + str((int(m.group(1))*63/255)) + ", " + str((int(m.group(2))*63/255)) + ", " + str((int(m.group(3))*63/255)) + ");" + "\n";
            index+=1;

inputFile.close();

vgaOutputFile = open(vgaOutputFilePath, 'w');
vgaOutputFile.write(vgaOutputString);
vgaOutputFile.close();

"""     
python tips
if re.match(r'^.*\.adb$',selectedFile):
"""
