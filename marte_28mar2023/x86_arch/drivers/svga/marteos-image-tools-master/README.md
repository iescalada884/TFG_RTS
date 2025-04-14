MaRTE OS Image Tools
====================

This package contains some scripts useful to easily convert images to a 
format you can use with MaRTE OS' VGA driver.

gpl2vga.py: 

given a GIMP palette file (.gpl) it generates the vga commands needed to initialize the palette in MaRTE.

ppm2array.py: 

given an image in ASCII ppm format and a GIMP palette file it generates an array containing the color index from the palette corresponding to each pixel from the image.

Right now it only generates a C format array, future versions will generate Ada arrays as well.

marte.gl: 

a GIMP palette example.