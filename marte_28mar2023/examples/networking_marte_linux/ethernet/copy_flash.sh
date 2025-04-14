#! /bin/sh
echo "Copying $1 to flash drive (sdb)"
cp $1 /media/marte/XtratuM/mprogram_lwip
echo "Power off flash drive (sdb)"
umount /media/marte/XtratuM/
udisksctl power-off -b /dev/sdb
