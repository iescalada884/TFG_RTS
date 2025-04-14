echo "Getting scheduling information from the serial port... (finish with Ctrl-C)"
stty -F /dev/ttyS0 ispeed 115200 ospeed 115200
cat /dev/ttyS0 > sched_info.dat
