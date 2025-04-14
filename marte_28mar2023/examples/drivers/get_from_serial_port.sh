echo "Getting  from the serial port at $1 bauds... (finish with Ctrl-C)"
stty -F /dev/ttyS0 ispeed $1 ospeed $1
cat /dev/ttyS0
