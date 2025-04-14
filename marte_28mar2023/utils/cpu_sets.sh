#!/bin/sh

if [ "$#" != 1 ]; then
    echo "Wrong argument, shell pid missing"
    echo " Use: cpu_sets.sh \$\$"
    return 1
fi

# Mount cpusets
if [ ! -d /dev/cpuset ];
then
	echo "Creating /dev/cpuset\n"
	mkdir /dev/cpuset
fi
if [ ! -e /dev/cpuset/tasks ];
then
	echo "Mounting /dev/cpuset\n"
	mount -t cpuset cpuset /dev/cpuset
fi


# Fix frequency CPU 3
echo userspace >  /sys/devices/system/cpu/cpu3/cpufreq/scaling_governor
cat /sys/devices/system/cpu/cpu3/cpufreq/cpuinfo_min_freq > /sys/devices/system/cpu/cpu3/cpufreq/scaling_setspeed

# Disable interrupts on CPU 3 (affinity to CPU 0)
# echo "Disable interrupts on CPU 3 (affinity to CPU 0)\n"
# for i in $(find /proc/irq -name "smp_affinity"); do echo 1 > $i; done


# Create cpusets
cd /dev/cpuset
if [ ! -d /dev/cpuset/sys ];
then
	echo "Creating /dev/cpuset/sys\n"
	mkdir sys
fi
echo 0-2 > sys/cpus
echo 1 > sys/cpu_exclusive
echo 0 > sys/mems


if [ ! -d /dev/cpuset/rt ];
then
	echo "Creating /dev/cpuset/rt\n"
	mkdir rt
fi
echo 3 > rt/cpus
echo 1 > rt/cpu_exclusive
echo 0 > rt/mems
echo 0 > rt/sched_load_balance
echo 1 > rt/mem_hardwall
for i in $(cat tasks); do
	echo "Moving task $i"
	echo $i >> sys/tasks;
done

# Move current shell to the rt cpuset
echo "Adding $1 pid to RT CPU set"
echo $1 > rt/tasks

# echo "Running mtests\n"
# export PATH=/home/mario/gnat/bin:/usr/lib/gps/:/home/mario/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/mario/bin:/home/mario/marte_os/bin_marte:/home/mario/marte_os/marte/utils:.
# mtests -d time
