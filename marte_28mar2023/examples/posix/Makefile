none:
	@echo "use 'make <target>'"
	@echo "   targets: periodic_threads_signals   producer_consumer   shared_variable_display   test_signal   remote_cpu_timers periodic_threads_clock_nanosleep"

periodic_threads_signals: periodic_threads_signals.c
	mgcc $(CFLAGS) periodic_threads_signals.c

producer_consumer: producer_consumer.c 
	mgcc $(CFLAGS) producer_consumer.c

shared_variable_display: shared_variable_display.c
	mgcc $(CFLAGS) shared_variable_display.c

test_signal: test_signal.c
	mgcc $(CFLAGS) test_signal.c

remote_cpu_timers: remote_cpu_timers.c
	mgcc $(CFLAGS) remote_cpu_timers.c

periodic_threads_clock_nanosleep: periodic_threads_clock_nanosleep.c
	mgcc $(CFLAGS) periodic_threads_clock_nanosleep.c
