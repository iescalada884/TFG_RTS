
all:

ada: test_clock_modulation.adb 
	mgnatmake test_clock_modulation.adb -Imarte_src_dirs
	@exec echo " [done]"

c: test_clock_modulation_c.c
	mgcc -Wall test_clock_modulation_c.c -o test_clock_modulation
	@exec echo " [done]"

clean:
	rm *~ *.o *.ali test_clock_modulation
	@exec echo " [clean]"
