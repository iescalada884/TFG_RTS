#!/bin/bash
set -e

echo "Copying MaRTE added files..."
cd marte_added_files/
cp *.h ../include/
cp *.cpp *.h ../build_lib_stm32duino/
#cp *.cpp *.h *.c ../build_lib_stm32duino/
cd ..
echo "  Copying MaRTE added files DONE."

echo "Copying MaRTE modified header files..."
cd marte_modified_files/
cp *.h ../include/
mv $HOME/.arduino15/packages/STM32/hardware/stm32/1.9.0/cores/arduino/wiring_time.h $HOME/.arduino15/packages/STM32/hardware/stm32/1.9.0/cores/arduino/wiring_time.h.orig
cp wiring_time.h $HOME/.arduino15/packages/STM32/hardware/stm32/1.9.0/cores/arduino/
echo "  Copying MaRTE modified header files DONE."

echo "Compiling MaRTE modified files..."
rm -f *.o
#mgcc -c *.c
mgcc -I../include -c *.c
rename 's/\.o$/\.c.o/' *.o
cd ..
echo "  Compiling MaRTE modified files DONE."

echo "Building libstm32duino.a library objects..."
cd build_lib_stm32duino
rm -rf build/
arduino --pref build.path=build/ --verify build_lib_stm32duino.ino
#arduino --pref compiler.cpp.extra_flags=-g --pref build.path=build/ --verify build_lib_stm32duino.ino
echo "  libstm32duino.a library objects DONE."

# Agrupacion de la libstm32duino.a

echo "Creating libstm32duino.a library..."
cd build/

mkdir obj/

cd core
find . -name "*.o" -exec cp {} ../obj/ \; #2>/dev/null
cd ..

cd libraries/
find . -name "*.o" -exec cp {} ../obj/ \; #2>/dev/null
cd ..

cd sketch/
find . -name "*.o" -exec cp {} ../obj/ \; #2>/dev/null
cd ..

cp ../../marte_modified_files/*.o obj/

cd obj
rm -f libstm32duino.a
arm-eabi-ar rc libstm32duino.a *.o
mv libstm32duino.a ../../..

echo "Restoring changed files..."
mv $HOME/.arduino15/packages/STM32/hardware/stm32/1.9.0/cores/arduino/wiring_time.h.orig $HOME/.arduino15/packages/STM32/hardware/stm32/1.9.0/cores/arduino/wiring_time.h
echo " DONE"





