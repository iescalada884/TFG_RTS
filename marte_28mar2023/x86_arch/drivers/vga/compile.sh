# mgcc mario.c sprites.c draw.c -o mprogram -Wall -I/home/alvaro/bin/marte/arch/drivers/svga -lsvga -lm
#mg++ mario.cpp csprite.cpp -o mprogram -Wall -L/home/alvaro/bin/marte/lib -I/home/alvaro/bin/marte/arch/drivers/svga -lsvga -lm 
# mg++ main.cpp cgame.cpp csprite.cpp cship.cpp -o mprogram -Wall -I/home/alvaro/bin/marte/arch/drivers/svga -I/home/alvaro/bin/marte/x86_arch/include/ustl/  -L/usr/lib/gcc/i486-linux-gnu/4.2/ -lsvga -lm
#mg++ main.cpp cgame.cpp csprite.cpp cship.cpp cenemies.cpp keyboard.c -o mprogram -Wall -I/home/alvaro/bin/marte/arch/drivers/svga -lsvga -lm
#cp mprogram /home/alvaro/source/marte/qemu/mprogram 
#mgcc mario.cpp -o mprogram -Wall -L/usr/lib/gcc/i486-linux-gnu/4.3/ -I/home/alvaro/bin/marte/arch/drivers/svga -lsvga -lm
mgcc test.c vgacore.c -o mprogram -Wall -lm