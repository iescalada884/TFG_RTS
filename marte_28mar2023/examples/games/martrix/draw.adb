-----------------------------------------------------------------------------
--                               MarTRix                                   -- 
-----------------------------------------------------------------------------
--                                                                         --
--  MarTRix is a tetris-like game for MaRTE OS.                            --
--  This code is distributed for educational purposes only.                --
--                                                                         --
--  author:   Alvaro Garcia Cuesta                                         --
--  email:    alvaro.garcia.cuesta@gmail.com                               --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: draw.adb                                                         --
--                                                                         --
--  this file contains the functions used to draw the visual elements.     --
-----------------------------------------------------------------------------
--                        last update: 16 Feb 09                           --
-----------------------------------------------------------------------------
with VGA_Marte;use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with blocks; use blocks;
with subblocks; use subblocks;
with config; use config;

package body Draw is

   use type Interfaces.C.Unsigned;
   use type Interfaces.C.int;
   
   
        ----------------------------------------
        --        DrawBlock                   --
        ----------------------------------------
        --  used to: draw a block from the blocks_pointer array in the screen, 
        --  using a flat color if plainColor is true or a subblock from the 
        --  subblocks_pointers array if it's false 
        
        procedure DrawBlock (blockNumber:integer;color:integer;PosX:integer;PosY:integer;plainColor:boolean) is        
                initX,initY:integer;
        begin
                
                for X in integer range 1..4 loop
                        for Y in integer range 1..4 loop
                                if blocks_pointers(blockNumber)(Y,X) = 1 then
                                        initX:=paddingLeft+(posX-1)*blockSize+(X-1)*blockSize;
                                        initY:=paddingUp+(posY-1)*blockSize+(Y-1)*blockSize;
                                        DrawSubBlock(initX,initY,color,plainColor);
                                end if;
                        end loop;
                end loop;
        end DrawBlock;
        
        ----------------------------------------
        --        DrawSubBlock                --
        ----------------------------------------
        --  used to: draw a subblock from the subblocks_pointer array
        
        procedure DrawSubBlock(posX:integer;posY:integer;color:integer;plainColor:boolean) is
                initX,initY,endX,endY:integer;
        begin
        
                if plainColor=true then
                        initX:=posX;
                        initY:=posY;
                        endX:=initX+blockSize;
                        endY:=initY+blockSize;
                        Vga_Rectangle_Fill(
                                (Interfaces.C.Unsigned(initX),Interfaces.C.Unsigned(initY)),
                                (Interfaces.C.Unsigned(endX),Interfaces.C.Unsigned(endY)),
                                Interfaces.C.Unsigned(color)
                        );
                else
                        for X in integer range 1..20 loop
                        for Y in integer range 1..20 loop                     
                                Vga_Pixel(
                                (Interfaces.C.unsigned(posX+X-1),
                                Interfaces.C.unsigned(posY+Y-1)),
                                Interfaces.C.unsigned(subblocks_pointers(color)(X,Y))
                                );
                        end loop;
                        end loop;
                end if;
        end DrawSubBlock;

        ----------------------------------------
        --        DrawMap                   --
        ----------------------------------------
        --  used to: draw the active area/map in the screen
        
        procedure DrawMap is
                initX,initY:integer;
        begin
                for X in integer range 1..10 loop
                for Y in integer range 1..20 loop
                        if map1(X,Y) = 0 then
                                initX:=paddingLeft+(X-1)*blockSize;
                                initY:=paddingUp+(Y-1)*blockSize;
                                DrawSubBlock(initX,initY,9,true);
                                
                        else
                                initX:=paddingLeft+(X-1)*blockSize;
                                initY:=paddingUp+(Y-1)*blockSize;
                                DrawSubBlock(initX,initY,map1(X,Y),false);
                        end if;
                end loop;
                end loop;
        end DrawMap;
        
        ----------------------------------------
        --        DrawMapBorder               --
        ----------------------------------------
        --  used to: draw the yellow border of the map
        --  and the gray blocks in the top
        
        procedure DrawMapBorder is
                initX,initY,endX,endY:integer;
        begin

                initX:=paddingLeft-2;
                initY:=paddingUp-2;
                endX:=initX+blockSize*10+4;
                endY:=initY+blockSize*20+4;
                Vga_Rectangle(
                        (Interfaces.C.Unsigned(initX),Interfaces.C.Unsigned(initY)),
                        (Interfaces.C.Unsigned(endX),Interfaces.C.Unsigned(endY)),
                        14
                );

                initX:=paddingLeft-blockSize;
                initY:=paddingUp-blockSize;
                for X in integer range 1..12 loop                
                        DrawSubBlock(initX,initY,8,false);
                        initX:=initX+blockSize;
                end loop;

        end DrawMapBorder;        

        ----------------------------------------
        --        DrawNextBlock               --
        ----------------------------------------
        --  used to: draw the next block window
        
        procedure DrawNextBlock is
                initX,initY:integer;
        begin
                for X in integer range 1..4 loop
                        for Y in integer range 1..4 loop
                                initX:=paddingNextLeft + (X-1)*blockSize;
                                initY:=paddingNextUp+(Y-1)*blockSize;
                                if blocks_pointers(nextBlockNumber)(Y,X) = 1 then                               
                                        DrawSubBlock(initX,initY,nextColor,false);
                                else
                                        DrawSubBlock(initX,initY,9,true);
                                end if;
                        end loop;
                end loop;
        end DrawNextBlock;

        ----------------------------------------
        --        DrawNextBlockBorder         --
        ----------------------------------------
        --  used to: draw the next block window's yellow border
        
        procedure DrawNextBlockBorder is
                initX,initY,endX,endY:integer;
        begin

                Vga_Text (Interfaces.C.Strings.New_String("NEXT"), (Interfaces.C.Unsigned(paddingNextLeft+25),Interfaces.C.Unsigned(paddingNextUp-20)), 15, 0);

                initX:=paddingNextLeft-2;
                initY:=paddingNextUp-2;
                endX:=initX+blockSize*4+4;
                endY:=initY+blockSize*4+4;
                Vga_Rectangle(
                        (Interfaces.C.Unsigned(initX),Interfaces.C.Unsigned(initY)),
                        (Interfaces.C.Unsigned(endX),Interfaces.C.Unsigned(endY)),
                        14
                );
        end DrawNextBlockBorder;

        ----------------------------------------
        --        DrawPoints                  --
        ----------------------------------------
        --  used to: draw the number of points
        
        procedure DrawPoints(points:integer) is
        begin
                Vga_Text (Interfaces.C.Strings.New_String("Points:"), (Interfaces.C.Unsigned(paddingNextLeft),Interfaces.C.Unsigned(paddingNextUp+130)), 15, 0);

                Vga_Text (Interfaces.C.Strings.New_String(points'img), (Interfaces.C.Unsigned(paddingNextLeft+ 55),Interfaces.C.Unsigned(paddingNextUp+130)), 15, 0);

        end DrawPoints;

end Draw;