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
--  file: martrix.adb                                                      --
--                                                                         --
--  this file contains the game loop.                                      --
--  controls:w = rotate, a = move left, d = move right, s = move down      --
-----------------------------------------------------------------------------
--                        last update: 22 Feb 09                           --
-----------------------------------------------------------------------------

with VGA_Marte; use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with ada.real_time;use ada.real_time;

with blocks;use blocks;
with draw; use draw;
with logic; use logic;
with config; use config;
with intro; use intro;

procedure martrix is

      use type Interfaces.C.Unsigned;
      use type Interfaces.C.int;      

begin
        ------------------
        --  INIT STUFF  --
        ------------------

        --  init vga
        if(doInit) then
                Error:=Init_Vga(G640X480X16,VGA,Interfaces.C.int(PCI_DEVICE_ID_S3_TRIO64V2));
                if Error/=0 then
                        raise Init_Error;
                end if;
        end if;

        ShowIntro; --  shows the marteos games logo
        
        --  choose first active block and next block
        ChooseNewBlock(true);
        --  draw box with next block
        DrawNextBlockBorder;
        DrawNextBlock;        
        
        -- draw text: author, initial points (0) and game title
        Vga_Text (Interfaces.C.Strings.New_String("by Alvaro Garcia - 2009"), (Interfaces.C.Unsigned(paddingLeft+10),460), 15, 0);

        DrawPoints(points);

        Vga_Text (Interfaces.C.Strings.New_String("- MaRTrix -"), (Interfaces.C.Unsigned(paddingNextLeft-2),Interfaces.C.Unsigned(paddingNextUp-75)), 4, 0);
        
        --  draw active area/map
        DrawMapBorder;
        DrawMap;
        
        --  draw first block at the top of the playable area
        DrawBlock(activeBlockNumber,color,blockX,blockY,false);
        --  reset freq counter (see config.ads for more info)
        blockDownFreqCounter:=0;

        -----------------
        --  GAME LOOP  --
        -----------------

        while gameOver/=true loop       
                
                --  check for user input and move the block if 
                --  needed
                CheckForMovement(activeBlockNumber);
                
                --  increase freq counter (see config.ads for more info)
                blockDownFreqCounter:=blockDownFreqCounter+1;

                if blockDownFreqCounter=blockDownFreq then
                        --  time to move the block down
                        MoveBlockDown(activeBlockNumber);
                        blockDownFreqCounter:=0;-- reset the counter
                end if;
                
                delay Standard.Duration(loopFreq);-- wait till next activation
        end loop;
        --  game finished
        Vga_Text (Interfaces.C.Strings.New_String("GAME OVER!!!!!!!!!!!!!!!!!!"), (400,20), 7, 0);
        delay 10.0; --  so i can see the game over message (not strictly necessary)
end martrix;