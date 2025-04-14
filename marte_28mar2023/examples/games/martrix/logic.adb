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
--  file: logic.adb                                                        --
--                                                                         --
--  this file contains functions related to the logic of the game          --
-----------------------------------------------------------------------------
--                        last update: 21 Feb 09                           --
-----------------------------------------------------------------------------
with VGA_Marte;use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;

with blocks; use blocks;
with draw; use draw;
with config; use config;
with sound; use sound;

package body Logic is

        use type Interfaces.C.Unsigned;
        use type Interfaces.C.int; 
        ----------------------------------------
        --         CheckForMovement           --
        ----------------------------------------
        --  used to: check keyboard input and move 
        --  the block if necessary

        procedure CheckForMovement(blockNumber:integer) is
        
        tempBlockNumber:integer:=0;

        begin
                Get_Immediate(key,keyAvailable);
                if keyAvailable=true then

                        if key = 's' or key = '2' then  --  move down
                                if (IsSpaceFree(blockNumber,blockX,blockY+1)) then
                                        blockY:=blockY+1;
                                        DrawBlock(blockNumber,9,blockX,blockY-1,true);
                                        DrawBlock(blockNumber,color,blockX,blockY,false);
                                end if;
                        elsif key = 'a' or key = '4' then  --  move left
                                if (IsSpaceFree(blockNumber,blockX-1,blockY)) then
                                        blockX:=blockX-1;
                                        DrawBlock(blockNumber,9,blockX+1,blockY,true);
                                        DrawBlock(blockNumber,color,blockX,blockY,false);
                                end if;
                        elsif key = 'd' or key = '6' then --  move right
                                if (IsSpaceFree(blockNumber,blockX+1,blockY)) then
                                        blockX:=blockX+1;
                                        DrawBlock(blockNumber,9,blockX-1,blockY,true);
                                        DrawBlock(blockNumber,color,blockX,blockY,false);
                                end if;
                        elsif key = 'w' or key = '8' then --  rotate
                        
                                if activeBlockNumber mod 4 = 0 then --last block position
                                        --  we have 28 elements in blocks_pointers, 4 per block
                                        --  we want to rotate between this 4, so if the fourth is
                                        --  reached next one is the first one
                                        tempBlockNumber:=activeBlockNumber-3;
                                        if (IsSpaceFree(tempBlockNumber,blockX,blockY)) then
                                                activeBlockNumber:=tempBlockNumber;
                                                DrawBlock(blockNumber,9,blockX,blockY,true);
                                                DrawBlock(activeBlockNumber,color,blockX,blockY,false);
                                        end if;
                                else
                                        tempBlockNumber:=activeBlockNumber+1;
                                        if (IsSpaceFree(tempBlockNumber,blockX,blockY)) then
                                                activeBlockNumber:=tempBlockNumber;
                                                DrawBlock(blockNumber,9,blockX,blockY,true);
                                                DrawBlock(activeBlockNumber,color,blockX,blockY,false);
                                        end if;
                                end if;   
                                
                        end if;

                        -- flush keyboard buffer
                        while keyAvailable=true loop
                                Get_Immediate(key,keyAvailable);
                        end loop;
                end if;
        end CheckForMovement;
        
        
        ----------------------------------------
        --         MoveBlockDown              --
        ----------------------------------------
        --  used to: move the block down when bloqFreqCounter
        --  equals bloqFreq (see config.ads for more info)
        
        procedure MoveBlockDown(blockNumber:integer) is

        begin         
                if (IsSpaceFree(blockNumber,blockX,blockY+1)) then
                        blockY:=blockY+1;                        
                        DrawBlock(blockNumber,9,blockX,blockY-1,true);
                        DrawBlock(blockNumber,color,blockX,blockY,false);
                else  --  block reached the bottom
                
                        -- add piece to map
                        for X in integer range 1..4 loop
                        for Y in integer range 1..4 loop
                                if blocks_pointers(blockNumber)(Y,X) = 1 then
                                        map1(blockX+X-1,blockY+Y-1):=color;
                                end if;
                        end loop;
                        end loop;
                        CheckMapLines;
                        DrawMap;
                        -- new block
                        ChooseNewBlock(false);
                        DrawNextBlock;
                        blockX:=3;
                        blockY:=0;
                        if (IsSpaceFree(activeBlockNumber,blockX,blockY)) then
                                DrawBlock(activeBlockNumber,color,blockX,blockY,false);
                        else
                                --  GAME OVER!!!!!!!!!!!!!!!!!!!!!
                                gameOver:=true;
                                DrawBlock(activeBlockNumber,1,blockX,blockY,false);
                                
                        end if;
                end if;
                                
        end MoveBlockDown;
        
        
        ----------------------------------------
        --         IsSpaceFree                --
        ----------------------------------------
        --  used to: check if the block fits in a position
        
        function IsSpaceFree(blockNumber:integer;posX:integer;posY:integer) return boolean is
        
        begin
                        
                for X in integer range 1..4 loop
                        for Y in integer range 1..4 loop
                                if blocks_pointers(blockNumber)(Y,X) = 1 then
                                
                                        if (posX+X-1)<1 or (posX+X-1)>10 or (posY+Y-1)>20 or (posY+Y-1)<1 then
                                                return false;
                                        end if;
                                        if map1(posX+X-1,posY+Y-1)>0 then
                                                return false;
                                        end if;
                                        
                                end if;
                        end loop;
                end loop;
                return true;
        end IsSpaceFree;
        
        
        ----------------------------------------
        --         CheckMapLines              --
        ----------------------------------------
        --  used to: check if there are any complete lines
        
        procedure CheckMapLines is
                lineFull:boolean:=true;
        begin
                for Y in integer range 1..20 loop
                        --  check each line
                        for X in integer range 1..10 loop
                                
                                if map1(X,Y) = 0 then
                                        lineFull:=false;
                                end if;
                        end loop;
                        if lineFull then
                                
                                MoveMapLinesDown(Y);
                                
                                points:=points+pointsPerLine;--  line complete, increase points
                                DrawPoints(points);
                                if soundOn then
                                        PlaySoundLine;
                                end if;
                                subPoints:=subPoints+pointsPerLine;
                                if subPoints>=pointsChangeVelocity then --  increase velocity
                                        subPoints:=0;--  reset subpoints
                                        if blockDownFreq-blockDownFreqIncrement > blockDownFreqMin then
                                                blockDownFreq:=blockDownFreq-blockDownFreqIncrement;
                                        end if;
                                end if;
                                
                        end if;
                        lineFull:=true;
                end loop;
        end CheckMapLines;
        
        
        ----------------------------------------
        --        MoveMapLinesDown            --
        ----------------------------------------
        --  used to: given a line number, 'deletes' that line and moves
        --  all the lines above it on position down
        
        procedure MoveMapLinesDown(line:integer) is
                lineFull:boolean:=true;
        begin
                for Y in integer range 1..line loop
                        --  check each line
                        if (line-Y+1)/=1 then
                                for X in integer range 1..10 loop
                                        map1(X,(line-Y+1)) := map1(X,(line-Y+1)-1);
                                end loop;    
                        else
                                for X in integer range 1..10 loop
                                        map1(X,1) := 0;
                                end loop; 
                        end if;
                end loop;
        end MoveMapLinesDown;
        
        
        ----------------------------------------
        --        ChooseNewBlock              --
        ----------------------------------------
        --  used to: choose new active block and next block randomly
        
        procedure ChooseNewBlock(first:boolean) is
                
                subtype rango is Integer range 1 .. 7;
                package Random_Die is new Ada.Numerics.Discrete_Random (rango);
                use Random_Die;
                G : Generator;
        begin
                if first then
                        Reset (G);
                        activeBlockNumber := (Random(G)-1)*4+1;--  this number will be used to
                        --  access the blocks_pointers array and choose the first orientation for a shape,
                        --  each block has 4 orientations
                        color:=(activeBlockNumber+3) / 4;--  28 blocks, 7 colors in subblocks_pointers
                        Reset (G);
                        nextBlockNumber := (Random(G)-1)*4+1;
                        nextColor:=(nextBlockNumber+3) / 4;
                else
                        activeBlockNumber := nextBlockNumber;
                        color := nextColor;
                        Reset (G);
                        nextBlockNumber := (Random(G)-1)*4+1;
                        nextColor:=(nextBlockNumber+3) / 4;
                        
                end if;
               
                
        end ChooseNewBlock;

end logic;