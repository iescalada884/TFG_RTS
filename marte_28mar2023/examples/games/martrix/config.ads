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
--  file: config.ads                                                       --
--                                                                         --
--  this file contains global variables and config parameters              --
-----------------------------------------------------------------------------
--                        last update: 21 Feb 09                           --
-----------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;
with vga_marte; use vga_marte;

package Config is


        use type Interfaces.C.Unsigned;
        use type Interfaces.C.int;
        
        --  error variables used in vga init
        Error : Interfaces.C.int;
        Init_Error, Color_Error, Mode_Error : exception;
        
        posLogoX:integer:=220;  --  startup marte os logo position
        posLogoY:integer:=190;

        doInit:boolean:=true;--  controls game loop ending
        key:Character;--  input from keyboard
        keyAvailable:boolean:=false;
        blockSize:integer:=20;-- size of the subblock's side       
        
        --  initial block's position in the active area/map (top center)
        blockX:Integer:=4;
        blockY:Integer:=0;
        
        --  position of the active area/map in the screen
        paddingLeft:integer:=150;
        paddingUp:integer:=40;
        --  position of the next block's window in the screen
        paddingNextLeft:integer:=390;
        paddingNextUp:integer:=150;
        
        --  block related variables (color and shape)
        activeBlockNumber:integer:=1;
        color:integer:=1;
        nextBlockNumber:integer:=1;
        nextColor:integer:=1;
        
        loopFreq:float:=1.0/24.0;--  period of the game loop
        
        blockDownFreq:integer:=20;-- initial number of periods till block is moved down
        
        blockDownFreqCounter:integer:=0;--  variable to check if we have reached
        --  the blockDownFreq value
        blockDownFreqIncrement:integer:=1;--  amount blockDownFreq is decreased 
        --  when new level is reached
        blockDownFreqMin:integer:=1;--  minimum value for blockDownFreq 
        
        
        points:integer:=0;--  total points
        subPoints:integer:=0;--  used to check when we have reached a new level
        pointsPerLine:integer:=10;
        pointsChangeVelocity:integer:=10;--  points necessary to reach new level
        
        gameOver:boolean:=false;--  has the user lost?
        
        soundOn:constant boolean:= false;--sound on or off?


end Config;
