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
--  file: draw.ads                                                         --
--                                                                         --
--  this file contains the functions used to draw the visual elements.     --
-----------------------------------------------------------------------------
--                        last update: 16 Feb 09                           --
-----------------------------------------------------------------------------
with VGA_Marte;
use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;

package Draw is

        use type Interfaces.C.Unsigned;
        use type Interfaces.C.int;
        
        procedure DrawBlock (blockNumber:integer;color:integer;PosX:integer;PosY:integer;plainColor:boolean);
        procedure DrawMap;
        procedure DrawMapBorder;
        procedure DrawSubBlock(posX:integer;posY:integer;color:integer;plainColor:boolean);
        procedure DrawNextBlock;
        procedure DrawNextBlockBorder;
        procedure DrawPoints(points:integer);

end Draw;
