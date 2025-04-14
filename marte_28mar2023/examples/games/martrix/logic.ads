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
--  file: logic.ads                                                        --
-----------------------------------------------------------------------------
--                        last update: 16 Feb 09                           --
-----------------------------------------------------------------------------
with VGA_Marte;
use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;

package Logic is

        use type Interfaces.C.Unsigned;
        use type Interfaces.C.int;
        
        procedure CheckForMovement(blockNumber:integer);
        procedure CheckMapLines;
        procedure MoveMapLinesDown(line:integer);
        procedure MoveBlockDown(blockNumber:integer);
        function IsSpaceFree(blockNumber:integer;posX:integer;posY:integer) return boolean;
        procedure ChooseNewBlock(first:boolean);

end Logic;