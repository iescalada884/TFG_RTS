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
--  file: intro.adb                                                        --
--  this file contains the procedure used to show the intro logos          --
-----------------------------------------------------------------------------
--                        last update: 22 Feb 09                           --
-----------------------------------------------------------------------------

with VGA_Marte;use VGA_Marte;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with config; use config;

package body Intro is

        use type Interfaces.C.Unsigned;
        use type Interfaces.C.int;
        
        anyKey:Character;
        anyKeyAvailable:boolean;

        procedure ShowIntro is
        begin
        
                --  show marte os games logo
                for X in integer range 1..200 loop
                        for Y in integer range 1..100 loop
                                if marteLogo(Y,X)/=0 then
                                        Vga_Pixel(
                                        (Interfaces.C.unsigned(posLogoX+X),
                                        Interfaces.C.unsigned(posLogoY+Y)),
                                        Interfaces.C.unsigned(marteLogo(Y,X))
                                        );
                                end if;
                end loop;
                end loop;
                
                delay 3.0;
                
                --  show press any key message                
                for X in integer range 1..200 loop
                        for Y in integer range 1..50 loop
                                if pressAnyKey(Y,X)/=0 then
                                        Vga_Pixel(
                                        (Interfaces.C.unsigned(posLogoX+5+X),
                                        Interfaces.C.unsigned(posLogoY+200+Y)),
                                        Interfaces.C.unsigned(pressAnyKey(Y,X))
                                        );
                                end if;
                end loop;
                end loop;                
                
                --  press any key to start
                Get_Immediate(anyKey,anyKeyAvailable);
                while anyKeyAvailable/=true loop
                                Get_Immediate(anyKey,anyKeyAvailable);
                end loop;
                --  clean up
                for X in integer range 1..200 loop
                        for Y in integer range 1..100 loop
                                if marteLogo(Y,X)/=0 then
                                        Vga_Pixel(
                                        (Interfaces.C.unsigned(posLogoX+X),
                                        Interfaces.C.unsigned(posLogoY+Y)),
                                        Interfaces.C.unsigned(0)
                                        );
                                end if;
                end loop;
                end loop;
                
                for X in integer range 1..200 loop
                        for Y in integer range 1..50 loop
                                if pressAnyKey(Y,X)/=0 then
                                        Vga_Pixel(
                                        (Interfaces.C.unsigned(posLogoX+5+X),
                                        Interfaces.C.unsigned(posLogoY+200+Y)),
                                        Interfaces.C.unsigned(0)
                                        );
                                end if;
                end loop;
                end loop; 
        end ShowIntro;
end Intro;