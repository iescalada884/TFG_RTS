--  Test for all architectures
--  Test program for the freelist implementation
with MaRTE_OS;
with Text_IO; use Text_IO;
with Reports;
with Freelist_Imports; use Freelist_Imports;
with Interfaces.C;

procedure Test_Freelist_Ada is
    use type Interfaces.C.Int;

    --  pragma Linker_Options ("../../misc/freelist.o");

    Numbers : array (Interfaces.C.Int range 0 .. 9) of Integer;
    The_List : Freelist_Type;
    List : access Freelist_Type := The_List'Unrestricted_Access;
    Err : Interfaces.C.Int;

    procedure Show is
    begin
        New_Line;
        for I in Numbers'Range loop
            Put_Line("num("&Interfaces.C.Int'Image (I)&") = "&Integer'Image(Numbers(I)));
        end loop;
        New_Line;
    end Show;

    procedure Add (Value : in Integer) is
        Pos : Interfaces.C.Int;
    begin
        Pos := Freelist_Alloc (List);
        if (Pos >= 0) then
            Numbers (Pos) := Value;
            Put_Line("Added num("
                     &Interfaces.C.Int'Image (Pos)&") = "&Integer'Image(Value));
        else
            Put_Line("alloc "&Integer'Image(Value)&" failed");
        end if;
    end Add;

    procedure Remove (Pos : in Interfaces.C.Int) is
        Err : Interfaces.C.Int;
    begin
        Err := Freelist_Free (List, Pos);
        if (Err = 0) then
            Put_Line("Removed num("
                     &Interfaces.C.Int'Image (Pos)&") = "&Integer'Image(Numbers (Pos)));
            Numbers (Pos) := 0;
        else
            Put_Line("free "&Interfaces.C.Int'Image(Pos)&" failed");
        end if;
    end Remove;

begin
    Reports.Init;

    Err := Freelist_Init (List, 10);

    if (Err /= 0) then
        Put_Line("Freelist_Init failed");
        return;
    end if;

    --  Add 11 elements to list
    for Elem in Integer range 100 .. 111 loop
        Add (Elem);
    end loop;
    Show;

    --  Remove 5 elements from list
    Remove (1);
    Remove (2);
    Remove (3);
    Remove (5);
    Remove (7);

    --  Remove already Removed element
    Remove (3);
    Show;

    --  Add 6 elements to list
    for Elem in Integer range 111 .. 117 loop
        Add (Elem);
    end loop;
    Show;

    --  Remove 5 elements from list
    Remove (1);
    Remove (2);
    Remove (3);
    Remove (5);
    Remove (7);
    Show;

    --  Add 6 elements to list
    for Elem in Integer range 111 .. 117 loop
        Add (Elem);
    end loop;
    Show;

    Reports.Test_OK;
end Test_Freelist_Ada;
