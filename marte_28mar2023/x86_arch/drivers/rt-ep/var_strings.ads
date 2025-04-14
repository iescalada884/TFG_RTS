with Ada.Text_IO; use Ada;
package Var_Strings is

   Max_Length : constant Integer := 64;

   type Var_String is private;

   Null_Var_String : constant Var_String;

   function To_String     (V : Var_String) return String;
   function To_Var_String (S : String)     return Var_String;

   function Length (V : Var_String) return Natural;

   procedure Get_Line (V : out Var_String);
   procedure Put_Line (V : in Var_String);
   procedure Put      (V : in Var_String);

   procedure Get_Line (F : in out Text_IO.File_Type; V : out Var_String);
   procedure Put_Line (F : in out Text_IO.File_Type; V : in Var_String);
   procedure Put      (F : in out Text_IO.File_Type; V : in Var_String);


   function "&" (V1, V2 : Var_String) return Var_String;
   function "&" (V : Var_String; S : String) return Var_String;
   function "&" (S : String; V : Var_String) return Var_String;
   function "&" (V : Var_String; C : Character) return Var_String;

   function "=" (V1, V2 : Var_String) return Boolean;
   function ">" (V1, V2 : Var_String) return Boolean;
   function "<" (V1, V2 : Var_String) return Boolean;
   function ">=" (V1, V2 : Var_String) return Boolean;
   function "<=" (V1, V2 : Var_String) return Boolean;

   function Element (V : Var_String; Index : Positive) return Character;

   function Slice (V : Var_String; Index1 : Positive; Index2 : Natural)
                  return Var_String;

   function To_Upper (V : Var_String) return Var_String;
   function To_Lower (V : Var_String) return Var_String;

   procedure Translate_To_Upper (V : in out Var_String);
   procedure Translate_To_Lower (V : in out Var_String);

private

   type Var_String is record
      Str : String (1 .. Max_Length);
      Num : Integer range 0 .. Max_Length := 0;
   end record;

   Null_Var_String : constant Var_String :=
     (Str => (others => ' '), Num => 0);

end Var_Strings;

