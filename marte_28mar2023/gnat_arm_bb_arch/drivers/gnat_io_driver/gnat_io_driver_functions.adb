--  Driver based on GNAT.IO package.

with Ada.Unchecked_Conversion;
with GNAT.IO;

package body GNAT_IO_Driver_Functions is

   use type Int, Buffer_Length;

   --  Subprograms from System.Text_IO.

   function System_Text_IO_Initialized return Boolean;
   pragma Import (C, System_Text_IO_Initialized,
                  "system__text_io__initialized");
   procedure System_Text_IO_Initialize;
   pragma Import (C, System_Text_IO_Initialize,
                  "system__text_io__initialize");

   --  Functions from newlib-bb.c

   function Write_Newlib_BB (Fd         : in File_Descriptor;
                             Buffer_Ptr : in Buffer_Ac;
                             Bytes      : in Buffer_Length) return Ssize_T;
   pragma Import (C, Write_Newlib_BB, "write_newlib_bb");

   function Read_Newlib_BB (Fd         : in File_Descriptor;
                            Buffer_Ptr : in Buffer_Ac;
                            Bytes      : in Buffer_Length) return Ssize_T;
   pragma Import (C, Read_Newlib_BB, "read_newlib_bb");


   ------------
   -- Create --
   ------------

   function Create return Int is
   begin
      if not System_Text_IO_Initialized then
         System_Text_IO_Initialize;
      end if;

      return 0;
   end Create;

   ------------
   -- Remove --
   ------------

   function Remove return Int is
   begin
      return 0;
   end Remove;

   ----------
   -- Open --
   ----------

   function Open (Fd   : in File_Descriptor;
                  Mode : in File_Access_Mode) return Int is
   begin
      return 0;
   end Open;

   -----------
   -- Close --
   -----------

   function Close (Fd : in File_Descriptor) return Int is
   begin
      return 0;
   end Close;

   ----------
   -- Read --
   ----------

   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T is
   begin
--        return -1;
      return Read_Newlib_BB (Fd, Buffer_Ptr, Bytes);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Ssize_T is
--        type String_Ac is access all String (Natural);
--        function To_String_Ac is
--          new Ada.Unchecked_Conversion (Buffer_Ac, String_Ac);
--        Str : String_Ac;
   begin
--        Str := To_String_Ac (Buffer_Ptr);
--        for I in 0 .. Bytes - 1 loop
--           GNAT.IO.Put (Str (Natural (I)));
--        end loop;
--
--        return Int (Bytes);
      return Write_Newlib_BB (Fd, Buffer_Ptr, Bytes);
   end Write;

   -----------
   -- Ioctl --
   -----------

   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac)  return Int is
   begin
      return 0;
   end Ioctl;

end GNAT_IO_Driver_Functions;
