with Interfaces.C;
with MaRTE.Integer_Types;

package body UART_Console_Driver_Functions is

   use type Int, Buffer_Length;

   Initialized : Boolean := False;


   procedure Put_Char (Char : MaRTE.Integer_Types.Unsigned_8);
   pragma Import (C, Put_Char, "putchar");
   --  Defined in ninjastorms/libc/stdio/putchar.c

   ------------
   -- Create --
   ------------

   function Create return Int is
   begin
      Initialized := True;

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
      return 0;
   end Read;

   -----------
   -- Write --
   -----------

   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Ssize_T is
   begin
      for I in 1 .. Bytes loop
         Put_Char (Buffer_Ptr (I));
      end loop;

      return Ssize_T (Bytes);
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

end UART_Console_Driver_Functions;
