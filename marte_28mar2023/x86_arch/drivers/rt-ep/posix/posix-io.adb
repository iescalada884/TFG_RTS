with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.IO_Exceptions;
with System;

package body POSIX.IO is

   type Path is new String (1 .. 16);
   type File_Access_Mode is new MaRTE.Integer_Types.Unsigned_32;
   for File_Access_Mode'Size use Int'Size;
   type Buffer_Length is new Size_T;
   type Buffer is array (1 .. Buffer_Length'Last) of Unsigned_8;
   pragma Pack (Buffer);
   type Buffer_Ac is access all Buffer;
   type Ioctl_Option_Value is new Int;

   ------------
   --  Open  --
   ------------

   function C_Open (Path_Name : access Path;
                  Mode : in File_Access_Mode) return Int;
   pragma Import (C, C_Open, "open");

   function Open
     (Name           : Pathname;
      Mode           : File_Mode)
     return File_Descriptor is
      Fd : Int;
      The_Path : aliased Path;
   begin
      for I in Name'Range loop
         The_Path (I) := Standard.Character (Name (I));
      end loop;
      The_Path (Name'Length + 1) := ASCII.NUL;
      Fd := C_Open (The_Path'Access, File_Access_Mode (File_Mode'Pos (Mode) + 1));
      return File_Descriptor (Fd);
   end Open;

   -------------
   --  Close  --
   -------------
   function C_Close (Fd : in File_Descriptor) return Int;
   pragma Import (C, C_Close, "close");

   procedure Close
     (File : in File_Descriptor) is
      Tmp : Int;  -- not useful
   begin
      Tmp := C_Close (File);
   end Close;

   ------------
   --  Read  --
   ------------
   function C_Read
      (Fd            : in File_Descriptor;
       Buffer_Ptr    : in Buffer_Ac;
       Bytes_To_Read : in Buffer_Length)
      return Int;
   pragma Import (C, C_Read, "read");

   procedure Read
     (File           : in  File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset) is

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, Buffer_Ac);

      Bytes_Read : Int;
      Buffer_Ptr : Buffer_Ac;

      use type Buffer_Length;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Buffer (1)'Address);
      Bytes_Read := C_Read (File, Buffer_Ptr, Buffer'Length * (Ada.Streams.Stream_Element'Size / 8));
      Last := Ada.Streams.Stream_Element_Offset (Bytes_Read / (Ada.Streams.Stream_Element'Size / 8));
   end Read;

   -------------
   --  Write  --
   -------------
   function C_Write
      (Fd             : in File_Descriptor;
       Buffer_Ptr     : in Buffer_Ac;
       Bytes_To_Write : in Buffer_Length)
     return Int;
   pragma Import (C, C_Write, "write");

   procedure Write
     (File           : in File_Descriptor;
      Buffer         : in Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset) is

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, Buffer_Ac);

      Bytes_Written : Int;
      Buffer_Ptr : Buffer_Ac;
      use type Buffer_Length;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Buffer (1)'Address);
      Bytes_Written := C_Write (File, Buffer_Ptr, Buffer'Length * Ada.Streams.Stream_Element'Size / 8);
      Last := Ada.Streams.Stream_Element_Offset (Bytes_Written);
   end Write;

   ---------------------
   --  Generic_Write  --
   ---------------------

   procedure Generic_Write
     (File           : in File_Descriptor;
      Item           : in Data_Type) is

      function To_Buffer_Ac is
         new Ada.Unchecked_Conversion (System.Address, Buffer_Ac);

      use type Buffer_Length;

   begin
      if (C_Write (File, To_Buffer_Ac (Item'Address), Item'Size / 8) < Item'Size / 8) then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Generic_Write;

   ---------------------
   --  Generic_Ioctl  --
   ---------------------
   function C_Ioctl
     (Fd             : in File_Descriptor;
      Request        : in Ioctl_Option_Value;
      Ioctl_Data_Ptr : in Buffer_Ac)
     return Int;
   pragma Import (C, C_Ioctl, "ioctl");

   procedure Generic_Ioctl
     (File     : in     File_Descriptor;
      Request  : in     Ioctl_Options_Type;
      Data     : in out Ioctl_Data_Type) is

      function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion
        (System.Address, Buffer_Ac);

      Tmp : Int;
      Buffer_Ptr : Buffer_Ac;

   begin
      Buffer_Ptr := Address_To_Buffer_Ac (Data'Address);
      Tmp := C_Ioctl (File,
                      Ioctl_Option_Value (Ioctl_Options_Type'Enum_Rep (Request)),
                      Buffer_Ptr);
   end Generic_Ioctl;

end POSIX.IO;
