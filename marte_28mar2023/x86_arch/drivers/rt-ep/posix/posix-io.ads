with Ada.Streams;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;

package POSIX.IO is

   use type MaRTE.Integer_Types.Int;

   Open_Files_Mx : constant := 8;

   type File_Descriptor is new Int range 0 .. Open_Files_Mx - 1;
   for File_Descriptor'Size use Int'Size;

   type File_Mode is (Read_Only, Write_Only, Read_Write);

   function Open
     (Name           : POSIX.Pathname;
      Mode           : File_Mode)
      return File_Descriptor;

   procedure Close
     (File           : in File_Descriptor);

   procedure Read
     (File           : in File_Descriptor;
      Buffer         : out Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (File           : in File_Descriptor;
      Buffer         : in Ada.Streams.Stream_Element_Array;
      Last           : out Ada.Streams.Stream_Element_Offset);

   generic
      type Data_Type is private;
   procedure Generic_Write
     (File           : in File_Descriptor;
      Item           : in Data_Type);

   generic
      type Ioctl_Options_Type is (<>);
      type Ioctl_Data_Type is private;
   procedure Generic_Ioctl
     (File     : in     File_Descriptor;
      Request  : in     Ioctl_Options_Type;
      Data     : in out Ioctl_Data_Type);

end POSIX.IO;
