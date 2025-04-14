--  Driver based on GNAT.IO package.

with Drivers_MaRTE;  use Drivers_MaRTE;

package GNAT_IO_Driver_Functions is

   function Create return Int;

   function Remove return Int;

   function Open (Fd   : in File_Descriptor;
                  Mode : in File_Access_Mode) return Int;

   function Close (Fd : in File_Descriptor) return Int;

   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T;

   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Ssize_T;

   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac) return Int;

end GNAT_IO_Driver_Functions;
