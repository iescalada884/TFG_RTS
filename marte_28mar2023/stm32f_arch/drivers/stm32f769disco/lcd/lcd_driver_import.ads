--  Driver for the STM32F769I-Disco LCD.

with Drivers_MaRTE;  use Drivers_MaRTE;
with System, Ada.Unchecked_Conversion;



package LCD_Driver_Import is

   --  Create
   function Create return Int;
   pragma Import (C, Create, "lcd_create");
   function Address_To_Create_Ac is
      new Ada.Unchecked_Conversion (System.Address, Create_Function_Ac);
   Create_Ac : Create_Function_Ac := Address_To_Create_Ac (Create'Address);

   --  Remove
--     function Remove return Int;
--     pragma Import (C, Remove, "lcd_remove");
--     function Address_To_Remove_Ac is
--        new Ada.Unchecked_Conversion (System.Address, Remove_Function_Ac);
--     Remove_Ac : Remove_Function_Ac := Address_To_Remove_Ac (Remove'Address);

   --  Open
--     function Open (Fd   : in File_Descriptor;
--                    Mode : in File_Access_Mode) return Int;
--     pragma Import (C, Open, "lcd_open");
--     function Address_To_Open_Ac is
--        new Ada.Unchecked_Conversion (System.Address, Open_Function_Ac);
--     Open_Ac : Open_Function_Ac := Address_To_Open_Ac (Open'Address);


   --  Close
--     function Close (Fd : in File_Descriptor)return Int;
--     pragma Import (C, Close, "lcd_close");
--     function Address_To_Close_Ac is
--        new Ada.Unchecked_Conversion (System.Address, Close_Function_Ac);
--     Close_Ac : Close_Function_Ac := Address_To_Close_Ac (Close'Address);


   --  Read
--     function Read (Fd         : in File_Descriptor;
--                    Buffer_Ptr : in Buffer_Ac;
--                    Bytes      : in Buffer_Length) return Int;
--     pragma Import (C, Read, "lcd_read");
--     function Address_To_Read_Ac is
--        new Ada.Unchecked_Conversion (System.Address, Read_Function_Ac);
--     Read_Ac : Read_Function_Ac := Address_To_Read_Ac (Read'Address);


   --  Write
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Write, "lcd_write");
   function Address_To_Write_Ac is
      new Ada.Unchecked_Conversion (System.Address, Write_Function_Ac);
   Write_Ac : Write_Function_Ac := Address_To_Write_Ac (Write'Address);


   --  Ioctl
--     function Ioctl (Fd             : in File_Descriptor;
--                     Request        : in Ioctl_Option_Value;
--                     Ioctl_Data_Ptr : in Buffer_Ac) return Int;
--     pragma Import (C, Ioctl, "lcd_ioctl");
--     function Address_To_Ioctl_Ac is
--        new Ada.Unchecked_Conversion (System.Address, Ioctl_Function_Ac);
--     Ioctl_Ac : Ioctl_Function_Ac := Address_To_Ioctl_Ac (Ioctl'Address);

end LCD_Driver_Import;
