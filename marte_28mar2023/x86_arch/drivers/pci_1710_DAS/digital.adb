---------------------------------------------------------------------
--
-- Identificador:       DIGITAL
--
-- Tipo de componente:  PAQUETE ADA (Cuerpo)
--
-- Modulo:              Digital.adb   --REAL--
--
-- Programador:         Sergio Martin
--
-- Fecha:               03/09/2008
--
-- Funciones que realiza:
--   Paquete que encapsula el manejo de la tarjeta Pci_1753
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha:
--       Programador:
--       Descripcion:
---------------------------------------------------------------------
with MaRTE.HAL.IO;           use MaRTE.HAL.IO; -- Inb_P, Outb_P, IO_Port

with MaRTE.Integer_Types;    use MaRTE.Integer_Types;-- Unsigned_8/16, shift
with Configuracion;          use Configuracion;
with pci_ada;
with Ada.Text_IO;

package body Digital is

   use Configuracion.Io_Digital;

   type Registers is (Register_A0,
                      Register_B0,
                      Register_C0,
                      Register_A1,
                      Register_B1,
                      Register_C1,
                      Register_A2,
                      Register_B2,
                      Register_C2,
                      Register_A3,
                      Register_B3,
                      Register_C3,
                      Register_Mode_0,
                      Register_Mode_1,
                      Register_Mode_2,
                      Register_Mode_3,
                      Interrupt_Port0,
                      Interrupt_Port1,
                      Interrupt_Port2,
                      Interrupt_Port3,
                      Pattern_Match_Value_A0,
                      Pattern_Match_Enable_A0,
                      Change_State_Register_B0);


   -- Get_Base_Address --
   function Get_Base_Address return MaRTE.HAL.IO.IO_Port is
      VENDOR_ID   : constant Unsigned_16 := 16#13FE#;
      DEVICE_ID   : constant Unsigned_16 := 16#1753#;
      From, Found : pci_ada.Pci_Device;
      Result      : integer;
   begin
      From.Device_Fn    := 0;
      From.Pci_Bus      := 0;

      Pci_ada.Pci_Find_Device(Result,VENDOR_ID,DEVICE_ID,From,Found);
      if Result = 0 then
         return MaRTE.HAL.IO.IO_Port(Found.Pci_Region(3).Base_Address);
      else
--          Ada.Text_IO.Put_Line("pci_region" & Integer'Image(Result) );
         raise Error_Drv_Digital;
      end if;
   end Get_Base_Address;

   Base : MaRTE.HAL.IO.IO_Port:=Get_Base_Address;


   Offset : constant array (Registers) of IO_Port :=
     (Register_A0              => 0,
      Register_B0              => 1,
      Register_C0              => 2,
      Register_Mode_0          => 3,
      Register_A1              => 4,
      Register_B1              => 5,
      Register_C1              => 6,
      Register_Mode_1          => 7,
      Register_A2              => 8,
      Register_B2              => 9,
      Register_C2              => 10,
      Register_Mode_2          => 11,
      Register_A3              => 12,
      Register_B3              => 13,
      Register_C3              => 14,
      Register_Mode_3          => 15,
      Interrupt_Port0          => 16,
      Interrupt_Port1          => 17,
      Interrupt_Port2          => 18,
      Interrupt_Port3          => 19,
      Pattern_Match_Value_A0   => 20,
      Pattern_Match_Enable_A0  => 24,
      Change_State_Register_B0 => 28);


   Last_Write : array (Registers range Register_A0..Register_C3) of Unsigned_8;

   ----------------
   -- Inicializa --
   ----------------

   Not_Initialized : Boolean := True;

   procedure Inicializa is
   begin
      --  On reset we configure ports 0 and 2 as inputs.
      --  The Control Register will contain 2#001_1011#
      --  On reset we configure ports 1 and 3 as outputs.
      --  The Control Register will contain 2#0000_0000#

      Outb_P
        (Base + Offset (Register_Mode_0),2#0001_1011#);
      Outb_P
        (Base + Offset (Register_Mode_1),2#0000_0000#);
      Outb_P
        (Base + Offset (Register_Mode_2),2#0001_1011#);
      Outb_P
        (Base + Offset (Register_Mode_3),2#0000_0000#);

      Not_Initialized := False;
      for i in IO_Digital.Canal_Salida loop
         pon_canal(i,0);
      end loop;
   end Inicializa;

   ---------------
   -- Pon_Canal --
   ---------------

   procedure Pon_Canal
     (Canal: IO_Digital.Canal_Salida;
      Valor: IO_Digital.Valor_Digital)
   is
      use IO_Digital;
      Reg  : Registers;
      Tmp  : Unsigned_8;
      Mask : Unsigned_8 := Shift_Left (16#01#, Natural (Canal mod 8));
   begin
      if Not_Initialized then
         Inicializa;
      end if;
      case Canal_Salida (Canal) is

         when 0..7   => Reg := Register_A1;
         when 8..15  => Reg := Register_B1;
         when 16..23 => Reg := Register_C1;
         when 24..31 => Reg := Register_A3;
         when 32..39 => Reg := Register_B3;
         when 40..47 => Reg := Register_C3;

         when others => raise Error_Drv_Digital;
      end case;
      if Valor = 0 then
         Tmp := Last_Write (Reg) and (not Mask);
      else
         Tmp := Last_Write (Reg) or Mask;
      end if;
      Outb_P (Base + Offset(Reg), Tmp);
      Last_Write (Reg) := Tmp;
   end Pon_Canal;

   ---------------
   -- Lee_Canal --
   ---------------

   function Lee_Canal
     (Canal: IO_Digital.Canal_Entrada)
     return IO_Digital.Valor_Digital
   is
      use IO_Digital;
      Reg  : Registers;
      Tmp : Unsigned_8;
      Mask : Unsigned_8 := Shift_Left (16#01#, Natural (Canal mod 8));
   begin
      if Not_Initialized then
         Inicializa;
      end if;
      case Canal_Entrada (Canal) is

         when 0..7   => Reg := Register_A0;
         when 8..15  => Reg := Register_B0;
         when 16..23 => Reg := Register_C0;
         when 24..31 => Reg := Register_A2;
         when 32..39 => Reg := Register_B2;
         when 40..47 => Reg := Register_C2;

         when others => raise Error_Drv_Digital;
      end case;
      Tmp := Inb_P (Base + Offset(Reg));
      if (Tmp and Mask) = 0 then
         return 0;
      else
         return 1;
      end if;
   end Lee_Canal;

   ------------
   -- Cierra --
   ------------

   procedure Cierra is
   begin
      null;
   end Cierra;

   --------------------------------
   -- Estado_Instalacion_Tarjeta --
   --------------------------------

   procedure Estado_Instalacion_Tarjeta (T: Tarjeta;
                                         Error: out Error_Instalacion;
                                         Dir: out Integer) is
      Err: Integer;
   begin
      Err:= 0;
      Dir:= Integer(Base);
      case (Err) is
         when  0 =>
            Error:= Tarjeta_Bien_Instalada;
         when  1 =>
            Error:= No_Tarjeta_En_Direc;
         when  2 =>
            Error:= No_Area_Swsm;
         when others =>
            raise Error_Drv_Digital;
            -- Normalmente sera -1:        -- error en la llamada a
            -- ioctl
      end case;
   end Estado_Instalacion_Tarjeta;

begin
    Inicializa;
end Digital;





