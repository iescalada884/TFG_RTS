---------------------------------------------------------------------
--
-- Identificador:       ANALOGICO_1710U
--
-- Tipo de componente:  PAQUETE ADA (Cuerpo)
--
-- Modulo:              Analogico_1710u.adb
--
-- Programador:         David Garcia Villaescusa
--
-- Fecha:               16/03/2016
--
-- Funciones que realiza:
--   Paquete con el manejo de la tarjeta pci_1710u.
--   Lectura y escritura analogica
--
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Modificaciones:
--    1) Fecha:
--       Programador:
--       Descripcion:
---------------------------------------------------------------------

with MaRTE.HAL.IO;        use MaRTE.HAL.IO; -- Inb_P, Outb_P, IO_Port
with MaRTE.Integer_Types; use MaRTE.Integer_Types; -- Unsigned_8/16, shift
with PCI_ADA;

with Ada.Text_Io;         use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

package body Analogico_1710u is

  use IO_Analogica;


  type Registers is (AD_Data1,AD_Data2,St_reg1,St_reg2,
                    DI1,DI2,Board,RCount0,RCount1,RCount2,
                    ADch_reg,MUXstop,MUXstart,AD_C_Reg,Cl_I,Cl_F,
                    DA_Out01,DA_Out02,DA_Out11,DA_Out12,DA_C_Reg,
                    DO1,DO2,CaC,CD,WCount0,WCount1,WCount2,CCtrl);

   -- Registers PAG 101
   Offset : constant array (Registers) of IO_Port :=
     (AD_Data1  =>1,
      AD_Data2  =>0,
      St_reg1   =>7,
      St_reg2   =>6,
      DI1   =>17,
      DI2   =>16,
      Board     =>20,
      RCount0   =>24,
      RCount1   =>26,
      RCount2   =>28,
      ADch_reg  =>2,
      MUXstop   =>5,
      MUXstart  =>4,
      AD_C_Reg  =>6,
      Cl_I  =>8,
      Cl_F  =>9,
      DA_Out01  =>11,
      DA_Out02  =>10,
      DA_Out11  =>13,
      DA_Out12  =>12,
      DA_C_Reg  =>14,
      DO1   =>17,
      DO2   =>16,
      CaC   =>19,
      CD    =>18,
      WCount0   =>24,
      WCount1   =>26,
      WCount2   =>28,
      CCtrl     =>30);

   type Input_Ranges is(Bipolar_5, Bipolar_2_5, Bipolar_1_25,Bipolar_0_625,
                        Bipolar_10, Unipolar_10, Unipolar_5, Unipolar_2_5,
                        Unipolar_1_25);
   --Gains Codes PAG 112
   Input_Range : constant array (Input_Ranges) of Unsigned_8 :=
     (Bipolar_5     => 2#0000_0000#,
      Bipolar_2_5   => 2#0000_0001#, -- OLD: 2#0000_0001#
      Bipolar_1_25  => 2#0000_0010#,
      Bipolar_0_625 => 2#0000_0011#,
      Bipolar_10    => 2#0000_0100#,
      Unipolar_10   => 2#0001_0000#,
      Unipolar_5    => 2#0001_0001#,
      Unipolar_2_5  => 2#0001_0010#,
      Unipolar_1_25 => 2#0001_0011#);

   type Output_Ranges is(Unipolar_10, Unipolar_5);

   Output_Range : constant array (Output_Ranges) of Unsigned_8 :=
     (Unipolar_5    => 2#0000_0000#,
      Unipolar_10   => 2#0000_0001#);

   ----------------------
   -- Get_Base_Address --
   ----------------------
   function Get_Base_Address return MaRTE.HAL.IO.IO_Port is
      VENDOR_ID   : constant Unsigned_16 := 16#13FE#;
      DEVICE_ID   : constant Unsigned_16 := 16#1710#;
      From        : PCI_ADA.Pci_Device;
      Found       : PCI_ADA.Pci_Device;
      Result      : integer;
   begin
      From.Device_Fn    := 0;
      From.Pci_Bus      := 0;
      Pci_Ada.Pci_Find_Device(Result,VENDOR_ID,DEVICE_ID,From,Found);
      if Result = 0 then
         return MaRTE.HAL.IO.IO_Port(Found.Pci_Region(3).Base_Address);
      else
         raise Error_Drv_Analogico;
      end if;
   end Get_Base_Address;

   Base : MaRTE.HAL.IO.IO_Port := Get_Base_Address;

   ----------------
   -- Inicializa --
   ----------------
   Not_Initialized : Boolean := True;

   procedure Inicializa is
   begin
      --   Modo de disparo por software
      --   Ganancia
      --   Poniendo en el registro ADch_reg Unipolar_10 PAG 114
      --   El user manual recomienda realziar la asignacion de modo y ganancia
      --   para cada canal de forma independiente.
      for i in IO_Analogica.Canal_Entrada loop
	MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(MUXstart)),unsigned_8(i));
      	MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(MUXstop)),unsigned_8(i));
      	MaRTE.HAL.IO.Outb_P(Base + Offset (ADch_reg),Input_Range(Bipolar_10));
      end loop;

      --   Modo de disparo por software
      --  Triger Sotfware St_reg2|00000001| PAG 118..119
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(St_reg2)),2#0000_0001#);

      -- Canal de salida
      MaRTE.HAL.IO.Outb_P(Base + Offset (DA_C_Reg),Output_Range(Unipolar_5));

      Not_Initialized := False;
   end Inicializa;

   -----------------------
   -- Lee_Canal_Entrada --
   -----------------------
   function Lee_Canal_Entrada
     (Canal: in IO_Analogica.Canal_Entrada)
     return IO_Analogica.Unidades_Convertidor is

      data_MSB_reg1 :Unsigned_16:= 16#00#;
      data_LSB_reg0 :Unsigned_16:= 16#00#;
      result    :Unsigned_16:= 16#00#;
      channel   :unsigned_16:= 16#00#;
      channel_in:unsigned_8 := 16#00#;

      AD_r    :Unsigned_16:= 16#00#;

   begin

      -- Set software trigger St_reg2|00000001| PAG 118..119
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(St_reg2)),2#0000_0001#);

      -- Clear FiFo and Interrupts PAG 121
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(Cl_F)), 2#0000_0000#);
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(Cl_I)), 2#0000_0000#);

      channel_in:=unsigned_8(canal);
      -- Put_Line ("Peticion de canal: "& unsigned_8'Image(channel_in));

      -- Select range and single ended PAG 114
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(MUXstart)),channel_in);
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(MUXstop)),channel_in);
      MaRTE.HAL.IO.Outb_P(Base + Offset (ADch_reg),Input_Range(Bipolar_10));

     --  Soft Trigger PAG 118
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(AD_Data2)),2#0000_0000#);

      --  Hay que esperar que el dato este disponible (8us) PAG 87
      delay(0.000_1); -- #### REVISAR DELAY PARA AJUSTAR ####

      --  Check Status
      --Status:=MaRTE.HAL.IO.Inw_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(St_reg2)));
      --Put("Status: "); Put (integer(Status),Base => 2); New_Line;

      --  Read Data
      result:=MaRTE.HAL.IO.Inw_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(AD_Data2)));
      -- Put("Result: "); Put (integer(result),Base => 2); New_Line;

      -- channel := shift_right(result and 16#F000#, 12);
      -- Put("Channel: "); Put (integer(channel)); New_Line;

      result := result and 16#0FFF#;

      -- Clear FiFo and Interrupts PAG 121
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(Cl_F)), 0);
      MaRTE.HAL.IO.Outb_P(MaRTE.HAL.IO.IO_Port(Base+ Offset(Cl_I)), 0);

      return IO_Analogica.Unidades_Convertidor(result);

   end Lee_Canal_Entrada;

   --------------------------
   -- Escribe_Canal_Salida --
   --------------------------
  procedure Escribe_Canal_Salida (Canal: in IO_Analogica.Canal_Salida;
  Valor: in IO_Analogica.Unidades_Convertidor) is
    Salida01 : Unsigned_8;
    Salida02 : unsigned_8;
  begin
    if Valor < 0 or Valor > 4095 then
      raise Consigna_Fuera_De_Rango;
    end if;
    Salida01 := Unsigned_8 (Shift_Right (Unsigned_32 (Valor) and 16#0F00#,8));
    Salida02 := Unsigned_8 (Unsigned_32 (Valor) and 16#00FF#);

    if Canal = 0 then
      MaRTE.HAL.IO.Outb_P (MaRTE.HAL.IO.IO_Port (Base + Offset (DA_Out01)),
      Salida01);
      MaRTE.HAL.IO.Outb_P (MaRTE.HAL.IO.IO_Port (Base + Offset (DA_Out02)),
      Salida02);
    elsif Canal = 1 then
      MaRTE.HAL.IO.Outb_P (MaRTE.HAL.IO.IO_Port (Base + Offset (DA_Out11)),
      Salida01);
      MaRTE.HAL.IO.Outb_P (MaRTE.HAL.IO.IO_Port (Base + Offset (DA_Out12)),
      Salida02);
    else
      raise Error_Drv_Analogico; -- Canal no valido
    end if;
  end Escribe_Canal_Salida;

   ------------
   -- Cierra --   DUMMY
   ------------
   procedure Cierra is
   begin
      null;
   end Cierra;

   --------------------------------
   -- Estado_Instalacion_Tarjeta --   DUMMY
   --------------------------------
   -- Este procedimiento devuelve informacion para conocer el estado de
   -- instalacion de la tarjeta. En "error" retorna el error que se produjo
   -- durante la instalacion:
   --   - TARJETA_BIEN_INSTALADA: ningun error.
   --   - NO_TARJETA_EN_DIREC: la tarjeta no esta en la direccion esperada.
   --   - NO_AREA_SWSM: aunque esta en la direccion esperada no hay un area de
   --     swsm que "cubra" dicha direccion.
   -- En "direc" se retorna la direccion en la que esta o deberia estar la
   -- tarjeta.
   procedure Estado_Instalacion_Tarjeta (Error: out Error_Instalacion;
         Direc:out Integer) is
      Err : Integer;
   begin
      Err:= 0;
      Direc:= Integer(Base);
      case (Err) is
         when  0 =>
            Error:= Tarjeta_Bien_Instalada;
         when  1 =>
            Error:= No_Tarjeta_En_Direccion;
         when  2 =>
            Error:= No_Area_Swsm;
         when others =>
            raise Error_Drv_Analogico;
            -- Normalmente sera -1. error en la llamada a ioctl
      end case;
   end Estado_Instalacion_Tarjeta;

end Analogico_1710u;
