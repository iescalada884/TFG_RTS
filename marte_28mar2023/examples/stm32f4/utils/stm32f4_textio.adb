--  Stm32f4_TextIO.adb
--  Ada Text_IO-like wrapper for STM32F4 UART console

with Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Stm32f4_TextIO is

   ----------------------------------------------------------------
   --  Imports of  functions for text ios                       --
   ----------------------------------------------------------------
   -- Import the C UART initialization function
   procedure uart_console_init (baudrate : Interfaces.C.int);
   pragma Import (C, uart_console_init, "uart_console_init");

   -- Import the C UART write function (string)
   procedure uart_print_console (msg : Interfaces.C.char_array);
   pragma Import (C, uart_print_console, "uart_print_console");

   -- Import the C UART write function (char)
   procedure uart_console_putchar (c : Interfaces.C.char);
   pragma Import (C, uart_console_putchar, "uart_console_putchar");

   -- Import the C UART error write function (string)
   procedure uart_print_error (msg : Interfaces.C.char_array);
   pragma Import (C, uart_print_error, "uart_print_error");

   -- Marte Direct_IO-like interface for STM32F4 UART console

   ----------------------------------
   --  Basic_Stdout_Initialization --
   ----------------------------------
   procedure Basic_Stdout_Initialization;
   pragma Import (Ada, Basic_Stdout_Initialization,
                    "basic_stdout_initialization");


   ----------------------------------
   --  Basic_Stderr_Initialization --
   ----------------------------------
   procedure Basic_Stderr_Initialization;
   pragma Import (Ada, Basic_Stderr_Initialization,
                    "basic_stderr_initialization");

   ----------------------------------------------------------------------------
   --  Functions for C Code  --------------------------------------------------
   ----------------------------------------------------------------------------

   ------------------------------
   --  Direct_Write_On_Stdout  --
   ------------------------------
   procedure Direct_Write_On_Stdout
     (Buffer_Ptr : in System.Address;
      Bytes      : in Interfaces.C.size_t);
   pragma Import (Ada, Direct_Write_On_Stdout, "direct_write_on_stdout");

   ------------------------------
   --  Direct_Write_On_Stderr  --
   ------------------------------
   procedure Direct_Write_On_Stderr
     (Buffer_Ptr : in System.Address;
      Bytes      : in Interfaces.C.size_t);
   pragma Import (Ada, Direct_Write_On_Stderr, "direct_write_on_stderr");

   ----------------------------
   -- Direct_Read_From_Stdin --
   ----------------------------
   function Direct_Read_From_Stdin return Interfaces.C.unsigned_char;
   pragma Import (Ada, Direct_Read_From_Stdin, "direct_read_from_stdin"); 


   -- Internal error message
   Str_Internal_Error : constant String :=
     Standard.ASCII.CR & "  MaRTE OS INTERNAL ERROR: ";
   Str_Fatal_Error : constant String := " (Fatal Error) ";

   ------------------------------
   --    Functions Text_IO     --   
   ------------------------------
   procedure Initialize is
   begin
      uart_console_init(115200);
      Basic_Stdout_Initialization;
      Basic_Stderr_Initialization;
   end Initialize;

   -- Generic call that change output console
   procedure Set_Mode (New_Mode : Output_Mode) is
   begin
      Mode := New_Mode;
   end Set_Mode;
   
   function Get_Active_Mode return Output_Mode is
   begin
      return Mode;
   end Get_Active_Mode;

   ----------------------
   -- Out wrappers     --
   ----------------------
   procedure Output (Str : in String) is
   begin
      if not Is_Initialized then
         Initialize;
         Is_Initialized := True;
      end if;

      if Mode = UART then
         -- Use UART console
         uart_print_console (Interfaces.C.To_C (Str));
      else
         -- Use direct write
         Direct_Write_On_Stdout (Str'Address, Interfaces.C.size_t (Str'Length));
      end if;
   end Output;

   procedure Output_Error (Str : in String) is
   begin
      if not Is_Initialized then
         Initialize;
         Is_Initialized := True;
      end if;

      if Mode = UART then
         -- Use UART console error output
         uart_print_error (Interfaces.C.To_C (Str));
      else
         -- Use direct write
         Direct_Write_On_Stderr (Str'Address, Interfaces.C.size_t (Str'Length));
      end if;
   end Output_Error;



   -----------------------------------
   ----    Text_IO Procedures     ----
   -----------------------------------
   procedure Put (Str : in String) is
   begin
      Output(Str);
   end Put;

   procedure New_Line is
      C : Character := Standard.ASCII.LF;
   begin
      Output(String'(1 => C));
   end New_Line;

   procedure Put (N : in Integer; Base : in Positive := 10) is
      C : Character;
      Mag : Positive;  -- Magnitude of the number
      Num_Abs : Natural;
      Is_Negative : Boolean := False;
      Digit : Natural;
   begin
      if N < 0 then
         Num_Abs := -N;
         Is_Negative := True;
      else
         Num_Abs := N;
      end if;

      Mag := 1;
      while Num_Abs / Mag >= Base loop
         Mag := Mag * Base;
      end loop;

      if Is_Negative then
         C := '-';
         Output(String'(1 => C));
      end if;

      loop
         Digit := Num_Abs / Mag;
         if Digit <= 9 then
            C := Character'Val (Character'Pos ('0') + Digit);
         else
            C := Character'Val (Character'Pos ('a') + Digit - 10);
         end if;

         Output(String'(1 => C));

         exit when Mag = 1;

         Num_Abs := Num_Abs - Digit * Mag;
         Mag := Mag / Base;
      end loop;
   end Put;

   procedure Put_Line (Str : in String) is
   begin
      Put(Str);
      New_Line;
   end Put_Line;

   -----------
   -- Error --
   -----------
   --
   --  MaRTE_Internal_Error
   procedure Error (Msg   : in String;
                    Fatal : in Boolean := False) is
   begin
      Put_Error (Str_Internal_Error);
      Put_Error (Msg, Fatal);
   end Error;

   ---------------
   -- Put_Error --
   ---------------
   procedure Put_Error (Msg : in String; Fatal : in Boolean := False) is
      procedure Exit_Process (Status : in Interfaces.C.int);
      pragma Import (C, Exit_Process, "exit");
   begin
      Output_Error(Msg);
      if Fatal then
         Output_Error(Str_Fatal_Error);
         -- Exit process if fatal
         Exit_Process(1);
      end if;
   end Put_Error;
   
end Stm32f4_TextIO;
