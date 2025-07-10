with Stm32f4_TextIO; use Stm32f4_TextIO;

procedure TestIO_Wrapper is
begin
   Put("Prueba mensaje regular");
   
   Put_Error("testando error");
   
   Set_Mode(UART);
   
   Put_Line("Prueba mensaje regular");
   
   Put_Error("testando error");
   
   
end TestIO_Wrapper;
