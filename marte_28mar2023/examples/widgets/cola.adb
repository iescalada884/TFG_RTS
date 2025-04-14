package body Cola is

   protected body Protegida is

      function Incrementa(X : in Indice; N : in Indice:=1)
                             return Indice is
      begin
         return (X+N) mod Max;
      end Incrementa;

      procedure Inicializa is
      begin
         La_Cola.Principio	:= 0;
         La_Cola.Fin		:= Max-1;
      end Inicializa;

      procedure Inserta (El_Elemento : in  Elemento) is
      begin
          if Incrementa(La_Cola.Fin,2) = La_Cola.Principio  then
            raise No_Cabe;
         else
            La_Cola.Fin:=Incrementa(La_Cola.Fin);
            La_Cola.Elementos(La_Cola.Fin):=El_Elemento;
      end if;
      end Inserta;

      procedure Extrae (El_Elemento: out Elemento) is
      begin
         if Esta_Vacia then
            raise No_Hay;
         else
            El_Elemento:= La_Cola.Elementos(La_Cola.Principio);
            La_Cola.Principio:=Incrementa(La_Cola.Principio);
         end if;
      end Extrae;

      function Esta_Vacia return Boolean is
      begin
          return Incrementa(La_Cola.Fin)=La_Cola.Principio;
      end Esta_Vacia;

   end Protegida;

begin
   Protegida.Inicializa;
end Cola;
