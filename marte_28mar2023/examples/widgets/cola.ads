generic
   type Elemento is private;
   Max :  Integer := 128;
package Cola is
   No_Hay : exception;
   No_Cabe : exception;

   subtype Indice is Integer range 0..Max-1;
   type Tipo_Elementos is array (Indice) of Elemento;
   type Cola_Protegida is record
      Principio,Fin : Indice;
      Elementos: Tipo_Elementos;
   end record;

   protected Protegida is
      procedure Inicializa;
      pragma Inline (Inicializa);

      procedure Inserta
        (El_Elemento : in  Elemento);
      -- puede elevar No_Cabe
      pragma Inline (Inserta);

      procedure Extrae
        (El_Elemento: out Elemento);
      -- puede elevar No_Hay
      pragma Inline (Extrae);

      function Esta_Vacia
        return Boolean;
      pragma Inline (Esta_Vacia);

   private
      La_Cola: Cola_Protegida;
   end Protegida;

end Cola;
