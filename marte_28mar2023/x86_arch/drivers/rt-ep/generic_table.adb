
package body Generic_Table is

   ---------
   -- Add --
   ---------

   procedure Add
     (E : in Element;
      I : out Index;
      T : in out Element_Table)
   is
   begin
      for J in Index loop
         if T(J).Empty then
            I:=J;
            T(I).Empty:=False;
            T(I).Content:=E;
            return;
         end if;
      end loop;
      raise No_Space;
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (I : Index;
      T : Element_Table)
      return Element
   is
   begin
      if T(I).Empty then
         raise Inexistent;
      else
         return T(I).Content;
      end if;
   end Item;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (I : in Index;
      T : in Element_Table)
     return Boolean
   is
   begin
      return T(I).Empty;
   end Is_Empty;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (I : in Index;
      T : in out Element_Table)
   is
   begin
      T(I).Empty:=True;
   end Delete;

   ---------
   -- Update --
   ---------

   procedure Update
     (New_E : in Element;
      I     : in Index;
      T     : in out Element_Table)
   is
   begin
      if T(I).Empty then
         raise Inexistent;
      else
         T(I).Content:=New_E;
      end if;
   end Update;

end Generic_Table;
