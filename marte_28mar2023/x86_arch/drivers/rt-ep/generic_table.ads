with Generic_Table_Exceptions;

generic
   type Index is range <>;
   type Element is private;
package Generic_Table is

   type Element_Table is private;

   Inexistent : exception renames Generic_Table_Exceptions.Inexistent;
   No_Space   : exception renames Generic_Table_Exceptions.No_Space;

   function Is_Empty
     (I : in Index;
      T : in Element_Table)
     return Boolean;

   procedure Add
     (E : in Element;
      I : out Index;
      T : in out Element_Table);
   -- May raise No_Space;

   function Item
     (I : Index;
      T : Element_Table)
     return Element;
   -- May raise Inexistent

   procedure Update
     (New_E : in Element;
      I     : in Index;
      T     : in out Element_Table);
   -- May raise Inexistent

   procedure Delete
     (I : in Index;
      T : in out Element_Table);

private

   type Element_Node is record
      Empty : Boolean:=True;
      Content : Element;
   end record;

   type Element_Table is array (Index) of Element_Node;

end Generic_Table;
