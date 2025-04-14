with Interfaces.C;

package Mouse_Bindings is

   procedure Mouse_Init;
   pragma Export (C, Mouse_Init, "mouse_init");

   function Mouse_Get_Y return Interfaces.C.Int;
   pragma Export (C, Mouse_Get_Y, "mouse_gety");

end Mouse_Bindings;
