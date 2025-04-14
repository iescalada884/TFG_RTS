with MaRTE_OS;
with Vga_Marte; use Vga_Marte;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

procedure Test_Panel is
      pragma linker_options ("-ldrivers -lsvga -lm");
      Error : Interfaces.C.Int;
begin

      Error:=Init_Vga(G640X480X16,VGA,PCI_DEVICE_ID_S3_TRIO64V2);
      if Error/=0 then
         return;
      end if;

      -- Fondo Gris claro
      Vga_Rectangle_Fill((0,0),(639,479),7);
      -- Marco Superior
      Vga_Rectangle((8,8),(368,89),0);
      Vga_Line((118,8),(118,89),0);
      Vga_Line((292,8),(292,89),0);

      -- Marcos Intermedios
      Vga_Rectangle((8,160),(249,270),0);
      Vga_Rectangle((443,150),(632,270),0);

      -- Marcos Inferiores
      Vga_Rectangle((34,370),(223,470),0);
      Vga_Rectangle((237,390),(428,470),0);
      Vga_Rectangle((443,280),(632,470),0);

      -- Marco Joystick
      Vga_Rectangle_Fill((330,284),(428,382),0);
      Vga_Rectangle_Fill((334,288),(424,378),9);
      Vga_Text(Interfaces.C.Strings.New_String("POSIC.JOY."),(340,275), 0, 15);

      -- Ejes Coordenados
      Vga_Line((334,288),(424,288),9);
      for X in Interfaces.C.Unsigned(334)..Interfaces.C.Unsigned(424) loop
         if X mod 2 = 0 then
            Vga_Pixel((X,333),15);
         end if;
      end loop;
      for Y in Interfaces.C.Unsigned(288)..Interfaces.C.Unsigned(378) loop
         if Y mod 2 = 0 then
            Vga_Pixel((379,Y),15);
         end if;
      end loop;

      -- Leyendas
      Vga_Text (Interfaces.C.Strings.New_String("GARRA"), (40,16), 0, 15);
      Vga_Text (Interfaces.C.Strings.New_String("HERRAMIENTA"), (161,16), 0, 15);
      Vga_Text (Interfaces.C.Strings.New_String("ERRORES"), (510,158), 0, 15);
      Vga_Text (Interfaces.C.Strings.New_String("AVANCE PASO A PASO"), (466,288), 0, 15);
      Vga_Text (Interfaces.C.Strings.New_String("POSICION/ORIENTACION"), (253,398), 0, 15);
      Vga_Text (Interfaces.C.Strings.New_String("1"), (25,260),  15, 0);
      Vga_Text (Interfaces.C.Strings.New_String("2"), (65,260),  15, 0);
      Vga_Text (Interfaces.C.Strings.New_String("3"), (105,260), 15, 0);
      Vga_Text (Interfaces.C.Strings.New_String("4"), (145,260), 15, 0);
      Vga_Text (Interfaces.C.Strings.New_String("5"), (185,260), 15, 0);
      Vga_Text (Interfaces.C.Strings.New_String("6"), (225,260), 15, 0);

      -- Flechas Avance Paso a Paso
      Vga_Line ((482,306),(474,306),0);
      Vga_Line ((474,306),(464,316),0);
      Vga_Line ((464,316),(464,312),0);
      Vga_Line ((464,316),(468,316),0);

      Vga_Line ((596,306),(604,306),0);
      Vga_Line ((604,306),(614,316),0);
      Vga_Line ((614,316),(614,312),0);
      Vga_Line ((614,316),(610,316),0);
      ----------------------------------
      Vga_Line ((539,374),(539,390),0);
      Vga_Line ((539,374),(535,378),0);
      Vga_Line ((539,374),(543,378),0);

      Vga_Line ((512,401),(528,401),0);
      Vga_Line ((512,401),(516,397),0);
      Vga_Line ((512,401),(516,405),0);

      Vga_Line ((566,401),(550,401),0);
      Vga_Line ((566,401),(562,397),0);
      Vga_Line ((566,401),(562,405),0);

      Vga_Line ((539,428),(539,412),0);
      Vga_Line ((539,428),(535,424),0);
      Vga_Line ((539,428),(543,424),0);

end Test_Panel;
