with Ada.Exceptions;
with System;

package body Bttv_Marte is

   procedure Check_Error(E : in Interfaces.C.Int) is
   begin
      case E is
         when -1 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "General error. Contact with MaRTE OS development group");
         when -2 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Mode parameter is no correct");
         when -3 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Width or height parameter is no correct");
         when -4 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "The width/height relation is no 4/3");
         when -5 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Format not supported");
         when -6 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Not enough memory for internal driver");
         when -7 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Frame grabber already initialize. Probably your frame grabber number is wrong");
         when -8 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Image or timestamp parameter is no correct");
         when -9 => Ada.Exceptions.Raise_Exception(Bttv_Error'Identity,
                                                   "Size of your buffer is no correct");
         when others => null;
      end case;
   end Check_Error;

   procedure Init_Video( Height : in Integer;
                         Width  : in Integer;
                         Format : in Integer;
                         Mode   : in Integer;
                         Configuration :in out Video_Multiple_Buffer) is

      function Init_Video_Multibuffer_Internal(Height : in     Interfaces.C.Int;
                                               Width  : in     Interfaces.C.Int;
                                               Format : in     Interfaces.C.Int;
                                               Mode   : in     Interfaces.C.Int;
                                               Buffer : in     System.Address)
                                              return Interfaces.C.Int;
      pragma Import (C, Init_Video_Multibuffer_Internal, "init_video_multibuffer");

      Error_Returned:Interfaces.C.Int;
   begin
      Error_Returned:=Init_Video_Multibuffer_Internal(Interfaces.C.Int(Height),
                                                      Interfaces.C.Int(Width),
                                                      Interfaces.C.Int(Format),
                                                      Interfaces.C.Int(Mode),
                                                      System.Address(Configuration'Address));

      Check_Error(Error_Returned);

   end Init_Video;


   procedure Destroy_Video_Multibuffer (Configuration : in out Video_Multiple_Buffer) is

      function Destroy_Video_Multibuffer_Internal (Buffer : in  System.Address)
                                                  return Interfaces.C.Int;
      pragma Import (C, Destroy_Video_Multibuffer_Internal,"destroy_video_multibuffer");

      Error_Returned:Interfaces.C.Int;
   begin
      Error_Returned:=Destroy_Video_Multibuffer_Internal(System.Address(Configuration'Address));

      Check_Error(Error_Returned);

   end Destroy_Video_Multibuffer;

   -- Init_image --
   ----------------

   procedure Init_Image(Configuration : in out Video_Multiple_Buffer;
                        Image : out System.Address) is

      function Init_Image_Internal(C : in System.Address)
                                  return System.Address;
      pragma Import(C,Init_Image_Internal,"init_image");

   begin
      Image:=Init_Image_Internal(System.Address(Configuration'Address));
   end Init_Image;

   -- Start_Frame_Grabber --
   -------------------------

   procedure Start_Frame_Grabber (Frame_grabber : in Integer;
                                  Configuration : in out Video_Multiple_Buffer) is

      function Start_Frame_Grabber_Internal (Frame_grabber : in Interfaces.C.Int;
                                             Buffer        : in System.Address)
                                            return Interfaces.C.Int;

      pragma Import (C, Start_Frame_Grabber_Internal, "start_frame_grabber");

      Error_Returned:Interfaces.C.Int;
   begin
      Error_Returned:=Start_Frame_Grabber_Internal(Interfaces.C.Int(Frame_Grabber),System.Address(Configuration'Address));

      Check_Error(Error_Returned);

   end Start_Frame_Grabber;

   procedure Wait_For_Next_Image (Configuration : in out Video_Multiple_Buffer;
                                  Im            : in System.Address;
                                  Size_Of_Image : in Integer;
                                  Timestamp     : in out Types.Timespec) is

      function Wait_For_Next_Image_Internal (Buffer        : in System.Address;
                                             Im            : in System.Address;
                                             Size_Of_Image : in Interfaces.C.Int;
                                             Timestamp     : in System.Address)
                                            return     Interfaces.C.Int;

      pragma Import (C, Wait_For_Next_Image_Internal, "wait_for_next_image");

      Error_Returned:Interfaces.C.Int;
   begin

      Error_Returned:=Wait_For_Next_Image_Internal(System.Address(Configuration'Address),Im,Interfaces.C.Int(Size_Of_Image),System.Address(Timestamp'Address));

      Check_Error(Error_Returned);

   end Wait_For_Next_Image;

   procedure Get_Last_Image (Configuration : in out Video_Multiple_Buffer;
                             Im            : in System.Address;
                             Size_Of_Image : in Integer;
                             Timestamp     : in out Types.Timespec) is
      function Get_Last_Image_Internal (Buffer        : in System.Address;
                                        Im            : in System.Address;
                                        Size_Of_Image : in Interfaces.C.Int;
                                        Timestamp     : in System.Address)
                                       return Interfaces.C.Int;

      pragma Import (C, Get_Last_Image_Internal, "get_last_image");
       Error_Returned:Interfaces.C.Int;
   begin

      Error_Returned:=Get_Last_Image_Internal(System.Address(Configuration'Address),Im,Interfaces.C.Int(Size_Of_Image),System.Address(Timestamp'Address));

      Check_Error(Error_Returned);

   end Get_Last_Image;





end Bttv_Marte;
