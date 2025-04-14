-- paquete que usa las funciones de la libreria (en C) de la camara
-- para inicializarla y capturar las imágenes

with Interfaces.C;
with System;
with Types;

--use type Char_Ptrs.Pointer;

package Bttv_Marte is

   -- Palette --
   VIDEO_PALETTE_GRAY   :constant Integer:=1;
   VIDEO_PALETTE_RGB565 :constant Integer:=3;
   VIDEO_PALETTE_RGB24  :constant Integer:=4;
   VIDEO_PALETTE_RGB32  :constant Integer:=5;

   -- Mode --
   INTERLACED           :constant Integer:=0;
   NON_INTERLACED       :constant Integer:=1;

   type Video_Multiple_Buffer is private;

   -- Exceptions --
   ----------------

   Bttv_Error: exception;

   -- Procedures --
   ----------------

   -- This is the first function to call.

   -- The mode value determines if we want to use the complete image (INTERLACED)
   -- or field-to-field (NON_INTERLACED).

   -- The height and width values specify the size of the capture. The value is
   -- dependent of the mode value. If value is INTERLACED, the maximun value that
   -- we can put is width=768 and height=576. With NON_INTERLACED
   -- width=384 and height=288. It is necessary that these values are 4/3
   -- proportion


   -- The format value can be one of this labels

   --     VIDEO_PALETTE_GREY            Linear greyscale
   --     VIDEO_PALETTE_RGB565          565 16 bit RGB
   --     VIDEO_PALETTE_RGB24           24bit RGB
   --     VIDEO_PALETTE_RGB32           32bit RGB
   -- That labels are defined in video_marte.h . In the future, I expect that there were
   -- more formats.Any volunteer?

   -- The last value,buffer, is an output parameter that identify
   -- the frame grabber
   -- The error is 0 if it is right.

   procedure Init_Video (Height        : in     Integer;
                         Width         : in     Integer;
                         Format        : in     Integer; -- VIDEO_PALETTE_****
                         Mode          : in     Integer; -- INTERLACED, NON_INTERLACED
                         Configuration : in out Video_Multiple_Buffer); -- video_multiple_buffer

   -- This is the last function to call. Dealocate struct video_multiple_buffer
   -- The error is 0 if it is right.

   procedure Destroy_Video_Multibuffer (Configuration : in out Video_Multiple_Buffer); -- video_multiple_buffer

   -- This is function is called after init_video_multibuffer and is the core
   -- of the library.
   -- The error is 0 if it is right.

   procedure Start_Frame_Grabber (Frame_grabber : in Integer;
                                  Configuration : in out Video_Multiple_Buffer); -- Video_Multiple_Buffer

   procedure Init_Image(Configuration : in out Video_Multiple_Buffer; -- Video_Multiple_Buffer
                        Image         : out System.Address);

   -- This function puts in image parameter the next valid field/frame from
   -- the frame grabber fbuf and the instant of that capture en timestamp
   -- parameter. The size_of_image value corresponds with expected size of
   -- the capture in bytes.
   -- The return is 0 if it is right.

   procedure Wait_For_Next_Image (Configuration        : in out Video_Multiple_Buffer;   -- Video_Multiple_Buffer;
                                  Im                   : in     System.Address;
                                  Size_Of_Image        : in     Integer;
                                  Timestamp            : in out Types.Timespec);

   --  This function puts in image parameter the actual valid field/frame from
   --  the frame grabber fbuf and the instant of that capture en timestamp
   --  parameter. The size_of_image value corresponds with expected size of
   --  the capture.
   --  The return is 0 if it is right.

   procedure Get_Last_Image (Configuration        : in out Video_Multiple_Buffer;   -- Video_Multiple_Buffer
                             Im                   : in System.Address;
                             Size_Of_Image        : in Integer;
                             Timestamp            : in out Types.Timespec);

private
   NUMBER_BUFFERS: constant :=3;

   type Buffer_Array is array (0..NUMBER_BUFFERS-1) of System.Address;
   pragma Convention(C,Buffer_Array);

   type Buffer_Timespec is array (0..NUMBER_BUFFERS-1) of Types.Timespec;
   pragma Convention(C,Buffer_Timespec);

   type Video_Multiple_Buffer is record
      Frame_Grabber   : Interfaces.C.Int;
      N_Buffers       : Interfaces.C.Int;
      Base            : Buffer_Array;
      Height          : Interfaces.C.Int;
      Width           : Interfaces.C.Int;
      Depth           : Interfaces.C.Int;
      Bytesperline    : Interfaces.C.Int;
      Format          : Interfaces.C.Int;
      Mode            : Interfaces.C.Int;
      Timestamp_Odd   : Buffer_Timespec;
      Timestamp_Even  : Buffer_Timespec;
   end record;
   pragma Convention(C,Video_Multiple_Buffer);


end Bttv_Marte;
