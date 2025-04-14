----------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
----------------------------------------------------------------------------
--                         Copyright (C) 2003-2005
--                     Universidad de Cantabria, SPAIN
--                        http://www.ctr.unican.es/
--
--    This program is free software; you can redistribute it and/or
--    modify it under the terms of the GNU General Public
--    License as published by the Free Software Foundation; either
--    version 2 of the License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    General Public License for more details.
--
--    You should have received a copy of the GNU General Public
--    License along with this program; if not, write to the
--    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--    Boston, MA 02111-1307, USA.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this  unit  does not  by itself cause  the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file  might be covered by the  GNU Public License.
--  ------------------------------------------------------------------------
--                                                     RT-EP /Ada95 {V1.1}
--
--                'r t e p - p r o t o c o l - c o r e . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol-core.ads'                                  By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RT-EP Core implementation. The Main Communication Task and Error Handling
--  Stuff.
--
--
--
--
-----------------------------------------------------------------------------


package RTEP.Protocol.Core is

   --  If there is any Initialitation Error.
   Initialitation_Error : exception;

   ---------------
   --  Init_Comm --
   ---------------
   --  init_comm must be used to start the communications
   --  thread, the tx and rx queues, and all the nodes are present. It will
   --  block the caller until the initialization is done and the token starts
   --  circulating so messages can be sent.

   --  Raise on Error:
   --  Creation_Error
   --  Unexpected_Error
   --  Initialization_Error

   procedure Init_Comm;
   pragma export (C, Init_Comm, "rtep_init_comm");

   -------------------------
   -- Is_RTEP_Initialized --
   -------------------------

   function Is_RTEP_Initialized return Boolean;

end RTEP.Protocol.Core;
