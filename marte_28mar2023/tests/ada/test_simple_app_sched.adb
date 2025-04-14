--  Test not for: x86/GAP2005 linux/GAP2005 linux_lib/GAP2005
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
--  Pragmas not supported in GPL2007
--  pragma Task_Dispatching_Policy (Priority_Specific);
--  pragma Priority_Specific_Dispatching (Application_Defined, 18, 14);
--  pragma Locking_Policy (Preemption_Level_Locking);

--  Most of this "withs" will be unnecessary when pragmas were implemented
with Ada.Application_Scheduling;
with Ada.Application_Scheduling.Support;
with Ada.Task_Identification;
with Ada.Unchecked_Conversion;
with System.OS_Interface.Application_Scheduling;
with MaRTE.HAL;
with System.OS_Interface;
with System.Task_Primitives;
with System.Task_Primitives.Operations;
with MaRTE.Kernel;
with MaRTE.Integer_Types;
with System.Tasking;

--  Other dependencies
with MaRTE_OS;
with Ada.Exceptions;
with System;
with Simple_Scheduler;
with Text_IO; use Text_IO;
with Reports;

procedure Test_Simple_App_Sched is

   Scheduler_Priority : constant System.Any_Priority := 10;

   pragma Priority (Scheduler_Priority + 1);
   --  {Javier} Mas alta que la del scheduler para que el begin-end se
   --  ejecute antes que el body de las tareas. Esto en la
   --  implementación con el pragma Application_Scheduler no será
   --  necesario, ya que el código correspondiente a este pragma se
   --  ejecutará durante la elaboración de la unidad de librería.

   --  pragma Application_Scheduler (Appscheduler_Type.Simple_AppScheduler,
   --                                Scheduler_Priority);
   --  << {Javier} Código correspondiente al pragma 'Application_Scheduler'
   type Simple_AppScheduler_Ac is access all Simple_Scheduler.Simple_Scheduler;
   Scheduler_Ac : Simple_AppScheduler_Ac;

   --------------------------------
   -- Application-scheduled task --
   --------------------------------
   task type Appsched_Task (Id : Integer) is
      pragma Priority (Scheduler_Priority);
      --  pragma Application_Defined_Sched_Parameters (My_Parametes'Access);
      --  pragma Preemption_Level (Param.PL);
   end Appsched_Task;

   task body Appsched_Task is

      --  {Javier} Esto deberá estar oculto al usuario (lo hará
      --  internamente el runtime)
      function To_OSI_Task_Id (Ada_Task_Id : Ada.Task_Identification.Task_Id)
                                return System.OS_Interface.Pthread_t;
      function To_OSI_Task_Id (Ada_Task_Id : Ada.Task_Identification.Task_Id)
                                return System.OS_Interface.Pthread_t is

         function Convert_Ids is new Ada.Unchecked_Conversion
           (Ada.Task_Identification.Task_Id, System.Tasking.Task_ID);
      begin
         return
           System.Task_Primitives.Operations.Get_Thread_Id
            (Convert_Ids (Ada_Task_Id));
      end To_OSI_Task_Id;
      use type MaRTE.Kernel.AppSched_Param_Size_T;
      Ret     : MaRTE.Integer_Types.Int;
      --  >>
   begin
      Reports.Init;
      Put_Line ("Start task body " & Integer'Image (Id) & " (step:" &
                Integer'Image (Simple_Scheduler.Step) & ")");
      Reports.Assert (Simple_Scheduler.Step=4);
      Simple_Scheduler.Step:=Simple_Scheduler.Step+1;
      pragma Assert (MaRTE.HAL.Are_Interrupts_Enabled);
      --  << {Javier} La asociación de los parámetros específicos de
      --  la tarea deberá realizarse en la elaboración de la
      --  tarea. Pienso que lo mejor sería hacerlo antes de la
      --  creación del thread del sistema operativo, en el punto donde
      --  se configura su objeto de atributos mediante la llamada a
      --  'Pthread_Attr_Setappschedparam', aunque también podría
      --  hacerse después de la creación del thread y en ese caso
      --  habría que utilizar 'Pthread_Setappschedparam'.
      Ret :=
        System.OS_Interface.Application_Scheduling.Pthread_Setappschedparam
        (To_OSI_Task_Id (Ada.Task_Identification.Current_Task),
         null, --  Param
         0); --  Kernel.AppSched_Param_Size_T (Param.all'Size / 8));
      --  >>

      --  << {Javier} código correspondiente al pragma Preemption_Level
      --  Lo que hago aquí es una "chapuza" para salir del paso. La manera
      --  de hacerlo bien será utilizando
      --  'System.OS_Interface.Application_Scheduling.
      --  Pthread_Attr_Setpreemptionlevel' antes de la creación del
      --  thread del sistema operativo, en el punto donde se configura
      --  su objeto de atributos.
      --
      --  Ahora el PL es la prioridad base de la tarea.
      --
      --  Kernel.Scheduler.Self.Active_Preemption_Level :=
      --    Kernel.Task_Preemption_Level (Param.PL);
      --  Kernel.Scheduler.Self.Base_Preemption_Level :=
      --    Kernel.Task_Preemption_Level (Param.PL);
      --  >>

      --  << {Javier} Asociación de la tarea con el Scheduler (deberá
      --  realizarse en la elaboración de la tarea, tras la creación
      --  del thread del sistema operativo y tras comprobar que para
      --  su nivel de prioridad existe un appscheduler).
      pragma Assert (MaRTE.HAL.Are_Interrupts_Enabled);
      Ada.Application_Scheduling.Support.Attach_Task_To_Scheduler
        (Ada.Task_Identification.Current_Task, Scheduler_Priority);
      --  >>

      --  Here start the user's code
      Put_Line ("Appsched_task with id:" & Integer'Image (Id) & " (step:" &
                Integer'Image (Simple_Scheduler.Step) & ")");
      Reports.Assert (Simple_Scheduler.Step=6);
      Simple_Scheduler.Step:=Simple_Scheduler.Step+1;

   exception
      when E : others =>
         Put ("Excep. in Appsched_Task:");
         Put (Ada.Exceptions.Exception_Name (E));
         Put_Line (" " & Ada.Exceptions.Exception_Message (E));
   end Appsched_Task;

   ---------------------
   -- Scheduled Tasks --
   ---------------------
   T1 : Appsched_Task (1);

begin
   Reports.Init;
   Put ("Main: Installing scheduler (step:" &
        Integer'Image (Simple_Scheduler.Step) & ")...");
   Reports.Assert (Simple_Scheduler.Step=1);
   Simple_Scheduler.Step:=Simple_Scheduler.Step+1;
   Scheduler_Ac := new Simple_Scheduler.Simple_Scheduler;
   Ada.Application_Scheduling.Support.Install_Scheduler (Scheduler_Ac,
                                                         Scheduler_Priority,
                                                         Scheduler_Priority);
   Put_Line ("done (step:" &
             Integer'Image (Simple_Scheduler.Step) & ")");
   Reports.Assert (Simple_Scheduler.Step=2);
   Simple_Scheduler.Step:=Simple_Scheduler.Step+1;
   delay 0.3;

   Put_Line ("Main: finish (step:" &
             Integer'Image (Simple_Scheduler.Step) & ")");
   Reports.Assert (Simple_Scheduler.Step=7);
   Reports.Test_OK;
end Test_Simple_App_Sched;
