with GNAT.Source_Info;

package Reports is

   procedure Init;

   procedure Assert
     (Condition : Boolean;
      Msg       : String := GNAT.Source_Info.Source_Location);

   procedure Test_OK;

end Reports;
