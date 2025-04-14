with MaRTE.POSIX_Constants;

package POSIX.Implementation.OK_Signals is

   --  OK (Sig) = True iff we can use Sig with sigwait ().

   OK : constant array (0 .. MaRTE.POSIX_Constants.SIGRTMAX) of Boolean :=
     (False, True,  True,  True,  True,  True,  True,  True,  True, False,
      True,  True,  True,  True,  True,  True,  True,  True,  True, False,
      True,  True,  True,  True,  True,  True,  True,  True,  True,  True,
      True,  True,  -- Non-RT signals
      others => True  -- RT signals
      );

   --  No_Default (Sig) = True iff we need to override the default
   --  treatment of Sig with a do-nothing handler before we try to
   --  use sigwait() with it.

   No_Default : constant array
                        (0 .. MaRTE.POSIX_Constants.SIGRTMAX) of Boolean :=
     (False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False, False, False, False, False, False, False, False, False,
      False, False,  -- Non-RT signals
      others => False  -- RT signals
      );

end POSIX.Implementation.OK_Signals;
