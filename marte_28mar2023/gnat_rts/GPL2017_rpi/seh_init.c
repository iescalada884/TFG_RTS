
/*  This unit contains support for SEH (Structured Exception Handling).
    Right now the only implementation is for Win32.  */

// MaRTE OS version

/* For all non Windows targets we provide a dummy SEH install handler.  */
void __gnat_install_SEH_handler (void *eh)
{
}
