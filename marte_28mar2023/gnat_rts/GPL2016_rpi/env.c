void
__gnat_setenv (char *name, char *value)
{
}

void
__gnat_getenv (char *name, int *len, char **value)
{
  *value[0] = 0;
  *len = 0;
}
