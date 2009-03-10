#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <sys/types.h>
#include <sys/param.h>
#include <dirent.h>

typedef struct dirent directory_entry;

CAMLprim value unix_read_next_dir(value vd)
{
  DIR *d;
  directory_entry *e;
  d = DIR_Val(vd);
  if (d == (DIR *) NULL) unix_error(EBADF, "readdir", Nothing);
  do {
    e = readdir((DIR *) d);
    if (e == (directory_entry *) NULL) raise_end_of_file();
  } while ( e->d_type != DT_DIR || !strcmp(e->d_name,".") || !strcmp(e->d_name,".." ));
  return copy_string(e->d_name);
}

CAMLprim value unix_realpath(value path)
{
  char buffer[PATH_MAX];
  char *r;
  r = realpath(String_val(path), buffer);
  if (r == NULL) uerror("realpath", path);
  return copy_string(buffer);
}


