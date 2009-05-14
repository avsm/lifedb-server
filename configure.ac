#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([lifedb-server], [0.1], [anil@recoil.org])
AC_CONFIG_SRCDIR([custom_unix_stubs.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR(./m4)

AC_CANONICAL_SYSTEM
plat=generic
case $target_os in
darwin*)
  plat=macos
  ;;
*)
  ;;
esac
PLATFORM_ML=platform_$plat.ml
AC_SUBST(PLATFORM_ML)

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([string.h sys/param.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([realpath])

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG(sqlite3)
if test "$OCAML_PKG_sqlite3" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'sqlite3'])
fi

AC_CHECK_OCAML_PKG([json-wheel])
if test "$OCAML_PKG_json_wheel" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'json-wheel'])
fi

AC_CHECK_OCAML_PKG([json-static])
if test "$OCAML_PKG_json_static" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'json-static'])
fi

AC_CHECK_OCAML_PKG([ANSITerminal])
if test "$OCAML_PKG_ANSITerminal" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'ANSITerminal'])
fi

AC_CHECK_OCAML_PKG([cryptokit])
if test "$OCAML_PKG_cryptokit" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'cryptokit'])
fi

AC_CHECK_OCAML_PKG([nethttpd-for-netcgi2])
if test "$OCAML_PKG_nethttpd-for-netcgi2" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'nethttpd-for-netcgi2'])
fi

AC_CHECK_OCAML_PKG([netstring])
if test "$OCAML_PKG_netstring" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib module 'netstring'])
fi


AC_CONFIG_FILES([Makefile Makefile.schema
                 client/ocaml/Makefile])
AC_OUTPUT
