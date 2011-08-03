# Makefile.am is used by automake 1.11 to generate Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(target)/$(gcc_version)

AM_CPPFLAGS = -I$(top_srcdir)/../include $(DEFS)
AM_CFLAGS = -Wall -Werror
AM_LIBTOOLFLAGS = --tag=disable-static

libexecsub_LTLIBRARIES = libfunction_reordering_plugin.la
gcc_build_dir = ../$(host_subdir)/gcc
in_gcc_libs = $(foreach lib, $(libexecsub_LTLIBRARIES), $(gcc_build_dir)/$(lib))

libfunction_reordering_plugin_la_SOURCES =  function_reordering_plugin.cc callgraph.cc
libfunction_reordering_plugin_la_LDFLAGS = $(lt_host_flags) -module -bindir $(libexecsubdir)
libfunction_reordering_plugin_la_DEPENDENCIES =  callgraph.h

all-local: $(in_gcc_libs)

$(in_gcc_libs) : $(gcc_build_dir)/%: %
	@if test "X`dlname=; . ./$*; echo dlname:$$dlname`" = "Xdlname:"; then \
	  echo WARNING: $* is static, not copying to $@ >&2 ; \
	else \
	  $(mkinstalldirs) $(gcc_build_dir) && \
	  $(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(INSTALL) $(INSTALL_STRIP_FLAG) $* `pwd`/$@ ; \
	fi
