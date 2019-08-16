## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-lists.texi
doc_mmck_lists_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-lists.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-lists.html/
	@: > doc/mmck-lists.html/$(am__dirstamp)

doc/mmck-lists.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-lists.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-lists.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-lists.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-lists.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-lists.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-lists.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_LISTS_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_LISTS_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_LISTS_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.lists.import.$(MMCK_SOEXT)
#
MMCK_LISTS_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the  shared library "(mmck lists)".  We  need to build
# and install all of them.
#
MMCK_LISTS_IMPORT_SHARED_LIBS	=

# This is the "(mmck lists)" shared library file pathname.
#
MMCK_LISTS_RUNTIME_SHARED_LIBS	= \
	lib/mmck.lists.$(MMCK_SOEXT)

## --------------------------------------------------------------------

# This is  a list of types  specification files, one for  each module.
# They are  *not* installed.   They are  concatenated to  generate the
# globl types file.
#
MMCK_LISTS_TYPES_FILES_MODULES	=

# This is  a list of the  types specification files.  To  be installed
# along with shared libraries.
#
MMCK_LISTS_TYPES_FILES_SHARED_LIBS	= \
	lib/mmck.lists.types

## --------------------------------------------------------------------

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_LISTS_TEMP_FILES		=


#### library (mmck lists): module version

MMCK_LISTS_MODULES		+= lib/mmck.lists.version.scm
MMCK_LISTS_RUNTIME_OBJECTS	+= lib/mmck.lists.version.$(OBJEXT)
MMCK_LISTS_IMPORT_SOURCES	+= lib/mmck.lists.version.import.scm
MMCK_LISTS_IMPORT_DEPS		+= lib/mmck.lists.version.import.$(MMCK_SOEXT)
MMCK_LISTS_IMPORT_SHARED_LIBS	+= lib/mmck.lists.version.import.$(MMCK_SOEXT)
MMCK_LISTS_TYPES_FILES_MODULES	+= lib/mmck.lists.version.types.scm
MMCK_LISTS_TEMP_FILES		+= \
	lib/mmck.lists.version.temp		\
	lib/mmck.lists.version.lock		\
	lib/mmck.lists.version.stamp

# A compiler  invocation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.lists.version.stamp: lib/mmck.lists.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.lists.version.temp
	@touch lib/mmck.lists.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lists.version.$(OBJEXT) $(<) -prologue lib/config.scm \
		-emit-types-file lib/mmck.lists.version.types.scm
	@$(MV) mmck.lists.version.import.scm lib/mmck.lists.version.import.scm
	@mv -f lib/mmck.lists.version.temp $@

lib/mmck.lists.version.types.scm: lib/mmck.lists.version.stamp

lib/mmck.lists.version.import.scm lib/mmck.lists.version.$(OBJEXT): lib/mmck.lists.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lists.version.lock lib/mmck.lists.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lists.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lists.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lists.version.stamp; \
	    $$dry rmdir lib/mmck.lists.version.lock; \
	  else \
	    while test -d lib/mmck.lists.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lists.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lists.version.import.$(MMCK_SOEXT): lib/mmck.lists.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lists): module assertions

MMCK_LISTS_MODULES		+= lib/mmck.lists.assertions.scm
MMCK_LISTS_RUNTIME_OBJECTS	+= lib/mmck.lists.assertions.$(OBJEXT)
MMCK_LISTS_IMPORT_SOURCES	+= lib/mmck.lists.assertions.import.scm
MMCK_LISTS_IMPORT_DEPS		+= lib/mmck.lists.assertions.import.$(MMCK_SOEXT)
MMCK_LISTS_IMPORT_SHARED_LIBS	+= lib/mmck.lists.assertions.import.$(MMCK_SOEXT)
MMCK_LISTS_TYPES_FILES_MODULES	+= lib/mmck.lists.assertions.types.scm
MMCK_LISTS_TEMP_FILES		+= \
	lib/mmck.lists.assertions.temp	\
	lib/mmck.lists.assertions.lock	\
	lib/mmck.lists.assertions.stamp

MMCK_LISTS_ASSERTIONS_DEPS = \
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lists.assertions.stamp: lib/mmck.lists.assertions.scm $(MMCK_LISTS_ASSERTIONS_DEPS)
	@rm -f lib/mmck.lists.assertions.temp
	@touch lib/mmck.lists.assertions.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lists.assertions.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.lists.assertions.types.scm
	@$(MV) mmck.lists.assertions.import.scm lib/mmck.lists.assertions.import.scm
	@mv -f lib/mmck.lists.assertions.temp $@

lib/mmck.lists.assertions.types.scm: lib/mmck.lists.assertions.stamp

lib/mmck.lists.assertions.import.scm lib/mmck.lists.assertions.$(OBJEXT): lib/mmck.lists.assertions.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lists.assertions.lock lib/mmck.lists.assertions.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lists.assertions.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lists.assertions.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lists.assertions.stamp; \
	    $$dry rmdir lib/mmck.lists.assertions.lock; \
	  else \
	    while test -d lib/mmck.lists.assertions.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lists.assertions.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lists.assertions.import.$(MMCK_SOEXT): lib/mmck.lists.assertions.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lists): module core

MMCK_LISTS_MODULES		+= lib/mmck.lists.core.scm
MMCK_LISTS_RUNTIME_OBJECTS	+= lib/mmck.lists.core.$(OBJEXT)
MMCK_LISTS_IMPORT_SOURCES	+= lib/mmck.lists.core.import.scm
MMCK_LISTS_IMPORT_DEPS		+= lib/mmck.lists.core.import.$(MMCK_SOEXT)
MMCK_LISTS_IMPORT_SHARED_LIBS	+= lib/mmck.lists.core.import.$(MMCK_SOEXT)
MMCK_LISTS_TYPES_FILES_MODULES	+= lib/mmck.lists.core.types.scm
MMCK_LISTS_TEMP_FILES		+= \
	lib/mmck.lists.core.temp	\
	lib/mmck.lists.core.lock	\
	lib/mmck.lists.core.stamp

MMCK_LISTS_CORE_DEPS = \
	lib/mmck.lists.assertions.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.lists.core.stamp: lib/mmck.lists.core.scm $(MMCK_LISTS_CORE_DEPS)
	@rm -f lib/mmck.lists.core.temp
	@touch lib/mmck.lists.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lists.core.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.lists.core.types.scm
	@$(MV) mmck.lists.core.import.scm lib/mmck.lists.core.import.scm
	@mv -f lib/mmck.lists.core.temp $@

lib/mmck.lists.core.types.scm: lib/mmck.lists.core.stamp

lib/mmck.lists.core.import.scm lib/mmck.lists.core.$(OBJEXT): lib/mmck.lists.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lists.core.lock lib/mmck.lists.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lists.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lists.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lists.core.stamp; \
	    $$dry rmdir lib/mmck.lists.core.lock; \
	  else \
	    while test -d lib/mmck.lists.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lists.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lists.core.import.$(MMCK_SOEXT): lib/mmck.lists.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lists): main module

MMCK_LISTS_MODULES		+= lib/mmck.lists.scm
MMCK_LISTS_RUNTIME_OBJECTS	+= lib/mmck.lists.$(OBJEXT)
MMCK_LISTS_IMPORT_SOURCES	+= lib/mmck.lists.import.scm
MMCK_LISTS_IMPORT_SHARED_LIBS	+= lib/mmck.lists.import.$(MMCK_SOEXT)
MMCK_LISTS_TEMP_FILES		+= \
	lib/mmck.lists.temp			\
	lib/mmck.lists.lock			\
	lib/mmck.lists.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.lists.stamp: lib/mmck.lists.scm $(MMCK_LISTS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.lists.temp
	@touch lib/mmck.lists.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.lists.$(OBJEXT) $(<)
	@$(MV) mmck.lists.import.scm lib/mmck.lists.import.scm
	@mv -f lib/mmck.lists.temp $@

lib/mmck.lists.import.scm lib/mmck.lists.$(OBJEXT): lib/mmck.lists.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.lists.lock lib/mmck.lists.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.lists.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.lists.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.lists.stamp; \
	    $$dry rmdir lib/mmck.lists.lock; \
	  else \
	    while test -d lib/mmck.lists.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.lists.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.lists.import.$(MMCK_SOEXT): lib/mmck.lists.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck lists): wrapping up

EXTRA_DIST	+= $(MMCK_LISTS_MODULES)

CLEANFILES	+= \
	$(MMCK_LISTS_RUNTIME_SHARED_LIBS)		\
	$(MMCK_LISTS_RUNTIME_OBJECTS)		\
	$(MMCK_LISTS_IMPORT_SHARED_LIBS)		\
	$(MMCK_LISTS_IMPORT_SOURCES)			\
	$(MMCK_LISTS_TYPES_FILES_MODULES)		\
	$(MMCK_LISTS_TYPES_FILES_SHARED_LIBS)	\
	$(MMCK_LISTS_TEMP_FILES)

lib/mmck.lists.$(MMCK_SOEXT): $(MMCK_LISTS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)

lib/mmck.lists.types: $(MMCK_LISTS_TYPES_FILES_MODULES)
	cat $(MMCK_LISTS_TYPES_FILES_MODULES) >$@


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_LISTS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_LISTS_RUNTIME_SHARED_LIBS)

# All the types specification files that we want to install.
#
MMCK_ALL_TYPES_FILES	= \
	$(MMCK_LISTS_TYPES_FILES_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= \
	$(MMCK_ALL_RUNTIME_SHARED_LIBS)		\
	$(MMCK_ALL_IMPORT_SHARED_LIBS)		\
	$(MMCK_ALL_TYPES_FILES)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-lists-core.scm
MMCK_CHECK_OBJECTS	+= tests/test-lists-core.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-lists-core.exe

tests/test-lists-core.$(OBJEXT): tests/test-lists-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-lists-core.exe: tests/test-lists-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
