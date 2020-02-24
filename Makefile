PROJECTNAME := matrices

SRCDIR := src
OBJS := pardiso_unsym_f.o

include variables.mak

override LIBRARY =

include commands.mak
