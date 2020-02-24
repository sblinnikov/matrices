ifdef SRCS
  OBJS := $(addsuffix .o, $(basename $(SRCS)))
endif
OBJS := $(OBJS:%=$(OBJDIR)/%)

ifdef LIBRARY
  TARGET := $(LIBDIR)/$(PROJECTNAME).a
else
  TARGET := $(BINDIR)/$(PROJECTNAME)
endif

.PHONY: all
all: $(LIBDIR) $(OBJDIR) $(TARGET) $(DEP_FILE)

$(LIBDIR):
	@$(CHK_DIR_EXISTS) $@ || $(MKDIR) $@

$(TMPDIR):
	@$(CHK_DIR_EXISTS) $@ || $(MKDIR) $@

$(OBJDIR):
	@$(CHK_DIR_EXISTS) $@ || $(MKDIR) $@

$(OBJDIR)/%.o $(OBJDIR)/%.mod:: %.f90
	$(FC) $(FFLAGS) -c $<

$(OBJDIR)/%.o: %.f
ifdef IFORT
  ifdef LINUX
		$(FC) $(FFLAGS) -extend_source 132 -c $<
  else ## WIN ##
		$(FC) $(FFLAGS) -extend_source:132 -c $<
  endif
else
	$(FC) $(FFLAGS) -c $<
endif

$(OBJDIR)/%.o: %.for
	$(FC) $(FFLAGS) -c $<

%.f: %.trf
	@$(TREFOR)

ifdef LIBRARY
  $(TARGET): $(OBJS)
		$(AR) cvr $@ $?
else
  $(TARGET): $(OBJS) $(LIBS)
		$(LINK) $^ $(LDFLAGS)
endif

.PHONY: clean
clean:
	$(RM) -r $(OBJDIR)/*
	$(RM) -r $(TARGET)

# Make dependencies
.PHONY: depend
depend: $(DEP_FILE)

# The .dep file depends on the source files, so it automatically gets updated
# when you change your source
$(DEP_FILE): $(TMPDIR) $(SRCS)
ifdef SRCS
	@echo "Making dependencies!"
	$(MAKEDEPEND) -w -o $(DEP_FILE) -f $(SRCS) -b $(OBJDIR)
endif

ifdef SRCS
  ifneq ($(MAKECMDGOALS),clean)
    -include $(DEP_FILE)
  endif
endif
