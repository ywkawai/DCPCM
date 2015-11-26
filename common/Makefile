include ../Mkinclude

# JINTPOBJ is defined in ../Mkinclude as follows;
# JINTPOBJ=interpolate_data.o


OBJS = field_def.o DCPCMComp_mod.o grid_mapping_util.o interpolation_data_latlon_mod.o

MODS =  component_field.mod  dcpcmcomp_mod.mod field_common.mod  field_def.mod \
	grid_mapping_util.mod \
	interpolation_data_latlon_mod.mod

all: lib gmapgen

lib: $(LIBDIR) $(INCDIR) $(LIBDIR)/$(COMMONLIB) $(LIBDIR)/$(JINTPOBJ)

$(LIBDIR)/$(COMMONLIB): $(OBJS) 
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(INSTALL) $(MODS) $(INCDIR)

$(LIBDIR)/$(JINTPOBJ): $(JINTPOBJ)
	$(INSTALL) $(JINTPOBJ) $(LIBDIR)

$(LIBDIR) $(INCDIR):
	mkdir -p $@

clean:
	$(RM) $(TRASH)
realclean: clean

$(JINTPOBJ): interpolation_data_latlon_mod.o

###########################

gmapgen: gmapgen_main.o grid_mapping_util.o
	$(LINK.f90) -o $@ $^ -L$(JCUPLIBDIR) -lspml-lapack-omp \
			-L$(ISPACKLIBDIR) -lisp        \
			-L$(GTOOL5LIBDIR) -lgtool5-mpi \
			-L$(NETCDFLIBDIR) -lnetcdff -lnetcdf

