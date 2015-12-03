include ../Mkinclude


TARGETS		= atm_driver ocn_driver # chm_driver


LIBS4ATM 	= $(LIBDIR)/$(ATMLIB) $(LIBDIR)/$(COMMONLIB)
#LIBS4ATM 	= $(LIBDIR)/$(ATMLIB) $(LIBDIR)/$(LANDLIB) $(LIBDIR)/$(COMMONLIB)
LIBS4OCN 	= $(LIBDIR)/$(OCNLIB) $(LIBDIR)/$(COMMONLIB)
#LIBS4CHM	= $(LIBDIR)/$(CHMLIB) $(LIBDIR)/$(COMMONLIB)

all: $(TARGETS) $(BINDIR)
	$(INSTALL) $(TARGETS) $(BINDIR)
clean:
	$(RM) $(TRASH)
realclean: clean
	$(RM) $(TARGETS)

atm_driver: atm_driver.o $(LIBS4ATM)
	$(LINK.f90) -o $@ $^  $(LIBS4ATM)  -L$(JCUPLIBDIR) -ljcup $(LIBDIR)/$(JINTPOBJ) \
						$(DCPAMLIB) $(SPMLLIB) $(ISPACKLIB) $(GTOOL5LIB) \
						-L$(NETCDFLIBDIR) -lnetcdff -lnetcdf

ocn_driver: ocn_driver.o $(LIBS4OCN)
	$(LINK.f90) -o $@ $^  $(LIBS4OCN)  -L$(JCUPLIBDIR) -ljcup $(LIBDIR)/$(JINTPOBJ) \
						$(DSOGCMLIB) $(SPMLLIB) -L$(ISPACKLIBDIR) -lisp  $(GTOOL5LIB) \
						-L$(NETCDFLIBDIR) -lnetcdff -lnetcdf -llapack

#chm_driver: chm_driver.o
#	$(LINK.f90) -o $@ $^  $(LIBS4CHM)  -L$(JCUPLIBDIR) -ljcup $(LIBDIR)/$(JINTPOBJ)



$(BINDIR):
	mkdir -p $@
