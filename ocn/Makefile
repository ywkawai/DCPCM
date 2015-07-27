include ../Mkinclude

COMPO	= ocn
OBJS	= $(COMPO).o
MODS	= mod_$(COMPO).mod
LIB	= $(OCNLIB)


all: lib

lib: $(LIBDIR) $(INCDIR) $(LIBDIR)/$(LIB)

$(LIBDIR)/$(LIB): $(OBJS) 
	$(AR) $(ARFLAGS) $@ $^
	$(INSTALL) $(MODS) $(INCDIR)

$(LIBDIR) $(INCDIR):
	mkdir -p $@

clean:
	$(RM) $(TRASH)
realclean: clean

$(COMPO).o: $(LIBDIR)/$(COMMONLIB) $(LIBDIR)/$(JINTPOBJ)
$(COMPO).o: $(LIBDIR)/$(ICELIB)

### Is below necessary/suitable ?
$(LIBDIR)/$(COMMONLIB) $(LIBDIR)/$(JINTPOBJ):
	$(MAKE) -C $(COMMONDIR)
$(LIBDIR)/$(ICELIB): 
	$(MAKE) -C $(ICEDIR)

