include Mkinclude

all clean realclean:
	$(MAKE) -C $(COMMONDIR) $@ 
#	$(MAKE) -C $(CHMDIR) $@
#	$(MAKE) -C $(ICEDIR) $@
	$(MAKE) -C $(OCNDIR) $@
#	$(MAKE) -C $(LANDDIR) $@
	$(MAKE) -C $(ATMDIR) $@
	$(MAKE) -C $(DRIVERDIR) $@

run:
	$(MAKE) -C ./run $@

realclean: clean-bin clean-lib clean-inc

clean-bin:
	-$(RM) bin/*
clean-lib:
	-$(RM) lib/*
clean-inc:
	-$(RM) include/*
