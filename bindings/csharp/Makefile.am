#EXTRA_DIST = ...

collectd.dll: collectd.cs
	$(CSC) -target:library -out:$@ $<

all-local: collectd.dll

install-exec-local: collectd.dll
	mkdir -p "$(DESTDIR)$(pkgdatadir)/csharp"
	$(INSTALL) -m 644 collectd.dll \
		"$(DESTDIR)$(pkgdatadir)/csharp"

clean-local:
	rm -f collectd.dll
