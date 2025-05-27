.PHONY: clean rls

EDD=edd

debug: dbg/std.savn
release: rls/std.savn

dbg/std.savn: dbg dbg/io.to dbg/mem.to dbg/convert.to dbg/str.to dbg/syscalls.to dbg/panic.to dbg/rt.to
	tar -o $@ $^
rls/std.savn: rls rls/io.to rls/mem.to rls/convert.to rls/str.to rls/syscalls.to rls/panic.to rls/rt.to
	tar -o $@ $^

dbg/syscalls.to: syscalls.telda
	tasm $^
	mv syscalls.to $@
dbg/%.to: %.ð
	$(EDD) -o $@ -c $^
dbg:
	mkdir $@

rls/syscalls.to: syscalls.telda
	tasm $^
	mv syscalls.to $@
	tstrip $@
rls/%.to: %.ð
	$(EDD) -o $@ -c -O $^
	tstrip $@
rls:
	mkdir $@

clean:
	rm -rv dbg rls
