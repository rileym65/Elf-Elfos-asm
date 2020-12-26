PROJECT = asm

$(PROJECT).prg: $(PROJECT).asm bios.inc
	../dateextended.pl > date.inc
	../build.pl > build.inc
	rcasm -l -v -x -d1802 $(PROJECT) 2>&1 | tee $(PROJECT).lst
	cat $(PROJECT).prg | sed -f adjust.sed > x.prg
	rm $(PROJECT).prg
	mv x.prg $(PROJECT).prg

clean:
	-rm $(PROJECT).prg


