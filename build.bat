ca65 "sources/legion_main.s" -o "build\legion_main.o" -t nes
ca65 "sources/pads.s" -o "build\pads.o" -t nes
ca65 "sources/init.s" -o "build\init.o" -t nes
ca65 "sources/ppuclear.s" -o "build\ppuclear.o" -t nes
ld65 -v -t nes "build\legion_main.o" "build\pads.o" "build\init.o" "build\ppuclear.o" -o "build\legion_main.nes"
