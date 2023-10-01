IF EXIST "build\*.o" DEL "build\*.o" /s
ca65 "sources/main.s" -o "build\main.o" -t nes
ca65 "sources/pads.s" -o "build\pads.o" -t nes
ca65 "sources/init.s" -o "build\init.o" -t nes
ca65 "sources/ppuclear.s" -o "build\ppuclear.o" -t nes
ca65 "sources/player.s" -o "build\player.o" -t nes
ld65 -v -t nes "build\main.o" "build\pads.o" "build\init.o" "build\ppuclear.o" "build\player.o" -o "build\decembers_fullmoon.nes"
