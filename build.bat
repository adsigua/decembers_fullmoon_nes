IF EXIST "build\*.o" DEL "build\*.o" /s
ca65 "sources/main.s" -o "build\main.o" -t nes
ca65 "sources/init.s" -o "build\init.o" -t nes
ca65 "sources/ppuclear.s" -o "build\ppuclear.o" -t nes
ca65 "sources/entities.s" -o "build\entities.o" -t nes
ca65 "sources/pads.s" -o "build\pads.o" -t nes
ca65 "sources/bg.s" -o "build\bg.o" -t nes
ca65 "sources/player.s" -o "build\player.o" -t nes
ca65 "sources/level.s" -o "build\level.o" -t nes
ld65 -v  --dbgfile  "build\decembers_fullmoon.dbg" -t nes "build\main.o" "build\init.o" "build\ppuclear.o" "build\entities.o" "build\pads.o" "build\bg.o" "build\player.o" "build\level.o" -o "build\decembers_fullmoon.nes" 
