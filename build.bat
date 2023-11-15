IF EXIST "build\*.o" DEL "build\*.o" /s
ca65 "sources/init.s" -g -o "build\init.o" -t nes
ca65 "sources/main.s" -g -o "build\main.o" -t nes
ca65 "sources/pads.s" -g -o "build\pads.o" -t nes
ca65 "sources/player.s" -g -o "build\player.o" -t nes
ca65 "sources/entities.s" -g -o "build\entities.o" -t nes
ca65 "sources/ppu_manager.s" -g -o "build\ppu_manager.o" -t nes
ca65 "sources/level.s" -g -o "build\level.o" -t nes
ca65 "sources/bg.s" -g -o "build\bg.o" -t nes
ca65 "sources/util.s" -g -o "build\util.o" -t nes
ld65 -v  --dbgfile  "build\decembers_fullmoon.dbg" -t nes "build\init.o" "build\main.o" "build\pads.o" "build\player.o" "build\entities.o" "build\ppu_manager.o" "build\level.o" "build\bg.o" "build\util.o" -o "build\decembers_fullmoon.nes" 
