del rtzx.sna rtzx.bin rtzx_main.bin rtzx_main.bin.zx0 rtzx.tap rtzx_codeonly_32768.tap 
sjasmplus --lst ..\code\main.asm

zx0 rtzx_main.bin


sjasmplus --lst ..\code\kickstart\kickstart.asm

copy /B ..\loader\basic_loader.tap + /B rtzx_codeonly_32768.tap rtzx.tap
