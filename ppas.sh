#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/attila/Projects/freeLazarus/powtwoman/postwoman
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /home/attila/Projects/freeLazarus/powtwoman/postwoman -T /home/attila/Projects/freeLazarus/powtwoman/link21037.res -e _start
if [ $? != 0 ]; then DoExitLink /home/attila/Projects/freeLazarus/powtwoman/postwoman; fi
IFS=$OFS
