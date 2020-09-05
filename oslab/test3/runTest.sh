#!/bin/bash
cd ..
sudo ./mount-hdc
cp test3/process.c  hdc/usr/root
cp linux-0.11/include/unistd.h hdc/usr/include
sudo umount hdc
./run
