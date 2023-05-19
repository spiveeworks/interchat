#!/bin/sh
gcc c_src/main.c `pkg-config x11 xext freetype2 --cflags --libs` && ./a.out $@
