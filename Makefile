plotty: plotty.c
	cc -O2 -o plotty plotty.c

plotty.c: plotty.scm
	ol -O2 -o plotty.c plotty.scm
