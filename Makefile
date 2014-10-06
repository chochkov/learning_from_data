CFLAGS=-Wall -g

clean:
	rm -f *.{o,out}

all: h1.c
	gcc -o h1.o h1.c
