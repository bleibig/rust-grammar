CC=clang
LDFLAGS=lfl -lm
ifeq ($(shell uname),Darwin)
	LDFLAGS=-ll
endif

all: parser-ll

parser-ll: parser-ll.o ../lexer.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser-ll.o: parser-ll.c
	$(CC) -c -o $@ $<

parser-ll.c: parser-ll.g
	LLnextgen $< --generate-lexer-wrapper=no --verbose

../lexer.o:
	make -C .. lexer.o

clean:
	rm -rf parser-ll parser-ll.c parser-ll.h *.o
