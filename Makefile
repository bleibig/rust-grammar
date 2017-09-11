CC=cc
LDFLAGS=-lfl -lm
ifeq ($(shell uname),Darwin)
	LDFLAGS=-ll
endif

FLEX ?= flex
BISON ?= bison

all: lexer parser-lalr

lexer: lexer_main.o lexer.o tokens.o
	$(CC) -o $@ $^ $(LDFLAGS)

lexer_main.o: lexer_main.c
	$(CC) -c -o $@ $<

tokens.o: tokens.c
	$(CC) -std=c99 -c -o $@ $<

lex.yy.c: lexer.l
	$(FLEX) $<

lexer.o: lex.yy.c tokens.h
	$(CC) -include tokens.h -c -o $@ $<

lexer-lalr.o: lex.yy.c parser-lalr.tab.h
	$(CC) -include parser-lalr.tab.h -c -o $@ $<

parser-lalr.o: parser-lalr.tab.c
	$(CC) -c -o $@ $<

parser-lalr-main.o: parser-lalr-main.c
	$(CC) -std=c99 -c -o $@ $<

parser-lalr: parser-lalr.o parser-lalr-main.o lexer-lalr.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser-lalr.tab.c parser-lalr.tab.h: parser-lalr.y
	$(BISON) $< -d -p rs -v --report=all --warnings=error=all

rlex: rlex.rs
	rustc $<

rparse: rparse.rs
	rustc $<

check: lexer parser parser-lalr
	./testparser.sh

clean:
	rm -rf *.o lexer parser rlex rparse lex.yy.c \
                   parser.c parser.h \
                   parser-lalr.tab.c parser-lalr.tab.h \
                   parser-lalr.output parser-lalr
