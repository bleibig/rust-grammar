CC=clang
LDFLAGS=-lfl
ifeq ($(shell uname),Darwin)
	LDFLAGS=-ll
endif

all: lexer parser

lexer: lexer_main.o lexer.o tokens.o
	$(CC) -o $@ $^ $(LDFLAGS)

lexer_main.o: lexer_main.c
	$(CC) -c -o $@ $<

tokens.o: tokens.c
	$(CC) -c -o $@ $<

lex.yy.c: lexer.l
	flex $<

lexer.o: lex.yy.c
	$(CC) -c -o $@ $<

parser.o: parser.c
	$(CC) -c -o $@ $<

parser: parser.o lexer.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser.c: parser.g
	LLnextgen $< --generate-lexer-wrapper=no --verbose

clean:
	rm -rf *.o lexer parser lex.yy.c parser.c parser.h
