CC=clang
LDFLAGS=-lfl
ifeq ($(shell uname),Darwin)
	LDFLAGS=-ll
endif

all: lexer parser parser-lalr rlex

lexer: lexer_main.o lexer.o tokens.o
	$(CC) -o $@ $^ $(LDFLAGS)

lexer_main.o: lexer_main.c
	$(CC) -c -o $@ $<

tokens.o: tokens.c
	$(CC) -c -o $@ $<

lex.yy.c: lexer.l
	flex $<

lexer.o: lex.yy.c tokens.h
	$(CC) -include tokens.h -c -o $@ $<

lexer-lalr.o: lex.yy.c parser-lalr.tab.h
	$(CC) -include parser-lalr.tab.h -c -o $@ $<

parser.o: parser.c
	$(CC) -c -o $@ $<

parser: parser.o lexer.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser-lalr.o: parser-lalr.tab.c
	$(CC) -c -o $@ $<

parser-lalr-main.o: parser-lalr-main.c
	$(CC) -c -o $@ $<

parser-lalr: parser-lalr.o parser-lalr-main.o lexer-lalr.o
	$(CC) -o $@ $^ $(LDFLAGS)

parser.c: parser.g
	LLnextgen $< --generate-lexer-wrapper=no --verbose

parser-lalr.tab.c parser-lalr.tab.h: parser-lalr.y
	bison $< -d -v

rlex: rlex.rs
	rustc $<

check: lexer parser parser-lalr
	./testparser.sh

clean:
	rm -rf *.o lexer parser rlex lex.yy.c \
                   parser.c parser.h \
                   parser-lalr.tab.c parser-lalr.tab.h \
                   parser-lalr.output parser-lalr
