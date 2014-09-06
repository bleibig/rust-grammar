%{

#include <stdio.h>
#include <ctype.h>

  // # Notes
  // This is lacking the folowing:
  // * Doc comments (they don't have much of a significance to the
  //   grammar and can just be treated as regular comments)
  //
  // This adds the following tokens not specified in the rust lexer:
  // * SHEBANG "#!" - this is neccesary to avoid first/first conficts
  //   with attribute parsing

static int num_hashes;
static int end_hashes;
static int saw_non_hash;

%}

%option stack

%x str
%x rawstr
%x rawstr_esc
%x byte
%x bytestr
%x rawbytestr
%x rawbytestr_nohash
%x pound
%x shebang_or_attr
%x ltorchar
%x linecomment
%x doc_line
%x blockcomment
%x doc_block

suf (i8|i16|i32|i64|i|u8|u16|u32|u64|u)

%%

[ \n\t\r]             { }

\/\/(\/|\!)           { BEGIN(doc_line); yymore(); }
<doc_line>\n          { BEGIN(INITIAL);
                        yyleng--;
                        yytext[yyleng] = 0;
                        return ((yytext[2] == '!') ? INNER_DOC_COMMENT : OUTER_DOC_COMMENT);
                      }
<doc_line>[^\n]*      { yymore(); }

\/\/|\/\/\/\/         { BEGIN(linecomment); }
<linecomment>\n       { BEGIN(INITIAL); }
<linecomment>[^\n]*   { }

\/\*(\*|\!)[^*]       { yy_push_state(INITIAL); yy_push_state(doc_block); yymore(); }
<doc_block>\/\*       { yy_push_state(doc_block); yymore(); }
<doc_block>\*\/       {
    yy_pop_state();
    if (yy_top_state() == doc_block) {
        yymore();
    } else {
        return ((yytext[2] == '!') ? INNER_DOC_COMMENT : OUTER_DOC_COMMENT);
    }
}
<doc_block>(.|\n)     { yymore(); }

\/\*                  { yy_push_state(blockcomment); }
<blockcomment>\/\*    { yy_push_state(blockcomment); }
<blockcomment>\*\/    { yy_pop_state(); }
<blockcomment>(.|\n)   { }

_        { return UNDERSCORE; }
as       { return AS; }
box      { return BOX; }
break    { return BREAK; }
const    { return CONST; }
continue { return CONTINUE; }
crate    { return CRATE; }
else     { return ELSE; }
enum     { return ENUM; }
extern   { return EXTERN; }
false    { return FALSE; }
fn       { return FN; }
for      { return FOR; }
if       { return IF; }
impl     { return IMPL; }
in       { return IN; }
let      { return LET; }
loop     { return LOOP; }
match    { return MATCH; }
mod      { return MOD; }
mut      { return MUT; }
once     { return ONCE; }
priv     { return PRIV; }
proc     { return PROC; }
pub      { return PUB; }
ref      { return REF; }
return   { return RETURN; }
self     { return SELF; }
static   { return STATIC; }
struct   { return STRUCT; }
trait    { return TRAIT; }
true     { return TRUE; }
type     { return TYPE; }
typeof   { return TYPEOF; }
unsafe   { return UNSAFE; }
use      { return USE; }
where    { return WHERE; }
while    { return WHILE; }

[a-zA-Z_][a-zA-Z0-9_]*      { return IDENT; }

0x[0-9a-fA-F_]+{suf}?                              { return LIT_INTEGER; }
0o[0-8_]+{suf}?                                    { return LIT_INTEGER; }
0b[01_]+{suf}?                                     { return LIT_INTEGER; }
[0-9][0-9_]*{suf}?                                 { return LIT_INTEGER; }
[0-9][0-9_]*\.(\.|[a-zA-Z])    { yyless(yyleng - 2); return LIT_INTEGER; }

[0-9][0-9_]*\.[0-9]*([eE][-\+]?[0-9]+)?(f32|f64)?   { return LIT_FLOAT; }
[0-9][0-9_]*(\.[0-9]*)?[eE][-\+]?[0-9]+(f32|f64)?   { return LIT_FLOAT; }
[0-9][0-9_]*(f32|f64)                               { return LIT_FLOAT; }

;      { return ';'; }
,      { return ','; }
\.\.\. { return DOTDOTDOT; }
\.\.   { return DOTDOT; }
\.     { return '.'; }
\(     { return '('; }
\)     { return ')'; }
\{     { return '{'; }
\}     { return '}'; }
\[     { return '['; }
\]     { return ']'; }
@      { return '@'; }
#      { BEGIN(pound); }
<pound>\! { BEGIN(shebang_or_attr); }
<shebang_or_attr>\[ { BEGIN(INITIAL); yyless(0); return SHEBANG; }
<shebang_or_attr>[^\[\n]*\n  { BEGIN(INITIAL); }
<pound>. { BEGIN(INITIAL); yyless(0); return '#'; }

\~     { return '~'; }
::     { return MOD_SEP; }
:      { return ':'; }
\$     { return '$'; }

==    { return EQEQ; }
=>    { return FAT_ARROW; }
=     { return '='; }
\!=   { return NE; }
\!    { return '!'; }
\<=   { return LE; }
\<\<  { return SHL; }
\<\<= { return BINOPEQ; }
\<    { return '<'; }
\>=   { return GE; }
\>\>  { return SHR; }
\>\>= { return BINOPEQ; }
\>    { return '>'; }

\x27                             { BEGIN(ltorchar); yymore(); }
<ltorchar>static                 { BEGIN(INITIAL); return STATIC_LIFETIME; }
<ltorchar>[a-zA-Z_][a-zA-Z0-9_]* { BEGIN(INITIAL); return LIFETIME; }
<ltorchar>\\[nrt\\\x27\x220]\x27 { BEGIN(INITIAL); return LIT_CHAR; }
<ltorchar>\\x[0-9a-fA-F]{2}\x27  { BEGIN(INITIAL); return LIT_CHAR; }
<ltorchar>\\u[0-9a-fA-F]{4}\x27  { BEGIN(INITIAL); return LIT_CHAR; }
<ltorchar>\\U[0-9a-fA-F]{8}\x27  { BEGIN(INITIAL); return LIT_CHAR; }
<ltorchar>.\x27                  { BEGIN(INITIAL); return LIT_CHAR; }
<ltorchar><<EOF>>                { BEGIN(INITIAL); return -1; }

b\x22              { BEGIN(bytestr); yymore(); }
<bytestr>\x22      { BEGIN(INITIAL); return LIT_BINARY; }
<bytestr>(.|\n)    { yymore(); }
<bytestr><<EOF>>   { return -1; }

br\x22                      { BEGIN(rawbytestr_nohash); yymore(); }
<rawbytestr_nohash>\x22     { BEGIN(INITIAL); return LIT_BINARY_RAW; }
<rawbytestr_nohash>(.|\n)   { yymore(); }
<rawbytestr_nohash><<EOF>>  { return -1; }

br/# {
    BEGIN(rawbytestr);
    yymore();
    num_hashes = 0;
    saw_non_hash = 0;
    end_hashes = 0;
}
<rawbytestr># {
    if (!saw_non_hash) {
        num_hashes++;
    } else if (end_hashes != 0) {
        end_hashes++;
        if (end_hashes == num_hashes) {
            BEGIN(INITIAL);
            return LIT_BINARY_RAW;
        }
    }
    yymore();
}
<rawbytestr>\x22# {
    end_hashes = 1;
    if (end_hashes == num_hashes) {
        BEGIN(INITIAL);
        return LIT_BINARY_RAW;
    }
    yymore();
}
<rawbytestr>(.|\n) {
    if (!saw_non_hash) {
        saw_non_hash = 1;
    }
    if (end_hashes != 0) {
        end_hashes = 0;
    }
    yymore();
}
<rawbytestr><<EOF>> { return -1; }

b\x27                        { BEGIN(byte); }
<byte>\\[nrt\\\x27\x220]\x27 { BEGIN(INITIAL); return LIT_BYTE; }
<byte>\\x[0-9a-fA-F]{2}\x27  { BEGIN(INITIAL); return LIT_BYTE; }
<byte>\\u[0-9a-fA-F]{4}\x27  { BEGIN(INITIAL); return LIT_BYTE; }
<byte>\\U[0-9a-fA-F]{8}\x27  { BEGIN(INITIAL); return LIT_BYTE; }
<byte>.\x27                  { BEGIN(INITIAL); return LIT_BYTE; }
<byte><<EOF>>                { BEGIN(INITIAL); return -1; }

r\x22           { BEGIN(rawstr); yymore(); }
<rawstr>\x22    { BEGIN(INITIAL); return LIT_STR_RAW; }
<rawstr>(.|\n)  { yymore(); }
<rawstr><<EOF>> { return -1; }

r/#             {
    BEGIN(rawstr_esc);
    yymore();
    num_hashes = 0;
    saw_non_hash = 0;
    end_hashes = 0;
}

<rawstr_esc># {
    if (!saw_non_hash) {
        num_hashes++;
    } else if (end_hashes != 0) {
        end_hashes++;
        if (end_hashes == num_hashes) {
            BEGIN(INITIAL);
            return LIT_STR_RAW;
        }
    }
    yymore();
}

<rawstr_esc>\x22# {
    end_hashes = 1;
    if (end_hashes == num_hashes) {
        BEGIN(INITIAL);
        return LIT_STR_RAW;
    }
    yymore();
}

<rawstr_esc>(.|\n) {
    if (!saw_non_hash) {
        saw_non_hash = 1;
    }
    if (end_hashes != 0) {
        end_hashes = 0;
    }
    yymore();
}

<rawstr_esc><<EOF>> { return -1; }

\x22                     { BEGIN(str); yymore(); }
<str>\x22                {
    BEGIN(INITIAL);
    // escaped newlines need to be removed.
    for (int i = 0, j = yyleng, k = 0; i < j; i++) {
        if (yytext[i] == '\\') {
            if (yytext[i + 1] == '\n') {
                k = i + 2;
                // with an escaped newline, rustc will ignore all whitespace
                // until the next non-whitespace character
                for (; k < yyleng; k++) {
                    if (!isblank(yytext[k])) {
                        break;
                    }
                }
                memmove(yytext + i, yytext + k, j - i);
                j -= k - i;
            }
        }
    }
    return LIT_STR;
}

<str><<EOF>>               { return -1; }
<str>\\[n\nrt\\\x27\x220]  { yymore(); }
<str>\\x[0-9a-fA-F]{2}     { yymore(); }
<str>\\u[0-9a-fA-F]{4}     { yymore(); }
<str>\U[0-9a-fA-F]{8}      { yymore(); }
<str>\\[^n\nrt\\\x27\x220] { return -1; }
<str>(.|\n)                { yymore(); }

-\>  { return RARROW; }
-    { return '-'; }
-=   { return BINOPEQ; }
&&   { return ANDAND; }
&    { return '&'; }
&=   { return BINOPEQ; }
\|\| { return OROR; }
\|   { return '|'; }
\|=  { return BINOPEQ; }
\+   { return '+'; }
\+=  { return BINOPEQ; }
\*   { return '*'; }
\*=  { return BINOPEQ; }
\/   { return '/'; }
\/=  { return BINOPEQ; }
\^   { return '^'; }
\^=  { return BINOPEQ; }
%    { return '%'; }
%=   { return BINOPEQ; }

<<EOF>> { return 0; }

%%

