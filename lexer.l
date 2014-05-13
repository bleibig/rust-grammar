%{

#include <stdio.h>

  // # Notes
  // This is lacking the folowing:
  // * Raw strings (they're not capable of being parsed by a
  //   regular-expression, though flex may allow some way around that)
  // * Doc comments (they don't have much of a significance to the
  //   grammar and can just be treated as regular comments)
  //
  // This adds the following tokens not specified in the rust lexer:
  // * SHEBANG "#!" - this is neccesary to avoid first/first conficts
  //   with attribute parsing

%}

%x str
%x ltorchar
%x linecomment
%x blockcomment

isuf (i8|i16|i32|i64|i)
usuf (u6|u16|u32|u64|u)

%%

[ \n\t\r]             { }
\/\/                  { BEGIN(linecomment); }
<linecomment>\n       { BEGIN(INITIAL); }
<linecomment>[^\n]*   { }
\/\*                  { BEGIN(blockcomment); }
<blockcomment>[^*]*   { }
<blockcomment>\*+[^/] { }
<blockcomment>\*\/    { BEGIN(INITIAL); }

_        { return UNDERSCORE; }
as       { return AS; }
box      { return BOX; }
break    { return BREAK; }
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
struct   { return STRUCT; }
trait    { return TRAIT; }
true     { return TRUE; }
type     { return TYPE; }
typeof   { return TYPEOF; }
unsafe   { return UNSAFE; }
use      { return USE; }
while    { return WHILE; }

[a-zA-Z_][a-zA-Z0-9_]*      { return IDENT; }

0x[0-9a-fA-F_]+{isuf}                              { return LIT_INT; }
0x[0-9a-fA-F_]+{usuf}                              { return LIT_UINT; }
0o[0-8_]+{isuf}                                    { return LIT_INT; }
0o[0-8_]+{usuf}                                    { return LIT_UINT; }
0b[01_]+{isuf}                                     { return LIT_INT; }
0b[01_]+{usuf}                                     { return LIT_UINT; }
[0-9][0-9_]*{isuf}                                 { return LIT_INT; }
[0-9][0-9_]*{usuf}                                 { return LIT_UINT; }

[0-9][0-9_]*\.[0-9]*([eE][-\+]?[0-9]+)?            { return LIT_FLOAT_UNSUFFIXED; }
[0-9][0-9_]*\.[0-9]*([eE][-\+]?[0-9]+)?(f32|f64)   { return LIT_FLOAT; }
[0-9][0-9_]*(\.[0-9]*)?[eE][-\+]?[0-9]+            { return LIT_FLOAT_UNSUFFIXED; }
[0-9][0-9_]*(\.[0-9]*)?[eE][-\+]?[0-9]+(f32|f64)   { return LIT_FLOAT; }

0x[0-9a-fA-F_]+ { return LIT_INT; }
0o[0-8_]+       { return LIT_INT; }
0b[01_]+        { return LIT_INT; }
[0-9][0-9_]*    { return LIT_INT; }

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
#!     { return SHEBANG; }
#      { return '#'; }
~      { return '~'; }
::     { return MOD_SEP; }
:      { return ':'; }
\$     { return '$'; }

==    { return EQEQ; }
=>    { return FAT_ARROW; }
=     { return '='; }
!=    { return NE; }
!     { return '!'; }
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

\x22                     { BEGIN(str); yymore(); }
<str>\x22                { BEGIN(INITIAL); return LIT_STR; }
<str><<EOF>>             { return -1; }
<str>\\[nrt\\\x27\x220]  { yymore(); }
<str>\\x[0-9a-fA-F]{2}   { yymore(); }
<str>\\u[0-9a-fA-F]{4}   { yymore(); }
<str>\U[0-9a-fA-F]{8}    { yymore(); }
<str>\\[^nrt\\\x27\x220] { return -1; }
<str>.                   { yymore(); }

-\>  { return RARROW; }
-    { return '-'; }
-=   { return BINOPEQ; };
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

