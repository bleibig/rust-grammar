#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "tokens.h"

extern char *yytext;

static char *desugar_num(char*, char*);

void print_token(int token) {
  switch (token) {
  case ';': printf("Semi"); break;
  case ',': printf("Comma"); break;
  case '.': printf("Dot"); break;
  case '(': printf("OpenDelim(Paren)"); break;
  case ')': printf("CloseDelim(Paren)"); break;
  case '{': printf("OpenDelim(Brace)"); break;
  case '}': printf("CloseDelim(Brace)"); break;
  case '[': printf("OpenDelim(Bracket)"); break;
  case ']': printf("CloseDelim(Bracket)"); break;
  case '@': printf("At"); break;
  case '#': printf("Pound"); break;
  case '~': printf("Tilde"); break;
  case ':': printf("Colon"); break;
  case '$': printf("Dollar"); break;
  case '?': printf("Question"); break;
  case '=': printf("Eq"); break;
  case '!': printf("Not"); break;
  case '<': printf("Lt"); break;
  case '>': printf("Gt"); break;
  case '-': printf("BinOp(Minus)"); break;
  case '&': printf("BinOp(And)"); break;
  case '|': printf("BinOp(Or)"); break;
  case '+': printf("BinOp(Plus)"); break;
  case '*': printf("BinOp(Star)"); break;
  case '/': printf("BinOp(Slash)"); break;
  case '^': printf("BinOp(Caret)"); break;
  case '%': printf("BinOp(Percent)"); break;
  case SHL: printf("BinOp(Shl)"); break;
  case SHR: printf("BinOp(Shr)"); break;
  case LE: printf("Le"); break;
  case EQEQ: printf("EqEq"); break;
  case NE: printf("Ne"); break;
  case GE: printf("Ge"); break;
  case ANDAND: printf("AndAnd"); break;
  case OROR: printf("OrOr"); break;
  case SHLEQ: printf("BinOpEq(Shl)"); break;
  case SHREQ: printf("BinOpEq(Shr)"); break;
  case MINUSEQ: printf("BinOpEq(Minus)"); break;
  case ANDEQ: printf("BinOpEq(And)"); break;
  case OREQ: printf("BinOpEq(Or)"); break;
  case PLUSEQ: printf("BinOpEq(Plus)"); break;
  case STAREQ: printf("BinOpEq(Star)"); break;
  case SLASHEQ: printf("BinOpEq(Slash)"); break;
  case CARETEQ: printf("BinOpEq(Caret)"); break;
  case PERCENTEQ: printf("BinOpEq(Percent)"); break;
  case DOTDOT: printf("DotDot"); break;
  case DOTDOTDOT: printf("DotDotDot"); break;
  case MOD_SEP: printf("ModSep"); break;
  case LARROW: printf("LArrow"); break;
  case RARROW: printf("RArrow"); break;
  case FAT_ARROW: printf("FatArrow"); break;
  case LIT_BYTE: printf("Byte(%s)", yytext); break;
  case LIT_CHAR: printf("Char(%s)", yytext); break;
  case LIT_INTEGER: printf("Integer(%s)", yytext); break;
  case LIT_FLOAT: printf("Float(%s)", yytext); break;
  case LIT_STR: printf("Str(%s)", yytext); break;
  case LIT_STR_RAW: printf("StrRaw(%s)", yytext); break;
  case IDENT: printf("Ident(%s)", yytext); break;
  case UNDERSCORE: printf("Underscore"); break;
  case LIFETIME: printf("Lifetime(%s)", yytext); break;
  case SELF: printf("Ident(self)"); break;
  case STATIC: printf("Ident(static)"); break;
  case AS: printf("Ident(as)"); break;
  case ABSTRACT: printf("Ident(abstract)"); break;
  case ALIGNOF: printf("Ident(alignof)"); break;
  case BECOME: printf("Ident(become)"); break;
  case BREAK: printf("Ident(break)"); break;
  case CATCH: printf("Ident(catch)"); break;
  case CRATE: printf("Ident(crate)"); break;
  case DEFAULT: printf("Ident(default)"); break;
  case DO: printf("Ident(do)"); break;
  case ELSE: printf("Ident(else)"); break;
  case ENUM: printf("Ident(enum)"); break;
  case EXTERN: printf("Ident(extern)"); break;
  case FALSE: printf("Ident(false)"); break;
  case FINAL: printf("Ident(final)"); break;
  case FN: printf("Ident(fn)"); break;
  case FOR: printf("Ident(for)"); break;
  case IF: printf("Ident(if)"); break;
  case IMPL: printf("Ident(impl)"); break;
  case IN: printf("Ident(in)"); break;
  case LET: printf("Ident(let)"); break;
  case LOOP: printf("Ident(loop)"); break;
  case MATCH: printf("Ident(match)"); break;
  case MACRO: printf("Macro(match)"); break;
  case MOD: printf("Ident(mod)"); break;
  case MOVE: printf("Ident(move)"); break;
  case MUT: printf("Ident(mut)"); break;
  case OFFSETOF: printf("Ident(offsetof)"); break;
  case OVERRIDE: printf("Ident(override)"); break;
  case PRIV: printf("Ident(priv)"); break;
  case PUB: printf("Ident(pub)"); break;
  case PURE: printf("Ident(pure)"); break;
  case REF: printf("Ident(ref)"); break;
  case RETURN: printf("Ident(return)"); break;
  case SIZEOF: printf("Ident(sizeof)"); break;
  case STRUCT: printf("Ident(struct)"); break;
  case SUPER: printf("Ident(super)"); break;
  case UNION: printf("Ident(union)"); break;
  case TRUE: printf("Ident(true)"); break;
  case TRAIT: printf("Ident(trait)"); break;
  case TYPE: printf("Ident(type)"); break;
  case UNSAFE: printf("Ident(unsafe)"); break;
  case UNSIZED: printf("Ident(unsized)"); break;
  case USE: printf("Ident(use)"); break;
  case VIRTUAL: printf("Ident(virtual)"); break;
  case WHILE: printf("Ident(while)"); break;
  case YIELD: printf("Ident(yield)"); break;
  case CONTINUE: printf("Ident(continue)"); break;
  case PROC: printf("Ident(proc)"); break;
  case BOX: printf("Ident(box)"); break;
  case CONST: printf("Ident(const)"); break;
  case WHERE: printf("Ident(where)"); break;
  case TYPEOF: printf("Ident(typeof)"); break;
  case INNER_DOC_COMMENT: printf("DocComment(%s)", yytext); break;
  case OUTER_DOC_COMMENT: printf("DocComment(%s)", yytext); break;
  case SHEBANG: printf("Pound\nNot"); break;
  case SHEBANG_LINE: printf("Shebang(%s)", yytext); break;
  case STATIC_LIFETIME: printf("Lifetime('static)"); break;
  default: printf("can't print token %d", token); abort();
  }
  printf("\n");
}

static int hex_to_num(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    } else {
        printf("error: invalid hex digit '%c'\n", c);
        abort();
    }
}

static char *desugar_num(char *tok, char *default_suffix) {
    int len = strlen(tok);
    int start = 0;
    int end;
    long long int val = 0;
    char *res = malloc(64);

    if (tok[0] == '0') {
        start = 2;
    }

    for (int i = 0, k = 0; i < len; i++) {
        if (tok[i] == '_') {
            k = i + 1;
            memmove(tok + i, tok + k, len - i);
            len -= k - i;
        }
    }

    end = len - 1;

    for (int i = len; i > 0; i--) {
        if (tok[i] == 'i' || tok[i] == 'u') {
            end = i;
            break;
        }
    }

    if (tok[1] == 'b') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            if (tok[i] == '1') {
                val += (long long int) pow(2, accum);
            }
        }
    } else if (tok[1] == 'x') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_' || tok[i] == 'u' || tok[i] == 'i') {
                accum--;
                continue;
            }
            val += hex_to_num(tok[i]) * (long long int) pow(16, accum);
        }
    } else if (tok[1] == 'o') {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            val += (tok[i] - '0') * (long long int) pow(8, accum);
        }
    } else {
        for (int i = end, accum = 0; i >= start; i--, accum++) {
            if (tok[i] == '_') {
                accum--;
                continue;
            }
            val += (tok[i] - '0') * (long long int) pow(10, accum);
        }
    }

    if (default_suffix[0] == 'u') {
        snprintf(res, 64, "%llu%s", val, (end == len -1) ? default_suffix : tok + end);
    } else {
        snprintf(res, 64, "%lld%s", val, (end == len -1) ? default_suffix : tok + end);
    }
    return res;
}
