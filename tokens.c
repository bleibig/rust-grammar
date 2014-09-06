#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "tokens.h"

extern char *yytext;

static char *binop_text(char*);
static char *desugar_num(char*, char*);

void print_token(int token) {
  switch (token) {
  case ';': printf("SEMI"); break;
  case ',': printf("COMMA"); break;
  case '.': printf("DOT"); break;
  case '(': printf("LPAREN"); break;
  case ')': printf("RPAREN"); break;
  case '{': printf("LBRACE"); break;
  case '}': printf("RBRACE"); break;
  case '[': printf("LBRACKET"); break;
  case ']': printf("RBRACKET"); break;
  case '@': printf("AT"); break;
  case '#': printf("POUND"); break;
  case '~': printf("TILDE"); break;
  case ':': printf("COLON"); break;
  case '$': printf("DOLLAR"); break;
  case '=': printf("EQ"); break;
  case '!': printf("NOT"); break;
  case '<': printf("LT"); break;
  case '>': printf("GT"); break;
  case '-': printf("BINOP(MINUS)"); break;
  case '&': printf("BINOP(AND)"); break;
  case '|': printf("BINOP(OR)"); break;
  case '+': printf("BINOP(PLUS)"); break;
  case '*': printf("BINOP(STAR)"); break;
  case '/': printf("BINOP(SLASH)"); break;
  case '^': printf("BINOP(CARET)"); break;
  case '%': printf("BINOP(PERCENT)"); break;
  case SHL: printf("BINOP(SHL)"); break;
  case SHR: printf("BINOP(SHR)"); break;
  case LE: printf("LE"); break;
  case EQEQ: printf("EQEQ"); break;
  case NE: printf("NE"); break;
  case GE: printf("GE"); break;
  case ANDAND: printf("ANDAND"); break;
  case OROR: printf("OROR"); break;
  case BINOPEQ: printf("BINOPEQ(%s)", binop_text(yytext)); break;
  case DOTDOT: printf("DOTDOT"); break;
  case DOTDOTDOT: printf("DOTDOTDOT"); break;
  case MOD_SEP: printf("MOD_SEP"); break;
  case RARROW: printf("RARROW"); break;
  case FAT_ARROW: printf("FAT_ARROW"); break;
  case LIT_BYTE: printf("LIT_BYTE(%s)", yytext); break;
  case LIT_CHAR: printf("LIT_CHAR(%s)", yytext); break;
  case LIT_INTEGER: printf("LIT_INTEGER(%s)", desugar_num(yytext, "")); break;
  case LIT_FLOAT: printf("LIT_FLOAT(%s)", yytext); break;
  case LIT_STR: printf("LIT_STR(%s)", yytext); break;
  case LIT_STR_RAW: printf("LIT_STR_RAW(%s)", yytext); break;
  case LIT_BINARY: printf("LIT_BINARY(%s)", yytext); break;
  case LIT_BINARY_RAW: printf("LIT_BINARY_RAW(%s)", yytext); break;
  case IDENT: printf("IDENT(%s)", yytext); break;
  case UNDERSCORE: printf("UNDERSCORE"); break;
  case LIFETIME: printf("LIFETIME(%s)", yytext); break;
  case SELF: printf("IDENT(self)"); break;
  case STATIC: printf("IDENT(static)"); break;
  case AS: printf("IDENT(as)"); break;
  case BREAK: printf("IDENT(break)"); break;
  case CRATE: printf("IDENT(crate)"); break;
  case ELSE: printf("IDENT(else)"); break;
  case ENUM: printf("IDENT(enum)"); break;
  case EXTERN: printf("IDENT(extern)"); break;
  case FALSE: printf("IDENT(false)"); break;
  case FN: printf("IDENT(fn)"); break;
  case FOR: printf("IDENT(for)"); break;
  case IF: printf("IDENT(if)"); break;
  case IMPL: printf("IDENT(impl)"); break;
  case IN: printf("IDENT(in)"); break;
  case LET: printf("IDENT(let)"); break;
  case LOOP: printf("IDENT(loop)"); break;
  case MATCH: printf("IDENT(match)"); break;
  case MOD: printf("IDENT(mod)"); break;
  case MUT: printf("IDENT(mut)"); break;
  case ONCE: printf("IDENT(once)"); break;
  case PRIV: printf("IDENT(priv)"); break;
  case PUB: printf("IDENT(pub)"); break;
  case REF: printf("IDENT(ref)"); break;
  case RETURN: printf("IDENT(return)"); break;
  case STRUCT: printf("IDENT(struct)"); break;
  case TRUE: printf("IDENT(true)"); break;
  case TRAIT: printf("IDENT(trait)"); break;
  case TYPE: printf("IDENT(type)"); break;
  case UNSAFE: printf("IDENT(unsafe)"); break;
  case USE: printf("IDENT(use)"); break;
  case WHILE: printf("IDENT(while)"); break;
  case CONTINUE: printf("IDENT(continue)"); break;
  case PROC: printf("IDENT(proc)"); break;
  case BOX: printf("IDENT(box)"); break;
  case CONST: printf("IDENT(const)"); break;
  case WHERE: printf("IDENT(where)"); break;
  case TYPEOF: printf("IDENT(typeof)"); break;
  case INNER_DOC_COMMENT: printf("INNER_DOC_COMMENT(%s)", yytext); break;
  case OUTER_DOC_COMMENT: printf("OUTER_DOC_COMMENT(%s)", yytext); break;
  case SHEBANG: printf("POUND\nNOT"); break;
  case STATIC_LIFETIME: printf("LIFETIME('static)"); break;
  default: printf("can't print token %d", token);
  }
  printf("\n");
}

static char *binop_text(char *tok) {
    switch (tok[0]) {
        case '-':
            return "MINUS";
        case '&':
            return "AND";
        case '|':
            return "OR";
        case '+':
            return "PLUS";
        case '*':
            return "STAR";
        case '/':
            return "SLASH";
        case '^':
            return "CARET";
        case '%':
            return "PERCENT";
        case '>':
            return "SHR";
        case '<':
            return "SHL";
        default:
            printf("error: unhandled binopeq token '%s'\n", tok);
            abort();
    }
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
