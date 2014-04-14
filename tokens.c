#include <stdio.h>
#include "tokens.h"

extern char *yytext;

void print_token(enum Token token) {
  switch (token) {
  case ';':
  case ',':
  case '.':
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case '@':
  case '#':
  case '~':
  case ':':
  case '$':
  case '=':
  case '!':
  case '<':
  case '>':
  case '-':
  case '&':
  case '|':
  case '+':
  case '*':
  case '/':
  case '^':
  case '%':  printf("%c", token); break;
  case SHL: printf("SHL"); break;
  case LE: printf("LE"); break;
  case EQEQ: printf("EQEQ"); break;
  case NE: printf("NE"); break;
  case GE: printf("GE"); break;
  case ANDAND: printf("ANDAND"); break;
  case OROR: printf("OROR"); break;
  case BINOPEQ: printf("BINOPEQ(%s)", yytext); break;
  case DOTDOT: printf("DOTDOT"); break;
  case DOTDOTDOT: printf("DOTDOTDOT"); break;
  case MOD_SEP: printf("SEP"); break;
  case RARROW: printf("RARROW"); break;
  case FAT_ARROW: printf("ARROW"); break;
  case LIT_CHAR: printf("LIT_CHAR(%s)", yytext); break;
  case LIT_INT: printf("LIT_INT(%s)", yytext); break;
  case LIT_UINT: printf("LIT_UINT(%s)", yytext); break;
  case LIT_INT_UNSUFFIXED: printf("LIT_UNSUFFIXED(%s)", yytext); break;
  case LIT_FLOAT: printf("LIT_FLOAT(%s)", yytext); break;
  case LIT_FLOAT_UNSUFFIXED: printf("LIT_FLOAT_UNSUFFIXED(%s)", yytext); break;
  case LIT_STR: printf("LIT_STR(%s)", yytext); break;
  case IDENT: printf("IDENT(%s)", yytext); break;
  case UNDERSCORE: printf("UNDERSCORE"); break;
  case LIFETIME: printf("LIFETIME(%s)", yytext); break;
  case SELF: printf("SELF"); break;
  case STATIC: printf("STATIC"); break;
  case AS: printf("AS"); break;
  case BREAK: printf("BREAK"); break;
  case CRATE: printf("CRATE"); break;
  case ELSE: printf("ELSE"); break;
  case ENUM: printf("ENUM"); break;
  case EXTERN: printf("EXTERN"); break;
  case FALSE: printf("FALSE"); break;
  case FN: printf("FN"); break;
  case FOR: printf("FOR"); break;
  case IF: printf("IF"); break;
  case IMPL: printf("IMPL"); break;
  case IN: printf("IN"); break;
  case LET: printf("LET"); break;
  case LOOP: printf("LOOP"); break;
  case MATCH: printf("MATCH"); break;
  case MOD: printf("MOD"); break;
  case MUT: printf("MUT"); break;
  case ONCE: printf("ONCE"); break;
  case PRIV: printf("PRIV"); break;
  case PUB: printf("PUB"); break;
  case REF: printf("REF"); break;
  case RETURN: printf("RETURN"); break;
  case STRUCT: printf("STRUCT"); break;
  case SUPER: printf("SUPER"); break;
  case TRUE: printf("TRUE"); break;
  case TRAIT: printf("TRAIT"); break;
  case TYPE: printf("TYPE"); break;
  case UNSAFE: printf("UNSAFE"); break;
  case USE: printf("USE"); break;
  case WHILE: printf("WHILE"); break;
  case CONTINUE: printf("CONTINUE"); break;
  case PROC: printf("PROC"); break;
  case BOX: printf("BOX"); break;
  case TYPEOF: printf("TYPEOF"); break;
  case SHEBANG: printf("SHEBANG"); break;
  case STATIC_LIFETIME: printf("STATIC_LIFETIME"); break;
  default: printf("can't print token %d", token);
  }
  printf("\n");
}
