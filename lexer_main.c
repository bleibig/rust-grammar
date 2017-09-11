#include <stdio.h>
#include "tokens.h"

extern int yylex();
extern int yyget_lineno();
extern void print_token(int);

int main(void) {
  while (1) {
    int token = yylex();
    if (token == 0) {
      break;
    }
    if (token < 0) {
      printf("error on line %d\n", yyget_lineno());
      break;
    }
    print_token(token);
  }
  return 0;
}
