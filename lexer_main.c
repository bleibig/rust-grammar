#include <stdio.h>
#include "tokens.h"

extern int yylex();

int main(void) {
  while (1) {
    int token = yylex();
    if (token == 0) {
      break;
    }
    if (token < 0) {
      printf("error\n");
      break;
    }
    print_token(token);
  }
  return 0;
}
