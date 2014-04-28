#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#define YYSTYPE struct node *

int yylex();
int yyparse();
extern int yydebug;

struct node {
  struct node *next;
  char const *name;
  int n_elems;
  struct node *elems[];
};

struct node *nodes = NULL;
int n_nodes;

struct node *mk_node(char const *name, int n, ...) {
  va_list ap;
  int i = 0;
  va_start(ap, n);
  printf("making node: %s\n", name);
  struct node *nd = (struct node *)malloc(sizeof(struct node)
					  + n * sizeof(struct node *));
  nd->next = nodes;
  nodes = nd;
  nd->name = name;
  nd->n_elems = n;
  while (i < n) {
    nd->elems[i++] = va_arg(ap, struct node *);
  }
  va_end(ap);
  n_nodes++;
  return nd;
}

void print_indent(int depth) {
  while (depth--) {
    printf(" ");
  }
}

void print_node(struct node *n, int depth) {
  int i = 0;
  print_indent(depth);
  printf("(%s\n", n->name);
  for (i = 0; i < n->n_elems; ++i) {
    print_node(n->elems[i], depth + 4);
  }
  print_indent(depth);
  printf(")\n");
}

int main() {
  int ret = 0;
  struct node *tmp;
  /* yydebug = 1; */
  ret = yyparse();
  printf("--- PARSE COMPLETE: ret:%d, n_nodes:%d ---\n", ret, n_nodes);
  if (nodes) {
    print_node(nodes, 0);
  }
  while (nodes) {
    tmp = nodes;
    nodes = tmp->next;
    free(tmp);
  }
}

void yyerror(char const *s) {
  fprintf (stderr, "%s\n", s);
}

