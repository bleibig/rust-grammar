#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

extern int yylex();
extern int rsparse();

static char pushback = '\0';
int rslex() {
  if (pushback == '\0') {
    return yylex();
  } else {
    char c = pushback;
    pushback = '\0';
    return c;
  }
}

void push_back(char c) {
  pushback = c;
}

extern int rsdebug;

struct node {
  struct node *next;
  struct node *prev;
  int own_string;
  char const *name;
  int n_elems;
  struct node *elems[];
};

struct node *nodes = NULL;
int n_nodes;

struct node *mk_node(char const *name, int n, ...) {
  va_list ap;
  int i = 0;
  unsigned sz = sizeof(struct node) + (n * sizeof(struct node *));
  struct node *nn, *nd = (struct node *)malloc(sz);

  printf("# New %d-ary node: %s = %p\n", n, name, nd);

  nd->own_string = 0;
  nd->prev = NULL;
  nd->next = nodes;
  if (nodes) {
    nodes->prev = nd;
  }
  nodes = nd;

  nd->name = name;
  nd->n_elems = n;

  va_start(ap, n);
  while (i < n) {
    nn = va_arg(ap, struct node *);
    printf("#   arg[%d]: %p\n", i, nn);
    printf("#            (%s ...)\n", nn->name);
    nd->elems[i++] = nn;
  }
  va_end(ap);
  n_nodes++;
  return nd;
}

struct node *mk_atom(char *name) {
  struct node *nd = mk_node((char const *)strdup(name), 0);
  nd->own_string = 1;
  return nd;
}

struct node *mk_none() {
  return mk_atom("<none>");
}

struct node *ext_node(struct node *nd, int n, ...) {
  va_list ap;
  int i = 0, c = nd->n_elems + n;
  unsigned sz = sizeof(struct node) + (c * sizeof(struct node *));
  struct node *nn;

  printf("# Extending %d-ary node by %d nodes: %s = %p",
	 nd->n_elems, c, nd->name, nd);

  if (nd->next) {
    nd->next->prev = nd->prev;
  }
  if (nd->prev) {
    nd->prev->next = nd->next;
  }
  nd = realloc(nd, sz);
  nd->prev = NULL;
  nd->next = nodes;
  nodes->prev = nd;
  nodes = nd;

  printf(" ==> %p\n", nd);

  va_start(ap, n);
  while (i < n) {
    nn = va_arg(ap, struct node *);
    printf("#   arg[%d]: %p\n", i, nn);
    printf("#            (%s ...)\n", nn->name);
    nd->elems[nd->n_elems++] = nn;
    ++i;
  }
  va_end(ap);
  return nd;
}

int const indent_step = 4;

void print_indent(int depth) {
  while (depth) {
    if (depth-- % indent_step == 0) {
      printf("|");
    } else {
      printf(" ");
    }
  }
}

void print_node(struct node *n, int depth) {
  int i = 0;
  print_indent(depth);
  if (n->n_elems == 0) {
    printf("%s\n", n->name);
  } else {
    printf("(%s\n", n->name);
    for (i = 0; i < n->n_elems; ++i) {
      print_node(n->elems[i], depth + indent_step);
    }
    print_indent(depth);
    printf(")\n");
  }
}

int main() {
  int ret = 0;
  struct node *tmp;
  /* rsdebug = 1; */
  ret = rsparse();
  printf("--- PARSE COMPLETE: ret:%d, n_nodes:%d ---\n", ret, n_nodes);
  if (nodes) {
    print_node(nodes, 0);
  }
  while (nodes) {
    tmp = nodes;
    nodes = tmp->next;
    if (tmp->own_string) {
      free((void*)tmp->name);
    }
    free(tmp);
  }
  return ret;
}

void rserror(char const *s) {
  fprintf (stderr, "%s\n", s);
}

