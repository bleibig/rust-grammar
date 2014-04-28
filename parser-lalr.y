%{
#define YYSTYPE struct node *
struct node;
extern int yylex();
extern void yyerror(char const *s);
extern struct node *mk_node(char const *name, int n, ...);
%}
%debug

%token SHL
%token SHR
%token LE
%token EQEQ
%token NE
%token GE
%token ANDAND
%token OROR
%token BINOPEQ
%token DOTDOT
%token DOTDOTDOT
%token MOD_SEP
%token RARROW
%token FAT_ARROW
%token LIT_CHAR
%token LIT_INT
%token LIT_UINT
%token LIT_INT_UNSUFFIXED
%token LIT_FLOAT
%token LIT_FLOAT_UNSUFFIXED
%token LIT_STR
%token LIT_STR_RAW
%token IDENT
%token UNDERSCORE
%token LIFETIME

// keywords
%token SELF
%token STATIC
%token SUPER
%token AS
%token BREAK
%token CRATE
%token ELSE
%token ENUM
%token EXTERN
%token FALSE
%token FN
%token FOR
%token IF
%token IMPL
%token IN
%token LET
%token LOOP
%token MATCH
%token MOD
%token MUT
%token ONCE
%token PRIV
%token PUB
%token REF
%token RETURN
%token STRUCT
%token TRUE
%token TRAIT
%token TYPE
%token UNSAFE
%token USE
%token WHILE
%token CONTINUE
%token PROC
%token BOX
%token TYPEOF

%token SHEBANG
%token STATIC_LIFETIME

 /*
   Quoting from the Bison manual:

   "Finally, the resolution of conflicts works by comparing the precedence
   of the rule being considered with that of the lookahead token. If the
   token's precedence is higher, the choice is to shift. If the rule's
   precedence is higher, the choice is to reduce. If they have equal
   precedence, the choice is made based on the associativity of that
   precedence level. The verbose output file made by ‘-v’ (see Invoking
   Bison) says how each conflict was resolved"
 */

// We expect no shift/reduce or reduce/reduce conflicts in this grammar;
// all potential ambiguities are scrutinized and eliminated manually.
%expect 0

// CONTINUE needs to be lower-precedence than IDENT so that 'continue
// foo' is shifted. Similarly, IDENT needs to be lower than '{' so
// that 'foo {' is shifted when trying to decide if we've got a
// struct-construction expr (esp. in contexts like 'if foo { .')

%nonassoc CONTINUE
%nonassoc IDENT
%nonassoc '('
%nonassoc '{'
%left '+' '-'

%start rust

%%

rust
: crate                                      { $$ = mk_node("crate", 0); }
;

// 5. Crates and source
crate
: maybe_inner_attrs maybe_mod_items          { $$ = $2; }
;

/// 6.3 Attributes
maybe_inner_attrs
: inner_attrs
| /* empty */
;

inner_attrs
: inner_attr
| inner_attrs inner_attr
;

inner_attr
: SHEBANG '[' meta_item ']'
;

maybe_outer_attrs
: outer_attrs
| /* empty */
;

outer_attrs
: outer_attr
| outer_attrs outer_attr
;

outer_attr
: '#' '[' meta_item ']'
;

meta_item
: IDENT
| IDENT '=' lit
| IDENT '(' meta_seq ')'
;

meta_seq
: meta_item
| meta_seq ',' meta_item
;

maybe_mod_items
: mod_items
| /* empty */
;

mod_items
: mod_item
| mod_items mod_item
;

mod_item
: maybe_outer_attrs item_or_view_item    { $$ = $2; }
;

lit
: LIT_CHAR
| LIT_INT
| LIT_UINT
| LIT_INT_UNSUFFIXED
| LIT_FLOAT
| LIT_FLOAT_UNSUFFIXED
| LIT_STR
| LIT_STR_RAW
| TRUE
| FALSE
;

stmt
: let
| item_or_view_item
| expr_stmt
| expr
;

// Things that can be an expr or a stmt, no semi required.
expr_stmt
: expr_match
| expr_if
| expr_while
| expr_loop
| expr_for
;

expr_match
: MATCH expr '{' match_clauses '}'
| MATCH expr '{' match_clauses ',' '}'
;

match_clauses
: match_clause
| match_clauses ',' match_clause
;

match_clause
: pats_or maybe_guard FAT_ARROW match_body
;

match_body
: expr
| expr_stmt
;

maybe_guard
: IF expr
| // empty
;

expr_if
: IF expr block
| IF expr block ELSE block_or_if
;

block_or_if
: block
| expr_if
;

expr_while
: WHILE expr block
;

expr_loop
: LOOP block
;

expr_for
: FOR expr IN expr block
;

let
: LET pat maybe_ty_ascription maybe_init_expr
;

maybe_ty_ascription
: ':' ty
| /* empty */
;

maybe_init_expr
: '=' expr
| /* empty */
;

pats_or
: pat
| pats_or '|' pat
;

pat
: IDENT
;

tys
: ty
| tys ',' ty
;

ty
: path
| '~' ty
| '*' maybe_mut ty
| '(' tys ')'
;

maybe_mut
: MUT
| /* empty */
;

path:

maybe_exprs
: exprs
| /* empty */
;

exprs
: expr
| exprs ',' expr
;

expr
: lit
| IDENT                            { $$ = mk_node("ident", 0); }
| IDENT struct_expr                { $$ = mk_node("struct", 1, $1); }
| expr '+' expr                    { $$ = mk_node("+", 2, $1, $2); }
| expr '(' maybe_exprs ')'         { $$ = mk_node("call", 1, $1); }
| CONTINUE                         { $$ = mk_node("continue", 0); }
| CONTINUE IDENT                   { $$ = mk_node("continue-label", 0); }
| UNSAFE block                     { $$ = mk_node("unsafe-block", 0); }
| block                            { $$ = mk_node("block", 0); }
;

struct_expr
: '{' field_inits default_field_init '}'
;

field_inits
: field_init
| field_inits ',' field_init
;

field_init
: maybe_mut IDENT ':' expr
;

default_field_init
: ','
| ',' DOTDOT expr
| /* empty */
;

item_or_view_item
: item_fn
;

item_fn
: FN IDENT maybe_generic_params fn_decl inner_attrs_and_block  { $$ = mk_node("fn", 1, $3); }
;

fn_decl
: fn_args ret_ty
;

fn_args
: '(' maybe_args_general ')'
;

maybe_args_general
: args_general
| /* empty */
;

args_general
: arg_general
| args_general ',' arg_general
;

arg_general
: pat ':' ty
;

ret_ty
: RARROW '!'
| RARROW ty
| /* empty */
;

inner_attrs_and_block
: '{' maybe_inner_attrs maybe_stmts '}'   { $$ = $2; }
;

block
: '{' maybe_stmts '}'
;

maybe_stmts
: stmts
| /* empty */
;

stmts
: stmts stmt
| stmt
;

maybe_generic_params
: generic_params
| /* empty */
;

generic_params
: '<' maybe_lifetimes maybe_ty_params '>'
;

maybe_ty_params
: ty_params
| /* empty */
;

ty_params
: ty_param
| ty_params ',' ty_param
;


// syntax::parse::parser::{PathParsingMode, parse_path}
path_no_types_allowed                  : IDENT mod_sep_idents ;
path_lifetime_and_types_without_colons : IDENT mod_sep_ident_generic_args ;
path_lifetime_and_types_with_colons    : IDENT mod_sep_ident_or_generic_args ;
path_lifetime_and_types_and_bounds     : IDENT mod_sep_ident_bounds_generic_args ;

mod_sep_idents
: MOD_SEP IDENT
| mod_sep_idents MOD_SEP IDENT
;

mod_sep_ident_generic_args
: MOD_SEP IDENT maybe_generic_args
| mod_sep_ident_generic_args MOD_SEP IDENT maybe_generic_args
;

mod_sep_ident_or_generic_args
: MOD_SEP IDENT
| MOD_SEP generic_args
| mod_sep_ident_or_generic_args MOD_SEP IDENT
| mod_sep_ident_or_generic_args MOD_SEP generic_args
;

mod_sep_ident_bounds_generic_args
: MOD_SEP IDENT maybe_bounds maybe_generic_args
| mod_sep_ident_bounds_generic_args MOD_SEP IDENT maybe_bounds maybe_generic_args
;

maybe_generic_args
: generic_args
| /* empty */
;

generic_args
: '<' lifetimes_or_tys '>'
;

ty_param
: maybe_unsized IDENT maybe_bounds maybe_ty_default
;

maybe_unsized
: unsized
| /* empty */
;

unsized
: TYPE
;

maybe_bounds
: ':' bounds
| /* empty */
;

bounds
: bound
| bounds '+' bound
;

bound
: STATIC_LIFETIME
| trait_ref
;

maybe_ty_default
: '=' ty
| /* empty */
;

lifetimes_or_tys
: lifetime_or_ty
| lifetimes_or_tys ',' lifetime_or_ty
;

lifetime_or_ty
: LIFETIME
| ty
;

maybe_lifetimes
: lifetimes
| /* empty */
;

lifetimes
: LIFETIME
| lifetimes ',' LIFETIME
;


trait_ref
: mod_sep_ident_generic_args
;
%%
