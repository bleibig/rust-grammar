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

// Binops and their precedence
%left '='
%left OROR
%left ANDAND
%left EQEQ NE
%left '<' '>' LE GE
%left '|'
%left '^'
%left '&'
%left SHL SHR
%left '+' '-'
%left AS
%left '*' '/' '%'

%start rust

%%

rust
: crate                                      { $$ = mk_node("crate", 0); }
;

crate
: maybe_inner_attrs maybe_mod_items          { $$ = $2; }
;

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
| /* empty */
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

maybe_tys
: tys
| /* empty */
;

tys
: ty
| tys ',' ty
;

ty
: path_generic_args_without_colons
| '~' ty
| '*' maybe_mut ty
| '(' maybe_tys ')'
| '&' maybe_lifetime maybe_mut ty
| TYPEOF '(' expr ')'
| ty_bare_fn
| proc_type
| '_'
;

ty_bare_fn
: maybe_extern_abi maybe_unsafe FN ty_fn_decl
;

maybe_unsafe
: UNSAFE
| /* empty */
;

maybe_extern_abi
: EXTERN maybe_abi
| /* empty */
;

maybe_abi
: str
| /* empty */
;

ty_fn_decl
: maybe_generic_params fn_params ret_ty
;

ty_closure
: maybe_unsafe maybe_once maybe_generic_params '|' params '|' ret_ty
| maybe_unsafe maybe_once maybe_generic_params OROR ret_ty
;

maybe_once
: ONCE
| /* empty */
;

proc_type
: PROC maybe_generic_params fn_params maybe_bounds ret_ty
;

maybe_mut
: MUT
| /* empty */
;

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
| expr '=' expr                    { $$ = mk_node("=", 2, $1, $2); }
| expr OROR expr                   { $$ = mk_node("||", 2, $1, $2); }
| expr ANDAND expr                 { $$ = mk_node("&&", 2, $1, $2); }
| expr EQEQ expr                   { $$ = mk_node("==", 2, $1, $2); }
| expr NE expr                     { $$ = mk_node("!=", 2, $1, $2); }
| expr '<' expr                    { $$ = mk_node("<", 2, $1, $2); }
| expr '>' expr                    { $$ = mk_node(">", 2, $1, $2); }
| expr LE expr                     { $$ = mk_node("<=", 2, $1, $2); }
| expr GE expr                     { $$ = mk_node(">=", 2, $1, $2); }
| expr '|' expr                    { $$ = mk_node("|", 2, $1, $2); }
| expr '^' expr                    { $$ = mk_node("^", 2, $1, $2); }
| expr '&' expr                    { $$ = mk_node("&", 2, $1, $2); }
| expr SHL expr                    { $$ = mk_node("<<", 2, $1, $2); }
| expr SHR expr                    { $$ = mk_node(">>", 2, $1, $2); }
| expr '+' expr                    { $$ = mk_node("+", 2, $1, $2); }
| expr '-' expr                    { $$ = mk_node("-", 2, $1, $2); }
| expr AS expr                     { $$ = mk_node("as", 2, $1, $2); }
| expr '*' expr                    { $$ = mk_node("*", 2, $1, $2); }
| expr '/' expr                    { $$ = mk_node("/", 2, $1, $2); }
| expr '%' expr                    { $$ = mk_node("%", 2, $1, $2); }
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
: visibility item_fn
| visibility item_extern_block
;

item_extern_block
: EXTERN CRATE item_extern_crate
| EXTERN maybe_abi item_fn
| EXTERN maybe_abi '{' item_foreign_mod '}'
;

item_extern_crate
: IDENT ';'
| IDENT '=' str ';'
;

item_foreign_mod
: maybe_inner_attrs maybe_foreign_items
;

maybe_foreign_items
: foreign_items
| /* empty */
;

foreign_items
: foreign_item
| foreign_items foreign_item
;

foreign_item
: visibility STATIC item_foreign_static
| visibility FN item_foreign_fn
| visibility UNSAFE item_foreign_fn
;

item_foreign_static
: maybe_mut IDENT ':' ty ';'
;

item_foreign_fn
: FN IDENT maybe_generic_params fn_decl_allow_variadic ';'
;

fn_decl_allow_variadic
: fn_params_allow_variadic ret_ty
;

fn_params_allow_variadic
: '(' param fn_params_allow_variadic_tail ')'
| '(' ')'
;

fn_params_allow_variadic_tail
: ',' DOTDOTDOT
| ',' param fn_params_allow_variadic_tail
;

visibility
: PUB
| /* empty */
;

item_fn
: FN IDENT maybe_generic_params fn_decl inner_attrs_and_block  { $$ = mk_node("fn", 1, $3); }
;

fn_decl
: fn_params ret_ty
;

fn_params
: '(' maybe_params ')'
;

maybe_params
: params
| /* empty */
;

params
: param
| params ',' param
;

param
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

// A path with no type parameters; e.g. `foo::bar::Baz`
path_no_types_allowed
: IDENT
| path_no_types_allowed MOD_SEP IDENT
;

// A path with a lifetime and type parameters, with no double colons
// before the type parameters; e.g. `foo::bar<'a>::Baz<T>`
path_generic_args_without_colons
: IDENT maybe_generic_args
| path_generic_args_without_colons MOD_SEP IDENT maybe_generic_args
;

// A path with a lifetime and type parameters with double colons before
// the type parameters; e.g. `foo::bar::<'a>::Baz::<T>`
path_generic_args_with_colons
: IDENT
| maybe_generic_args
| path_generic_args_with_colons MOD_SEP IDENT
| path_generic_args_with_colons MOD_SEP maybe_generic_args
;

// A path with a lifetime and type parameters with bounds before the last
// set of type parameters only; e.g. `foo::bar<'a>::Baz:X+Y<T>` This
// form does not use extra double colons.
path_generic_args_and_bounds
: IDENT maybe_bounds maybe_generic_args
| path_generic_args_and_bounds MOD_SEP IDENT maybe_bounds maybe_generic_args
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

maybe_lifetime
: LIFETIME
| /* empty */
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
: path_generic_args_without_colons
;

str
: LIT_STR
| LIT_STR_RAW
;
