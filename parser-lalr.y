%{
#define YYSTYPE struct node *
struct node;
extern int yylex();
extern void yyerror(char const *s);
extern struct node *mk_node(char const *name, int n, ...);
extern struct node *ext_node(struct node *nd, int n, ...);
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

// Binops & unops, and their precedences
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
%nonassoc '~'

// CONTINUE needs to be lower-precedence than IDENT so that 'continue
// foo' is shifted. Similarly, IDENT needs to be lower than '{' so
// that 'foo {' is shifted when trying to decide if we've got a
// struct-construction expr (esp. in contexts like 'if foo { .')
%nonassoc CONTINUE
%nonassoc IDENT

%nonassoc '('
%nonassoc '{' '}'
%nonassoc ';'

%start rust

%%

rust
: crate                                      { $$ = mk_node("crate", 1, $1); }
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
: mod_item                               { $$ = mk_node("mod-items", 1, $1); }
| mod_items mod_item                     { $$ = ext_node($1, 1, $2); }
;

mod_item
: maybe_outer_attrs item_or_view_item    { $$ = $2; }
;

nonblock_item
: visibility static
;

block_item
: visibility item_fn                     { $$ = $2; }
| visibility item_extern_block
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

item_or_view_item
: visibility item_use                         { $$ = $2; }
| visibility item_fn                          { $$ = $2; }
| visibility item_extern_block                { $$ = $2; }
| visibility item_struct                      { $$ = $2; }
| visibility item_enum                        { $$ = $2; }
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

item_use
: USE path_no_types_allowed ';'                              { $$ = mk_node("use", 0); }
| USE path_no_types_allowed MOD_SEP '{' maybe_idents '}' ';' { $$ = mk_node("use", 0); }
| USE path_no_types_allowed MOD_SEP '*' ';'                  { $$ = mk_node("use", 0); }
| USE IDENT '=' path_no_types_allowed ';'                    { $$ = mk_node("use", 0); }
;

maybe_idents
: idents
| /* empty */
;

idents
: IDENT
| idents ',' IDENT
;

item_fn
: FN IDENT maybe_generic_params fn_decl inner_attrs_and_block  { $$ = mk_node("fn", 1, $5); }
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

maybe_generic_params
: generic_params
| /* empty */
;

generic_params
: '<' lifetimes '>'
| '<' lifetimes ',' ty_params '>'
| '<' ty_params '>'
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

// structs
item_struct
: STRUCT IDENT maybe_generic_params struct_args     { $$ = mk_node("struct", 0); }
;

struct_args
: '{' struct_decl_fields '}'
| '{' struct_decl_fields ',' '}'
| '(' ')' ';'
| '(' struct_tuple_fields ')' ';'
| '(' struct_tuple_fields ',' ')' ';'
| ';'
;

struct_decl_fields
: struct_decl_field
| struct_decl_fields ',' struct_decl_field
| /* empty */
;

struct_decl_field
: maybe_outer_attrs visibility IDENT ':' ty
;

struct_tuple_fields
: struct_tuple_field
| struct_tuple_fields ',' struct_tuple_field
;

struct_tuple_field
: maybe_outer_attrs ty
;

// enums
item_enum
: ENUM IDENT maybe_generic_params '{' enum_defs '}'     { $$ = mk_node("enum", 0); }
| ENUM IDENT maybe_generic_params '{' enum_defs ',' '}' { $$ = mk_node("enum", 0); }
;

enum_defs
: enum_def
| enum_defs ',' enum_def
| /* empty */
;

enum_def
: maybe_outer_attrs visibility IDENT enum_args
;

enum_args
: '{' struct_decl_fields '}'
| '{' struct_decl_fields ',' '}'
| '(' maybe_tys ')'
| '=' expr
| /* empty */
;

///////////////////////////////////////////////////////////////////////
//////////// dynamic part: statements, expressions, values ////////////
///////////////////////////////////////////////////////////////////////

inner_attrs_and_block
: '{' maybe_inner_attrs stmts '}'   { $$ = $3; }
;

block
: '{' stmts '}'                     { $$ = mk_node("block", 1, $2); }
;

// There are two sub-grammars within a "stmts: exprs" derivation
// depending on whether each stmt-expr is a block-expr form; this is to
// handle the "semicolon rule" for stmt sequencing that permits
// writing
//
//     if foo { bar } 10
//
// as a sequence of two stmts (one if-expr stmt, one lit-10-expr
// stmt). Unfortunately by permitting juxtaposition of exprs in
// sequence like that, the non-block expr grammar has to have a
// second limited sub-grammar that excludes the prefix exprs that
// are ambiguous with binops. That is to say:
//
//     {10} - 1
//
// should parse as (progn (progn 10) (- 1)) not (- (progn 10) 1), that
// is to say, two statements rather than one, at least according to
// the mainline rust parser.
//
// So we wind up with a 3-way split in exprs that occur in stmt lists:
// block, nonblock-prefix, and nonblock-nonprefix.
//
// In non-stmts contexts, expr can relax this trichotomy.

stmts
: stmts block_stmt                                            { $$ = ext_node($1, 1, $2); }
| stmts block_stmt nonblock_prefix_stmt                       { $$ = ext_node($1, 2, $2, $3); }
| stmts nonblock_nonprefix_stmt                               { $$ = ext_node($1, 1, $2); }
| stmts nonblock_nonprefix_stmt ';'                           { $$ = ext_node($1, 1, $2); }
| stmts nonblock_nonprefix_stmt ';' nonblock_prefix_stmt      { $$ = ext_node($1, 2, $2, $3); }
| nonblock_prefix_stmt                                        { $$ = mk_node("stmts", 1, $1); }
| /* empty */                                                 { $$ = mk_node("stmts", 0); }
;

nonblock_nonprefix_stmt
: let
| nonblock_item
| nonblock_nonprefix_expr
;

nonblock_prefix_stmt
: nonblock_prefix_expr
;

block_stmt
: block_item
| block_expr
;

maybe_exprs
: exprs
| /* empty */                                                 { $$ = mk_node("(no-expr)", 0); }
;

exprs
: expr                                                        { $$ = mk_node("exprs", 1, $1); }
| exprs ',' expr                                              { $$ = ext_node($1, 1, $2); }
;

nonblock_nonprefix_expr
: lit
| IDENT                                               { $$ = mk_node("ident", 0); }
| IDENT struct_expr                                   { $$ = mk_node("struct", 1, $2); }
| nonblock_nonprefix_expr '=' expr                    { $$ = mk_node("=", 2, $1, $3); }
| nonblock_nonprefix_expr OROR expr                   { $$ = mk_node("||", 2, $1, $3); }
| nonblock_nonprefix_expr ANDAND expr                 { $$ = mk_node("&&", 2, $1, $3); }
| nonblock_nonprefix_expr EQEQ expr                   { $$ = mk_node("==", 2, $1, $3); }
| nonblock_nonprefix_expr NE expr                     { $$ = mk_node("!=", 2, $1, $3); }
| nonblock_nonprefix_expr '<' expr                    { $$ = mk_node("<", 2, $1, $3); }
| nonblock_nonprefix_expr '>' expr                    { $$ = mk_node(">", 2, $1, $3); }
| nonblock_nonprefix_expr LE expr                     { $$ = mk_node("<=", 2, $1, $3); }
| nonblock_nonprefix_expr GE expr                     { $$ = mk_node(">=", 2, $1, $3); }
| nonblock_nonprefix_expr '|' expr                    { $$ = mk_node("|", 2, $1, $3); }
| nonblock_nonprefix_expr '^' expr                    { $$ = mk_node("^", 2, $1, $3); }
| nonblock_nonprefix_expr '&' expr                    { $$ = mk_node("&", 2, $1, $3); }
| nonblock_nonprefix_expr SHL expr                    { $$ = mk_node("<<", 2, $1, $3); }
| nonblock_nonprefix_expr SHR expr                    { $$ = mk_node(">>", 2, $1, $3); }
| nonblock_nonprefix_expr '+' expr                    { $$ = mk_node("+", 2, $1, $3); }
| nonblock_nonprefix_expr '-' expr                    { $$ = mk_node("-", 2, $1, $3); }
| nonblock_nonprefix_expr AS expr                     { $$ = mk_node("as", 2, $1, $3); }
| nonblock_nonprefix_expr '*' expr                    { $$ = mk_node("*", 2, $1, $3); }
| nonblock_nonprefix_expr '/' expr                    { $$ = mk_node("/", 2, $1, $3); }
| nonblock_nonprefix_expr '%' expr                    { $$ = mk_node("%", 2, $1, $3); }
| nonblock_nonprefix_expr '(' maybe_exprs ')'         { $$ = mk_node("call", 2, $1, $3); }
| CONTINUE                                            { $$ = mk_node("continue", 0); }
| CONTINUE IDENT                                      { $$ = mk_node("continue-label", 0); }
;

expr
: lit
| IDENT                            { $$ = mk_node("ident", 0); }
| IDENT struct_expr                { $$ = mk_node("struct", 1, $2); }
| expr '=' expr                    { $$ = mk_node("=", 2, $1, $3); }
| expr OROR expr                   { $$ = mk_node("||", 2, $1, $3); }
| expr ANDAND expr                 { $$ = mk_node("&&", 2, $1, $3); }
| expr EQEQ expr                   { $$ = mk_node("==", 2, $1, $3); }
| expr NE expr                     { $$ = mk_node("!=", 2, $1, $3); }
| expr '<' expr                    { $$ = mk_node("<", 2, $1, $3); }
| expr '>' expr                    { $$ = mk_node(">", 2, $1, $3); }
| expr LE expr                     { $$ = mk_node("<=", 2, $1, $3); }
| expr GE expr                     { $$ = mk_node(">=", 2, $1, $3); }
| expr '|' expr                    { $$ = mk_node("|", 2, $1, $3); }
| expr '^' expr                    { $$ = mk_node("^", 2, $1, $3); }
| expr '&' expr                    { $$ = mk_node("&", 2, $1, $3); }
| expr SHL expr                    { $$ = mk_node("<<", 2, $1, $3); }
| expr SHR expr                    { $$ = mk_node(">>", 2, $1, $3); }
| expr '+' expr                    { $$ = mk_node("+", 2, $1, $3); }
| expr '-' expr                    { $$ = mk_node("-", 2, $1, $3); }
| expr AS expr                     { $$ = mk_node("as", 2, $1, $3); }
| expr '*' expr                    { $$ = mk_node("*", 2, $1, $3); }
| expr '/' expr                    { $$ = mk_node("/", 2, $1, $3); }
| expr '%' expr                    { $$ = mk_node("%", 2, $1, $3); }
| expr '(' maybe_exprs ')'         { $$ = mk_node("call", 2, $1, $3); }
| CONTINUE                         { $$ = mk_node("continue", 0); }
| CONTINUE IDENT                   { $$ = mk_node("continue-label", 0); }
| block_expr                       { $$ = $1 }
;

nonblock_prefix_expr
: '-' expr                         { $$ = mk_node("-", 1, $2); }
| '*' expr                         { $$ = mk_node("*", 1, $2); }
| '~' expr                         { $$ = mk_node("~", 1, $2); }
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

block_expr
: expr_match
| expr_if
| expr_while
| expr_loop
| expr_for
| UNSAFE block                     { $$ = mk_node("unsafe", 1, $1); }
| block
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
: pats_or maybe_guard FAT_ARROW expr
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
: LET pat maybe_ty_ascription maybe_init_expr ';'
;

static
: STATIC pat ':' ty '=' expr ';'

lit
: LIT_CHAR                   { $$ = mk_node("lit-char", 0); }
| LIT_INT                    { $$ = mk_node("lit-int", 0); }
| LIT_UINT                   { $$ = mk_node("lit-uint", 0); }
| LIT_INT_UNSUFFIXED         { $$ = mk_node("lit-int", 0); }
| LIT_FLOAT                  { $$ = mk_node("lit-float", 0); }
| LIT_FLOAT_UNSUFFIXED       { $$ = mk_node("lit-float", 0); }
| LIT_STR                    { $$ = mk_node("lit-str", 0); }
| LIT_STR_RAW                { $$ = mk_node("lit-str", 0); }
| TRUE                       { $$ = mk_node("lit-true", 0); }
| FALSE                      { $$ = mk_node("lit-false", 0); }
;

str
: LIT_STR                    { $$ = mk_node("lit-str", 0); }
| LIT_STR_RAW                { $$ = mk_node("lit-str", 0); }
;
