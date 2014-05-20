%{
#define YYSTYPE struct node *
struct node;
extern int yylex();
extern void yyerror(char const *s);
extern struct node *mk_node(char const *name, int n, ...);
extern struct node *mk_atom(char *text);
extern struct node *ext_node(struct node *nd, int n, ...);
extern char *yytext;
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
%token DOC_COMMENT

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

// IDENT needs to be lower than '{' so that 'foo {' is shifted when
// trying to decide if we've got a struct-construction expr (esp. in
// contexts like 'if foo { .')
//
// IDENT also needs to be lower precedence than '<' so that '<' in
// 'foo:bar . <' is shifted (in a trait reference occurring in a
// bounds list), parsing as foo:(bar<baz>) rather than (foo:bar)<baz>.
%precedence IDENT

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

// RETURN needs to be lower-precedence than all the block-expr
// starting keywords, so that juxtapositioning them in a stmts
// like 'return if foo { 10 } else { 22 }' shifts 'if' rather
// than reducing a no-argument return.
%precedence RETURN
%precedence FOR IF LOOP MATCH UNSAFE WHILE

%precedence '{' '[' '(' '.'

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
| %empty
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
| %empty
;

outer_attrs
: outer_attr
| outer_attrs outer_attr
;

outer_attr
: '#' '[' meta_item ']'
;

meta_item
: ident
| ident '=' lit
| ident '(' meta_seq ')'
;

meta_seq
: meta_item
| meta_seq ',' meta_item
;

maybe_mod_items
: mod_items
| %empty
;

mod_items
: mod_item                               { $$ = mk_node("mod-items", 1, $1); }
| mod_items mod_item                     { $$ = ext_node($1, 1, $2); }
;

attrs_and_vis
: maybe_outer_attrs visibility
;

mod_item
: attrs_and_vis item_or_view_item    { $$ = $2; }
;

block_item
: item_fn
| item_extern_block
;

maybe_ty_ascription
: ':' ty
| %empty
;

maybe_init_expr
: '=' expr
| %empty
;

pats_or
: pat
| pats_or '|' pat
;

pat
: '_'
| '&' pat
| '(' pat_tup ')'
| '[' pat_vec ']'
| ident '@' pat
| lit_or_path
| lit_or_path DOTDOT lit_or_path
| path_generic_args_with_colons '{' pat_struct '}'
| path_generic_args_with_colons '(' DOTDOT ')'
| path_generic_args_with_colons '(' pat_tup ')'
| REF ident
| MUT ident
| BOX pat
;

lit_or_path
: path_generic_args_with_colons
| lit
;

pat_field_name
: MUT IDENT
| REF IDENT
| IDENT
;

pat_field
: pat_field_name
| pat_field_name ':' pat
;

pat_fields
: pat_field
| pat_fields ',' pat_field
;

pat_struct
: pat_fields
| pat_fields ',' DOTDOT
| DOTDOT
;

pat_tup
: pat
| pat_tup ',' pat
;

pat_vec
: pat_vec_elt
| pat_vec ',' pat_vec_elt
;

pat_vec_elt
: pat
| DOTDOT ident
;

maybe_tys
: tys
| %empty
;

tys
: ty
| tys ',' ty
;

ty
: ty_prim
| ty_closure
| '(' maybe_tys ')'
| '_'
;

ty_prim
: path_generic_args_and_bounds
| BOX ty
| '*' maybe_mut ty
| '&' maybe_lifetime maybe_mut ty
| TYPEOF '(' expr ')'
| ty_bare_fn
| ty_proc
;

ty_bare_fn
:                         FN ty_fn_decl
|                  UNSAFE FN ty_fn_decl
| EXTERN maybe_abi        FN ty_fn_decl
| EXTERN maybe_abi UNSAFE FN ty_fn_decl
;

ty_fn_decl
: maybe_generic_params fn_params ret_ty
;

ty_closure
: UNSAFE maybe_once maybe_generic_params '|' params '|' ret_ty
|        maybe_once maybe_generic_params '|' params '|' ret_ty
| UNSAFE maybe_once maybe_generic_params OROR ret_ty
|        maybe_once maybe_generic_params OROR ret_ty
;

maybe_once
: ONCE
| %empty
;

ty_proc
: PROC maybe_generic_params fn_params maybe_bounds ret_ty
;

maybe_mut
: MUT
| %empty
;

item_or_view_item
: item_fn
| item_extern_block
| item_struct
| item_enum
| item_type
| item_trait
| item_impl
| view_item
;

view_item
: USE view_paths ';'                          { $$ = mk_node("use", 1, $2); }
;

view_paths
: view_path                                   { $$ = mk_node("view-paths", 0); }
| view_paths ',' view_path                    { $$ = ext_node($1, 1, $3); }
;

view_path
: path_no_types_allowed                       { $$ = mk_node("use-one", 1, $1); }
| path_no_types_allowed '{' idents '}'        { $$ = mk_node("use-multi", 2, $1, $3); }
| path_no_types_allowed MOD_SEP '*' ';'       { $$ = mk_node("use-star", 1, $1); }
| ident '=' path_no_types_allowed ';'         { $$ = mk_node("use-ident", 1, $1, $3); }

;

item_extern_block
: EXTERN CRATE item_extern_crate
| EXTERN maybe_abi item_fn
| EXTERN maybe_abi '{' item_foreign_mod '}'
;

maybe_abi
: str
| %empty
;

item_extern_crate
: ident ';'
| ident '=' str ';'
;

item_foreign_mod
: maybe_inner_attrs maybe_foreign_items
;

maybe_foreign_items
: foreign_items
| %empty
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
: maybe_mut ident ':' ty ';'
;

item_foreign_fn
: FN ident maybe_generic_params fn_decl_allow_variadic ';'
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
| %empty
;

idents
: ident
| idents ',' ident
;

item_type
: TYPE ident maybe_generic_params '=' ty ';'
;

item_trait
: TRAIT ident maybe_generic_params maybe_supertraits '{' maybe_trait_methods '}'
;

maybe_supertraits
: ':' supertraits
| %empty
;

supertraits
: trait_ref
| supertraits '+' trait_ref
;

maybe_trait_methods
: trait_methods
| %empty
;

trait_methods
: trait_method
| trait_methods trait_method
;

maybe_unsafe
: UNSAFE
| %empty
;

trait_method
: attrs_and_vis maybe_unsafe FN ident maybe_generic_params fn_decl ';'
| attrs_and_vis maybe_unsafe FN ident maybe_generic_params fn_decl inner_attrs_and_block
;

// There are two forms of impl:
//
// impl (<...>)? TY { ... }
// impl (<...>)? TRAIT for TY { ... }
//
// Unfortunately since TY can begin with '<' itself -- as part of a
// closure type -- there's an s/r conflict when we see '<' after IMPL:
// should we reduce one of the early rules of TY (such as maybe_once)
// or shall we continue shifting into the generic_params list for the
// impl?
//
// The production parser disambiguates a different case here by
// permitting / requiring the user to provide parens around types when
// they are ambiguous with traits. We do the same here, regrettably,
// by splitting ty into ty and ty_prim.
item_impl
: IMPL maybe_generic_params ty_prim '{' maybe_impl_methods '}'
| IMPL maybe_generic_params '(' ty ')' '{' maybe_impl_methods '}'
| IMPL maybe_generic_params trait_ref FOR ty '{' maybe_impl_methods '}'
;

maybe_impl_methods
: impl_methods
| %empty
;

impl_methods
: impl_method
| impl_methods impl_method
;

impl_method
: attrs_and_vis maybe_unsafe FN ident maybe_generic_params inner_attrs_and_block
;

item_fn
: FN ident maybe_generic_params fn_decl inner_attrs_and_block  { $$ = mk_node("fn", 1, $5); }
;

fn_decl
: fn_params ret_ty
;

fn_params
: '(' maybe_params ')'
;

maybe_params
: params
| %empty
;

params
: param
| params ',' param
;

param
: pat ':' ty
;

inferrable_params
: inferrable_param
| inferrable_params ',' inferrable_param
;

inferrable_param
: pat maybe_ty_ascription
;

ret_ty
: RARROW '!'
| RARROW ty
| %empty
;

maybe_generic_params
: generic_params
| %empty
;

generic_params
: '<' lifetimes '>'
| '<' lifetimes ',' ty_params '>'
| '<' ty_params '>'
;

ty_params
: ty_param
| ty_params ',' ty_param
;

// A path with no type parameters; e.g. `foo::bar::Baz`
//
// These show up in 'use' view-items, because these are processed
// without respect to types.
path_no_types_allowed
: ident
| path_no_types_allowed MOD_SEP ident
;

// A path with a lifetime and type parameters, with no double colons
// before the type parameters; e.g. `foo::bar<'a>::Baz<T>`
//
// These show up in "trait references", the components of
// type-parameter bounds lists, as well as in the prefix of the
// path_generic_args_and_bounds rule, which is the full form of a
// named typed expression.
//
// They do not have (nor need) an extra '::' before '<' because
// unlike in expr context, there are no "less-than" type exprs to
// be ambiguous with.
path_generic_args_without_colons
: %prec IDENT
  ident
| %prec IDENT
  ident generic_args
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident generic_args
;

// A path with a lifetime and type parameters with double colons before
// the type parameters; e.g. `foo::bar::<'a>::Baz::<T>`
//
// These show up in expr context, in order to disambiguate from "less-than"
// expressions.
path_generic_args_with_colons
: ident
| path_generic_args_with_colons MOD_SEP ident
| path_generic_args_with_colons MOD_SEP generic_args
;

// A path with a lifetime and type parameters with bounds before the last
// set of type parameters only; e.g. `foo::bar<'a>::Baz:X+Y<T>` This
// form does not use extra double colons.
//
path_generic_args_and_bounds
: path_generic_args_without_colons maybe_bounds maybe_generic_args
;

maybe_generic_args
: generic_args
| %empty
;

generic_args
: '<' lifetimes_or_tys '>'
;

ty_param
: maybe_unsized ident maybe_bounds maybe_ty_default
;

maybe_unsized
: unsized
| %empty
;

unsized
: TYPE
;

maybe_bounds
: ':' bounds
| %empty
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
| %empty
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
| %empty
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
: STRUCT ident maybe_generic_params struct_args     { $$ = mk_node("struct", 0); }
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
| %empty
;

struct_decl_field
: attrs_and_vis ident ':' ty
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
: ENUM ident maybe_generic_params '{' enum_defs '}'     { $$ = mk_node("enum", 0); }
| ENUM ident maybe_generic_params '{' enum_defs ',' '}' { $$ = mk_node("enum", 0); }
;

enum_defs
: enum_def
| enum_defs ',' enum_def
| %empty
;

enum_def
: attrs_and_vis ident enum_args
;

enum_args
: '{' struct_decl_fields '}'
| '{' struct_decl_fields ',' '}'
| '(' maybe_tys ')'
| '=' expr
| %empty
;

///////////////////////////////////////////////////////////////////////
//////////// dynamic part: statements, expressions, values ////////////
///////////////////////////////////////////////////////////////////////

inner_attrs_and_block
: '{' maybe_inner_attrs stmts '}'        { $$ = $3; }
;

block
: '{' stmts '}'               { $$ = mk_node("block", 1, $2); }
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
: stmts let                                        { $$ = ext_node($1, 1, $2); }
| stmts let nonblock_expr_stmt                     { $$ = ext_node($1, 2, $2, $3); }
| stmts static                                     { $$ = ext_node($1, 1, $2); }
| stmts static nonblock_expr_stmt                  { $$ = ext_node($1, 2, $2, $3); }
| stmts block_item                                 { $$ = ext_node($1, 1, $2); }
| stmts block_item nonblock_expr_stmt              { $$ = ext_node($1, 2, $2, $3); }
| stmts block_expr                                 { $$ = ext_node($1, 1, $2); }
| stmts block_expr nonblock_expr_stmt              { $$ = ext_node($1, 2, $2, $3); }
| stmts ';'
| stmts ';' nonblock_expr_stmt                     { $$ = ext_node($1, 1, $3); }
| nonblock_expr_stmt                               { $$ = mk_node("stmts", 1, $1); }
| %empty                                      { $$ = mk_node("stmts", 0); }
;

nonblock_expr_stmt
: nonblock_prefix_expr
| nonblock_nonprefix_expr
;

maybe_exprs
: exprs
| %empty
;

exprs
: expr                                                        { $$ = mk_node("exprs", 1, $1); }
| exprs ',' expr                                              { $$ = ext_node($1, 1, $2); }
;

nonblock_nonprefix_expr
: lit
| %prec IDENT
  path_generic_args_with_colons
| path_generic_args_with_colons '{' field_inits default_field_init '}'
| nonblock_nonprefix_expr '.' ident
| nonblock_nonprefix_expr '[' expr ']'
| nonblock_nonprefix_expr '(' maybe_exprs ')'
| '(' maybe_exprs ')'
| CONTINUE                                            { $$ = mk_node("continue", 0); }
| CONTINUE ident                                      { $$ = mk_node("continue-label", 0); }
| RETURN                                              { $$ = mk_node("return", 0); }
| RETURN expr                                         { $$ = mk_node("return-expr", 1, $2); }
| BREAK                                               { $$ = mk_node("break", 0); }
| BREAK ident                                         { $$ = mk_node("break-ident", 0); }
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
;

expr
: lit
| %prec IDENT
  path_generic_args_with_colons
| path_generic_args_with_colons '{' field_inits default_field_init '}'
| expr '.' ident
| expr '[' expr ']'
| expr '(' maybe_exprs ')'        { $$ = mk_node("call", 2, $1, $3); }
| '(' maybe_exprs ')'
| CONTINUE                                            { $$ = mk_node("continue", 0); }
| CONTINUE ident                                      { $$ = mk_node("continue-label", 0); }
| RETURN                                              { $$ = mk_node("return", 0); }
| RETURN expr                                         { $$ = mk_node("return-expr", 1, $2); }
| BREAK                                               { $$ = mk_node("break", 0); }
| BREAK ident                                         { $$ = mk_node("break-ident", 0); }
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
| block_expr
| lambda_expr
;

nonblock_prefix_expr
: '-' expr                         { $$ = mk_node("-", 1, $2); }
| '*' expr                         { $$ = mk_node("*", 1, $2); }
| '~' expr                         { $$ = mk_node("~", 1, $2); }
| lambda_expr
;

lambda_expr
: OROR expr                        { $$ = mk_node("lambda", 2, mk_node("nil", 0), $2); }
| '|' inferrable_params '|' expr   { $$ = mk_node("lambda", 2, $2, $4); }
;

field_inits
: field_init
| field_inits ',' field_init
;

field_init
: maybe_mut ident ':' expr
;

default_field_init
: ','
| ',' DOTDOT expr
| %empty
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
| %empty
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
: LIT_CHAR                   { $$ = mk_node("lit-char", 1, mk_atom(yytext)); }
| LIT_INT                    { $$ = mk_node("lit-int", 1, mk_atom(yytext)); }
| LIT_UINT                   { $$ = mk_node("lit-uint", 1, mk_atom(yytext)); }
| LIT_INT_UNSUFFIXED         { $$ = mk_node("lit-int", 1, mk_atom(yytext)); }
| LIT_FLOAT                  { $$ = mk_node("lit-float", 1, mk_atom(yytext)); }
| LIT_FLOAT_UNSUFFIXED       { $$ = mk_node("lit-float", 1, mk_atom(yytext)); }
| LIT_STR                    { $$ = mk_node("lit-str", 1, mk_atom(yytext)); }
| LIT_STR_RAW                { $$ = mk_node("lit-str-raw", 1, mk_atom(yytext)); }
| TRUE                       { $$ = mk_node("lit-true", 1, mk_atom(yytext)); }
| FALSE                      { $$ = mk_node("lit-false", 1, mk_atom(yytext)); }
;

str
: LIT_STR                    { $$ = mk_node("lit-str", 1, mk_atom(yytext)); }
| LIT_STR_RAW                { $$ = mk_node("lit-str-raw", 1, mk_atom(yytext)); }
;

ident
: IDENT                      { $$ = mk_node("ident", 1, mk_atom(yytext)); }
;
