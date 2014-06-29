%{
#define YYSTYPE struct node *
struct node;
extern int yylex();
extern void yyerror(char const *s);
extern struct node *mk_node(char const *name, int n, ...);
extern struct node *mk_atom(char *text);
extern struct node *mk_none();
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

// A couple fake-precedence symbols to use in rules associated with +
// and < in trailing type contexts. These come up when you have a type
// in the RHS of operator-AS, such as "foo as bar<baz>". The "<" there
// has to be shifted so the parser keeps trying to parse a type, even
// though it might well consider reducing the type "bar" and then
// going on to "<" as a subsequent binop. The "+" case is with
// trailing type-bounds ("foo as bar:A+B"), for the same reason.
%precedence SHIFTPLUS SHIFTLT

// Binops & unops, and their precedences
%precedence BOX
%left '=' BINOPEQ
%left OROR
%left ANDAND
%left EQEQ NE
%left '<' '>' LE GE
%left '|'
%left '^'
%left '&'
%left SHL SHR
%left '+' '-'
%precedence AS
%left '*' '/' '%'

// RETURN needs to be lower-precedence than all the block-expr
// starting keywords, so that juxtapositioning them in a stmts
// like 'return if foo { 10 } else { 22 }' shifts 'if' rather
// than reducing a no-argument return.
%precedence RETURN
%precedence FOR IF LOOP MATCH UNSAFE WHILE

%precedence '{' '[' '(' '.'

%start crate

%%

crate
: maybe_inner_attrs maybe_mod_items  { mk_node("crate", 2, $1, $2); }
;

maybe_inner_attrs
: inner_attrs
| %empty                   { $$ = mk_none(); }
;

inner_attrs
: inner_attr               { $$ = mk_node("InnerAttrs", 1, $1); }
| inner_attrs inner_attr   { $$ = ext_node($1, 1, $2); }
;

inner_attr
: SHEBANG '[' meta_item ']'   { $$ = mk_node("InnerAttr", 0); }
;

maybe_outer_attrs
: outer_attrs
| %empty                   { $$ = mk_none(); }
;

outer_attrs
: outer_attr               { $$ = mk_node("OuterAttrs", 1, $1); }
| outer_attrs outer_attr   { $$ = ext_node($1, 1, $2); }
;

outer_attr
: '#' '[' meta_item ']'    { $$ = $3; }
;

meta_item
: ident                    { $$ = mk_node("MetaWord", 1, $1); }
| ident '=' lit            { $$ = mk_node("MetaNameValue", 2, $1, $3); }
| ident '(' meta_seq ')'   { $$ = mk_node("MetaList", 2, $1, $3); }
;

meta_seq
: meta_item                { $$ = mk_node("MetaItems", 1, $1); }
| meta_seq ',' meta_item   { $$ = ext_node($1, 1, $3); }
;

maybe_mod_items
: mod_items
| %empty             { $$ = mk_none(); }
;

mod_items
: mod_item                               { $$ = mk_node("Items", 1, $1); }
| mod_items mod_item                     { $$ = ext_node($1, 1, $2); }
;

attrs_and_vis
: maybe_outer_attrs visibility           { $$ = mk_node("AttrsAndVis", 2, $1, $2); }
;

mod_item
: attrs_and_vis item    { $$ = mk_node("Item", 2, $1, $2); }
;

item
: item_static
| item_type
| block_item
| view_item
;

view_item
: USE view_path ';'                           { $$ = mk_node("ViewItemUse", 1, $2); }
| EXTERN CRATE ident ';'                      { $$ = mk_node("ViewItemExternCrate", 1, $3); }
| EXTERN CRATE ident '=' str ';'              { $$ = mk_node("ViewItemExternCrate", 2, $3, $5); }
;


view_path
: path_no_types_allowed                        { $$ = mk_node("ViewPathSimple", 1, $1); }
| path_no_types_allowed MOD_SEP '{' idents '}' { $$ = mk_node("ViewPathList", 2, $1, $4); }
| path_no_types_allowed MOD_SEP '*'            { $$ = mk_node("ViewPathGlob", 1, $1); }
| ident '=' path_no_types_allowed              { $$ = mk_node("ViewPathSimple", 2, $1, $3); }
;

block_item
: item_fn
| item_mod
| item_foreign_mod          { $$ = mk_node("ItemForeignMod", 1, $1); }
| item_struct
| item_enum
| item_trait
| item_impl
;

maybe_ty_ascription
: ':' ty { $$ = $2; }
| %empty { $$ = mk_none(); }
;

maybe_init_expr
: '=' expr { $$ = $2; }
| %empty   { $$ = mk_none(); }
;

pats_or
: pat              { $$ = mk_node("Pats", 1, $1); }
| pats_or '|' pat  { $$ = ext_node($1, 1, $3); }
;

pat
: '_'
| '&' pat                                { $$ = mk_node("PatRegion", 1, $2); }
| '(' pat_tup ')'                        { $$ = mk_node("PatTup", 1, $2); }
| '[' pat_vec ']'                        { $$ = mk_node("PatVec", 1, $2); }
| lit_or_path
| lit_or_path DOTDOT lit_or_path         { $$ = mk_node("PatRange", 2, $1, $3); }
| path_expr '{' pat_struct '}'           { $$ = mk_node("PatStruct", 2, $1, $3); }
| path_expr '(' DOTDOT ')'               { $$ = mk_node("PatEnum", 1, $1); }
| path_expr '(' pat_tup ')'              { $$ = mk_node("PatEnum", 2, $1, $3); }
| binding_mode ident                     { $$ = mk_node("PatIdent", 2, $1, $2); }
|              ident '@' pat             { $$ = mk_node("PatIdent", 3, mk_node("BindByValue", 1, mk_atom("MutImmutable")), $1, $3); }
| binding_mode ident '@' pat             { $$ = mk_node("PatIdent", 3, $1, $2, $4); }
| BOX pat                                { $$ = mk_node("PatUniq", 1, $2); }
;

binding_mode
: REF         { $$ = mk_node("BindByRef", 1, mk_atom("MutImmutable")); }
| REF MUT     { $$ = mk_node("BindByRef", 1, mk_atom("MutMutable")); }
| MUT         { $$ = mk_node("BindByValue", 1, mk_atom("MutMutable")); }
;

lit_or_path
: path_expr    { $$ = mk_node("PatLit", 1, $1); }
| lit          { $$ = mk_node("PatLit", 1, $1); }
;

pat_field
: ident            { $$ = mk_node("PatField", 1, $1); }
| ident ':' pat    { $$ = mk_node("PatField", 2, $1, $3); }
;

pat_fields
: pat_field                  { $$ = mk_node("PatFields", 1, $1); }
| pat_fields ',' pat_field   { $$ = ext_node($1, 1, $3); }
;

pat_struct
: pat_fields                 { $$ = mk_node("PatStruct", 2, $1, mk_atom("false")); }
| pat_fields ',' DOTDOT      { $$ = mk_node("PatStruct", 2, $1, mk_atom("true")); }
| DOTDOT                     { $$ = mk_node("PatStruct", 1, mk_atom("true")); }
;

pat_tup
: pat               { $$ = mk_node("pat_tup", 1, $1); }
| pat_tup ',' pat   { $$ = ext_node($1, 1, $3); }
;

pat_vec
: pat_vec_elts                                  { $$ = mk_node("PatVec", 3, $1, mk_none(), mk_none()); }
| pat_vec_elts ',' DOTDOT pat                   { $$ = mk_node("PatVec", 3, $1, $4, mk_none()); }
| pat_vec_elts ',' DOTDOT pat ',' pat_vec_elts  { $$ = mk_node("PatVec", 3, $1, $4, $6); }
|                  DOTDOT pat ',' pat_vec_elts  { $$ = mk_node("PatVec", 3, mk_none(), $2, $4); }
|                  DOTDOT pat                   { $$ = mk_node("PatVec", 3, mk_none(), $2, mk_none()); }
| %empty                                        { $$ = mk_node("PatVec", 3, mk_none(), mk_none(), mk_none()); }
;

pat_vec_elts
: pat                    { $$ = mk_node("PatVecElts", 1, $1); }
| pat_vec_elts ',' pat   { $$ = ext_node($1, 1, $3); }
;

maybe_tys
: tys
| %empty  { $$ = mk_none(); }
;

tys
: ty                 { $$ = mk_node("tys", 1, $1); }
| tys ',' ty         { $$ = ext_node($1, 1, $3); }
;

ty
: ty_prim
| ty_closure
| '(' tys ')'                          { $$ = mk_node("TyTup", 1, $2); }
| '(' ')'                              { $$ = mk_atom("TyNil"); }
;

ty_prim
: path_generic_args_and_bounds         { $$ = mk_node("TyPath", 2, mk_node("global", 1, mk_atom("false")), $1); }
| MOD_SEP path_generic_args_and_bounds { $$ = mk_node("TyPath", 2, mk_node("global", 1, mk_atom("true")), $2); }
| BOX ty                               { $$ = mk_node("TyBox", 1, $2); }
| '*' maybe_mut ty                     { $$ = mk_node("TyPtr", 2, $2, $3); }
| '&' maybe_lifetime maybe_mut ty      { $$ = mk_node("TyRptr", 3, $2, $3, $4); }
| '[' ty ']'                           { $$ = mk_node("TyVec", 1, $2); }
| '[' ty ',' DOTDOT expr ']'           { $$ = mk_node("TyFixedLengthVec", 2, $2, $5); }
| TYPEOF '(' expr ')'                  { $$ = mk_node("TyTypeof", 1, $3); }
| '_'                                  { $$ = mk_atom("TyInfer"); }
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
: generic_params fn_params ret_ty
;

ty_closure
: UNSAFE maybe_once generic_params '|' params '|' ret_ty
|        maybe_once generic_params '|' params '|' ret_ty
| UNSAFE maybe_once generic_params OROR ret_ty
|        maybe_once generic_params OROR ret_ty
;

maybe_once
: ONCE   { $$ = mk_atom("Once"); }
| %empty { $$ = mk_atom("Many"); }
;

ty_proc
: PROC generic_params fn_params maybe_bounds ret_ty
;

maybe_mut
: MUT    { $$ = mk_atom("MutMutable"); }
| %empty { $$ = mk_atom("MutImmutable"); }
;

item_mod
: MOD ident ';'                                       { $$ = mk_node("ItemMod", 1, $2); }
| MOD ident '{' maybe_inner_attrs maybe_mod_items '}' { $$ = mk_node("ItemMod", 3, $2, $4, $5); }
;

item_foreign_mod
: EXTERN maybe_abi '{' maybe_inner_attrs maybe_foreign_items '}'
;

maybe_abi
: str
| %empty { $$ = mk_none(); }
;

maybe_foreign_items
: foreign_items
| %empty { $$ = mk_none(); }
;

foreign_items
: foreign_item
| foreign_items foreign_item
;

foreign_item
: attrs_and_vis STATIC item_foreign_static
| attrs_and_vis FN item_foreign_fn
| attrs_and_vis UNSAFE item_foreign_fn
;

item_foreign_static
: maybe_mut ident ':' ty ';'
;

item_foreign_fn
: FN ident generic_params fn_decl_allow_variadic ';'
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
: PUB      { $$ = mk_atom("Public"); }
| %empty   { $$ = mk_atom("Inherited"); }
;

idents
: ident            { $$ = mk_node("ident", 1, $1); }
| idents ',' ident { $$ = ext_node($1, 1, $3); }
;

item_type
: TYPE ident generic_params '=' ty ';'  { $$ = mk_node("ItemTy", 3, $2, $3, $5); }
;

item_trait
: TRAIT ident generic_params maybe_supertraits '{' maybe_trait_methods '}'
{
  $$ = mk_node("ItemTrait", 4, $2, $3, $4, $6);
}
;

maybe_supertraits
: ':' supertraits
| %empty { $$ = mk_none(); }
;

supertraits
: trait_ref
| supertraits '+' trait_ref
;

maybe_trait_methods
: trait_methods
| %empty { $$ = mk_none(); }
;

trait_methods
: trait_method
| trait_methods trait_method
;

maybe_unsafe
: UNSAFE
| %empty { $$ = mk_none(); }
;

trait_method
: attrs_and_vis maybe_unsafe FN ident generic_params fn_decl ';'
| attrs_and_vis maybe_unsafe FN ident generic_params fn_decl inner_attrs_and_block
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
: IMPL generic_params ty_prim '{' maybe_impl_methods '}'           { $$ = mk_node("ItemImpl", 3, $2, $3, $5); }
| IMPL generic_params '(' ty ')' '{' maybe_impl_methods '}'        { $$ = mk_node("ItemImpl", 3, $2, $4, $7); }
| IMPL generic_params trait_ref FOR ty '{' maybe_impl_methods '}'  { $$ = mk_node("ItemImpl", 4, $2, $3, $5, $7); }
;

maybe_impl_methods
: impl_methods
| %empty { $$ = mk_none(); }
;

impl_methods
: impl_method
| impl_methods impl_method
;

impl_method
: attrs_and_vis maybe_unsafe FN ident generic_params inner_attrs_and_block
;

item_fn
: maybe_unsafe FN ident generic_params fn_decl inner_attrs_and_block
{
  $$ = mk_node("ItemFn", 5, $1, $3, $4, $5, $6);
}
;

fn_decl
: fn_params ret_ty   { $$ = mk_node("FnDecl", 2, $1, $2); }
;

fn_params
: '(' maybe_params ')'  { $$ = $2; }
;

maybe_params
: params
| %empty  { $$ = mk_none(); }
;

params
: param                { $$ = mk_node("Args", 1, $1); }
| params ',' param     { $$ = ext_node($1, 1, $3); }
;

param
: pat ':' ty   { $$ = mk_node("Arg", 2, $1, $3); }
;

inferrable_params
: inferrable_param
| inferrable_params ',' inferrable_param
;

inferrable_param
: pat maybe_ty_ascription
;

ret_ty
: RARROW '!' { $$ = mk_none(); }
| RARROW ty { $$ = mk_node("ret-ty", 1, $2); }
| %empty { $$ = mk_none(); }
;

generic_params
: '<' lifetimes '>'                   { $$ = mk_node("Generics", 2, $2, mk_none()); }
| '<' lifetimes ',' ty_params '>'     { $$ = mk_node("Generics", 2, $2, $4); }
| '<' ty_params '>'                   { $$ = mk_node("Generics", 2, mk_none(), $2); }
| %empty                              { $$ = mk_none(); }
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
  ident                  { $$ = mk_node("components", 1, $1); }
| %prec IDENT
  ident generic_args     { $$ = mk_node("components", 2, $1, $2); }
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident      { $$ = ext_node($1, 1, $3); }
| %prec IDENT
  path_generic_args_without_colons MOD_SEP ident generic_args { $$ = ext_node($1, 2, $3, $4); }
;

// A path with a lifetime and type parameters with double colons before
// the type parameters; e.g. `foo::bar::<'a>::Baz::<T>`
//
// These show up in expr context, in order to disambiguate from "less-than"
// expressions.
path_generic_args_with_colons
: ident  { $$ = mk_node("components", 1, $1); }
| path_generic_args_with_colons MOD_SEP ident { $$ = ext_node($1, 1, $3); }
| path_generic_args_with_colons MOD_SEP generic_args { $$ = ext_node($1, 1, $3); }
;

// A path with a lifetime and type parameters with bounds before the last
// set of type parameters only; e.g. `foo::bar<'a>::Baz:X+Y<T>` This
// form does not use extra double colons.
path_generic_args_and_bounds
: path_generic_args_without_colons ':' bounds generic_args  { $$ = ext_node($1, 2, $3, $4); }
| %prec SHIFTLT
  path_generic_args_without_colons ':' bounds  { $$ = ext_node($1, 1, $3); }
| %prec SHIFTLT
  path_generic_args_without_colons
;

generic_args
: '<' lifetimes_or_tys '>' { $$ = $2; }
;

ty_param
: maybe_unsized ident maybe_bounds maybe_ty_default
;

maybe_unsized
: unsized
| %empty { $$ = mk_none(); }
;

unsized
: TYPE
;

maybe_bounds
: %prec SHIFTPLUS
  ':' bounds
| %empty { $$ = mk_none(); }
;

bounds
: bound
| bounds '+' bound
;

bound
: STATIC_LIFETIME  { $$ = mk_node("lifetime", 1, mk_atom("static")); }
| trait_ref
;

maybe_ty_default
: '=' ty
| %empty
;

lifetimes_or_tys
: lifetime_or_ty                          { $$ = mk_node("LifetimesOrTys", 1, $1); }
| lifetimes_or_tys ',' lifetime_or_ty     { $$ = ext_node($1, 1, $3); }
;

lifetime_or_ty
: lifetime
| ty
;

maybe_lifetime
: lifetime
| %empty { $$ = mk_none(); }
;

lifetimes
: lifetime
| lifetimes ',' lifetime
;

lifetime
: LIFETIME                      { $$ = mk_node("lifetime", 1, mk_atom(yytext)); }
;

trait_ref
: path_generic_args_without_colons
;

// structs
item_struct
: STRUCT ident generic_params struct_args     { $$ = mk_node("ItemStruct", 0); }
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
| %empty { $$ = mk_none(); }
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
: ENUM ident generic_params '{' enum_defs '}'     { $$ = mk_node("ItemEnum", 0); }
| ENUM ident generic_params '{' enum_defs ',' '}' { $$ = mk_node("ItemEnum", 0); }
;

enum_defs
: enum_def
| enum_defs ',' enum_def
| %empty { $$ = mk_none(); }
;

enum_def
: attrs_and_vis ident enum_args
;

enum_args
: '{' struct_decl_fields '}'
| '{' struct_decl_fields ',' '}'
| '(' maybe_tys ')'
| '=' expr
| %empty { $$ = mk_none(); }
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
| stmts item_static                                { $$ = ext_node($1, 1, $2); }
| stmts item_static nonblock_expr_stmt             { $$ = ext_node($1, 2, $2, $3); }
| stmts block_item                                 { $$ = ext_node($1, 1, $2); }
| stmts block_item nonblock_expr_stmt              { $$ = ext_node($1, 2, $2, $3); }
| stmts block_expr                                 { $$ = ext_node($1, 1, $2); }
| stmts block_expr nonblock_expr_stmt              { $$ = ext_node($1, 2, $2, $3); }
| stmts ';'
| stmts ';' nonblock_expr_stmt                     { $$ = ext_node($1, 1, $3); }
| nonblock_expr_stmt                               { $$ = mk_node("stmts", 1, $1); }
| %empty                                           { $$ = mk_node("stmts", 0); }
;

nonblock_expr_stmt
: nonblock_prefix_expr
| nonblock_nonprefix_expr
;

maybe_exprs
: exprs
| %empty { $$ = mk_none(); }
;

exprs
: expr                                                        { $$ = mk_node("exprs", 1, $1); }
| exprs ',' expr                                              { $$ = ext_node($1, 1, $2); }
;

path_expr
: path_generic_args_with_colons
| MOD_SEP path_generic_args_with_colons
;

nonblock_nonprefix_expr
: lit                                                 { $$ = mk_node("ExprLit", 1, $1); }
| %prec IDENT
  path_expr                                           { $$ = mk_node("ExprPath", 1, $1); }
| SELF                                                { $$ = mk_node("ExprPath", 1, mk_node("ident", 1, mk_atom("self"))); }
| path_expr '!' delimited_token_trees                 { $$ = mk_node("ExprMac", 2, $1, $3); }
| path_expr '{' field_inits default_field_init '}'    { $$ = mk_node("ExprStruct", 3, $1, $3, $4); }
| nonblock_nonprefix_expr '.' ident                   { $$ = mk_node("ExprField", 2, $1, $3); }
| nonblock_nonprefix_expr '[' expr ']'                { $$ = mk_node("ExprIndex", 2, $1, $3); }
| nonblock_nonprefix_expr '(' maybe_exprs ')'         { $$ = mk_node("ExprCall", 2, $1, $3); }
| '(' maybe_exprs ')'                                 { $$ = mk_node("ExprParen", 1, $2); }
| CONTINUE                                            { $$ = mk_node("ExprAgain", 0); }
| CONTINUE ident                                      { $$ = mk_node("ExprAgain", 1, $2); }
| RETURN                                              { $$ = mk_node("ExprRet", 0); }
| RETURN expr                                         { $$ = mk_node("ExprRet", 1, $2); }
| BREAK                                               { $$ = mk_node("ExprBreak", 0); }
| BREAK ident                                         { $$ = mk_node("ExprBreak", 1, $2); }
| nonblock_nonprefix_expr '=' expr                    { $$ = mk_node("ExprAssign", 2, $1, $3); }
| nonblock_nonprefix_expr BINOPEQ expr                { $$ = mk_node("ExprAssignOp", 2, $1, $3); }
| nonblock_nonprefix_expr OROR expr                   { $$ = mk_node("ExprBinary", 3, mk_atom("BiOr"), $1, $3); }
| nonblock_nonprefix_expr ANDAND expr                 { $$ = mk_node("ExprBinary", 3, mk_atom("BiAnd"), $1, $3); }
| nonblock_nonprefix_expr EQEQ expr                   { $$ = mk_node("ExprBinary", 3, mk_atom("BiEq"), $1, $3); }
| nonblock_nonprefix_expr NE expr                     { $$ = mk_node("ExprBinary", 3, mk_atom("BiNe"), $1, $3); }
| nonblock_nonprefix_expr '<' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiLt"), $1, $3); }
| nonblock_nonprefix_expr '>' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiGt"), $1, $3); }
| nonblock_nonprefix_expr LE expr                     { $$ = mk_node("ExprBinary", 3, mk_atom("BiLe"), $1, $3); }
| nonblock_nonprefix_expr GE expr                     { $$ = mk_node("ExprBinary", 3, mk_atom("BiGe"), $1, $3); }
| nonblock_nonprefix_expr '|' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitOr"), $1, $3); }
| nonblock_nonprefix_expr '^' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitXor"), $1, $3); }
| nonblock_nonprefix_expr '&' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitAnd"), $1, $3); }
| nonblock_nonprefix_expr SHL expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiShl"), $1, $3); }
| nonblock_nonprefix_expr SHR expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiShr"), $1, $3); }
| nonblock_nonprefix_expr '+' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiAdd"), $1, $3); }
| nonblock_nonprefix_expr '-' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiSub"), $1, $3); }
| nonblock_nonprefix_expr '*' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiMul"), $1, $3); }
| nonblock_nonprefix_expr '/' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiDiv"), $1, $3); }
| nonblock_nonprefix_expr '%' expr                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiRem"), $1, $3); }
| nonblock_nonprefix_expr AS ty                       { $$ = mk_node("ExprCast", 2, $1, $3); }
| BOX nonblock_nonprefix_expr                         { $$ = mk_node("ExprBox", 1, $2); }
;

expr
: lit                                                 { $$ = mk_node("ExprLit", 1, $1); }
| %prec IDENT
  path_expr                                           { $$ = mk_node("ExprPath", 1, $1); }
| SELF                                                { $$ = mk_node("ExprPath", 1, mk_node("ident", 1, mk_atom("self"))); }
| path_expr '!' delimited_token_trees                 { $$ = mk_node("ExprMac", 2, $1, $3); }
| path_expr '{' field_inits default_field_init '}'    { $$ = mk_node("ExprStruct", 3, $1, $3, $4); }
| expr '.' ident                                      { $$ = mk_node("ExprField", 2, $1, $3); }
| expr '[' expr ']'                                   { $$ = mk_node("ExprIndex", 2, $1, $3); }
| expr '(' maybe_exprs ')'                            { $$ = mk_node("ExprCall", 2, $1, $3); }
| '(' maybe_exprs ')'                                 { $$ = mk_node("ExprParen", 1, $2); }
| CONTINUE                                            { $$ = mk_node("ExprAgain", 0); }
| CONTINUE ident                                      { $$ = mk_node("ExprAgain", 1, $2); }
| RETURN                                              { $$ = mk_node("ExprRet", 0); }
| RETURN expr                                         { $$ = mk_node("ExprRet", 1, $2); }
| BREAK                                               { $$ = mk_node("ExprBreak", 0); }
| BREAK ident                                         { $$ = mk_node("ExprBreak", 1, $2); }
| expr '=' expr                                       { $$ = mk_node("ExprAssign", 2, $1, $3); }
| expr BINOPEQ expr                                   { $$ = mk_node("ExprAssignOp", 2, $1, $3); }
| expr OROR expr                                      { $$ = mk_node("ExprBinary", 3, mk_atom("BiOr"), $1, $3); }
| expr ANDAND expr                                    { $$ = mk_node("ExprBinary", 3, mk_atom("BiAnd"), $1, $3); }
| expr EQEQ expr                                      { $$ = mk_node("ExprBinary", 3, mk_atom("BiEq"), $1, $3); }
| expr NE expr                                        { $$ = mk_node("ExprBinary", 3, mk_atom("BiNe"), $1, $3); }
| expr '<' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiLt"), $1, $3); }
| expr '>' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiGt"), $1, $3); }
| expr LE expr                                        { $$ = mk_node("ExprBinary", 3, mk_atom("BiLe"), $1, $3); }
| expr GE expr                                        { $$ = mk_node("ExprBinary", 3, mk_atom("BiGe"), $1, $3); }
| expr '|' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitOr"), $1, $3); }
| expr '^' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitXor"), $1, $3); }
| expr '&' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiBitAnd"), $1, $3); }
| expr SHL expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiShl"), $1, $3); }
| expr SHR expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiShr"), $1, $3); }
| expr '+' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiAdd"), $1, $3); }
| expr '-' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiSub"), $1, $3); }
| expr '*' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiMul"), $1, $3); }
| expr '/' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiDiv"), $1, $3); }
| expr '%' expr                                       { $$ = mk_node("ExprBinary", 3, mk_atom("BiRem"), $1, $3); }
| expr AS ty                                          { $$ = mk_node("ExprCast", 2, $1, $3); }
| BOX expr                                            { $$ = mk_node("ExprBox", 1, $2); }
| block_expr
| nonblock_prefix_expr
;

nonblock_prefix_expr
: '-' expr                         { $$ = mk_node("-", 1, $2); }
| '*' expr                         { $$ = mk_node("*", 1, $2); }
| '&' expr                         { $$ = mk_node("&", 1, $2); }
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
: ','               { $$ = mk_none(); }
| ',' DOTDOT expr   { $$ = $3; }
| %empty            { $$ = mk_none(); }
;

block_expr
: expr_match
| expr_if
| expr_while
| expr_loop
| expr_for
| UNSAFE block                               { $$ = mk_node("UnsafeBlock", 1, $1); }
| block
;

expr_match
: MATCH expr '{' match_clauses '}'           { $$ = mk_node("ExprMatch", 2, $2, $4); }
| MATCH expr '{' match_clauses ',' '}'       { $$ = mk_node("ExprMatch", 2, $2, $4); }
;

match_clauses
: match_clause                               { $$ = mk_node("Arms", 1, $1); }
| match_clauses ',' match_clause             { $$ = ext_node($1, 1, $3) ; }
;

match_clause
: pats_or maybe_guard FAT_ARROW expr         { $$ = mk_node("Arm", 3, $1, $2, $4); }
;

maybe_guard
: IF expr           { $$ = $2; }
| %empty            { $$ = mk_none(); }
;

expr_if
: IF expr block                              { $$ = mk_node("ExprIf", 2, $2, $3); }
| IF expr block ELSE block_or_if             { $$ = mk_node("ExprIf", 3, $2, $3, $5); }
;

block_or_if
: block
| expr_if
;

expr_while
: WHILE expr block                           { $$ = mk_node("ExprWhile", 2, $2, $3); }
;

expr_loop
: LOOP block                                 { $$ = mk_node("ExprLoop", 1, $2); }
;

expr_for
: FOR expr IN expr block                     { $$ = mk_node("ExprForLoop", 3, $2, $4, $5); }
;

let
: LET pat maybe_ty_ascription maybe_init_expr ';' { $$ = mk_node("DeclLocal", 3, $2, $3, $4); }
;

item_static
: STATIC pat ':' ty '=' expr ';'  { $$ = mk_node("ItemStatic", 3, $2, $4, $6); }

lit
: LIT_CHAR                   { $$ = mk_node("LitChar", 1, mk_atom(yytext)); }
| LIT_INT                    { $$ = mk_node("LitInt", 1, mk_atom(yytext)); }
| LIT_UINT                   { $$ = mk_node("LitUint", 1, mk_atom(yytext)); }
| LIT_INT_UNSUFFIXED         { $$ = mk_node("LitIntUnsuffixed", 1, mk_atom(yytext)); }
| LIT_FLOAT                  { $$ = mk_node("LitFloat", 1, mk_atom(yytext)); }
| LIT_FLOAT_UNSUFFIXED       { $$ = mk_node("LitFloatUnsuffixed", 1, mk_atom(yytext)); }
| TRUE                       { $$ = mk_node("LitBool", 1, mk_atom(yytext)); }
| FALSE                      { $$ = mk_node("LitBool", 1, mk_atom(yytext)); }
| str
;

str
: LIT_STR                    { $$ = mk_node("LitStr", 1, mk_atom(yytext), mk_atom("CookedStr")); }
| LIT_STR_RAW                { $$ = mk_node("LitStr", 1, mk_atom(yytext), mk_atom("RawStr")); }
;

ident
: IDENT                      { $$ = mk_node("ident", 1, mk_atom(yytext)); }
;

unpaired_token
: SHL                        { $$ = mk_atom(yytext); }
| SHR                        { $$ = mk_atom(yytext); }
| LE                         { $$ = mk_atom(yytext); }
| EQEQ                       { $$ = mk_atom(yytext); }
| NE                         { $$ = mk_atom(yytext); }
| GE                         { $$ = mk_atom(yytext); }
| ANDAND                     { $$ = mk_atom(yytext); }
| OROR                       { $$ = mk_atom(yytext); }
| BINOPEQ                    { $$ = mk_atom(yytext); }
| DOTDOT                     { $$ = mk_atom(yytext); }
| DOTDOTDOT                  { $$ = mk_atom(yytext); }
| MOD_SEP                    { $$ = mk_atom(yytext); }
| RARROW                     { $$ = mk_atom(yytext); }
| FAT_ARROW                  { $$ = mk_atom(yytext); }
| LIT_CHAR                   { $$ = mk_atom(yytext); }
| LIT_INT                    { $$ = mk_atom(yytext); }
| LIT_UINT                   { $$ = mk_atom(yytext); }
| LIT_INT_UNSUFFIXED         { $$ = mk_atom(yytext); }
| LIT_FLOAT                  { $$ = mk_atom(yytext); }
| LIT_FLOAT_UNSUFFIXED       { $$ = mk_atom(yytext); }
| LIT_STR                    { $$ = mk_atom(yytext); }
| LIT_STR_RAW                { $$ = mk_atom(yytext); }
| IDENT                      { $$ = mk_atom(yytext); }
| UNDERSCORE                 { $$ = mk_atom(yytext); }
| LIFETIME                   { $$ = mk_atom(yytext); }
| ';'                        { $$ = mk_atom(yytext); }
| ','                        { $$ = mk_atom(yytext); }
| '.'                        { $$ = mk_atom(yytext); }
| '@'                        { $$ = mk_atom(yytext); }
| '#'                        { $$ = mk_atom(yytext); }
| '~'                        { $$ = mk_atom(yytext); }
| ':'                        { $$ = mk_atom(yytext); }
| '$'                        { $$ = mk_atom(yytext); }
| '='                        { $$ = mk_atom(yytext); }
| '!'                        { $$ = mk_atom(yytext); }
| '<'                        { $$ = mk_atom(yytext); }
| '>'                        { $$ = mk_atom(yytext); }
| '-'                        { $$ = mk_atom(yytext); }
| '&'                        { $$ = mk_atom(yytext); }
| '|'                        { $$ = mk_atom(yytext); }
| '+'                        { $$ = mk_atom(yytext); }
| '*'                        { $$ = mk_atom(yytext); }
| '/'                        { $$ = mk_atom(yytext); }
| '^'                        { $$ = mk_atom(yytext); }
| '%'                        { $$ = mk_atom(yytext); }
;

token_trees
: %empty                     { $$ = mk_node("TokenTrees", 0); }
| token_trees token_tree     { $$ = ext_node($1, 1, $2); }
;

token_tree
: delimited_token_trees
| unpaired_token         { $$ = mk_node("TTTok", 1, $1); }
;

delimited_token_trees
: '(' token_trees ')'
{
  $$ = mk_node("TTDelim", 3,
               mk_node("TTTok", 1, mk_atom("(")),
               $2,
               mk_node("TTTok", 1, mk_atom(")")));
}

| '{' token_trees '}'
{
  $$ = mk_node("TTDelim", 3,
               mk_node("TTTok", 1, mk_atom("{")),
               $2,
               mk_node("TTTok", 1, mk_atom("}")));
}

| '[' token_trees ']'
{
  $$ = mk_node("TTDelim", 3,
               mk_node("TTTok", 1, mk_atom("[")),
               $2,
               mk_node("TTTok", 1, mk_atom("]")));
}
;
