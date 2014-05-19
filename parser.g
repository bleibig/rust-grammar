%start rust_parser, rust;

%options "generate-llmessage";

%token SHL;
%token SHR;
%token LE;
%token EQEQ;
%token NE;
%token GE;
%token ANDAND;
%token OROR;
%token BINOPEQ;
%token DOTDOT;
%token DOTDOTDOT;
%token MOD_SEP;
%token RARROW;
%token FAT_ARROW;
%token LIT_CHAR;
%token LIT_INT;
%token LIT_UINT;
%token LIT_INT_UNSUFFIXED;
%token LIT_FLOAT;
%token LIT_FLOAT_UNSUFFIXED;
%token LIT_STR;
%token LIT_STR_RAW;
%token IDENT;
%token UNDERSCORE;
%token LIFETIME;

// keywords
%token SELF;
%token STATIC;
%token AS;
%token BREAK;
%token CRATE;
%token ELSE;
%token ENUM;
%token EXTERN;
%token FALSE;
%token FN;
%token FOR;
%token IF;
%token IMPL;
%token IN;
%token LET;
%token LOOP;
%token MATCH;
%token MOD;
%token MUT;
%token ONCE;
%token PRIV;
%token PUB;
%token REF;
%token RETURN;
%token STRUCT;
%token TRUE;
%token TRAIT;
%token TYPE;
%token UNSAFE;
%token USE;
%token WHILE;
%token CONTINUE;
%token PROC;
%token BOX;
%token TYPEOF;
%token DOC_COMMENT;

%token SHEBANG;
%token STATIC_LIFETIME;

{

int main(void) {
    rust_parser();
    return 0;
}

}

// Entry point
rust : crate ;

// 5. Crates and source
crate : inner_attr* mod_item* ;

// 6. Items and Attributes
/// 6.1 Items
item_or_view_item
    : visibility [ item_use
                 | extern_block_item
                 | item_const
                 | UNSAFE? item_fn
                 | item_mod
                 | item_type
                 | item_enum
                 | item_trait
                 | item_impl
                 | item_struct
                 ]
    ;
visibility : [ PUB | PRIV ]? ;

/// 6.1.1 Type Parameters
ty_param : unsized? IDENT bounds? [ '=' ty ]? ;
unsized : TYPE ;
bounds : ':' [ bound [ '+' bound ]* ]? ;
bound : STATIC_LIFETIME | trait_ref ;

/// 6.1.2 Modules
item_mod : MOD IDENT [ ';' | '{' inner_attr* mod_item* '}' ] ;
mod_item : outer_attr* item_or_view_item ;

/// 6.2.1.1 Extern crate declarations
item_extern_crate : IDENT [ '=' STR ]? ';' ;

// syntax::parse::parser::{PathParsingMode, parse_path}
path_no_types_allowed                  : IDENT [ MOD_SEP IDENT ]+ ;
path_lifetime_and_types_without_colons : IDENT [ MOD_SEP IDENT [ '<' lifetimes_or_tys '>' ]? ]+ ;
path_lifetime_and_types_with_colons    : IDENT [ MOD_SEP [ IDENT | '<' lifetimes_or_tys '>' ] ]+ ;
path_lifetime_and_types_and_bounds     : IDENT [ MOD_SEP IDENT bounds?  [ '<' lifetimes_or_tys '>' ]? ]+ ;

// syntax::parse::parser::{parse_generic_values_after_lt, parse_lifetimes}
lifetimes_or_tys : '<' lifetime_or_ty [',' lifetime_or_ty ]* '>' ;
lifetime_or_ty : [ LIFETIME | ty ] ;

/// 6.2.1.2 Use declarations
item_use : USE view_path ';' ;
view_path : IDENT [ '=' non_global_path | view_path_mods ]? ;
view_path_mods : MOD_SEP [ IDENT view_path_mods? | view_path_items ] ;
view_path_items : '{' [ IDENT [ ',' IDENT ]* ]? '}' | '*' ;
path : MOD_SEP? non_global_path ;
non_global_path : IDENT [ MOD_SEP IDENT ]* ;
opt_abis : STR? ;
item_foreign_mod : inner_attr* foreign_item* ;
foreign_item
    : visibility [ STATIC item_foreign_static
                 | [ FN | UNSAFE ] item_foreign_fn
                 ]
    ;
item_foreign_static : MUT? IDENT ':' ty ';' ;
item_foreign_fn : FN IDENT generics? fn_decl_allow_variadic ';' ;

/// 6.1.3 Functions
item_fn : FN IDENT generics? fn_decl inner_attrs_and_block ;
inner_attrs_and_block : '{' inner_attr* block_tail '}' ;
generics : '<' lifetime_or_ty_param_list '>' ;
lifetime_or_ty_param_list : LIFETIME [ ',' lifetime_or_ty_param_list ]? | ty_param_list ;
ty_param_list : ty_param [ ',' ty_param ]* ;
lifetimes : LIFETIME [ ',' LIFETIME ]* ;
fn_decl : fn_args ret_ty ;
fn_decl_allow_variadic : fn_args_allow_variadic ret_ty ;
fn_args : '(' [ arg_general [ ',' arg_general ]* ]? ')' ;
fn_args_allow_variadic :  '(' [ arg_general fn_args_allow_variadic_tail? ]? ')' ;
fn_args_allow_variadic_tail : ',' [ arg_general fn_args_allow_variadic_tail? | DOTDOTDOT ] ;
arg_general : pat ':' ty ;
ret_ty : [ RARROW [ '!' | ty ] ]? ;

/// 6.1.4 Type Definitions
item_type : TYPE IDENT generics? '=' ty ';' ;

/// 6.1.5 Structures
item_struct
    : STRUCT IDENT generics? [ '{' struct_decl_field '}'
                             | '(' [ struct_tuple_field ]? ')' ';'
                             | ';'
                             ]
    ;
struct_decl_field : outer_attr* visibility IDENT ':' ty struct_decl_field_sep? ;
struct_decl_field_sep : ',' struct_decl_field? ;
struct_tuple_field : outer_attr* ty struct_tuple_field_sep? ;
struct_tuple_field_sep : ',' struct_tuple_field? ;

/// 6.1.6 Enumerations
item_enum : ENUM IDENT generics? '{' enum_def? '}' ;
enum_def
    : outer_attr* visibility IDENT [ '{' struct_decl_field '}'
                                   | '(' ty [ ',' ty ]* ')'
                                   | '=' expr
                                   ]? enum_def_sep?
    ;
enum_def_sep : ',' enum_def? ;

/// 6.1.7 Static items
item_const : STATIC MUT? IDENT ':' ty '=' expr ';' ;

/// 6.1.8 Traits
item_trait : TRAIT IDENT generics? [ ':' trait_ref_list ]? trait_methods ;
trait_ref_list : trait_ref [ '+' trait_ref ]* ;
trait_ref : path_lifetime_and_types_without_colons ;
trait_methods : '{' outer_attr* visibility UNSAFE? FN IDENT generics? fn_decl [ ';' | inner_attrs_and_block ] ;

/// 6.1.9 Implementations
item_impl : IMPL generics? ty [ FOR ty ]? '{' inner_attr* method* '}' ;
method : outer_attr* visibility UNSAFE? FN IDENT generics? fn_decl inner_attrs_and_block ;

/// 6.1.10 External blocks
extern_block_item
    : EXTERN [ CRATE item_extern_crate
             | opt_abis [ item_fn
                        | '{' item_foreign_mod '}'
                        ]
             ]
    ;

/// 6.3 Attributes
inner_attr : SHEBANG '[' meta_item ']' ;
outer_attr : '#' '[' meta_item ']' ;
meta_item : IDENT [ '=' lit | meta_seq ]? ;
meta_seq : '(' [ meta_item [ ',' meta_item ]* ]? ')' ;

// 7. Statements and expressions
/// 7.1 Statements
// ambiguity: item_or_view_item and expr can both start with UNSAFE
// also there should be macro here, but that's ambiguous with bottom_expr's macro
stmt
    : let
    | item_or_view_item
    | expr
    ;
let : LET pat [ ':' ty ]? [ '=' expr ]? ;

/// 7.2 Expressions
expr : binops [ [ '=' | BINOPEQ ] expr ]? ;
bottom_expr
    : '(' [ ')' | expr [ ',' expr ]* ')' ]
    | /* UNSAFE */ block // ambiguity: UNSAFE could start "unsafe fn" in item_or_view_item
//    | lambda_expr // ambiguity with the trailing expr
//    | proc_decl expr // ambiguity with the trailing expr
    | SELF
    | if_expr
    | while_expr
    | [ LIFETIME ':' ]? [ for_expr | loop_expr ]
    | continue_expr
    | match_expr
    | vec_expr
    | return_expr
    | break_expr
    | path [ // struct_expr? // ambiguity with blocks
           | '!' macro
           ]
    | lit_no_unit // regular 'lit' includes unit expr which is handled above
    ;

/// 7.2.4 Structure expressions
struct_expr : '{' field_inits default_field_init '}' ;
field_inits : field_init [ ',' field_init ]* ;
field_init : MUT? IDENT ':' expr ;
default_field_init : [ ',' [ DOTDOT expr ]? ]? ;

/// 7.2.5 Block expressions
block : '{' block_tail '}' ;
block_tail : [ stmt? ';' ]* ;

/// 7.2.6 Method-call expressions
dot_or_call_expr : bottom_expr dot_or_call* ;
dot_or_call
    : '.' IDENT [ MOD_SEP generics ]?
    | '(' expr [ ',' expr ]* ')'
    | '[' expr ']'
    ;

/// 7.2.7 Field expressions - handled by dot_or_call_expr
/// 7.2.8 Vector expressions
vec_expr : '[' [ ']' | expr [ ',' [ DOTDOT expr | vec_elems ] ]? ']' ] ;
vec_elems : expr vec_elems1? ;
vec_elems1 : ',' vec_elems? ;

/// 7.2.9 Index expressions - handled by dot_or_call_expr
/// 7.2.10 Unary operator expressions
prefix_expr
    : '!' prefix_expr
    | '-' prefix_expr
    | '*' prefix_expr
    | '&' /* LIFETIME? */ MUT? prefix_expr // ambiguity: lifetime after & may be a loop label
    | '~' prefix_expr
    | BOX prefix_expr // ambiguity: what to do with box ( expr? )?
    | dot_or_call_expr
    ;

/// 7.2.11 binary operator expressions
binops : expr_1 ;
expr_1 : expr_2 prec_1_binop? ;
prec_1_binop : OROR expr_2 prec_1_binop? ;
expr_2 : expr_3 prec_2_binop? ;
prec_2_binop : ANDAND expr_3 prec_2_binop? ;
expr_3 : expr_4 prec_3_binop? ;
prec_3_binop : [ EQEQ | NE ] expr_4 prec_3_binop? ;
expr_4 : expr_6 prec_4_binop? ;
prec_4_binop : [ '<' | LE | GE | '>' ] expr_6 prec_4_binop? ;
expr_6 : expr_7 prec_6_binop? ;
prec_6_binop : '|' expr_7 prec_6_binop? ;
expr_7 : expr_8 prec_7_binop? ;
prec_7_binop : '^' expr_8 prec_7_binop? ;
expr_8 : expr_9 prec_8_binop? ;
prec_8_binop : '&' expr_9 prec_8_binop? ;
expr_9 : expr_10 prec_9_binop? ;
prec_9_binop : [ SHL | SHR ] expr_10 prec_9_binop? ;
expr_10 : expr_11 prec_10_binop? ;
prec_10_binop : [ '+' | '-' ] expr_11 prec_10_binop? ;
expr_11 : expr_12 prec_11_binop? ;
prec_11_binop : [ '*' | '/' | '%' ] expr_12 prec_11_binop? ;
expr_12 : prefix_expr prec_12_binop? ;
prec_12_binop : AS prefix_expr prec_12_binop? ;

/// 7.2.12 Grouped expressions - handled in bottom_expr
/// 7.2.13 Call expressions - handled in dot_or_call_expr
/// 7.2.14 Lambda expressions
lambda_expr : fn_block_decl expr ;
fn_block_decl : [ '|' fn_block_arg [ ',' fn_block_arg ]* '|' | OROR ] [ RARROW ty ]? ;
fn_block_arg : pat [ ':' ty ]? ;
proc_decl : PROC '(' [ fn_block_arg [ ',' fn_block_arg ]* ]? ')' [ RARROW ty ]? ;

/// 7.2.15 While loops
while_expr : WHILE expr block ;

/// 7.2.16 Infinite loops
loop_expr : LOOP UNSAFE? block ;

/// 7.2.17 Break expressions
break_expr : BREAK LIFETIME? ;

/// 7.2.18 Continue expressions
continue_expr : CONTINUE LIFETIME? ;

/// 7.2.19 For expressions
for_expr : FOR pat IN expr block ;

/// 7.2.20 If expressions
if_expr : IF expr block [ ELSE [ if_expr | block ] ]? ;

/// 7.2.21 Match expressions
match_expr : MATCH expr '{' match_arm [ ',' match_arm ]* '}' ;
match_arm : pat [ IF expr ]? FAT_ARROW expr ;
pat
    : UNDERSCORE
    | '~' pat
    | '&' pat
    | '(' [ pat [ ',' pat]* ]? ')'
    | '[' pat_vec_elements ']'
    | '-'? number // [ DOTDOT [ path | literal_maybe_minus ] ]? // ambiguity here with ident patterns
    | REF? MUT? IDENT [ '@' pat ]?
    // can_be_enum_or_struct ... ( [ < { ::
    ;
pat_vec_elements : pat [ [ ',' pat_vec_elements ]? | DOTDOT IDENT [ ',' pat_vec_elements_no_slice ]? ] ;
pat_vec_elements_no_slice : pat [ ',' pat_vec_elements_no_slice ]? ;

/// 7.2.22 Return expressions
return_expr : RETURN /* expr? */ ; // ambiguity with trailing expr?

// 8.1 Types
ty
    : '(' [ ')' | ty [ ',' ty ]+ ')' ]
    | '~' ty
    | '*' MUT? ty
    | '[' ty fixed_vstore ']'
    | '&' /* LIFETIME? */ MUT? ty // ambiguity: LIFETIME could be start of ty_closure
    | ty_bare_fn
//    | ty_closure // cannot coexist with ty_bare_fn here, both can start with UNSAFE
    | TYPEOF '(' expr ')'
    | proc_type
    | path
    | '_'
    ;

/// 8.1.3 Tuple types - handled in ty
/// 8.1.4 Vector types
fixed_vstore : [ ',' DOTDOT expr ]? ;

/// 8.1.8 Pointer types - handled in ty
/// 8.1.9 Function types
ty_bare_fn : [ EXTERN opt_abis ]? UNSAFE? FN ty_fn_decl ;
ty_fn_decl : [ '<' lifetimes '>' ]? fn_args ret_ty ;

/// 8.1.10 Closure types
ty_closure : UNSAFE? ONCE? '<' lifetimes '>' [ OROR | '|' arg_general [ ',' arg_general ]* '|' ] ret_ty ;
proc_type : PROC [ '<' lifetimes '>' ]? fn_args bounds? ret_ty ;

/// Macros

// path '!' is already parsed
macro : parendelim | bracedelim ;
delimited : parendelim | bracketdelim | bracedelim ;
parendelim : '(' token_tree ')' ;
bracketdelim : '[' token_tree ']' ;
bracedelim : '{' token_tree '}' ;
token_tree : [ non_delimiter | delimited ]* ;
non_delimiter : '=' | '<' | '>' | '.' | '~' | ',' | ';' | ':' | '#' | '$'
    | LE | EQEQ | NE | GE | ANDAND | OROR | BINOPEQ | DOTDOT
    | DOTDOTDOT | MOD_SEP | RARROW | FAT_ARROW | LIT_CHAR | LIT_INT
    | LIT_INT_UNSUFFIXED | LIT_FLOAT | LIT_FLOAT_UNSUFFIXED | LIT_STR
    | LIT_STR_RAW | IDENT | UNDERSCORE | LIFETIME | SELF | STATIC | AS
    | BREAK | CRATE | ELSE | ENUM | EXTERN | FALSE | FN | FOR
    | IF | IMPL | IN | LET | LOOP | MATCH | MOD | MUT | ONCE | PRIV
    | PUB | REF | RETURN | SHL | SHR | STRUCT | TRUE | TRAIT | TYPE
    | UNSAFE | USE | WHILE | CONTINUE | PROC | BOX | TYPEOF
    ;

/// Utility rules
STR : LIT_STR | LIT_STR_RAW ;
lit : TRUE | FALSE | token_lit ;
number : LIT_INT | LIT_UINT | LIT_INT_UNSUFFIXED | LIT_FLOAT | LIT_FLOAT_UNSUFFIXED ;
token_lit
    : LIT_CHAR
    | LIT_INT
    | LIT_UINT
    | LIT_INT_UNSUFFIXED
    | LIT_FLOAT
    | LIT_FLOAT_UNSUFFIXED
    | LIT_STR
    | LIT_STR_RAW
    | '(' ')'
    ;
lit_no_unit
    : TRUE
    | FALSE
    | LIT_CHAR
    | LIT_INT
    | LIT_UINT
    | LIT_INT_UNSUFFIXED
    | LIT_FLOAT
    | LIT_FLOAT_UNSUFFIXED
    | LIT_STR
    | LIT_STR_RAW
    ;
