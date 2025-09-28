#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <iostream>
#include <unordered_map>
#include <stack>
#include "regex_lexer.h"
using namespace std;

/*
  Revised parser.h
  - More AST node kinds (Member, Index, Postfix)
  - AST nodes optionally hold the token that produced them for diagnostics
  - Scope stack + context flags in Parser
  - Cleaner parse API
  - Added support for C-style function declarations:
      - `int fname(int x, int y) { ... }`
    by introducing parse_function_decl_after_header(...) which is called
    when the parser sees `type identifier` followed by '('.
*/

enum class ParseErrorKind {
    // core/token expectation errors
    UnexpectedEOF,
    FailedToFindToken,
    ExpectedTypeToken,
    ExpectedIdentifier,
    UnexpectedToken,
    ExpectedFloatLit,
    ExpectedIntLit,
    ExpectedStringLit,
    ExpectedBoolLit,
    ExpectedExpr,

    // structural / syntax errors
    MissingSemicolon,
    MismatchedParenthesis,
    MismatchedBrace,
    MismatchedBracket,

    // declaration / parameter errors
    DuplicateDeclaration,
    MissingFunctionBody,
    InvalidParameterList,

    // expression / operator errors
    ExpectedOperand,
    ExpectedOperator,
    InvalidOperatorUsage,
    InvalidAssignmentTarget,
    FunctionArgMismatch,

    // control flow errors
    MisplacedReturn,
    MisplacedBreakContinue,

    // semantic-ish errors (to be raised later ideally)
    TypeMismatch,
    UndeclaredIdentifier,

    // generic
    GenericError
};

struct ParseError {
    ParseErrorKind kind;
    optional<Token> token; // token that triggered the error (if available)
    string message;

    ParseError() = default;
    ParseError(ParseErrorKind k,  optional<Token> t,  string m)
        : kind(k), token( move(t)), message( move(m)) {}
};

// ---------------- AST base ----------------
struct ASTNode {
    virtual ~ASTNode() = default;
    virtual void print(int indent=0) const = 0;

    // optional token for diagnostics (may be empty)
    optional<Token> origin;
};

// Convenience aliases
using ASTNodePtr =  unique_ptr<ASTNode>;

// ---------------- Types ----------------
struct TypeNode : ASTNode {
    string name; // e.g. "int", "string", or user-defined
    TypeNode( string n) : name( move(n)) {}
    void print(int indent=0) const override;
};

// ---------------- Expressions ----------------
struct Expr : ASTNode { };
using ExprPtr =  unique_ptr<Expr>;

struct LiteralExpr : Expr {
    string value; // lexeme or normalized literal text
    LiteralExpr( string v) : value( move(v)) {}
    void print(int indent=0) const override;
};

struct IdentExpr : Expr {
    string name;
    IdentExpr( string n) : name( move(n)) {}
    void print(int indent=0) const override;
};

// Unary prefix (e.g., -x, !x, *p)
struct UnaryExpr : Expr {
    Token op;
    ExprPtr right;
    UnaryExpr(Token o, ExprPtr r) : op(o), right( move(r)) {}
    void print(int indent=0) const override;
};

// Postfix (e.g., x++, x--)
struct PostfixExpr : Expr {
    ExprPtr left;
    Token op;
    PostfixExpr(ExprPtr l, Token o) : left( move(l)), op(o) {}
    void print(int indent=0) const override;
};

// Binary operator expression (e.g., a + b, a << b)
struct BinaryExpr : Expr {
    ExprPtr left;
    Token op;
    ExprPtr right;
    BinaryExpr(ExprPtr l, Token o, ExprPtr r) : left( move(l)), op(o), right( move(r)) {}
    void print(int indent=0) const override;
};

// Function call: callee(args...)
struct CallExpr : Expr {
    ExprPtr callee;
    vector<ExprPtr> args;
    CallExpr(ExprPtr c,  vector<ExprPtr> a) : callee( move(c)), args( move(a)) {}
    void print(int indent=0) const override;
};

// Member access: obj.field
struct MemberExpr : Expr {
    ExprPtr object;
    string member; // identifier (the field name)
    Token op; // token for '.' or '->' if useful
    MemberExpr(ExprPtr o,  string m, Token oper) : object( move(o)), member( move(m)), op(oper) {}
    void print(int indent=0) const override;
};

// Indexing: arr[index]
struct IndexExpr : Expr {
    ExprPtr object;
    ExprPtr index;
    IndexExpr(ExprPtr o, ExprPtr i) : object( move(o)), index( move(i)) {}
    void print(int indent=0) const override;
};

// ---------------- Statements / Declarations ----------------
struct Stmt : ASTNode { };
using StmtPtr =  unique_ptr<Stmt>;

struct ExprStmt : Stmt {
    ExprPtr expr;
    ExprStmt(ExprPtr e) : expr( move(e)) {}
    void print(int indent=0) const override;
};

struct ReturnStmt : Stmt {
    optional<ExprPtr> expr;
    ReturnStmt( optional<ExprPtr> e) : expr( move(e)) {}
    void print(int indent=0) const override;
};

struct BlockStmt : Stmt {
    vector<StmtPtr> statements;
    BlockStmt( vector<StmtPtr> s = {}) : statements( move(s)) {}
    void print(int indent=0) const override;
};

struct VarDecl : Stmt {
    string name;
    optional<TypeNode> typeNode;
    optional<ExprPtr> initializer;
    VarDecl( string n,  optional<TypeNode> t,  optional<ExprPtr> i)
        : name( move(n)), typeNode( move(t)), initializer( move(i)) {}
    void print(int indent=0) const override;
};

struct IfStmt : Stmt {
    ExprPtr condition;
    unique_ptr<BlockStmt> thenBranch;
    // elseBranch is nullable; use nullptr if absent
    unique_ptr<BlockStmt> elseBranch;
    IfStmt(ExprPtr c,  unique_ptr<BlockStmt> t,  unique_ptr<BlockStmt> e = nullptr)
        : condition( move(c)), thenBranch( move(t)), elseBranch( move(e)) {}
    void print(int indent=0) const override;
};

struct WhileStmt : Stmt {
    ExprPtr condition;
    unique_ptr<BlockStmt> body;
    WhileStmt(ExprPtr c,  unique_ptr<BlockStmt> b) : condition( move(c)), body( move(b)) {}
    void print(int indent=0) const override;
};

struct ForStmt : Stmt {
    // for(init; cond; post) { body }
    // init may be variable decl or expression statement (or null)
    optional<StmtPtr> init;
    optional<ExprPtr> condition;
    optional<ExprPtr> post;
    unique_ptr<BlockStmt> body;
    ForStmt( optional<StmtPtr> i,  optional<ExprPtr> c,  optional<ExprPtr> p,  unique_ptr<BlockStmt> b)
        : init( move(i)), condition( move(c)), post( move(p)), body( move(b)) {}
    void print(int indent=0) const override;
};

struct BreakStmt : Stmt { void print(int indent=0) const override; };
struct ContinueStmt : Stmt { void print(int indent=0) const override; };

struct FuncParam {
    string name;
    TypeNode type;
};

struct FuncDecl : Stmt {
    string name;
    vector<FuncParam> params;
    optional<TypeNode> retType;
    unique_ptr<BlockStmt> body;
    FuncDecl( string n,  vector<FuncParam> p,  optional<TypeNode> r,  unique_ptr<BlockStmt> b)
        : name( move(n)), params( move(p)), retType( move(r)), body( move(b)) {}
    void print(int indent=0) const override;
};

struct Program : ASTNode {
    vector<StmtPtr> items;
    void print(int indent=0) const override;
};

// ---------------- Parser interface ----------------
class Parser {
public:
    // constructor uses tokens from lexer
    Parser(const  vector<Token>& tokens);

    // parse whole program (returns AST root). Caller handles exceptions / ParseError.
    unique_ptr<Program> parse_program();

private:
    const  vector<Token>& tokens_;
    size_t pos_ = 0;
    mutable RegexLexer lexUtil_; // for token name mapping and debug printing

    // ---------- Parser context & scopes ----------
    // stack of scopes; each scope maps identifier -> count (for duplicate checks)
    vector< unordered_map< string,int>> scope_stack_;

    // helper counters for context checks
    int function_depth_ = 0; // >0 inside functions
    int loop_depth_ = 0;     // >0 inside loops

    // ------------- helpers -------------
    const Token& peek() const;
    const Token& peek_n(size_t n) const; // 1-based
    const Token& previous() const;
    const Token& advance();
    bool is_at_end() const;

    // token checks (prefer token-type checks; optionally lexeme)
    bool matchType(TokenType t);                  // consume if next token.type == t
    bool matchAnyType(const  initializer_list<TokenType>& types);
    bool checkType(TokenType t) const;
    bool checkEitherLexemeOrType(const  string &lexOrType) const; // keep for compatibility

    // lexeme-based functions kept for compatibility
    bool matchLexeme(const  string &lexeme);
    bool matchEither(const  string &lexOrType);
    bool checkEither(const  string &lexOrType) const;

    // lookahead without consuming
    bool checkTypeN(size_t n, TokenType t) const;

    // expect next token to be something, otherwise throw ParseError
    const Token& expectType(TokenType t, ParseErrorKind errKind);
    const Token& expectEither(const  string &lexOrType, ParseErrorKind errKind);

    // --------- scope helpers -----------
    void push_scope();
    void pop_scope();
    void declare_symbol_in_current_scope(const  string &name);
    bool is_declared_in_any_scope(const  string &name) const;
    bool is_declared_in_current_scope(const  string &name) const;

    // --------- context helpers ----------
    void enter_function();   // increment function_depth_
    void exit_function();
    void enter_loop();       // increment loop_depth_
    void exit_loop();

    // ---------- error recovery -----------
    void synchronize();

    // ---------- top-level parsing ----------
    StmtPtr parse_declaration();
    StmtPtr parse_var_decl_or_stmt();

    // Parse a function declared with a leading 'fn' keyword:
    //    fn name(params...) [: ret] { ... }
    StmtPtr parse_function_decl();

    // NEW: Parse a function *after* the parser has already consumed a type token
    // and an identifier (i.e. when encountering: `<type> <ident> ( ... ) { ... }`).
    // This helper makes the parser able to distinguish between a variable decl
    // and a C-style typed function declaration.
    StmtPtr parse_function_decl_after_header(const Token& typeTok, const Token& nameTok);

    unique_ptr<BlockStmt> parse_block();

    // ---------- statements ----------
    StmtPtr parse_statement();
    StmtPtr parse_return_stmt();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();
    StmtPtr parse_for_stmt();
    StmtPtr parse_break_stmt();
    StmtPtr parse_continue_stmt();

    // ---------- expressions (Pratt parser support) ----------
    ExprPtr parse_expression();
    ExprPtr parse_assignment();   // handles = and compound assign
    ExprPtr parse_pratt(int min_bp = 0); // core Pratt loop
    ExprPtr parse_primary();

    // convenience: parse call arg list etc.
    vector<ExprPtr> parse_argument_list();

    // precedence binding functions: accept TokenType and lexeme
    int prefix_bp_by_lexeme(const  string &op) const;
    int prefix_bp_by_type(TokenType t) const;
    pair<int,int> infix_bp_by_lexeme(const  string &op) const;
    pair<int,int> infix_bp_by_type(TokenType t) const;

    // error constructors
    ParseError make_error(ParseErrorKind kind, const Token* t = nullptr,  string msg = "") const;
    ParseError make_error(ParseErrorKind kind, const Token& t,  string msg = "") const;
};
