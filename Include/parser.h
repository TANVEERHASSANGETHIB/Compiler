#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <iostream>
#include <unordered_map>
#include "regex_lexer.h" // reuse lexer Token and TokenType

// Expanded Parse error kinds
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

    // semantic-ish errors (some may be raised later in a semantic phase)
    TypeMismatch,
    UndeclaredIdentifier,

    // generic
    GenericError
};

struct ParseError {
    ParseErrorKind kind;
    std::optional<Token> token; // token that triggered the error (if available)
    std::string message;

    ParseError() = default;
    ParseError(ParseErrorKind k, std::optional<Token> t, std::string m)
        : kind(k), token(std::move(t)), message(std::move(m)) {}
};

// AST base
struct ASTNode { virtual ~ASTNode() = default; virtual void print(int indent=0) const = 0; };
using ASTNodePtr = std::unique_ptr<ASTNode>;

// --- Types ---
// Simple type node: for now we just store the token text representing the type
struct TypeNode : ASTNode {
    std::string name; // e.g. "int" or user-defined type
    TypeNode(std::string n): name(std::move(n)) {}
    void print(int indent=0) const override;
};

// Expressions
struct Expr : ASTNode { };
using ExprPtr = std::unique_ptr<Expr>;

struct LiteralExpr : Expr {
    std::string value; // lexeme
    LiteralExpr(std::string v): value(std::move(v)) {}
    void print(int indent=0) const override;
};
struct IdentExpr : Expr {
    std::string name;
    IdentExpr(std::string n): name(std::move(n)) {}
    void print(int indent=0) const override;
};
struct UnaryExpr : Expr {
    Token op;
    ExprPtr right;
    UnaryExpr(Token o, ExprPtr r): op(o), right(std::move(r)) {}
    void print(int indent=0) const override;
};
struct BinaryExpr : Expr {
    ExprPtr left;
    Token op;
    ExprPtr right;
    BinaryExpr(ExprPtr l, Token o, ExprPtr r): left(std::move(l)), op(o), right(std::move(r)) {}
    void print(int indent=0) const override;
};
struct CallExpr : Expr {
    ExprPtr callee;
    std::vector<ExprPtr> args;
    CallExpr(ExprPtr c, std::vector<ExprPtr> a): callee(std::move(c)), args(std::move(a)) {}
    void print(int indent=0) const override;
};

// Statements / Decls
struct Stmt : ASTNode { };
using StmtPtr = std::unique_ptr<Stmt>;

struct ExprStmt : Stmt {
    ExprPtr expr;
    ExprStmt(ExprPtr e): expr(std::move(e)) {}
    void print(int indent=0) const override;
};
struct ReturnStmt : Stmt {
    std::optional<ExprPtr> expr;
    ReturnStmt(std::optional<ExprPtr> e): expr(std::move(e)) {}
    void print(int indent=0) const override;
};
struct BlockStmt : Stmt {
    std::vector<StmtPtr> statements;
    BlockStmt(std::vector<StmtPtr> s = {}) : statements(std::move(s)) {}
    void print(int indent=0) const override;
};
struct VarDecl : Stmt {
    std::string name;
    std::optional<TypeNode> typeNode;
    std::optional<ExprPtr> initializer;
    VarDecl(std::string n, std::optional<TypeNode> t, std::optional<ExprPtr> i)
        : name(std::move(n)), typeNode(std::move(t)), initializer(std::move(i)) {}
    void print(int indent=0) const override;
};
struct IfStmt : Stmt {
    ExprPtr condition;
    std::unique_ptr<BlockStmt> thenBranch;
    std::optional<std::unique_ptr<BlockStmt>> elseBranch; // may hold an else block
    IfStmt(ExprPtr c, std::unique_ptr<BlockStmt> t, std::optional<std::unique_ptr<BlockStmt>> e)
        : condition(std::move(c)), thenBranch(std::move(t)), elseBranch(std::move(e)) {}
    void print(int indent=0) const override;
};
struct WhileStmt : Stmt {
    ExprPtr condition;
    std::unique_ptr<BlockStmt> body;
    WhileStmt(ExprPtr c, std::unique_ptr<BlockStmt> b): condition(std::move(c)), body(std::move(b)) {}
    void print(int indent=0) const override;
};
struct ForStmt : Stmt {
    // for(init; cond; step) { body }
    std::optional<StmtPtr> init; // var decl or expression stmt
    std::optional<ExprPtr> condition;
    std::optional<ExprPtr> post;
    std::unique_ptr<BlockStmt> body;
    ForStmt(std::optional<StmtPtr> i, std::optional<ExprPtr> c, std::optional<ExprPtr> p, std::unique_ptr<BlockStmt> b)
        : init(std::move(i)), condition(std::move(c)), post(std::move(p)), body(std::move(b)) {}
    void print(int indent=0) const override;
};
struct BreakStmt : Stmt { void print(int indent=0) const override; };
struct ContinueStmt : Stmt { void print(int indent=0) const override; };

struct FuncParam {
    std::string name;
    TypeNode type;
};

struct FuncDecl : Stmt {
    std::string name;
    std::vector<FuncParam> params; // name, type
    std::optional<TypeNode> retType;
    std::unique_ptr<BlockStmt> body;
    FuncDecl(std::string n,
             std::vector<FuncParam> p,
             std::optional<TypeNode> r,
             std::unique_ptr<BlockStmt> b)
        : name(std::move(n)), params(std::move(p)), retType(std::move(r)), body(std::move(b)) {}
    void print(int indent=0) const override;
};

struct Program : ASTNode {
    std::vector<StmtPtr> items;
    void print(int indent=0) const override;
};

// Parser
class Parser {
public:
    // parser will create an internal RegexLexer utility instance to map token names
    Parser(const std::vector<Token>& tokens);

    // parse entire program; throws ParseError on fatal error
    std::unique_ptr<Program> parse_program();

private:
    const std::vector<Token>& tokens_;
    size_t pos_ = 0;
    mutable RegexLexer lexUtil_; // used for tokenTypeToString mapping

    // helpers
    const Token& peek() const;
    // peek n tokens ahead (1-based: peek_n(1) == peek())
    const Token& peek_n(size_t n) const;
    const Token& previous() const;
    const Token& advance();
    bool is_at_end() const;

    // LL(1/LL(2)) utilities
    bool matchLexeme(const std::string &lexeme); // consume if lexeme matches
    bool matchTypeName(const std::string &typeName); // consume if tokenTypeToString == typeName
    bool matchEither(const std::string &lexOrType); // convenience (lexeme or typeName)
    bool checkEither(const std::string &lexOrType) const;

    // lookahead by n tokens and check lexeme or typeName
    bool matchLexemeN(size_t n, const std::string &lexeme);
    bool matchTypeNameN(size_t n, const std::string &typeName) const;

    // return reference to the token if next token matches, otherwise raise ParseError
    const Token& expectEither(const std::string &lexOrType, ParseErrorKind errKind);

    // error recovery
    void synchronize();

    // parse functions (recursive-descent)
    StmtPtr parse_declaration();
    StmtPtr parse_var_decl_or_stmt();
    std::unique_ptr<FuncDecl> parse_function_decl();
    std::unique_ptr<BlockStmt> parse_block();

    StmtPtr parse_statement();
    StmtPtr parse_return_stmt();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();
    StmtPtr parse_for_stmt();
    StmtPtr parse_break_stmt();
    StmtPtr parse_continue_stmt();

    // expressions (Pratt)
    ExprPtr parse_expression();
    ExprPtr parse_assignment();
    ExprPtr parse_pratt(int min_bp = 0);
    ExprPtr parse_primary();

    // helpers for parsing lists
    std::vector<ExprPtr> parse_argument_list();
    std::vector<StmtPtr> parse_statement_list();

    // precedence table helpers (use lexeme operators)
    int prefix_bp(const std::string &op) const;
    std::pair<int,int> infix_bp(const std::string &op) const;

    // utilities to build specific ParseError instances
    ParseError make_error(ParseErrorKind kind, const Token* t = nullptr, std::string msg = "") const;
    ParseError make_error(ParseErrorKind kind, const Token& t, std::string msg = "") const;

    // optional (simple) symbol table helpers used for limited semantic checks
    // (these are helpers only; full semantic analysis belongs in a separate phase)
    std::unordered_map<std::string,int> local_scope_count_; // simple duplicate-decl detector per-parse (not full scoping)

};

// End of header
