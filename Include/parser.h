#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <iostream>
#include "regex_lexer.h" // reuse lexer Token and TokenType

// Parse error kinds requested
enum class ParseErrorKind {
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
};

struct ParseError {
    ParseErrorKind kind;
    std::optional<Token> token;
    std::string message;
};

// AST base
struct ASTNode { virtual ~ASTNode() = default; virtual void print(int indent=0) const = 0; };
using ASTNodePtr = std::unique_ptr<ASTNode>;

// Expressions
struct Expr : ASTNode { };
using ExprPtr = std::unique_ptr<Expr>;

struct LiteralExpr : Expr {
    std::string value;
    // store the lexeme and let print show it
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
    std::optional<std::string> typeName;
    std::optional<ExprPtr> initializer;
    VarDecl(std::string n, std::optional<std::string> t, std::optional<ExprPtr> i)
        : name(std::move(n)), typeName(std::move(t)), initializer(std::move(i)) {}
    void print(int indent=0) const override;
};
struct FuncDecl : Stmt {
    std::string name;
    std::vector<std::pair<std::string,std::string>> params; // name, type
    std::optional<std::string> retType;
    std::unique_ptr<BlockStmt> body;
    FuncDecl(std::string n,
             std::vector<std::pair<std::string,std::string>> p,
             std::optional<std::string> r,
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
    std::unique_ptr<Program> parse_program();

private:
    const std::vector<Token>& tokens_;
    size_t pos_ = 0;
    mutable RegexLexer lexUtil_; // used for tokenTypeToString mapping

    // helpers
    const Token& peek() const;
    const Token& previous() const;
    const Token& advance();
    bool is_at_end() const;
    bool matchLexeme(const std::string &lexeme); // check by lexeme e.g. "(" or "fn"
    bool matchTypeName(const std::string &typeName); // check tokenTypeToString == typeName
    bool matchEither(const std::string &lexOrType); // convenience (lexeme or typeName)
    bool checkEither(const std::string &lexOrType) const;
    const Token& expectEither(const std::string &lexOrType, ParseErrorKind errKind);

    // parse functions
    StmtPtr parse_declaration();
    StmtPtr parse_var_decl_or_stmt();
    std::unique_ptr<FuncDecl> parse_function_decl();
    std::unique_ptr<BlockStmt> parse_block();

    StmtPtr parse_statement();
    StmtPtr parse_return_stmt();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();

    // expressions (Pratt)
    ExprPtr parse_expression();
    ExprPtr parse_assignment();
    ExprPtr parse_pratt(int min_bp = 0);
    ExprPtr parse_primary();

    // precedence table helpers (use lexeme operators)
    int prefix_bp(const std::string &op) const;
    std::pair<int,int> infix_bp(const std::string &op) const;

    ParseError make_error(ParseErrorKind kind, const Token* t = nullptr, std::string msg = "") const;
};
