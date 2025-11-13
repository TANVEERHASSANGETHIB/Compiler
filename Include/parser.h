#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <iostream>
#include <unordered_map>
#include <stack>
#include "regex_lexer.h"
#include "scope.h"
#include "scope_errors.h"

using namespace std;

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
    MissingSemicolon,
    MismatchedParenthesis,
    MismatchedBrace,
    MismatchedBracket,
    DuplicateDeclaration,
    MissingFunctionBody,
    InvalidParameterList,
    ExpectedOperand,
    ExpectedOperator,
    InvalidAssignmentTarget,
    FunctionArgMismatch,
    UndefinedFunctionCalled,
    MisplacedReturn,
    MisplacedBreakContinue,
    TypeMismatch,
    UndeclaredIdentifier,
    GenericError
};

struct ParseError {
    ParseErrorKind kind;
    optional<Token> token;
    string message;
    ParseError() = default;
    ParseError(ParseErrorKind k, optional<Token> t, string m)
        : kind(k), token(move(t)), message(move(m)) {}
};

struct ASTNode {
    virtual ~ASTNode() = default;
    virtual void print(int indent=0) const = 0;
    optional<Token> origin;
};

using ASTNodePtr = unique_ptr<ASTNode>;

struct TypeNode : ASTNode {
    string name;
    TypeNode(string n) : name(move(n)) {}
    void print(int indent=0) const override;
};

struct Expr : ASTNode { };
using ExprPtr = unique_ptr<Expr>;

struct LiteralExpr : Expr {
    string value;
    LiteralExpr(string v) : value(move(v)) {}
    void print(int indent=0) const override;
};

struct IdentExpr : Expr {
    string name;
    IdentExpr(string n) : name(move(n)) {}
    void print(int indent=0) const override;
};

struct UnaryExpr : Expr {
    Token op;
    ExprPtr right;
    UnaryExpr(Token o, ExprPtr r) : op(o), right(move(r)) {}
    void print(int indent=0) const override;
};

struct PostfixExpr : Expr {
    ExprPtr left;
    Token op;
    PostfixExpr(ExprPtr l, Token o) : left(move(l)), op(o) {}
    void print(int indent=0) const override;
};

struct BinaryExpr : Expr {
    ExprPtr left;
    Token op;
    ExprPtr right;
    BinaryExpr(ExprPtr l, Token o, ExprPtr r) : left(move(l)), op(o), right(move(r)) {}
    void print(int indent=0) const override;
};

struct CallExpr : Expr {
    ExprPtr callee;
    vector<ExprPtr> args;
    CallExpr(ExprPtr c, vector<ExprPtr> a) : callee(move(c)), args(move(a)) {}
    void print(int indent=0) const override;
};

struct MemberExpr : Expr {
    ExprPtr object;
    string member;
    Token op;
    MemberExpr(ExprPtr o, string m, Token oper) : object(move(o)), member(move(m)), op(oper) {}
    void print(int indent=0) const override;
};

struct IndexExpr : Expr {
    ExprPtr object;
    ExprPtr index;
    IndexExpr(ExprPtr o, ExprPtr i) : object(move(o)), index(move(i)) {}
    void print(int indent=0) const override;
};

struct Stmt : ASTNode { };
using StmtPtr = unique_ptr<Stmt>;

struct ExprStmt : Stmt {
    ExprPtr expr;
    ExprStmt(ExprPtr e) : expr(move(e)) {}
    void print(int indent=0) const override;
};

struct ReturnStmt : Stmt {
    optional<ExprPtr> expr;
    ReturnStmt(optional<ExprPtr> e) : expr(move(e)) {}
    void print(int indent=0) const override;
};

struct BlockStmt : Stmt {
    vector<StmtPtr> statements;
    BlockStmt(vector<StmtPtr> s = {}) : statements(move(s)) {}
    void print(int indent=0) const override;
};

struct VarDecl : Stmt {
    string name;
    optional<TypeNode> typeNode;
    optional<ExprPtr> initializer;
    VarDecl(string n, optional<TypeNode> t, optional<ExprPtr> i)
        : name(move(n)), typeNode(move(t)), initializer(move(i)) {}
    void print(int indent=0) const override;
};

struct IfStmt : Stmt {
    ExprPtr condition;
    unique_ptr<BlockStmt> thenBranch;
    unique_ptr<BlockStmt> elseBranch;
    IfStmt(ExprPtr c, unique_ptr<BlockStmt> t, unique_ptr<BlockStmt> e = nullptr)
        : condition(move(c)), thenBranch(move(t)), elseBranch(move(e)) {}
    void print(int indent=0) const override;
};

struct WhileStmt : Stmt {
    ExprPtr condition;
    unique_ptr<BlockStmt> body;
    WhileStmt(ExprPtr c, unique_ptr<BlockStmt> b) : condition(move(c)), body(move(b)) {}
    void print(int indent=0) const override;
};

struct ForStmt : Stmt {
    optional<StmtPtr> init;
    optional<ExprPtr> condition;
    optional<ExprPtr> post;
    unique_ptr<BlockStmt> body;
    ForStmt(optional<StmtPtr> i, optional<ExprPtr> c, optional<ExprPtr> p, unique_ptr<BlockStmt> b)
        : init(move(i)), condition(move(c)), post(move(p)), body(move(b)) {}
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
    FuncDecl(string n, vector<FuncParam> p, optional<TypeNode> r, unique_ptr<BlockStmt> b)
        : name(move(n)), params(move(p)), retType(move(r)), body(move(b)) {}
    void print(int indent=0) const override;
};

struct Program : ASTNode {
    vector<StmtPtr> items;
    void print(int indent=0) const override;
};

class Parser {
public:
    Parser(const vector<Token>& tokens);
    unique_ptr<Program> parse_program();
    const vector<ParseError>& get_errors() const { return errors_; }
    ScopeStack& get_scope_stack() { return scopeStack_; }

private:
    const vector<Token>& tokens_;
    size_t pos_ = 0;
    mutable RegexLexer lexUtil_;
    ScopeStack scopeStack_;
    int function_depth_ = 0;
    int loop_depth_ = 0;
    vector<ParseError> errors_;

    const Token& peek() const;
    const Token& peek_n(size_t n) const;
    const Token& previous() const;
    const Token& advance();
    bool is_at_end() const;

    bool matchType(TokenType t);
    bool matchAnyType(const initializer_list<TokenType>& types);
    bool checkType(TokenType t) const;
    bool checkEitherLexemeOrType(const string &lexOrType) const;

    bool matchLexeme(const string &lexeme);
    bool matchEither(const string &lexOrType);
    bool checkEither(const string &lexOrType) const;

    bool checkTypeN(size_t n, TokenType t) const;

    const Token& expectType(TokenType t, ParseErrorKind errKind);
    const Token& expectEither(const string &lexOrType, ParseErrorKind errKind);

    void push_scope(const string& name = "");
    void pop_scope();
    void declare_variable(const string& name, TokenType dataType = T_INT, int line = 0, int col = 0);
    void declare_function(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line = 0, int col = 0);
    void declare_parameter(const string& name, TokenType dataType = T_INT, int line = 0, int col = 0);
    
    void check_variable_access(const string& name, const Token& token);
    void check_function_call(const string& name, const vector<ExprPtr>& args, const Token& token);
    bool is_declared_in_current_scope(const string& name) const;

    void enter_function();
    void exit_function();
    void enter_loop();
    void exit_loop();

    void synchronize();
    void report_error(ParseErrorKind kind, const Token* token = nullptr, string msg = "");
    void report_error(ParseErrorKind kind, const Token& token, string msg = "");
    void check_for_common_typos(const Token& token);

    // helper to consistently consume semicolons (avoids double-advance bug)
    bool consumeSemicolon();

    StmtPtr parse_declaration();
    StmtPtr parse_var_decl_or_stmt();
    StmtPtr parse_function_decl();
    // header-based function continuation (parses after type and name already consumed)
    StmtPtr parse_function_decl_after_header(const Token& typeTok, const Token& nameTok);
    unique_ptr<BlockStmt> parse_block();
    StmtPtr parse_statement();
    StmtPtr parse_return_stmt();
    StmtPtr parse_if_stmt();
    StmtPtr parse_while_stmt();
    StmtPtr parse_for_stmt();
    StmtPtr parse_break_stmt();
    StmtPtr parse_continue_stmt();

    ExprPtr parse_expression();
    ExprPtr parse_assignment();
    ExprPtr parse_pratt(int min_bp = 0);
    ExprPtr parse_primary();
    vector<ExprPtr> parse_argument_list();

    int prefix_bp_by_lexeme(const string &op) const;
    int prefix_bp_by_type(TokenType t) const;
    pair<int,int> infix_bp_by_lexeme(const string &op) const;
    pair<int,int> infix_bp_by_type(TokenType t) const;

    TokenType typeNameToTokenType(const string& typeName) const;
    ParseError make_error(ParseErrorKind kind, const Token* t = nullptr, string msg = "") const;
    ParseError make_error(ParseErrorKind kind, const Token& t, string msg = "") const;
    // Add the missing overload for nullopt
    ParseError make_error(ParseErrorKind kind, std::nullopt_t, string msg = "") const {
        return ParseError{kind, std::nullopt, std::move(msg)};
    }
};
