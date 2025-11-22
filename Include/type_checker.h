#pragma once
#include "parser.h"
#include <variant>
#include <stdexcept>
#include <vector>

enum class TypeChkError {
    ErroneousVarDecl,
    FnCallParamCount,
    FnCallParamType,
    ErroneousReturnType,
    ExpressionTypeMismatch,
    ExpectedBooleanExpression,
    ErroneousBreak,
    NonBooleanCondStmt,
    EmptyExpression,
    AttemptedBoolOpOnNonBools,
    AttemptedBitOpOnNonNumeric,
    AttemptedShiftOnNonInt,
    AttemptedAddOpOnNonNumeric,
    AttemptedExponentiationOfNonNumeric,
    ReturnStmtNotFound,
    MissingReturnInMain, // NEW: Specific error for main function
};

class TypeCheckException : public std::runtime_error {
public:
    TypeChkError kind;
    std::optional<Token> token;
    TypeCheckException(TypeChkError k, const std::string& msg, std::optional<Token> t = std::nullopt)
        : std::runtime_error(msg), kind(k), token(t) {}
};

struct Type {
    TokenType tokenType;
    bool isError = false;

    Type(TokenType tt = T_INT) : tokenType(tt) {}
    static Type Error() { Type t; t.isError = true; return t; }

    bool isNumeric() const { return tokenType == T_INT || tokenType == T_FLOAT; }
    bool isInt() const { return tokenType == T_INT; }
    bool isFloat() const { return tokenType == T_FLOAT; }
    bool isBool() const { return tokenType == T_BOOL; }
    bool isString() const { return tokenType == T_STRING; }

    bool operator==(const Type& other) const { return tokenType == other.tokenType; }
    bool operator!=(const Type& other) const { return !(*this == other); }
};

class TypeChecker {
    ScopeStack& scopeStack_;
    std::stack<TokenType> currentFunctionReturnType_;
    int loopDepth_ = 0;
    std::vector<TypeCheckException> errors_;

    Type checkExpr(const Expr* expr);
    Type checkLiteral(const LiteralExpr* lit);
    Type checkIdent(const IdentExpr* ident);
    Type checkUnary(const UnaryExpr* unary);
    Type checkBinary(const BinaryExpr* bin);
    Type checkCall(const CallExpr* call);
    Type checkPostfix(const PostfixExpr* postfix);
    Type checkMember(const MemberExpr* member);
    Type checkIndex(const IndexExpr* index);

    Type checkBinaryHelper(const Type& left, const Type& right, const std::string& op, const Token& token);

    void checkStmt(const Stmt* stmt);
    void checkBlock(const BlockStmt* block);
    void checkVarDecl(const VarDecl* decl);
    void checkFuncDecl(const FuncDecl* decl);
    void checkReturn(const ReturnStmt* ret);
    void checkIf(const IfStmt* ifstmt);
    void checkWhile(const WhileStmt* whilestmt);
    void checkFor(const ForStmt* forstmt);
    void checkBreakContinue(const ASTNode* node, TypeChkError errorType);
    
    // NEW: Helper functions for better return statement checking
    bool checkForReturnStatements(const BlockStmt* block);
    bool hasAllPathsReturn(const BlockStmt* block);

    TokenType typeNodeToTokenType(const TypeNode* tn) const;
    void reportError(TypeChkError kind, const std::string& msg, std::optional<Token> token = std::nullopt);

public:
    TypeChecker(ScopeStack& scopeStack) : scopeStack_(scopeStack) {}
    bool check(const Program* program);
    const std::vector<TypeCheckException>& get_errors() const { return errors_; }
    void clear_errors() { errors_.clear(); }
};