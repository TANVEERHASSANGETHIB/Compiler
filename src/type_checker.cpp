#include "type_checker.h"
#include <iostream>

TokenType TypeChecker::typeNodeToTokenType(const TypeNode* tn) const {
    if (!tn) return T_INT;
    if (tn->name == "int") return T_INT;
    if (tn->name == "float") return T_FLOAT;
    if (tn->name == "bool") return T_BOOL;
    if (tn->name == "string") return T_STRING;
#ifdef T_VOID
    if (tn->name == "void") return T_VOID;
#else
    if (tn->name == "void") return T_INT; // fallback until you add T_VOID to TokenType
#endif
    return T_INT;
}


void TypeChecker::reportError(TypeChkError kind, const std::string& msg, std::optional<Token> token) {
    errors_.emplace_back(kind, msg, token);
}

Type TypeChecker::checkLiteral(const LiteralExpr* lit) {
    const std::string& val = lit->value;
    if (val == "true" || val == "false") return Type(T_BOOL);
    if (val.front() == '"' || val.front() == '\'') return Type(T_STRING);
    if (val.find('.') != std::string::npos || val.find('e') != std::string::npos || val.find('E') != std::string::npos) {
        return Type(T_FLOAT);
    }
    return Type(T_INT);
}

Type TypeChecker::checkIdent(const IdentExpr* ident) {
    const Symbol* sym = scopeStack_.lookup(ident->name);
    if (!sym) {
        reportError(TypeChkError::ExpressionTypeMismatch,
            "Use of undeclared identifier '" + ident->name + "'", ident->origin);
        return Type::Error();
    }
    return Type(sym->dataType);
}

Type TypeChecker::checkUnary(const UnaryExpr* unary) {
    Type right = checkExpr(unary->right.get());
    if (right.isError) return Type::Error();

    std::string op = unary->op.value;
    if (op == "!" || op == "not") {
        if (!right.isBool()) {
            reportError(TypeChkError::AttemptedBoolOpOnNonBools,
                "Boolean operator '!' applied to non-boolean", unary->op);
            return Type::Error();
        }
        return Type(T_BOOL);
    }
    if (op == "-" || op == "+") {
        if (!right.isNumeric()) {
            reportError(TypeChkError::AttemptedAddOpOnNonNumeric,
                "Unary '" + op + "' on non-numeric type", unary->op);
            return Type::Error();
        }
        return right;
    }
    if (op == "~") {
        if (!right.isInt()) {
            reportError(TypeChkError::AttemptedBitOpOnNonNumeric,
                "Bitwise NOT '~' on non-integer", unary->op);
            return Type::Error();
        }
        return Type(T_INT);
    }
    reportError(TypeChkError::ExpressionTypeMismatch,
        "Unknown unary operator '" + op + "'", unary->op);
    return Type::Error();
}

Type TypeChecker::checkPostfix(const PostfixExpr* postfix) {
    Type left = checkExpr(postfix->left.get());
    if (left.isError) return Type::Error();
    
    std::string op = postfix->op.value;
    if (op != "++" && op != "--") {
        reportError(TypeChkError::ExpressionTypeMismatch,
            "Invalid postfix operator", postfix->op);
        return Type::Error();
    }

    if (!left.isNumeric()) {
        reportError(TypeChkError::AttemptedAddOpOnNonNumeric,
            "Increment/decrement on non-numeric type", postfix->op);
        return Type::Error();
    }

    return left;
}

Type TypeChecker::checkBinary(const BinaryExpr* bin) {
    Type left = checkExpr(bin->left.get());
    Type right = checkExpr(bin->right.get());
    if (left.isError || right.isError) return Type::Error();
    
    std::string op = bin->op.value;
    bool isAssignment = op.back() == '=';
    std::string baseOp = isAssignment ? op.substr(0, op.size() - 1) : op;

    if (op == "=") {
        if (left != right) {
            reportError(TypeChkError::ExpressionTypeMismatch,
                "Assignment of incompatible types", bin->op);
            return Type::Error();
        }
        return left;
    }

    if (isAssignment && !baseOp.empty()) {
        Type res = checkBinaryHelper(left, right, baseOp, bin->op);
        if (res.isError) return Type::Error();
        if (left != res) {
            reportError(TypeChkError::ExpressionTypeMismatch,
                "Compound assignment type mismatch", bin->op);
            return Type::Error();
        }
        return left;
    }

    return checkBinaryHelper(left, right, op, bin->op);
}

Type TypeChecker::checkBinaryHelper(const Type& left, const Type& right, const std::string& op, const Token& token) {
    if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
        if (!left.isNumeric() || !right.isNumeric()) {
            reportError(TypeChkError::AttemptedAddOpOnNonNumeric,
                "Arithmetic operator '" + op + "' on non-numeric types", token);
            return Type::Error();
        }
        return left.isFloat() || right.isFloat() ? Type(T_FLOAT) : Type(T_INT);
    }

    if (op == "**") {
        if (!left.isNumeric() || !right.isNumeric()) {
            reportError(TypeChkError::AttemptedExponentiationOfNonNumeric,
                "Exponentiation on non-numeric types", token);
            return Type::Error();
        }
        return Type(T_FLOAT);
    }

    if (op == "&" || op == "|" || op == "^") {
        if (!left.isInt() || !right.isInt()) {
            reportError(TypeChkError::AttemptedBitOpOnNonNumeric,
                "Bitwise operator '" + op + "' on non-integer", token);
            return Type::Error();
        }
        return Type(T_INT);
    }

    if (op == "<<" || op == ">>") {
        if (!left.isInt() || !right.isInt()) {
            reportError(TypeChkError::AttemptedShiftOnNonInt,
                "Shift operator on non-integer", token);
            return Type::Error();
        }
        return Type(T_INT);
    }

    if (op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=") {
        if ((left.isNumeric() && right.isNumeric()) ||
            (left.isBool() && right.isBool()) ||
            (left.isString() && right.isString())) {
            return Type(T_BOOL);
        }
        reportError(TypeChkError::ExpressionTypeMismatch,
            "Comparison between incompatible types", token);
        return Type::Error();
    }

    if (op == "&&" || op == "||") {
        if (!left.isBool() || !right.isBool()) {
            reportError(TypeChkError::AttemptedBoolOpOnNonBools,
                "Logical operator '" + op + "' on non-boolean", token);
            return Type::Error();
        }
        return Type(T_BOOL);
    }

    reportError(TypeChkError::ExpressionTypeMismatch,
        "Unknown binary operator '" + op + "'", token);
    return Type::Error();
}

Type TypeChecker::checkCall(const CallExpr* call) {
    // Only handle simple identifier callees for now
    if (auto ident = dynamic_cast<const IdentExpr*>(call->callee.get())) {
        // First compute arg Types
        std::vector<Type> argTypes;
        for (const auto& arg : call->args) {
            argTypes.push_back(checkExpr(arg.get()));
        }
        for (const auto& at : argTypes) {
            if (at.isError) return Type::Error();
        }

        // Convert to TokenType vector for lookupFunction
        std::vector<TokenType> argTokenTypes;
        for (const auto& at : argTypes) argTokenTypes.push_back(at.tokenType);

        // Try to find an exact matching overload
        const Symbol* func = scopeStack_.lookupFunction(ident->name, argTokenTypes);
        if (!func) {
            // If no exact overload, attempt to find any function with that name (to provide better diagnostics)
            const Symbol* anyFunc = scopeStack_.lookup(ident->name);
            if (!anyFunc || anyFunc->type != SymbolType::FUNCTION) {
                reportError(TypeChkError::FnCallParamType,
                    "Call to undefined function '" + ident->name + "'", call->callee->origin);
                return Type::Error();
            }
            // Found a function but signature doesn't match
            reportError(TypeChkError::FnCallParamCount,
                "No overload of '" + ident->name + "' matches the given argument types", call->callee->origin);
            return Type::Error();
        }

        // Check param count (redundant if lookupFunction matched params, but keep for clarity)
        if (func->paramTypes.size() != argTypes.size()) {
            reportError(TypeChkError::FnCallParamCount,
                "Function '" + ident->name + "' expects " + std::to_string(func->paramTypes.size()) +
                " arguments but got " + std::to_string(argTypes.size()), ident->origin);
            return Type::Error();
        }

        // Finally we already matched arg types, but if you want an extra check:
        for (size_t i = 0; i < argTypes.size(); ++i) {
            if (Type(func->paramTypes[i]) != argTypes[i]) {
                reportError(TypeChkError::FnCallParamType,
                    "Argument " + std::to_string(i+1) + " type mismatch in call to '" + ident->name + "'",
                    call->args[i]->origin);
                return Type::Error();
            }
        }

        // Return the function return type
        return Type(func->dataType);
    }

    reportError(TypeChkError::ExpressionTypeMismatch,
        "Call to non-function", call->callee->origin);
    return Type::Error();
}

Type TypeChecker::checkMember(const MemberExpr*) {
    reportError(TypeChkError::ExpressionTypeMismatch,
        "Member access not supported (no structs)", std::nullopt);
    return Type::Error();
}

Type TypeChecker::checkIndex(const IndexExpr*) {
    reportError(TypeChkError::ExpressionTypeMismatch,
        "Index access not supported (no arrays)", std::nullopt);
    return Type::Error();
}

Type TypeChecker::checkExpr(const Expr* expr) {
    if (!expr) {
        reportError(TypeChkError::EmptyExpression, "Empty expression", std::nullopt);
        return Type::Error();
    }

    if (auto lit = dynamic_cast<const LiteralExpr*>(expr)) return checkLiteral(lit);
    if (auto ident = dynamic_cast<const IdentExpr*>(expr)) return checkIdent(ident);
    if (auto unary = dynamic_cast<const UnaryExpr*>(expr)) return checkUnary(unary);
    if (auto bin = dynamic_cast<const BinaryExpr*>(expr)) return checkBinary(bin);
    if (auto call = dynamic_cast<const CallExpr*>(expr)) return checkCall(call);
    if (auto postfix = dynamic_cast<const PostfixExpr*>(expr)) return checkPostfix(postfix);
    if (auto member = dynamic_cast<const MemberExpr*>(expr)) return checkMember(member);
    if (auto index = dynamic_cast<const IndexExpr*>(expr)) return checkIndex(index);

    reportError(TypeChkError::ExpressionTypeMismatch, "Unknown expression type", expr->origin);
    return Type::Error();
}

void TypeChecker::checkVarDecl(const VarDecl* decl) {
    TokenType declaredType = typeNodeToTokenType(decl->typeNode ? &*decl->typeNode : nullptr);

    if (decl->initializer) {
        Type initType = checkExpr(decl->initializer->get());
        if (!initType.isError && Type(declaredType) != initType) {
            reportError(TypeChkError::ErroneousVarDecl,
                "Variable '" + decl->name + "' declared as " + (decl->typeNode ? decl->typeNode->name : "int") +
                " but initialized with incompatible type", (*decl->initializer)->origin);
        }
    }
}

void TypeChecker::checkReturn(const ReturnStmt* ret) {
    if (currentFunctionReturnType_.empty()) return; // shouldn't happen, but guard

    TokenType expected = currentFunctionReturnType_.top();

    // If the function is void (expected == T_VOID), returning an expression is an error.
#ifdef T_VOID
    if (expected == T_VOID) {
        if (ret->expr) {
            reportError(TypeChkError::ErroneousReturnType,
                "Returning a value from a void function", (*ret->expr)->origin);
        }
        return;
    }
#else
    // If no T_VOID support, treat no-expression return as allowed only when expected == T_INT (old behavior)
    // This branch kept for compatibility; ideally add T_VOID to TokenType and use the branch above.
#endif

    // Non-void expected
    if (!ret->expr) {
        // Missing return value
        reportError(TypeChkError::ErroneousReturnType,
            "Missing return value in function", ret->origin);
        return;
    }

    Type actual = checkExpr(ret->expr->get());
    if (!actual.isError && Type(expected) != actual) {
        reportError(TypeChkError::ErroneousReturnType,
            "Return type mismatch", (*ret->expr)->origin);
    }
}


// NEW: Check if a block contains at least one return statement
bool TypeChecker::checkForReturnStatements(const BlockStmt* block) {
    for (const auto& stmt : block->statements) {
        if (dynamic_cast<const ReturnStmt*>(stmt.get())) {
            return true;
        }
        // Check nested blocks (like in if statements)
        if (auto nestedBlock = dynamic_cast<const BlockStmt*>(stmt.get())) {
            if (checkForReturnStatements(nestedBlock)) {
                return true;
            }
        }
    }
    return false;
}
// helper: recursively search for any ReturnStmt inside a Stmt
static bool contains_return(const Stmt* stmt) {
    if (!stmt) return false;
    if (dynamic_cast<const ReturnStmt*>(stmt)) return true;
    if (auto block = dynamic_cast<const BlockStmt*>(stmt)) {
        for (const auto &s : block->statements) {
            if (contains_return(s.get())) return true;
        }
        return false;
    }
    if (auto ifs = dynamic_cast<const IfStmt*>(stmt)) {
        if (contains_return(ifs->thenBranch.get())) return true;
        if (ifs->elseBranch && contains_return(ifs->elseBranch.get())) return true;
        return false;
    }
    if (auto wh = dynamic_cast<const WhileStmt*>(stmt)) {
        return contains_return(wh->body.get());
    }
    if (auto fr = dynamic_cast<const ForStmt*>(stmt)) {
        return contains_return(fr->body.get());
    }
    // add other stmt kinds as needed
    return false;
}

void TypeChecker::checkFuncDecl(const FuncDecl* decl) {
    // Defensive checks
    if (!decl) return;

    TokenType retType = typeNodeToTokenType(decl->retType ? &*decl->retType : nullptr);

    // Push the expected return type for nested return checks
    currentFunctionReturnType_.push(retType);

    // Enter a fresh function scope for type checking; we must populate it with parameters
    scopeStack_.enterScope("function:" + decl->name);

    // Declare parameters in this scope so they can be found by checkIdent
    for (const auto &p : decl->params) {
        TokenType ptt = typeNodeToTokenType(&p.type);
        bool ok = scopeStack_.addSymbol(p.name, SymbolType::PARAMETER, ptt, 0, 0);
        if (!ok) {
            // report duplicate parameter or redefinition
            reportError(TypeChkError::ErroneousVarDecl,
                        "Parameter redefinition or duplicate parameter name '" + p.name + "' in function '" + decl->name + "'",
                        decl->origin);
            // continue; do not return — try to continue typechecking other params/body
        }
    }

    

    // Walk the body and type-check statements. Also detect presence of any return (including nested).
    bool hasReturn = false;
    for (const auto& stmt : decl->body->statements) {
        if (contains_return(stmt.get())) hasReturn = true;
        checkStmt(stmt.get());
    }

    // Decide whether missing return is an error:
    // If the function explicitly declared a return type (retType from decl->retType is meaningful),
    // and that return type is not void (if T_VOID exists), require at least one return.
#if defined(T_VOID)
    if (decl->retType && retType != T_VOID && !hasReturn) {
        reportError(TypeChkError::ReturnStmtNotFound,
            "Missing return statement in function '" + decl->name + "' with specified return type", decl->origin);
    }
#else
    // If T_VOID doesn't exist in your TokenType, use the presence of decl->retType to decide:
    if (decl->retType && !decl->retType->name.empty() && !hasReturn) {
        // If the declared return type is "int" by default you may want special-casing; adapt to language rules.
        reportError(TypeChkError::ReturnStmtNotFound,
            "Missing return statement in function '" + decl->name + "' with specified return type", decl->origin);
    }
#endif

    scopeStack_.exitScope();
    currentFunctionReturnType_.pop();
}


void TypeChecker::checkIf(const IfStmt* stmt) {
    Type cond = checkExpr(stmt->condition.get());
    if (!cond.isError && !cond.isBool()) {
        reportError(TypeChkError::NonBooleanCondStmt,
            "If condition must be boolean", stmt->condition->origin);
    }

    checkBlock(stmt->thenBranch.get());
    if (stmt->elseBranch) checkBlock(stmt->elseBranch.get());
}

void TypeChecker::checkWhile(const WhileStmt* stmt) {
    Type cond = checkExpr(stmt->condition.get());
    if (!cond.isError && !cond.isBool()) {
        reportError(TypeChkError::NonBooleanCondStmt,
            "While condition must be boolean", stmt->condition->origin);
    }

    ++loopDepth_;
    checkBlock(stmt->body.get());
    --loopDepth_;
}

void TypeChecker::checkFor(const ForStmt* stmt) {
    ++loopDepth_;
    if (stmt->init) checkStmt(stmt->init->get());
    if (stmt->condition) {
        Type c = checkExpr(stmt->condition->get());
        if (!c.isError && !c.isBool()) {
            reportError(TypeChkError::NonBooleanCondStmt, "For condition must be bool", stmt->condition->get()->origin);
        }
    }
    if (stmt->post) checkExpr(stmt->post->get());
    checkBlock(stmt->body.get());
    --loopDepth_;
}

void TypeChecker::checkBreakContinue(const ASTNode* node, TypeChkError errorType) {
    if (loopDepth_ == 0) {
        reportError(errorType, errorType == TypeChkError::ErroneousBreak ? "break outside loop" : "continue outside loop", node->origin);
    }
}

void TypeChecker::checkStmt(const Stmt* stmt) {
    if (auto block = dynamic_cast<const BlockStmt*>(stmt)) {
        scopeStack_.enterScope("block");
        for (const auto& s : block->statements) checkStmt(s.get());
        scopeStack_.exitScope();
    }
    else if (auto var = dynamic_cast<const VarDecl*>(stmt)) checkVarDecl(var);
    else if (auto func = dynamic_cast<const FuncDecl*>(stmt)) checkFuncDecl(func);
    else if (auto expr = dynamic_cast<const ExprStmt*>(stmt)) {
        if (expr->expr) checkExpr(expr->expr.get());
    }
    else if (auto ret = dynamic_cast<const ReturnStmt*>(stmt)) checkReturn(ret);
    else if (auto ifs = dynamic_cast<const IfStmt*>(stmt)) checkIf(ifs);
    else if (auto wh = dynamic_cast<const WhileStmt*>(stmt)) checkWhile(wh);
    else if (auto fr = dynamic_cast<const ForStmt*>(stmt)) checkFor(fr);
    else if (dynamic_cast<const BreakStmt*>(stmt)) checkBreakContinue(stmt, TypeChkError::ErroneousBreak);
    else if (dynamic_cast<const ContinueStmt*>(stmt)) checkBreakContinue(stmt, TypeChkError::ErroneousBreak);
}

void TypeChecker::checkBlock(const BlockStmt* block) {
    scopeStack_.enterScope("block");
    for (const auto& s : block->statements) checkStmt(s.get());
    scopeStack_.exitScope();
}

bool TypeChecker::check(const Program* program) {
    errors_.clear();
    for (const auto& item : program->items) {
        checkStmt(item.get());
    }
    return errors_.empty();
}