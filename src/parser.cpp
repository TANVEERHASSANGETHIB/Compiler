#include "parser.h"
#include <cctype>
#include <stdexcept>
#include <cassert>

using namespace std;

static void printIndent(int indent) {
    for (int i = 0; i < indent; i++) cout << "  ";
}

void TypeNode::print(int indent) const {
    printIndent(indent);
    cout << "Type(" << name << ")\n";
}
void LiteralExpr::print(int indent) const {
    printIndent(indent); cout << "Literal(" << value << ")\n";
}
void IdentExpr::print(int indent) const {
    printIndent(indent); cout << "Ident(" << name << ")\n";
}
void UnaryExpr::print(int indent) const {
    printIndent(indent); cout << "Unary(" << op.value << ")\n";
    if (right) right->print(indent+1);
}
void PostfixExpr::print(int indent) const {
    printIndent(indent); cout << "Postfix(" << op.value << ")\n";
    if (left) left->print(indent+1);
}
void BinaryExpr::print(int indent) const {
    printIndent(indent); cout << "Binary(" << op.value << ")\n";
    if (left) left->print(indent+1);
    if (right) right->print(indent+1);
}
void CallExpr::print(int indent) const {
    printIndent(indent); cout << "Call\n";
    if (callee) callee->print(indent+1);
    for (auto &a : args) if (a) a->print(indent+1);
}
void MemberExpr::print(int indent) const {
    printIndent(indent); cout << "Member(" << member << ")\n";
    if (object) object->print(indent+1);
}
void IndexExpr::print(int indent) const {
    printIndent(indent); cout << "Index\n";
    if (object) object->print(indent+1);
    if (index) index->print(indent+1);
}
void ExprStmt::print(int indent) const {
    printIndent(indent); cout << "ExprStmt\n";
    if (expr) expr->print(indent+1);
}
void ReturnStmt::print(int indent) const {
    printIndent(indent); cout << "Return\n";
    if (expr && *expr) (*expr)->print(indent+1);
}
void BlockStmt::print(int indent) const {
    printIndent(indent); cout << "Block\n";
    for (auto &s : statements) if (s) s->print(indent+1);
}
void VarDecl::print(int indent) const {
    printIndent(indent);
    cout << "VarDecl " << name;
    if (typeNode) cout << " : " << typeNode->name;
    cout << "\n";
    if (initializer && *initializer) (*initializer)->print(indent+1);
}
void IfStmt::print(int indent) const {
    printIndent(indent); cout << "If\n";
    if (condition) condition->print(indent+1);
    if (thenBranch) thenBranch->print(indent+1);
    if (elseBranch) {
        printIndent(indent); cout << "Else\n";
        elseBranch->print(indent+1);
    }
}
void WhileStmt::print(int indent) const {
    printIndent(indent); cout << "While\n";
    if (condition) condition->print(indent+1);
    if (body) body->print(indent+1);
}
void ForStmt::print(int indent) const {
    printIndent(indent); cout << "For\n";
    if (init) { printIndent(indent+1); cout << "Init\n"; (*init)->print(indent+2); }
    if (condition) { printIndent(indent+1); cout << "Cond\n"; (*condition)->print(indent+2); }
    if (post) { printIndent(indent+1); cout << "Post\n"; (*post)->print(indent+2); }
    if (body) body->print(indent+1);
}
void BreakStmt::print(int indent) const { printIndent(indent); cout << "Break\n"; }
void ContinueStmt::print(int indent) const { printIndent(indent); cout << "Continue\n"; }
void FuncDecl::print(int indent) const {
    printIndent(indent);
    cout << "Func " << name << "(";
    for (size_t i=0;i<params.size(); ++i) {
        if (i) cout << ", ";
        cout << params[i].name << ":" << params[i].type.name;
    }
    cout << ")";
    if (retType) cout << " -> " << retType->name;
    cout << "\n";
    if (body) body->print(indent+1);
}
void Program::print(int indent) const {
    printIndent(indent); cout << "Program\n";
    for (auto &it : items) if (it) it->print(indent+1);
}

Parser::Parser(const vector<Token>& tokens)
    : tokens_(tokens), pos_(0), lexUtil_(), scopeStack_(), function_depth_(0), loop_depth_(0) {}

const Token& Parser::peek() const { return peek_n(1); }
const Token& Parser::peek_n(size_t n) const {
    size_t idx = pos_ + (n - 1);
    if (idx < tokens_.size()) return tokens_[idx];
    static Token eofTok;
    return eofTok;
}
const Token& Parser::previous() const {
    if (pos_ == 0) return peek();
    return tokens_[pos_-1];
}
const Token& Parser::advance() {
    if (!is_at_end()) ++pos_;
    return previous();
}
bool Parser::is_at_end() const {
    const Token &t = peek();
    return t.type == T_EOF;
}

bool Parser::matchType(TokenType t) {
    if (is_at_end()) return false;
    if (peek().type == t) { advance(); return true; }
    return false;
}
bool Parser::matchAnyType(const initializer_list<TokenType>& types) {
    for (auto t : types) if (matchType(t)) return true;
    return false;
}
bool Parser::checkType(TokenType t) const {
    if (is_at_end()) return false;
    return peek().type == t;
}
bool Parser::checkTypeN(size_t n, TokenType t) const {
    if (is_at_end()) return false;
    const Token &tt = peek_n(n);
    return tt.type == t;
}
bool Parser::matchLexeme(const string &lexeme) {
    if (is_at_end()) return false;
    if (peek().value == lexeme) { advance(); return true; }
    return false;
}
bool Parser::matchEither(const string &lexOrType) {
    if (matchLexeme(lexOrType)) return true;
    const Token &p = peek();
    if (lexUtil_.tokenTypeToString(p.type) == lexOrType) { advance(); return true; }
    return false;
}
bool Parser::checkEither(const string &lexOrType) const {
    if (is_at_end()) return false;
    const Token &t = peek();
    if (t.value == lexOrType) return true;
    if (lexUtil_.tokenTypeToString(t.type) == lexOrType) return true;
    return false;
}
bool Parser::checkEitherLexemeOrType(const string &lexOrType) const {
    return checkEither(lexOrType);
}

const Token& Parser::expectType(TokenType t, ParseErrorKind errKind) {
    if (checkType(t)) return advance();
    throw make_error(errKind, &peek(), string("Expected token type ") + lexUtil_.tokenTypeToString(t));
}
const Token& Parser::expectEither(const string &lexOrType, ParseErrorKind errKind) {
    if (checkEither(lexOrType)) return advance();
    throw make_error(errKind, &peek(), string("Expected token ") + lexOrType);
}

void Parser::push_scope(const string& name) { scopeStack_.enterScope(name); }
void Parser::pop_scope() { scopeStack_.exitScope(); }

void Parser::declare_variable(const string& name, TokenType dataType, int line, int col) {
    if (!scopeStack_.addSymbol(name, SymbolType::VARIABLE, dataType, line, col)) {
        throw ScopeError(ScopeErrorType::VariableRedefinition, name);
    }
}

void Parser::declare_function(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line, int col) {
    if (!scopeStack_.addFunction(name, returnType, paramTypes, line, col)) {
        throw ScopeError(ScopeErrorType::FunctionPrototypeRedefinition, name);
    }
}

void Parser::declare_parameter(const string& name, TokenType dataType, int line, int col) {
    if (!scopeStack_.addSymbol(name, SymbolType::PARAMETER, dataType, line, col)) {
        throw ScopeError(ScopeErrorType::VariableRedefinition, name);
    }
}

void Parser::check_variable_access(const string& name, const Token& token) {
    Symbol* symbol = scopeStack_.lookup(name);
    if (!symbol) {
        report_error(ParseErrorKind::UndeclaredIdentifier, token, 
            "Identifier '" + name + "' is not declared");
    } else if (symbol->type != SymbolType::VARIABLE && symbol->type != SymbolType::PARAMETER) {
        // don't throw for functions (they can be used as callee)
    }
}

void Parser::check_function_call(const string& name, const vector<ExprPtr>& args, const Token& token) {
    const Symbol* symbol = scopeStack_.lookup(name);
    if (!symbol || symbol->type != SymbolType::FUNCTION) {
        // Report undefined function, but do not enforce parameter count here.
        report_error(ParseErrorKind::UndefinedFunctionCalled, token, 
            "Function '" + name + "' is not declared");
    }
    // Defer exact overload/param type checks to the TypeChecker which has static types
}

bool Parser::is_declared_in_current_scope(const string& name) const {
    return scopeStack_.lookupCurrent(name) != nullptr;
}

void Parser::enter_function() { ++function_depth_; }
void Parser::exit_function() { if (function_depth_>0) --function_depth_; }
void Parser::enter_loop() { ++loop_depth_; }
void Parser::exit_loop() { if (loop_depth_>0) --loop_depth_; }

ParseError Parser::make_error(ParseErrorKind kind, const Token* t, string msg) const {
    ParseError e;
    e.kind = kind;
    if (t) e.token = *t;
    e.message = move(msg);
    return e;
}

ParseError Parser::make_error(ParseErrorKind kind, const Token& t, string msg) const {
    return make_error(kind, &t, move(msg));
}

void Parser::report_error(ParseErrorKind kind, const Token* token, string msg) {
    errors_.push_back(make_error(kind, token, move(msg)));
}

void Parser::report_error(ParseErrorKind kind, const Token& token, string msg) {
    report_error(kind, &token, move(msg));
}

void Parser::check_for_common_typos(const Token& token) {
    static const unordered_map<string, string> common_typos = {
        {"retun", "return"},
        {"funtion", "function"},
        {"whille", "while"},
        {"floot", "float"},
        {"bol", "bool"},
        {"sting", "string"}
    };
    
    auto it = common_typos.find(token.value);
    if (it != common_typos.end()) {
        report_error(ParseErrorKind::UnexpectedToken, token, 
                    "Did you mean '" + it->second + "'?");
    }
}

void Parser::synchronize() {
    // Skip tokens until we find a statement boundary
    while (!is_at_end()) {
        const Token &t = peek();
        
        // Statement boundaries: semicolon, braces, or statement keywords
        if (t.type == T_SEMICOLON || t.value == ";") { 
            advance(); 
            return; 
        }
        if (t.type == T_BRACER || t.value == "}") return;
        if (t.type == T_BRACEL || t.value == "{") return;
        
        // Statement starters
        if (t.type == T_RETURN || t.value == "return" ||
            t.type == T_IF || t.value == "if" ||
            t.type == T_WHILE || t.value == "while" ||
            t.type == T_FOR || t.value == "for" ||
            t.type == T_BREAK || t.value == "break" ||
            t.type == T_CONTINUE || t.value == "continue" ||
            t.type == T_LET || t.value == "let" ||
            t.type == T_CONST || t.value == "const" ||
            t.type == T_INT || t.value == "int" ||
            t.type == T_FLOAT || t.value == "float" ||
            t.type == T_STRING || t.value == "string" ||
            t.type == T_BOOL || t.value == "bool") {
            return;
        }
        
        advance();
    }
}

unique_ptr<Program> Parser::parse_program() {
    auto program = make_unique<Program>();
    while (!is_at_end()) {
        try {
            StmtPtr s = parse_declaration();
            if (s) program->items.push_back(move(s));
        } catch (const ParseError &err) {
            errors_.push_back(err);
            synchronize();
        } catch (const ScopeError &err) {
            errors_.push_back(make_error(ParseErrorKind::GenericError, nullptr, err.what()));
            synchronize();
        }
    }
    return program;
}

bool Parser::consumeSemicolon() {
    if (matchEither(";") || matchType(T_SEMICOLON)) return true;
    report_error(ParseErrorKind::MissingSemicolon, &peek(), "Expected ;");
    return false;
}

StmtPtr Parser::parse_declaration() {
    const Token &p = peek();
    bool isTypeToken = (p.type == T_INT || p.value == "int" || p.type == T_FLOAT || p.value == "float" ||
                       p.type == T_BOOL || p.value == "bool" || p.type == T_STRING || p.value == "string" || p.type == T_VOID || p.value == "void");

    if (isTypeToken) {
        Token typeTok = advance();
        string typename_text = typeTok.value;
        TokenType dataType = typeNameToTokenType(typename_text);

        if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0] == '_'))) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after type");
        }
        Token idTok = advance();
        string name = idTok.value;

        // If next token is '(' => function declaration (we have already consumed typeTok and idTok)
        if (checkEither("(") || checkType(T_PARENL)) {
            return parse_function_decl_after_header(typeTok, idTok);
        }

        optional<ExprPtr> initializer;
        if (matchEither("=") || checkType(T_ASSIGNOP)) {
            // If matchEither consumed "=", continue; if checkType matched ASSIGNOP but didn't advance, advance now.
            if (!matchEither("=") && checkType(T_ASSIGNOP)) advance();
            initializer = parse_expression();
        }

        consumeSemicolon();

        declare_variable(name, dataType, idTok.line, idTok.column);
        return make_unique<VarDecl>(name, optional<TypeNode>(TypeNode(typename_text)), move(initializer));
    }

    if (checkEither("fn") || p.type == T_FUNCTION) {
        return parse_function_decl();
    }

    if (checkEither("let") || checkEither("const") || p.type == T_LET || p.type == T_CONST) {
        return parse_var_decl_or_stmt();
    }

    return parse_statement();
}

StmtPtr Parser::parse_var_decl_or_stmt() {
    Token kw = advance();
    if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after var/let");
    }
    Token idTok = advance();
    string name = idTok.value;

    optional<TypeNode> typeNode;
    TokenType dataType = T_INT;
    if (matchEither(":") || matchType(T_COLON)) {
        if (!checkType(T_IDENTIFIER)) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected type name after ':'");
        Token typeTok = advance();
        typeNode = TypeNode(typeTok.value);
        dataType = typeNameToTokenType(typeTok.value);
    }

    declare_variable(name, dataType, idTok.line, idTok.column);

    optional<ExprPtr> initializer;
    if (matchEither("=") || matchType(T_ASSIGNOP)) {
        if (!matchEither("=") && checkType(T_ASSIGNOP)) advance();
        initializer = parse_expression();
    }

    consumeSemicolon();

    return make_unique<VarDecl>(name, move(typeNode), move(initializer));
}

StmtPtr Parser::parse_function_decl() {
    // Handle both "int func()" and "fn func()" syntax
    Token typeTok;
    Token nameTok;
    
    if (checkEither("fn") || peek().type == T_FUNCTION) {
        advance(); // consume 'fn'
        if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected function name after fn");
        }
        nameTok = advance();
        typeTok = Token{T_INT, "int", nameTok.line, nameTok.column}; // Default return type
    } else {
        // Type-based function declaration (int func())
        typeTok = advance();
        if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected function name after type");
        }
        nameTok = advance();
    }

    // Now call the helper to continue parsing header/body
    return parse_function_decl_after_header(typeTok, nameTok);
}

StmtPtr Parser::parse_function_decl_after_header(const Token& typeTok, const Token& nameTok) {
    // typeTok and nameTok are already consumed by caller
    string fname = nameTok.value;
    string returnTypeName = typeTok.value;
    TokenType returnType = typeNameToTokenType(returnTypeName);

    expectEither("(", ParseErrorKind::FailedToFindToken);

    vector<FuncParam> params;
    vector<TokenType> paramTypes;

    if (!checkEither(")") && !checkType(T_PARENR)) {
        while (true) {
            // For "int func(int a, int b)" syntax
            const Token &ptypeTok = peek();
            if (!(ptypeTok.type == T_INT || ptypeTok.value == "int" || ptypeTok.type == T_FLOAT || ptypeTok.value == "float" ||
                  ptypeTok.type == T_STRING || ptypeTok.value == "string" || ptypeTok.type == T_BOOL || ptypeTok.value == "bool")) {
                throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected parameter type");
            }
            Token consumedType = advance();
            TokenType paramType = typeNameToTokenType(consumedType.value);
            paramTypes.push_back(paramType);

            if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
                throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected parameter name");
            }
            Token pnameTok = advance();

            params.push_back(FuncParam{pnameTok.value, TypeNode(consumedType.value)});

            if (checkEither(")") || checkType(T_PARENR)) break;
            if (matchEither(",") || matchType(T_COMMA)) continue;
            throw make_error(ParseErrorKind::InvalidParameterList, &peek(), "Expected ',' or ')' in parameter list");
        }
    }

    expectEither(")", ParseErrorKind::FailedToFindToken);

    // Handle return type for fn syntax or explicit return specifier
    if (matchEither(":") || matchType(T_COLON)) {
        if (!checkType(T_IDENTIFIER)) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected return type after ':'");
        Token rtok = advance();
        returnTypeName = rtok.value;
        returnType = typeNameToTokenType(returnTypeName);
    }

    optional<TypeNode> retType = TypeNode(returnTypeName);
    
    // Declare function in current scope (should be global)
    declare_function(fname, returnType, paramTypes, nameTok.line, nameTok.column);

    enter_function();
    push_scope("function:" + fname);
    
    // Declare parameters in function scope
    for (auto &p : params) {
        declare_parameter(p.name, typeNameToTokenType(p.type.name), nameTok.line, nameTok.column);
    }

    unique_ptr<BlockStmt> body;
    try {
        body = parse_block();
    } catch (const ParseError &err) {
        pop_scope();
        exit_function();
        throw;
    }

    pop_scope();
    exit_function();
    return make_unique<FuncDecl>(fname, move(params), move(retType), move(body));
}

unique_ptr<BlockStmt> Parser::parse_block() {
    if (!matchEither("{") && !matchType(T_BRACEL)) {
        throw make_error(ParseErrorKind::FailedToFindToken, &peek(), "Expected '{' for block");
    }
    auto block = make_unique<BlockStmt>();
    push_scope("block");
    
    while (!is_at_end()) {
        // Check if we've reached the end of the block
        if (checkEither("}") || checkType(T_BRACER)) {
            break;
        }
        
        try {
            StmtPtr s = parse_declaration();
            if (s) block->statements.push_back(move(s));
        } catch (const ParseError &err) {
            errors_.push_back(err);
            synchronize();
        } catch (const ScopeError &err) {
            errors_.push_back(make_error(ParseErrorKind::GenericError, nullptr, err.what()));
            synchronize();
        }
    }
    
    // Expect the closing brace
    if (!matchEither("}") && !matchType(T_BRACER)) {
        // Don't throw, just report error and try to continue
        report_error(ParseErrorKind::FailedToFindToken, &peek(), "Expected '}' to close block");
    } 
    // matchEither already consumes the '}' if present

    pop_scope();
    return block;
}

StmtPtr Parser::parse_statement() {
    if (matchEither("return") || matchType(T_RETURN)) return parse_return_stmt();
    if (matchEither("if") || matchType(T_IF)) return parse_if_stmt();
    if (matchEither("while") || matchType(T_WHILE)) return parse_while_stmt();
    if (matchEither("for") || matchType(T_FOR)) return parse_for_stmt();
    if (matchEither("break") || matchType(T_BREAK)) return parse_break_stmt();
    if (matchEither("continue") || matchType(T_CONTINUE)) return parse_continue_stmt();
    if (matchEither("{") || matchType(T_BRACEL)) {
        --pos_;
        auto b = parse_block();
        return StmtPtr(move(b));
    }

    if (checkEither("let") || checkEither("const") || checkType(T_LET) || checkType(T_CONST)) {
        return parse_var_decl_or_stmt();
    }

    // Try to parse as expression statement, but handle errors gracefully
    try {
        ExprPtr e = parse_expression();
        
        consumeSemicolon();

        return make_unique<ExprStmt>(move(e));
    } catch (const ParseError &err) {
        // If we can't parse as expression, report error and try to recover
        errors_.push_back(err);
        
        // Check for common typos
        if (peek().type == T_IDENTIFIER) {
            check_for_common_typos(peek());
        }
        
        report_error(ParseErrorKind::UnexpectedToken, &peek(), 
                    "Unexpected token in statement: '" + peek().value + "'");
        
        // Try to synchronize to the next statement
        synchronize();
        
        // Return a dummy statement or nullptr to continue parsing
        return nullptr;
    }
}

StmtPtr Parser::parse_return_stmt() {
    if (function_depth_ == 0) {
        throw make_error(ParseErrorKind::MisplacedReturn, &previous(), "return outside function");
    }

    optional<ExprPtr> maybe;
    if (!checkEither(";") && !checkType(T_SEMICOLON)) {
        maybe = parse_expression();
    }
    
    consumeSemicolon();

    return make_unique<ReturnStmt>(move(maybe));
}

StmtPtr Parser::parse_if_stmt() {
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> thenBlock;
    if (checkEither("{") || checkType(T_BRACEL)) {
        thenBlock = parse_block();
    } else {
        StmtPtr thenStmt = parse_statement();
        thenBlock = make_unique<BlockStmt>();
        thenBlock->statements.push_back(move(thenStmt));
    }

    unique_ptr<BlockStmt> elseBlock = nullptr;
    if (matchEither("else") || matchType(T_ELSE)) {
        if (checkEither("{") || checkType(T_BRACEL)) {
            elseBlock = parse_block();
        } else {
            StmtPtr elseStmt = parse_statement();
            auto eb = make_unique<BlockStmt>();
            eb->statements.push_back(move(elseStmt));
            elseBlock = move(eb);
        }
    }

    return StmtPtr(new IfStmt(move(cond), move(thenBlock), move(elseBlock)));
}

StmtPtr Parser::parse_while_stmt() {
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> body;
    enter_loop();
    try {
        if (checkEither("{") || checkType(T_BRACEL)) {
            body = parse_block();
        } else {
            StmtPtr s = parse_statement();
            auto b = make_unique<BlockStmt>();
            b->statements.push_back(move(s));
            body = move(b);
        }
    } catch (const ParseError &err) {
        exit_loop();
        throw;
    }
    exit_loop();
    return StmtPtr(new WhileStmt(move(cond), move(body)));
}

StmtPtr Parser::parse_for_stmt() {
    expectEither("(", ParseErrorKind::FailedToFindToken);
    optional<StmtPtr> init;

    if (!checkEither(";")) {
        const Token &p = peek();
        bool isTypeToken = (p.type == T_INT || p.value == "int" || p.type == T_FLOAT || p.value == "float" ||
                           p.type == T_BOOL || p.value == "bool" || p.type == T_STRING || p.value == "string");

        if (isTypeToken) {
            Token typeTok = advance();
            TokenType dataType = typeNameToTokenType(typeTok.value);
            if (!checkType(T_IDENTIFIER) && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
                throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after type in for-init");
            }
            Token idTok = advance();
            string name = idTok.value;

            optional<ExprPtr> initializer;
            if (matchEither("=") || checkType(T_ASSIGNOP)) {
                if (!matchEither("=") && checkType(T_ASSIGNOP)) advance();
                initializer = parse_expression();
            }

            declare_variable(name, dataType, idTok.line, idTok.column);
            init = make_unique<VarDecl>(name, optional<TypeNode>(TypeNode(typeTok.value)), move(initializer));
        } else if (checkEither("let") || checkType(T_LET) || checkEither("const") || checkType(T_CONST)) {
            init = parse_var_decl_or_stmt();
        } else {
            ExprPtr e = parse_expression();
            init = make_unique<ExprStmt>(move(e));
        }
    }

    expectEither(";", ParseErrorKind::FailedToFindToken);
    optional<ExprPtr> cond;
    if (!checkEither(";")) cond = parse_expression();
    expectEither(";", ParseErrorKind::FailedToFindToken);

    optional<ExprPtr> post;
    if (!checkEither(")")) post = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> body;
    enter_loop();
    try {
        if (checkEither("{") || checkType(T_BRACEL)) {
            body = parse_block();
        } else {
            StmtPtr s = parse_statement();
            auto b = make_unique<BlockStmt>();
            b->statements.push_back(move(s));
            body = move(b);
        }
    } catch (const ParseError &err) {
        exit_loop();
        throw;
    }
    exit_loop();
    return StmtPtr(new ForStmt(move(init), move(cond), move(post), move(body)));
}

StmtPtr Parser::parse_break_stmt() {
    if (loop_depth_ == 0) throw make_error(ParseErrorKind::MisplacedBreakContinue, &previous(), "break outside loop");
    consumeSemicolon();
    return make_unique<BreakStmt>();
}

StmtPtr Parser::parse_continue_stmt() {
    if (loop_depth_ == 0) throw make_error(ParseErrorKind::MisplacedBreakContinue, &previous(), "continue outside loop");
    consumeSemicolon();
    return make_unique<ContinueStmt>();
}

static bool isNumberLexemeLocal(const string &s) {
    if (s.empty()) return false;
    size_t i = 0;
    if (s[0] == '+' || s[0] == '-') i = 1;
    bool hasDigit = false, hasDot = false;
    for (; i < s.size(); ++i) {
        if (isdigit((unsigned char)s[i])) hasDigit = true;
        else if (s[i] == '.') { if (hasDot) return false; hasDot = true; }
        else return false;
    }
    return hasDigit;
}

ExprPtr Parser::parse_expression() { return parse_assignment(); }

ExprPtr Parser::parse_assignment() {
    auto left = parse_pratt(0);
    if (checkEither("=") || checkType(T_ASSIGNOP) || peek().value == "+=" || peek().value == "-=" || 
        peek().value == "*=" || peek().value == "/=" || peek().value == "%=" || peek().value == "&=" ||
        peek().value == "|=" || peek().value == "^=" || peek().value == "<<=" || peek().value == ">>=") {
        Token op = advance();
        if (!dynamic_cast<IdentExpr*>(left.get()) && !dynamic_cast<MemberExpr*>(left.get()) && !dynamic_cast<IndexExpr*>(left.get())) {
            throw make_error(ParseErrorKind::InvalidAssignmentTarget, &op, "Invalid assignment target");
        }
        auto right = parse_assignment();
        return make_unique<BinaryExpr>(move(left), op, move(right));
    }
    return left;
}

int Parser::prefix_bp_by_lexeme(const string &op) const {
    if (op == "!" || op == "not") return 15;
    if (op == "-" || op == "+") return 15;
    if (op == "~") return 15;
    return 0;
}

int Parser::prefix_bp_by_type(TokenType t) const {
    if (t == T_NOT) return 15;
    if (t == T_MINUS) return 15;
    if (t == T_PLUS) return 15;
    if (t == T_BITNOT) return 15;
    return 0;
}

pair<int,int> Parser::infix_bp_by_lexeme(const string &op) const {
    if (op == "*" || op == "/" || op == "%") return {13,13};
    if (op == "+" || op == "-") return {12,12};
    if (op == "<<" || op == ">>") return {11,11};
    if (op == "&") return {10,10};
    if (op == "^") return {9,9};
    if (op == "|") return {8,8};
    if (op == "==" || op == "!=") return {7,7};
    if (op == "<" || op == ">" || op == "<=" || op == ">=") return {7,7};
    if (op == "&&") return {5,5};
    if (op == "||") return {4,4};
    if (op == "," ) return {1,1};
    return {0,0};
}

pair<int,int> Parser::infix_bp_by_type(TokenType t) const {
    if (t == T_MULT || t == T_DIV || t == T_MOD) return {13,13};
    if (t == T_PLUS || t == T_MINUS) return {12,12};
    if (t == T_LEFTSHIFT || t == T_RIGHTSHIFT) return {11,11};
    if (t == T_BITAND) return {10,10};
    if (t == T_BITXOR) return {9,9};
    if (t == T_BITOR) return {8,8};
    if (t == T_EQUALSOP || t == T_NEQ) return {7,7};
    if (t == T_LT || t == T_GT || t == T_LTE || t == T_GTE) return {7,7};
    if (t == T_AND) return {5,5};
    if (t == T_OR) return {4,4};
    if (t == T_COMMA) return {1,1};
    return {0,0};
}

ExprPtr Parser::parse_pratt(int min_bp) {
    const Token &t0 = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t0, "Unexpected EOF in expression");
    
    ExprPtr left;
    int p_bp = prefix_bp_by_lexeme(t0.value);
    if (p_bp == 0) p_bp = prefix_bp_by_type(t0.type);

    if (p_bp > 0) {
        Token op = advance();
        ExprPtr rhs = parse_pratt(p_bp);
        left = make_unique<UnaryExpr>(op, move(rhs));
    } else {
        left = parse_primary();
    }

    while (!is_at_end()) {
        const Token &cur = peek();
        string opLex = cur.value;
        pair<int,int> bp = infix_bp_by_lexeme(opLex);
        if (bp.first == 0) bp = infix_bp_by_type(cur.type);
        
        if (bp.first == 0 || bp.first < min_bp) {
            if (opLex == "(" || cur.type == T_PARENL) {
                if (auto ident = dynamic_cast<IdentExpr*>(left.get())) {
                    advance();
                    vector<ExprPtr> args;
                    if (!checkEither(")") && !checkType(T_PARENR)) {
                        do {
                            args.push_back(parse_expression());
                        } while (matchEither(",") || matchType(T_COMMA));
                    }
                    expectEither(")", ParseErrorKind::FailedToFindToken);
                    check_function_call(ident->name, args, cur);
                    left = make_unique<CallExpr>(move(left), move(args));
                } else {
                    advance();
                    vector<ExprPtr> args = parse_argument_list();
                    expectEither(")", ParseErrorKind::FailedToFindToken);
                    left = make_unique<CallExpr>(move(left), move(args));
                }
                continue;
            } else if (opLex == "[" || cur.type == T_BRACKL) {
                advance();
                ExprPtr idx = parse_expression();
                expectEither("]", ParseErrorKind::FailedToFindToken);
                left = make_unique<IndexExpr>(move(left), move(idx));
                continue;
            } else if (opLex == "." || cur.type == T_DOT || opLex == "->" || cur.type == T_ARROW) {
                Token op = advance();
                if (!checkType(T_IDENTIFIER)) throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected member name after '.' or '->'");
                Token mem = advance();
                left = make_unique<MemberExpr>(move(left), mem.value, op);
                continue;
            } else if (opLex == "++" || cur.type == T_INCREMENT || opLex == "--" || cur.type == T_DECREMENT) {
                Token op = advance();
                left = make_unique<PostfixExpr>(move(left), op);
                continue;
            } else break;
        }

        Token op = advance();
        int rbp = bp.second;
        ExprPtr right = parse_pratt(rbp + 0);
        left = make_unique<BinaryExpr>(move(left), op, move(right));
    }

    return left;
}

ExprPtr Parser::parse_primary() {
    const Token &t = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t);

    if (t.type == T_STRINGLIT) {
        Token tok = advance();
        return make_unique<LiteralExpr>(tok.value);
    }

    if (t.type == T_INTLIT) {
        Token tok = advance();
        return make_unique<LiteralExpr>(tok.value);
    }
    
    if (t.type == T_FLOATLIT) {
        Token tok = advance();
        return make_unique<LiteralExpr>(tok.value);
    }

    if (t.type == T_BOOLLIT || t.value == "true" || t.value == "false") {
        Token tok = advance();
        return make_unique<LiteralExpr>(tok.value);
    }

    if (!t.value.empty() && isNumberLexemeLocal(t.value)) {
        Token tok = advance();
        return make_unique<LiteralExpr>(tok.value);
    }

    if (t.type == T_IDENTIFIER || (!t.value.empty() && (isalpha((unsigned char)t.value[0]) || t.value[0]=='_'))) {
        Token tok = advance();
        
        check_for_common_typos(tok);
        check_variable_access(tok.value, tok);
        return make_unique<IdentExpr>(tok.value);
    }

    if (t.value == "(" || t.type == T_PARENL) {
        advance();
        ExprPtr e = parse_expression();
        expectEither(")", ParseErrorKind::FailedToFindToken);
        return e;
    }

    throw make_error(ParseErrorKind::ExpectedExpr, &t, "Expected expression.");
}

vector<ExprPtr> Parser::parse_argument_list() {
    vector<ExprPtr> args;
    if (!checkEither(")") && !checkType(T_PARENR)) {
        do {
            args.push_back(parse_expression());
        } while (matchEither(",") || matchType(T_COMMA));
    }
    return args;
}

TokenType Parser::typeNameToTokenType(const string& typeName) const {
    if (typeName == "int") return T_INT;
    if (typeName == "float") return T_FLOAT;
    if (typeName == "string") return T_STRING;
    if (typeName == "bool") return T_BOOL;

    if (typeName == "void") return T_VOID;
    return T_INT;
}

