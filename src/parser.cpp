#include "parser.h"
#include <cctype>
#include <stdexcept>
#include <cassert>

using namespace std;

static void printIndent(int indent) {
    for (int i = 0; i < indent; i++)
    {
        cout << "  ";
    }
}

void TypeNode::print(int indent) const
{
    printIndent(indent);
    cout << "Type(" << name << ")\n";
}
void LiteralExpr::print(int indent) const
{
    printIndent(indent);  cout << "Literal(" << value << ")\n";
}
void IdentExpr::print(int indent) const
{
    printIndent(indent);  cout << "Ident(" << name << ")\n";
}
void UnaryExpr::print(int indent) const
{
    printIndent(indent);  cout << "Unary(" << op.value << ")\n";
    if (right) right->print(indent+1);
}
void PostfixExpr::print(int indent) const
{
    printIndent(indent);  cout << "Postfix(" << op.value << ")\n";
    if (left) left->print(indent+1);
}
void BinaryExpr::print(int indent) const
{
    printIndent(indent);  cout << "Binary(" << op.value << ")\n";
    if (left) left->print(indent+1);
    if (right) right->print(indent+1);
}
void CallExpr::print(int indent) const
{
    printIndent(indent);  cout << "Call\n";
    if (callee)
    {
        callee->print(indent+1);
    }
    for (auto &a : args) if (a) a->print(indent+1);
}
void MemberExpr::print(int indent) const
{
    printIndent(indent);  cout << "Member(" << member << ")\n";
    if (object) object->print(indent+1);
}
void IndexExpr::print(int indent) const
{
    printIndent(indent);  cout << "Index\n";
    if (object) object->print(indent+1);
    if (index) index->print(indent+1);
}
void ExprStmt::print(int indent) const
{
    printIndent(indent);  cout << "ExprStmt\n";
    if (expr) expr->print(indent+1);
}
void ReturnStmt::print(int indent) const
{
    printIndent(indent);  cout << "Return\n";
    if (expr && *expr) (*expr)->print(indent+1);
}
void BlockStmt::print(int indent) const
{
    printIndent(indent);  cout << "Block\n";
    for (auto &s : statements) if (s) s->print(indent+1);
}
void VarDecl::print(int indent) const
{
    printIndent(indent);
    cout << "VarDecl " << name;
    if (typeNode)  cout << " : " << typeNode->name;
    cout << "\n";
    if (initializer && *initializer) (*initializer)->print(indent+1);
}
void IfStmt::print(int indent) const
{
    printIndent(indent);  cout << "If\n";
    if (condition) condition->print(indent+1);
    if (thenBranch) thenBranch->print(indent+1);
    if (elseBranch) {
        printIndent(indent);  cout << "Else\n";
        elseBranch->print(indent+1);
    }
}
void WhileStmt::print(int indent) const
{
    printIndent(indent);  cout << "While\n";
    if (condition) condition->print(indent+1);
    if (body) body->print(indent+1);
}
void ForStmt::print(int indent) const
{
    printIndent(indent);  cout << "For\n";
    if (init) { printIndent(indent+1);  cout << "Init\n"; (*init)->print(indent+2); }
    if (condition) { printIndent(indent+1);  cout << "Cond\n"; (*condition)->print(indent+2); }
    if (post) { printIndent(indent+1);  cout << "Post\n"; (*post)->print(indent+2); }
    if (body) body->print(indent+1);
}
void BreakStmt::print(int indent) const { printIndent(indent);  cout << "Break\n"; }
void ContinueStmt::print(int indent) const { printIndent(indent);  cout << "Continue\n"; }
void FuncDecl::print(int indent) const {
    printIndent(indent);
    cout << "Func " << name << "(";
    for (size_t i=0;i<params.size(); ++i) {
        if (i)  cout << ", ";
        cout << params[i].name << ":" << params[i].type.name;
    }
    cout << ")";
    if (retType)  cout << " -> " << retType->name;
    cout << "\n";
    if (body) body->print(indent+1);
}
void Program::print(int indent) const {
    printIndent(indent);  cout << "Program\n";
    for (auto &it : items) if (it) it->print(indent+1);
}

// ---------------- parser internals ----------------
Parser::Parser(const  vector<Token>& tokens)
    : tokens_(tokens), pos_(0), lexUtil_(), scope_stack_(), function_depth_(0), loop_depth_(0) {
    // initial global scope
    push_scope();
}

// ---------- token helpers ----------
const Token& Parser::peek() const {
    return peek_n(1);
}
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
    string name = lexUtil_.tokenTypeToString(t.type);
    if (name == "T_EOF" || name == "EOF") return true;
    if (t.value.empty() && name == "") return true;
    return false;
}

// token checks: direct by TokenType
bool Parser::matchType(TokenType t) {
    if (is_at_end()) return false;
    if (peek().type == t) { advance(); return true; }
    return false;
}
bool Parser::matchAnyType(const  initializer_list<TokenType>& types) {
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

// fallback / compatibility lexeme-based checks (kept for convenience)
bool Parser::matchLexeme(const  string &lexeme) {
    if (is_at_end()) return false;
    if (peek().value == lexeme) { advance(); return true; }
    return false;
}
bool Parser::matchEither(const  string &lexOrType) {
    if (matchLexeme(lexOrType)) return true;
    const Token &p = peek();
    if (lexUtil_.tokenTypeToString(p.type) == lexOrType) { advance(); return true; }
    return false;
}
bool Parser::checkEither(const  string &lexOrType) const {
    if (is_at_end()) return false;
    const Token &t = peek();
    if (t.value == lexOrType) return true;
    if (lexUtil_.tokenTypeToString(t.type) == lexOrType) return true;
    return false;
}
bool Parser::checkEitherLexemeOrType(const  string &lexOrType) const {
    return checkEither(lexOrType);
}

// expect helpers
const Token& Parser::expectType(TokenType t, ParseErrorKind errKind) {
    if (checkType(t)) return advance();
    throw make_error(errKind, &peek(),  string("Expected token type ") + lexUtil_.tokenTypeToString(t));
}
const Token& Parser::expectEither(const  string &lexOrType, ParseErrorKind errKind) {
    if (checkEither(lexOrType)) return advance();
    throw make_error(errKind, &peek(),  string("Expected token ") + lexOrType);
}

// ---------------- scope helpers ----------------
void Parser::push_scope() {
    scope_stack_.push_back( unordered_map< string,int>{});
}
void Parser::pop_scope() {
    if (!scope_stack_.empty()) scope_stack_.pop_back();
}
void Parser::declare_symbol_in_current_scope(const  string &name) {
    if (scope_stack_.empty()) push_scope();
    scope_stack_.back()[name] += 1;
}
bool Parser::is_declared_in_any_scope(const  string &name) const {
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->find(name) != it->end()) return true;
    }
    return false;
}
bool Parser::is_declared_in_current_scope(const  string &name) const {
    if (scope_stack_.empty()) return false;
    return scope_stack_.back().find(name) != scope_stack_.back().end();
}

// --------- context helpers ----------
void Parser::enter_function() { ++function_depth_; }
void Parser::exit_function() { if (function_depth_>0) --function_depth_; }
void Parser::enter_loop() { ++loop_depth_; }
void Parser::exit_loop() { if (loop_depth_>0) --loop_depth_; }

// ---------- error & recovery ----------
ParseError Parser::make_error(ParseErrorKind kind, const Token* t,  string msg) const {
    ParseError e;
    e.kind = kind;
    if (t) e.token = *t;
    e.message =  move(msg);
    return e;
}
ParseError Parser::make_error(ParseErrorKind kind, const Token& t,  string msg) const {
    return make_error(kind, &t,  move(msg));
}

// simple synchronize: skip tokens until a likely statement boundary / top-level decl
void Parser::synchronize() {
    if (!is_at_end()) advance();

    while (!is_at_end()) {
        const Token &t = peek();
        string tname = lexUtil_.tokenTypeToString(t.type);
        if (t.value == ";" || tname == "T_SEMICOLON") { advance(); return; }
        if (t.value == "}" || tname == "T_RBRACE") return;
        if (t.value == "fn" || tname == "T_FN" || t.value == "let" || t.value == "const"
            || tname == "T_LET" || tname == "T_CONST") return;
        advance();
    }
}

// ---------------- top-level ----------------
unique_ptr<Program> Parser::parse_program() {
    auto program =  make_unique<Program>();
    while (!is_at_end()) {
        try {
            StmtPtr s = parse_declaration();
            if (s) program->items.push_back( move(s));
        } catch (const ParseError &err) {
            cerr << "ParseError: " << err.message << "\n";
            synchronize();
        }
    }
    return program;
}

// --------------- declarations & statements ----------------
StmtPtr Parser::parse_declaration() {
    // handle C-style typed var decls and functions first:
    const Token &p = peek();
    string tname = lexUtil_.tokenTypeToString(p.type);

    bool isTypeToken = (tname == "T_INT" || p.value == "int"
        || tname == "T_FLOAT" || p.value == "float"
        || tname == "T_BOOL" || p.value == "bool"
        || tname == "T_STRING" || p.value == "string");

    if (isTypeToken) {
        // consume type token
        Token typeTok = advance();
        string typename_text = typeTok.value;

        // expect identifier
        if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && ( isalpha((unsigned char)peek().value[0]) || peek().value[0] == '_'))) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after type '" + typename_text + "'");
        }
        Token idTok = advance();
        string name = idTok.value;

        // Lookahead: if next token is '(' -> function decl (C style)
        if (checkEither("(") || lexUtil_.tokenTypeToString(peek().type) == "T_LPAREN" || lexUtil_.tokenTypeToString(peek().type) == "T_PARENL" || peek().value == "(") {
            // delegate to helper that expects we've already consumed typeTok and idTok
            return parse_function_decl_after_header(typeTok, idTok);
        }

        // Otherwise treat as variable declaration (with optional initializer/semicolon)
        optional<ExprPtr> initializer;
        if (matchEither("=") || checkEither("T_ASSIGN") || checkEither("T_ASSIGNOP")) {
            // if we didn't consume the assign via matchEither, make sure to advance
            if (!(previous().value == "=" || lexUtil_.tokenTypeToString(previous().type) == "T_ASSIGN" || lexUtil_.tokenTypeToString(previous().type) == "T_ASSIGNOP")) {
                advance();
            }
            initializer = parse_expression();
        }

        // optional semicolon
        if (matchEither(";") || checkEither("T_SEMICOLON")) {
            // consumed if present
        }

        if (is_declared_in_current_scope(name)) {
            throw make_error(ParseErrorKind::DuplicateDeclaration, &idTok, "Duplicate declaration of " + name);
        }
        declare_symbol_in_current_scope(name);

        return make_unique<VarDecl>(name, optional<TypeNode>(TypeNode(typename_text)), move(initializer));
    }

    // 'fn' style function decls
    if (checkEither("fn") || lexUtil_.tokenTypeToString(p.type) == "T_FN") {
        return parse_function_decl();
    }

    // let/const style
    if (checkEither("let") || checkEither("const") || lexUtil_.tokenTypeToString(p.type) == "T_LET" || lexUtil_.tokenTypeToString(p.type) == "T_CONST") {
        return parse_var_decl_or_stmt();
    }

    // otherwise statement
    return parse_statement();
}

StmtPtr Parser::parse_var_decl_or_stmt() {
    Token kw = advance(); // consume let/const
    if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after var/let");
    }
    Token idTok = advance();
    string name = idTok.value;

    if (is_declared_in_current_scope(name)) {
        throw make_error(ParseErrorKind::DuplicateDeclaration, &idTok, "Duplicate declaration of " + name);
    }
    declare_symbol_in_current_scope(name);

    optional<TypeNode> typeNode;
    if (matchEither(":") || checkEither("T_COLON")) {
        if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected type name after ':'");
        Token typeTok = advance();
        typeNode = TypeNode(typeTok.value);
    }

    optional<ExprPtr> initializer;
    if (matchEither("=") || checkEither("T_ASSIGN")) {
        if (!(previous().value == "=" || lexUtil_.tokenTypeToString(previous().type) == "T_ASSIGN")) {
            // consume assign token if necessary (attempt)
            advance();
        }
        initializer = parse_expression();
    }

    if (matchEither(";") || checkEither("T_SEMICOLON")) {
        // consumed
    }
    return make_unique<VarDecl>(name, move(typeNode), move(initializer));
}

// NEW helper: parse a C-style function declaration after we've consumed type and identifier
StmtPtr Parser::parse_function_decl_after_header(const Token& typeTok, const Token& nameTok) {
    string fname = nameTok.value;
    string returnTypeName = typeTok.value;

    // At this point peek() should be '('
    expectEither("(", ParseErrorKind::FailedToFindToken);

    vector<FuncParam> params;
    // parse params of the form: ( type ident, type ident, ... )
    if (!checkEither(")") && !checkEither("T_RPAREN")) {
        while (true) {
            // parameter type
            const Token &ptypeTok = peek();
            string ptypeName = lexUtil_.tokenTypeToString(ptypeTok.type);
            // Accept token types like T_INT or lexeme "int"
            if (!(ptypeName == "T_INT" || ptypeTok.value == "int" ||
                  ptypeName == "T_FLOAT" || ptypeTok.value == "float" ||
                  ptypeName == "T_STRING" || ptypeTok.value == "string" ||
                  ptypeName == "T_BOOL" || ptypeTok.value == "bool")) {
                throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected parameter type");
            }
            Token consumedType = advance(); // consume type token

            // parameter name
            if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
                throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected parameter name");
            }
            Token pnameTok = advance();

            params.push_back(FuncParam{pnameTok.value, TypeNode(consumedType.value)});

            // comma or end
            if (checkEither(")") || checkEither("T_RPAREN")) break;
            if (matchEither(",") || matchEither("T_COMMA")) {
                // continue
                continue;
            } else {
                throw make_error(ParseErrorKind::InvalidParameterList, &peek(), "Expected ',' or ')' in parameter list");
            }
        }
    }

    expectEither(")", ParseErrorKind::FailedToFindToken);

    optional<TypeNode> retType;
    // return type already captured from typeTok; we can keep it
    retType = TypeNode(returnTypeName);

    // now parse body: require '{'
    // enter function context and push param scope
    enter_function();
    push_scope();
    // declare params
    for (auto &p : params) declare_symbol_in_current_scope(p.name);

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

    // declare function name in current scope (if desired)
    if (is_declared_in_current_scope(fname)) {
        throw make_error(ParseErrorKind::DuplicateDeclaration, &nameTok, "Duplicate declaration of " + fname);
    }
    declare_symbol_in_current_scope(fname);

    return make_unique<FuncDecl>(fname, move(params), move(retType), move(body));
}

StmtPtr Parser::parse_function_decl() {
    // consume 'fn'
    advance();
    if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && ( isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected function name after fn");
    }
    Token nameTok = advance();
    string fname = nameTok.value;

    expectEither("(", ParseErrorKind::FailedToFindToken);
    vector<FuncParam> params;
    if (!checkEither(")") && !checkEither("T_RPAREN")) {
        do {
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected parameter name");
            Token pnameTok = advance();
            expectEither(":", ParseErrorKind::FailedToFindToken);
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected type in parameter");
            Token ptypeTok = advance();
            params.push_back(FuncParam{pnameTok.value, TypeNode(ptypeTok.value)});
        } while (matchEither(",") || matchEither("T_COMMA"));
    }
    expectEither(")", ParseErrorKind::FailedToFindToken);

    optional<TypeNode> retType;
    if (matchEither(":") || matchEither("T_COLON")) {
        if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected return type after ':'");
        Token rtok = advance();
        retType = TypeNode(rtok.value);
    }

    // parse body as block; set function context and new scope for params
    enter_function();
    push_scope();
    for (auto &p : params) declare_symbol_in_current_scope(p.name);

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

    if (is_declared_in_current_scope(fname)) {
        throw make_error(ParseErrorKind::DuplicateDeclaration, &nameTok, "Duplicate declaration of " + fname);
    }
    declare_symbol_in_current_scope(fname);

    return make_unique<FuncDecl>(fname, move(params), move(retType), move(body));
}

unique_ptr<BlockStmt> Parser::parse_block() {
    // expect '{'
    if (!matchEither("{") && !matchEither("T_LBRACE")) {
        throw make_error(ParseErrorKind::FailedToFindToken, &peek(), "Expected '{' for block");
    }
    auto block =  make_unique<BlockStmt>();
    push_scope();
    while (!checkEither("}") && !checkEither("T_RBRACE") && !is_at_end()) {
        try {
            StmtPtr s = parse_declaration();
            if (s) block->statements.push_back( move(s));
        } catch (const ParseError &err) {
            cerr << "ParseError in block: " << err.message << "\n";
            synchronize();
        }
    }
    expectEither("}", ParseErrorKind::FailedToFindToken);
    pop_scope();
    return block;
}

StmtPtr Parser::parse_statement() {
    if (matchEither("return") || matchEither("T_RETURN")) return parse_return_stmt();
    if (matchEither("if") || matchEither("T_IF")) return parse_if_stmt();
    if (matchEither("while") || matchEither("T_WHILE")) return parse_while_stmt();
    if (matchEither("for") || matchEither("T_FOR")) return parse_for_stmt();
    if (matchEither("break") || matchEither("T_BREAK")) return parse_break_stmt();
    if (matchEither("continue") || matchEither("T_CONTINUE")) return parse_continue_stmt();
    if (matchEither("{") || matchEither("T_LBRACE")) {
        // parser consumed '{' here; back up and let parse_block handle it uniformly
        --pos_;
        auto b = parse_block();
        return StmtPtr( move(b));
    }

    // var decl inside a statement
    if (checkEither("let") || checkEither("const") || checkEither("T_LET") || checkEither("T_CONST")) {
        return parse_var_decl_or_stmt();
    }

    // expression statement
    ExprPtr e = parse_expression();
    // optional semicolon
    if (matchEither(";") || matchEither("T_SEMICOLON")) {
        return make_unique<ExprStmt>( move(e));
    } else {
        // allow bare expression as statement
        return make_unique<ExprStmt>( move(e));
    }
}

StmtPtr Parser::parse_return_stmt() {
    // 'return' has been consumed already (by parse_statement)
    if (function_depth_ == 0) {
        throw make_error(ParseErrorKind::MisplacedReturn, &previous(), "return outside function");
    }

    optional<ExprPtr> maybe;
    if (!checkEither(";") && !checkEither("T_SEMICOLON")) {
        maybe = parse_expression();
    }
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // missing semicolon allowed (but could throw)
    }
    return make_unique<ReturnStmt>( move(maybe));
}

StmtPtr Parser::parse_if_stmt() {
    // '(' expected after 'if'
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> thenBlock;
    StmtPtr thenStmt;

    // if next token is block, parse block; otherwise parse single statement and wrap into block
    if (checkEither("{") || checkEither("T_LBRACE")) {
        thenBlock = parse_block();
    } else {
        thenStmt = parse_statement();
        thenBlock =  make_unique<BlockStmt>();
        thenBlock->statements.push_back( move(thenStmt));
    }

    unique_ptr<BlockStmt> elseBlock = nullptr;
    if (matchEither("else") || matchEither("T_ELSE")) {
        if (checkEither("{") || checkEither("T_LBRACE")) {
            elseBlock = parse_block();
        } else {
            StmtPtr elseStmt = parse_statement();
            auto eb =  make_unique<BlockStmt>();
            eb->statements.push_back( move(elseStmt));
            elseBlock =  move(eb);
        }
    }

    return StmtPtr(new IfStmt( move(cond),  move(thenBlock),  move(elseBlock)));
}

StmtPtr Parser::parse_while_stmt() {
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> body;
    // enter loop context so nested 'break'/'continue' are valid
    enter_loop();
    try {
        if (checkEither("{") || checkEither("T_LBRACE")) {
            body = parse_block();
        } else {
            StmtPtr s = parse_statement();
            auto b =  make_unique<BlockStmt>();
            b->statements.push_back( move(s));
            body =  move(b);
        }
    } catch (const ParseError &err) {
        // ensure we exit loop context on error
        exit_loop();
        throw;
    }
    exit_loop();

    return StmtPtr(new WhileStmt( move(cond),  move(body)));
}

StmtPtr Parser::parse_for_stmt() {
    expectEither("(", ParseErrorKind::FailedToFindToken);

    optional<StmtPtr> init;

    if (!checkEither(";")) {
        const Token &p = peek();
        string tname = lexUtil_.tokenTypeToString(p.type);

        bool isTypeToken = (tname == "T_INT" || p.value == "int"
                         || tname == "T_FLOAT" || p.value == "float"
                         || tname == "T_BOOL" || p.value == "bool"
                         || tname == "T_STRING" || p.value == "string");

        if (isTypeToken) {
            // Parse typed initializer: type ident [= expr]
            Token typeTok = advance(); // consume type
            if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && (isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
                throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after type in for-init");
            }
            Token idTok = advance();
            string name = idTok.value;

            optional<ExprPtr> initializer;
            if (matchEither("=") || checkEither("T_ASSIGN") || checkEither("T_ASSIGNOP")) {
                if (!(previous().value == "=" || lexUtil_.tokenTypeToString(previous().type) == "T_ASSIGN" || lexUtil_.tokenTypeToString(previous().type) == "T_ASSIGNOP")) {
                    advance();
                }
                initializer = parse_expression();
            }

            // declare in current scope (simpler than trying to alter block scoping here)
            if (is_declared_in_current_scope(name)) {
                throw make_error(ParseErrorKind::DuplicateDeclaration, &idTok, "Duplicate declaration of " + name);
            }
            declare_symbol_in_current_scope(name);

            init = make_unique<VarDecl>(name, optional<TypeNode>(TypeNode(typeTok.value)), move(initializer));
        }
        else if (checkEither("let") || checkEither("T_LET") || checkEither("const") || checkEither("T_CONST")) {
            init = parse_var_decl_or_stmt();
        } else {
            ExprPtr e = parse_expression();
            init =  make_unique<ExprStmt>( move(e));
        }
    }

    expectEither(";", ParseErrorKind::FailedToFindToken);

    optional<ExprPtr> cond;
    if (!checkEither(";")) {
        cond = parse_expression();
    }
    expectEither(";", ParseErrorKind::FailedToFindToken);

    optional<ExprPtr> post;
    if (!checkEither(")")) {
        post = parse_expression();
    }
    expectEither(")", ParseErrorKind::FailedToFindToken);

    unique_ptr<BlockStmt> body;
    // enter loop context so nested 'break'/'continue' are valid
    enter_loop();
    try {
        if (checkEither("{") || checkEither("T_LBRACE")) {
            body = parse_block();
        } else {
            StmtPtr s = parse_statement();
            auto b =  make_unique<BlockStmt>();
            b->statements.push_back( move(s));
            body =  move(b);
        }
    } catch (const ParseError &err) {
        exit_loop();
        throw;
    }
    exit_loop();

    return StmtPtr(new ForStmt( move(init),  move(cond),  move(post),  move(body)));
}

StmtPtr Parser::parse_break_stmt() {
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // allow missing semicolon
    }
    if (loop_depth_ == 0) throw make_error(ParseErrorKind::MisplacedBreakContinue, &previous(), "break outside loop");
    return make_unique<BreakStmt>();
}
StmtPtr Parser::parse_continue_stmt() {
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // allow missing semicolon
    }
    if (loop_depth_ == 0) throw make_error(ParseErrorKind::MisplacedBreakContinue, &previous(), "continue outside loop");
    return make_unique<ContinueStmt>();
}

// ------------------ Expressions (Pratt parser) ------------------
static bool isNumberLexemeLocal(const  string &s) {
    if (s.empty()) return false;
    size_t i = 0;
    if (s[0] == '+' || s[0] == '-') i = 1;
    bool hasDigit = false, hasDot = false;
    for (; i < s.size(); ++i) {
        if ( isdigit((unsigned char)s[i])) hasDigit = true;
        else if (s[i] == '.') { if (hasDot) return false; hasDot = true; }
        else return false;
    }
    return hasDigit;
}

ExprPtr Parser::parse_expression() {
    return parse_assignment();
}

ExprPtr Parser::parse_assignment() {
    auto left = parse_pratt(0);

    // support assignment and compound assignments
    if (checkEither("=") || checkEither("T_ASSIGN") || checkEither("T_ASSIGNOP") ||
        peek().value == "+=" || peek().value == "-=" || peek().value == "*=" || peek().value == "/=" ||
        peek().value == "<<=" || peek().value == ">>=" || peek().value == "&=" || peek().value == "|=" || peek().value == "^=") {
        Token op = advance();
        if (!dynamic_cast<IdentExpr*>(left.get()) &&
            !dynamic_cast<MemberExpr*>(left.get()) &&
            !dynamic_cast<IndexExpr*>(left.get())) {
            throw make_error(ParseErrorKind::InvalidAssignmentTarget, &op, "Invalid assignment target");
        }
        auto right = parse_assignment(); // right-assoc for assignment
        return make_unique<BinaryExpr>( move(left), op,  move(right));
    }
    return left;
}

// prefix binding power
int Parser::prefix_bp_by_lexeme(const  string &op) const {
    if (op == "!" || op == "T_NOT") return 15;
    if (op == "-" || op == "T_MINUS") return 15;
    if (op == "+" || op == "T_PLUS") return 15;
    if (op == "*" /* unary deref */) return 15; // allow unary * as deref
    return 0;
}
int Parser::prefix_bp_by_type(TokenType t) const {
    string tn = lexUtil_.tokenTypeToString(t);
    if (tn == "T_NOT") return 15;
    if (tn == "T_MINUS") return 15;
    if (tn == "T_PLUS") return 15;
    if (tn == "T_STAR") return 15; // unary *
    return 0;
}

// infix binding power: return (lbp, rbp)
// NOTE: do NOT treat '(' as a normal infix operator here. Calls are handled
// as a special postfix in parse_pratt.
pair<int,int> Parser::infix_bp_by_lexeme(const  string &op) const {
    if (op == "*" || op == "/" || op == "%") return {13,13};
    if (op == "+" || op == "-") return {12,12};
    if (op == "<<" || op == ">>") return {11,11};      // shifts
    if (op == "&") return {10,10};                     // bitwise and
    if (op == "^") return {9,9};                       // bitwise xor
    if (op == "|") return {8,8};                       // bitwise or
    if (op == "==" || op == "!=") return {7,7};
    if (op == "<" || op == ">" || op == "<=" || op == ">=") return {7,7};
    if (op == "&&") return {5,5};
    if (op == "||") return {4,4};
    if (op == "," ) return {1,1}; // comma operator lowest precedence
    return {0,0};
}
pair<int,int> Parser::infix_bp_by_type(TokenType t) const {
    string tn = lexUtil_.tokenTypeToString(t);
    if (tn == "T_STAR" || tn == "T_DIV" || tn == "T_MOD") return {13,13};
    if (tn == "T_PLUS" || tn == "T_MINUS") return {12,12};
    if (tn == "T_LSHIFT" || tn == "T_RSHIFT" || tn == "T_LEFTSHIFT" || tn == "T_RIGHTSHIFT") return {11,11};
    if (tn == "T_AND" || tn == "&") return {10,10};
    if (tn == "T_XOR" || tn == "^") return {9,9};
    if (tn == "T_OR" || tn == "|") return {8,8};
    if (tn == "T_EQ" || tn == "T_NEQ" || tn == "==" || tn == "!=") return {7,7};
    if (tn == "T_LT" || tn == "T_GT" || tn == "T_LE" || tn == "T_GE") return {7,7};
    if (tn == "T_ANDAND" || tn == "T_LOGICALAND" || tn == "&&") return {5,5};
    if (tn == "T_OROR" || tn == "T_LOGICALOR" || tn == "||") return {4,4};
    if (tn == "T_COMMA") return {1,1};
    return {0,0};
}

ExprPtr Parser::parse_pratt(int min_bp) {
    const Token &t0 = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t0, "Unexpected EOF in expression");
    ExprPtr left;
    string tlex = t0.value;
    string tname = lexUtil_.tokenTypeToString(t0.type);

    int p_bp = prefix_bp_by_lexeme(tlex);
    if (p_bp == 0) p_bp = prefix_bp_by_type(t0.type);

    if (p_bp > 0) {
        Token op = advance();
        ExprPtr rhs = parse_pratt(p_bp);
        left =  make_unique<UnaryExpr>(op,  move(rhs));
    } else {
        left = parse_primary();
    }

    while (!is_at_end()) {
        const Token &cur = peek();
        string opLex = cur.value;
        pair<int,int> bp = infix_bp_by_lexeme(opLex);
        if (bp.first == 0) bp = infix_bp_by_type(cur.type);
        if (bp.first == 0 || bp.first < min_bp) {
            // handle postfix/calls/index/member before breaking
            if (opLex == "(" || lexUtil_.tokenTypeToString(cur.type) == "T_LPAREN" || lexUtil_.tokenTypeToString(cur.type) == "T_PARENL") {
                // function call
                advance(); // consume '('
                vector<ExprPtr> args;
                if (!checkEither(")") && !checkEither("T_RPAREN")) {
                    do {
                        args.push_back(parse_expression());
                    } while (matchEither(",") || matchEither("T_COMMA"));
                }
                expectEither(")", ParseErrorKind::FailedToFindToken);
                left =  make_unique<CallExpr>( move(left),  move(args));
                continue;
            } else if (opLex == "[" || lexUtil_.tokenTypeToString(cur.type) == "T_LBRACKET") {
                // index
                advance(); // consume '['
                ExprPtr idx = parse_expression();
                expectEither("]", ParseErrorKind::FailedToFindToken);
                left =  make_unique<IndexExpr>( move(left),  move(idx));
                continue;
            } else if (opLex == "." || lexUtil_.tokenTypeToString(cur.type) == "T_DOT" ||
                       opLex == "->" || lexUtil_.tokenTypeToString(cur.type) == "T_ARROW") {
                Token op = advance(); // consume '.' or '->'
                if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected member name after '.' or '->'");
                Token mem = advance();
                left =  make_unique<MemberExpr>( move(left), mem.value, op);
                continue;
            } else if (opLex == "++" || lexUtil_.tokenTypeToString(cur.type) == "T_PLUSPLUS" ||
                       opLex == "--" || lexUtil_.tokenTypeToString(cur.type) == "T_MINUSMINUS") {
                Token op = advance();
                left =  make_unique<PostfixExpr>( move(left), op);
                continue;
            } else {
                break;
            }
        }

        // binary infix operator
        Token op = advance();
        int rbp = bp.second;
        ExprPtr right = parse_pratt(rbp + 0);
        left =  make_unique<BinaryExpr>( move(left), op,  move(right));
    }

    return left;
}

ExprPtr Parser::parse_primary() {
    const Token &t = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t);

    string ttypeName = lexUtil_.tokenTypeToString(t.type);

    if (ttypeName.find("STRING") !=  string::npos || ttypeName == "T_STRINGLIT" || ttypeName == "T_STR") {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }

    if (ttypeName.find("INT") !=  string::npos || ttypeName == "T_INT_LITERAL") {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }
    if (ttypeName.find("FLOAT") !=  string::npos || ttypeName == "T_FLOAT_LITERAL") {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }

    if (t.value == "true" || t.value == "false" || ttypeName == "T_BOOL") {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }

    if (!t.value.empty() && isNumberLexemeLocal(t.value)) {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }

    if (!t.value.empty() && (t.value.front() == '"' || t.value.front() == '\'')) {
        Token tok = advance();
        return  make_unique<LiteralExpr>(tok.value);
    }

    if (!t.value.empty() && ( isalpha((unsigned char)t.value[0]) || t.value[0]=='_') || lexUtil_.tokenTypeToString(t.type) == "T_IDENTIFIER") {
        Token tok = advance();
        return  make_unique<IdentExpr>(tok.value);
    }

    if (t.value == "(" || lexUtil_.tokenTypeToString(t.type) == "T_LPAREN" || lexUtil_.tokenTypeToString(t.type) == "T_PARENL") {
        advance();
        ExprPtr e = parse_expression();
        expectEither(")", ParseErrorKind::FailedToFindToken);
        return e;
    }

    throw make_error(ParseErrorKind::ExpectedExpr, &t, "Expected expression.");
}

// convenience: parse argument list (unused by call handler here)
vector<ExprPtr> Parser::parse_argument_list() {
    vector<ExprPtr> args;
    if (!checkEither(")") && !checkEither("T_PARENR")) {
        do {
            args.push_back(parse_expression());
        } while (matchEither(",") || matchEither("T_COMMA"));
    }
    return args;
}
