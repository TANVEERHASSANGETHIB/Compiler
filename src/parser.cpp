// parser.cpp
#include "parser.h"
#include <cctype>
#include <stdexcept>

// ---------------- printing helpers ----------------
static void printIndent(int indent) {
    for (int i=0;i<indent;++i) std::cout << "  ";
}

void TypeNode::print(int indent) const {
    printIndent(indent);
    std::cout << "Type(" << name << ")\n";
}

void LiteralExpr::print(int indent) const {
    printIndent(indent); std::cout << "Literal(" << value << ")\n";
}
void IdentExpr::print(int indent) const {
    printIndent(indent); std::cout << "Ident(" << name << ")\n";
}
void UnaryExpr::print(int indent) const {
    printIndent(indent); std::cout << "Unary(" << op.value << ")\n";
    if (right) right->print(indent+1);
}
void BinaryExpr::print(int indent) const {
    printIndent(indent); std::cout << "Binary(" << op.value << ")\n";
    if (left) left->print(indent+1);
    if (right) right->print(indent+1);
}
void CallExpr::print(int indent) const {
    printIndent(indent); std::cout << "Call\n";
    if (callee) callee->print(indent+1);
    for (auto &a : args) if (a) a->print(indent+1);
}
void ExprStmt::print(int indent) const {
    printIndent(indent); std::cout << "ExprStmt\n";
    if (expr) expr->print(indent+1);
}
void ReturnStmt::print(int indent) const {
    printIndent(indent); std::cout << "Return\n";
    if (expr && *expr) (*expr)->print(indent+1);
}
void BlockStmt::print(int indent) const {
    printIndent(indent); std::cout << "Block\n";
    for (auto &s : statements) if (s) s->print(indent+1);
}
void VarDecl::print(int indent) const {
    printIndent(indent);
    std::cout << "VarDecl " << name;
    if (typeNode) std::cout << " : " << typeNode->name;
    std::cout << "\n";
    if (initializer && *initializer) (*initializer)->print(indent+1);
}
void IfStmt::print(int indent) const {
    printIndent(indent); std::cout << "If\n";
    if (condition) condition->print(indent+1);
    if (thenBranch) thenBranch->print(indent+1);
    if (elseBranch && *elseBranch) {
        printIndent(indent); std::cout << "Else\n";
        (*elseBranch)->print(indent+1);
    }
}
void WhileStmt::print(int indent) const {
    printIndent(indent); std::cout << "While\n";
    if (condition) condition->print(indent+1);
    if (body) body->print(indent+1);
}
void ForStmt::print(int indent) const {
    printIndent(indent); std::cout << "For\n";
    if (init) { printIndent(indent+1); std::cout << "Init\n"; (*init)->print(indent+2); }
    if (condition) { printIndent(indent+1); std::cout << "Cond\n"; (*condition)->print(indent+2); }
    if (post) { printIndent(indent+1); std::cout << "Post\n"; (*post)->print(indent+2); }
    if (body) body->print(indent+1);
}
void BreakStmt::print(int indent) const {
    printIndent(indent); std::cout << "Break\n";
}
void ContinueStmt::print(int indent) const {
    printIndent(indent); std::cout << "Continue\n";
}
void FuncDecl::print(int indent) const {
    printIndent(indent);
    std::cout << "Func " << name << "(";
    for (size_t i=0;i<params.size(); ++i) {
        if (i) std::cout << ", ";
        std::cout << params[i].name << ":" << params[i].type.name;
    }
    std::cout << ")";
    if (retType) std::cout << " -> " << retType->name;
    std::cout << "\n";
    if (body) body->print(indent+1);
}
void Program::print(int indent) const {
    printIndent(indent); std::cout << "Program\n";
    for (auto &it : items) if (it) it->print(indent+1);
}

// ---------------- parser internals ----------------
Parser::Parser(const std::vector<Token>& tokens)
    : tokens_(tokens), pos_(0), lexUtil_() {}

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
    std::string name = lexUtil_.tokenTypeToString(t.type);
    if (name == "T_EOF" || name == "EOF" || t.value.empty()) return true;
    return false;
}

bool Parser::matchLexeme(const std::string &lexeme) {
    if (is_at_end()) return false;
    if (peek().value == lexeme) { advance(); return true; }
    return false;
}
bool Parser::matchTypeName(const std::string &typeName) {
    if (is_at_end()) return false;
    if (lexUtil_.tokenTypeToString(peek().type) == typeName) { advance(); return true; }
    return false;
}
bool Parser::matchEither(const std::string &lexOrType) {
    if (matchLexeme(lexOrType)) return true;
    return matchTypeName(lexOrType);
}
bool Parser::checkEither(const std::string &lexOrType) const {
    if (is_at_end()) return false;
    const Token &t = peek();
    if (t.value == lexOrType) return true;
    if (lexUtil_.tokenTypeToString(t.type) == lexOrType) return true;
    return false;
}

bool Parser::matchLexemeN(size_t n, const std::string &lexeme) {
    if (is_at_end()) return false;
    const Token &t = peek_n(n);
    if (t.value == lexeme) { size_t target = pos_ + (n-1); pos_ = target + 1; return true; }
    return false;
}
bool Parser::matchTypeNameN(size_t n, const std::string &typeName) const {
    if (is_at_end()) return false;
    const Token &t = peek_n(n);
    return lexUtil_.tokenTypeToString(t.type) == typeName;
}

const Token& Parser::expectEither(const std::string &lexOrType, ParseErrorKind errKind) {
    if (checkEither(lexOrType)) return advance();
    throw make_error(errKind, &peek(), std::string("Expected token ") + lexOrType);
}

ParseError Parser::make_error(ParseErrorKind kind, const Token* t, std::string msg) const {
    ParseError e;
    e.kind = kind;
    if (t) e.token = *t;
    e.message = std::move(msg);
    return e;
}
ParseError Parser::make_error(ParseErrorKind kind, const Token& t, std::string msg) const {
    return make_error(kind, &t, std::move(msg));
}

// simple synchronize: skip tokens until a likely statement boundary / decl start
void Parser::synchronize() {
    advance();
    while (!is_at_end()) {
        const Token &t = peek();
        if (t.value == ";" || lexUtil_.tokenTypeToString(t.type) == "T_SEMICOLON") { advance(); return; }
        std::string n = lexUtil_.tokenTypeToString(t.type);
        if (n == "T_FN" || t.value == "fn" || t.value == "let" || t.value == "const" ||
            n == "T_LET" || n == "T_CONST") return;
        advance();
    }
}

// ---------------- top-level ----------------
std::unique_ptr<Program> Parser::parse_program() {
    auto program = std::make_unique<Program>();
    while (!is_at_end()) {
        try {
            StmtPtr s = parse_declaration();
            if (s) program->items.push_back(std::move(s));
        } catch (const ParseError &err) {
            // report (user may handle), attempt recovery
            std::cerr << "ParseError: " << err.message << "\n";
            synchronize();
        }
    }
    return program;
}

// --------------- declarations & statements ----------------
StmtPtr Parser::parse_declaration() {
    // --- Accept C-style 'type name = expr;' declarations where a type token appears first ---
    // Common type tokens: T_INT, T_FLOAT, T_BOOL, T_STRING (adjust as your lexer defines them)
    if ( checkEither("T_INT") || checkEither("int")
      || checkEither("T_FLOAT") || checkEither("float")
      || checkEither("T_BOOL") || checkEither("bool")
      || checkEither("T_STRING") || checkEither("string")
      /* add more type tokens here if needed */ ) {

        // consume the type token
        std::string tname = advance().value; // e.g., "int"

        // next must be identifier (variable name)
        if (!checkEither("T_IDENTIFIER")) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after type '" + tname + "'");
        }
        std::string name = advance().value;

        // optional initializer
        std::optional<ExprPtr> initializer;
        if ( matchEither("=") || matchEither("T_ASSIGNOP") || matchEither("T_ASSIGN") || matchEither("T_ASSIGNOP") ) {
            initializer = parse_expression();
        }

        // optional semicolon
        if (matchEither(";") || matchEither("T_SEMICOLON")) {
            // consumed
        } // if missing semicolon we allow it (or throw MissingSemicolon if you prefer)

        // record simple duplicate-decl check (same as in parse_var_decl_or_stmt)
        if (local_scope_count_.find(name) != local_scope_count_.end()) {
            throw make_error(ParseErrorKind::DuplicateDeclaration, &previous(), "Duplicate declaration of " + name);
        }
        local_scope_count_[name] = 1;

        return std::make_unique<VarDecl>(name, std::optional<TypeNode>(TypeNode(tname)), std::move(initializer));
    }

    // --- function declarations ('fn') ---
    if (checkEither("fn") || checkEither("T_FN")) {
        auto f = parse_function_decl();
        return StmtPtr(f.release());
    }

    // --- let/const style declarations ---
    if (checkEither("let") || checkEither("const") || checkEither("T_LET") || checkEither("T_CONST")) {
        return parse_var_decl_or_stmt();
    }

    // otherwise statement
    return parse_statement();
}

StmtPtr Parser::parse_var_decl_or_stmt() {
    // This handles top-level or local var declarations
    // consume let/const
    Token kw = advance();
    // expect identifier
    if (!checkEither("T_IDENTIFIER") && !(peek().value.size() && (std::isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected identifier after var/let");
    }
    std::string name = advance().value;

    // duplicate decl check (simple)
    if (local_scope_count_.find(name) != local_scope_count_.end()) {
        throw make_error(ParseErrorKind::DuplicateDeclaration, &previous(), "Duplicate declaration of " + name);
    }
    local_scope_count_[name] = 1;

    std::optional<TypeNode> typeNode;
    if (matchEither(":") || matchEither("T_COLON")) {
        if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected type name after ':'");
        typeNode = TypeNode(advance().value);
    }
    std::optional<ExprPtr> initializer;
    if (matchEither("=") || matchEither("T_ASSIGN")) {
        initializer = parse_expression();
    }
    // optional semicolon
    if (matchEither(";") || matchEither("T_SEMICOLON")) {
        // ok
    }
    return std::make_unique<VarDecl>(name, std::move(typeNode), std::move(initializer));
}

std::unique_ptr<FuncDecl> Parser::parse_function_decl() {
    // expects: fn name ( params ) [: ret] { body }
    // consume 'fn'
    advance();
    if (!checkEither("T_IDENTIFIER") && ! (peek().value.size() && (std::isalpha((unsigned char)peek().value[0]) || peek().value[0]=='_'))) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected function name after fn");
    }
    std::string name = advance().value;
    expectEither("(", ParseErrorKind::FailedToFindToken);
    std::vector<FuncParam> params;
    if (!checkEither(")") && !checkEither("T_RPAREN")) {
        do {
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedIdentifier, &peek(), "Expected parameter name");
            std::string pname = advance().value;
            expectEither(":", ParseErrorKind::FailedToFindToken);
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected type in parameter");
            std::string ptype = advance().value;
            params.push_back(FuncParam{pname, TypeNode(ptype)});
        } while (matchEither(",") || matchEither("T_COMMA"));
    }
    expectEither(")", ParseErrorKind::FailedToFindToken);
    std::optional<TypeNode> retType;
    if (matchEither(":") || matchEither("T_COLON")) {
        if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek(), "Expected return type after ':'");
        retType = TypeNode(advance().value);
    }
    // parse body as block
    auto body = parse_block();
    // create FuncDecl object
    std::vector<FuncParam> pcopy;
    for (auto &p : params) pcopy.push_back(p);
    return std::make_unique<FuncDecl>(name, std::move(pcopy), std::move(retType), std::move(body));
}

std::unique_ptr<BlockStmt> Parser::parse_block() {
    // expect '{'
    if (!matchEither("{") && !matchEither("T_LBRACE")) {
        throw make_error(ParseErrorKind::FailedToFindToken, &peek(), "Expected '{' for block");
    }
    auto block = std::make_unique<BlockStmt>();
    while (!checkEither("}") && !checkEither("T_RBRACE") && !is_at_end()) {
        try {
            StmtPtr s = parse_declaration();
            if (s) block->statements.push_back(std::move(s));
        } catch (const ParseError &err) {
            std::cerr << "ParseError in block: " << err.message << "\n";
            synchronize();
        }
    }
    expectEither("}", ParseErrorKind::FailedToFindToken);
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
        // parse_block expects '{' consumed, but we've consumed it; step back to let parse_block handle it
        --pos_;
        return parse_block();
    }

    // expression statement or var declaration inside a statement (e.g., "let x = 5;")
    if (checkEither("let") || checkEither("const") || checkEither("T_LET") || checkEither("T_CONST")) {
        return parse_var_decl_or_stmt();
    }

    // expression statement
    ExprPtr e = parse_expression();
    // optional semicolon
    if (matchEither(";") || matchEither("T_SEMICOLON")) {
        return std::make_unique<ExprStmt>(std::move(e));
    } else {
        // allow bare expression as statement
        return std::make_unique<ExprStmt>(std::move(e));
    }
}

StmtPtr Parser::parse_return_stmt() {
    std::optional<ExprPtr> maybe;
    if (!checkEither(";") && !checkEither("T_SEMICOLON")) {
        maybe = parse_expression();
    }
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // allow missing semicolon but it's okay; could throw MissingSemicolon if desired
    }
    return std::make_unique<ReturnStmt>(std::move(maybe));
}

StmtPtr Parser::parse_if_stmt() {
    // 'if' already consumed by caller
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);
    std::unique_ptr<BlockStmt> thenBlock;
    StmtPtr thenStmt;
    if (checkEither("{") || checkEither("T_LBRACE")) {
        thenBlock = parse_block();
        // wrap thenBlock into a statement pointer
        return StmtPtr(new IfStmt(std::move(cond), std::move(thenBlock), std::optional<std::unique_ptr<BlockStmt>>{}));
    } else {
        thenStmt = parse_statement();
        // put thenStmt inside a block
        auto b = std::make_unique<BlockStmt>();
        b->statements.push_back(std::move(thenStmt));
        std::optional<std::unique_ptr<BlockStmt>> elseb;
        if (matchEither("else") || matchEither("T_ELSE")) {
            if (checkEither("{") || checkEither("T_LBRACE")) {
                elseb = parse_block();
            } else {
                StmtPtr elseStmt = parse_statement();
                auto eb = std::make_unique<BlockStmt>();
                eb->statements.push_back(std::move(elseStmt));
                elseb = std::move(eb);
            }
        }
        return StmtPtr(new IfStmt(std::move(cond), std::move(b), std::move(elseb)));
    }
}

StmtPtr Parser::parse_while_stmt() {
    // 'while' consumed by caller
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);
    std::unique_ptr<BlockStmt> body;
    if (checkEither("{") || checkEither("T_LBRACE")) body = parse_block();
    else {
        StmtPtr s = parse_statement();
        auto b = std::make_unique<BlockStmt>();
        b->statements.push_back(std::move(s));
        body = std::move(b);
    }
    return StmtPtr(new WhileStmt(std::move(cond), std::move(body)));
}

StmtPtr Parser::parse_for_stmt() {
    // 'for' already consumed
    expectEither("(", ParseErrorKind::FailedToFindToken);
    // init
    std::optional<StmtPtr> init;
    if (!checkEither(";")) {
        if (checkEither("let") || checkEither("T_LET") || checkEither("const") || checkEither("T_CONST")) {
            init = parse_var_decl_or_stmt();
        } else {
            ExprPtr e = parse_expression();
            init = std::make_unique<ExprStmt>(std::move(e));
        }
    }
    expectEither(";", ParseErrorKind::FailedToFindToken);
    // cond
    std::optional<ExprPtr> cond;
    if (!checkEither(";")) {
        cond = parse_expression();
    }
    expectEither(";", ParseErrorKind::FailedToFindToken);
    // post
    std::optional<ExprPtr> post;
    if (!checkEither(")")) {
        post = parse_expression();
    }
    expectEither(")", ParseErrorKind::FailedToFindToken);
    // body
    std::unique_ptr<BlockStmt> body;
    if (checkEither("{") || checkEither("T_LBRACE")) body = parse_block();
    else {
        StmtPtr s = parse_statement();
        auto b = std::make_unique<BlockStmt>();
        b->statements.push_back(std::move(s));
        body = std::move(b);
    }
    return StmtPtr(new ForStmt(std::move(init), std::move(cond), std::move(post), std::move(body)));
}

StmtPtr Parser::parse_break_stmt() {
    // 'break' consumed
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // allow missing semicolon
    }
    return std::make_unique<BreakStmt>();
}
StmtPtr Parser::parse_continue_stmt() {
    // 'continue' consumed
    if (!matchEither(";") && !matchEither("T_SEMICOLON")) {
        // allow missing semicolon
    }
    return std::make_unique<ContinueStmt>();
}

// ------------------ Expressions (Pratt) ------------------
ExprPtr Parser::parse_expression() {
    return parse_assignment();
}

ExprPtr Parser::parse_assignment() {
    auto left = parse_pratt(0);
    // assignment lexeme '=' or T_ASSIGN
    if (matchEither("=") || matchEither("T_ASSIGN")) {
        Token op = previous();
        // simple check: left must be ident or indexing/member etc. For now, only allow IdentExpr
        // If left is not an IdentExpr, raise InvalidAssignmentTarget
        if (!dynamic_cast<IdentExpr*>(left.get())) {
            throw make_error(ParseErrorKind::InvalidAssignmentTarget, &op, "Invalid assignment target");
        }
        auto right = parse_assignment();
        return std::make_unique<BinaryExpr>(std::move(left), op, std::move(right));
    }
    return left;
}

int Parser::prefix_bp(const std::string &op) const {
    if (op == "!" || op == "T_NOT") return 15;
    if (op == "-" || op == "T_MINUS") return 15;
    if (op == "+") return 15;
    return 0;
}
std::pair<int,int> Parser::infix_bp(const std::string &op) const {
    if (op == "*" || op == "T_STAR" || op == "/") return {12,12};
    if (op == "+" || op == "-" || op == "T_PLUS" || op == "T_MINUS") return {11,11};
    if (op == "==" || op == "!=") return {8,8};
    if (op == "<" || op == ">" || op == "<=" || op == ">=") return {9,9};
    if (op == "&&" || op == "T_AND") return {4,4};
    if (op == "||" || op == "T_OR") return {3,3};
    if (op == "(" || op == "T_LPAREN") return {20,19}; // call: left-assoc special
    return {0,0};
}

static bool isNumberLexeme(const std::string &s) {
    if (s.empty()) return false;
    size_t i = 0;
    if (s[0] == '+' || s[0] == '-') i = 1;
    bool hasDigit = false, hasDot = false;
    for (; i < s.size(); ++i) {
        if (std::isdigit((unsigned char)s[i])) hasDigit = true;
        else if (s[i] == '.') { if (hasDot) return false; hasDot = true; }
        else return false;
    }
    return hasDigit;
}

ExprPtr Parser::parse_pratt(int min_bp) {
    // prefix
    const Token &t0 = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t0, "Unexpected EOF in expression");
    ExprPtr left;
    std::string tlex = t0.value;
    std::string tname = lexUtil_.tokenTypeToString(t0.type);

    int p_bp = prefix_bp(tlex);
    if (p_bp == 0) p_bp = prefix_bp(tname);

    if (p_bp > 0) {
        Token op = advance();
        ExprPtr rhs = parse_pratt(p_bp);
        left = std::make_unique<UnaryExpr>(op, std::move(rhs));
    } else {
        left = parse_primary();
    }

    while (!is_at_end()) {
        const Token &cur = peek();
        std::string opLex = cur.value;
        std::pair<int,int> bp = infix_bp(opLex);
        if (bp.first == 0) bp = infix_bp(lexUtil_.tokenTypeToString(cur.type));
        if (bp.first == 0 || bp.first < min_bp) break;

        // function call special-case: '(' after a primary
        if (opLex == "(" || lexUtil_.tokenTypeToString(cur.type) == "T_LPAREN") {
            // call
            advance(); // consume '('
            std::vector<ExprPtr> args;
            if (!checkEither(")") && !checkEither("T_RPAREN")) {
                do {
                    args.push_back(parse_expression());
                } while (matchEither(",") || matchEither("T_COMMA"));
            }
            expectEither(")", ParseErrorKind::FailedToFindToken);
            left = std::make_unique<CallExpr>(std::move(left), std::move(args));
            continue;
        }

        Token op = advance();
        int rbp = bp.second;
        // for left-assoc operators we parse at rbp+0, for right-assoc we'd use rbp-1 convention.
        ExprPtr right = parse_pratt(rbp + 0);
        left = std::make_unique<BinaryExpr>(std::move(left), op, std::move(right));
    }

    return left;
}

ExprPtr Parser::parse_primary() {
    const Token &t = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t);

    // Number literal detection
    if (!t.value.empty() && isNumberLexeme(t.value)) {
        Token tok = advance();
        return std::make_unique<LiteralExpr>(tok.value);
    }

    // string literal (starting with quote)
    if (!t.value.empty() && (t.value.front() == '"' || t.value.front() == '\'')) {
        Token tok = advance();
        return std::make_unique<LiteralExpr>(tok.value);
    }

    // boolean literal
    if (t.value == "true" || t.value == "false") {
        Token tok = advance();
        return std::make_unique<LiteralExpr>(tok.value);
    }

    // identifier
    if (!t.value.empty() && (std::isalpha((unsigned char)t.value[0]) || t.value[0]=='_')) {
        Token tok = advance();
        return std::make_unique<IdentExpr>(tok.value);
    }

    // parenthesized
    if (t.value == "(" || lexUtil_.tokenTypeToString(t.type) == "T_LPAREN") {
        advance();
        ExprPtr e = parse_expression();
        expectEither(")", ParseErrorKind::FailedToFindToken);
        return e;
    }

    throw make_error(ParseErrorKind::ExpectedExpr, &t, "Expected expression.");
}

// helpers for parsing lists (unused externally but handy)
std::vector<ExprPtr> Parser::parse_argument_list() {
    std::vector<ExprPtr> args;
    if (!checkEither(")") && !checkEither("T_RPAREN")) {
        do {
            args.push_back(parse_expression());
        } while (matchEither(",") || matchEither("T_COMMA"));
    }
    return args;
}
std::vector<StmtPtr> Parser::parse_statement_list() {
    std::vector<StmtPtr> out;
    while (!is_at_end() && !checkEither("}") && !checkEither("T_RBRACE")) {
        out.push_back(parse_declaration());
    }
    return out;
}
