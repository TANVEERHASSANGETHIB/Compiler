#include "parser.h"
#include <cctype>
#include <stdexcept>

// ---------------- printing helpers ----------------
static void printIndent(int indent) {
    for (int i=0;i<indent;++i) std::cout << "  ";
}
void LiteralExpr::print(int indent) const {
    printIndent(indent); std::cout << "Literal(" << value << ")\n";
}
void IdentExpr::print(int indent) const {
    printIndent(indent); std::cout << "Ident(" << name << ")\n";
}
void UnaryExpr::print(int indent) const {
    printIndent(indent); std::cout << "Unary(" << op.value << ")\n";
    right->print(indent+1);
}
void BinaryExpr::print(int indent) const {
    printIndent(indent); std::cout << "Binary(" << op.value << ")\n";
    left->print(indent+1);
    right->print(indent+1);
}
void CallExpr::print(int indent) const {
    printIndent(indent); std::cout << "Call\n";
    callee->print(indent+1);
    for (auto &a : args) a->print(indent+1);
}
void ExprStmt::print(int indent) const {
    printIndent(indent); std::cout << "ExprStmt\n";
    expr->print(indent+1);
}
void ReturnStmt::print(int indent) const {
    printIndent(indent); std::cout << "Return\n";
    if (expr) (*expr)->print(indent+1);
}
void BlockStmt::print(int indent) const {
    printIndent(indent); std::cout << "Block\n";
    for (auto &s : statements) s->print(indent+1);
}
void VarDecl::print(int indent) const {
    printIndent(indent);
    std::cout << "VarDecl " << name;
    if (typeName) std::cout << " : " << *typeName;
    std::cout << "\n";
    if (initializer) (*initializer)->print(indent+1);
}
void FuncDecl::print(int indent) const {
    printIndent(indent);
    std::cout << "Func " << name << "(";
    for (size_t i=0;i<params.size(); ++i) {
        if (i) std::cout << ", ";
        std::cout << params[i].first << ":" << params[i].second;
    }
    std::cout << ")";
    if (retType) std::cout << " -> " << *retType;
    std::cout << "\n";
    if (body) body->print(indent+1);
}
void Program::print(int indent) const {
    printIndent(indent); std::cout << "Program\n";
    for (auto &it : items) it->print(indent+1);
}

// ---------------- parser internals ----------------
Parser::Parser(const std::vector<Token>& tokens)
    : tokens_(tokens), pos_(0), lexUtil_() {}

const Token& Parser::peek() const {
    if (pos_ < tokens_.size()) return tokens_[pos_];
    static Token eofTok; // default
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
    // prefer using tokenTypeToString for EOF, but tolerate empty token.value as EOF
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
const Token& Parser::expectEither(const std::string &lexOrType, ParseErrorKind errKind) {
    if (checkEither(lexOrType)) return advance();
    throw make_error(errKind, &peek(), "Expected token " + lexOrType);
}

ParseError Parser::make_error(ParseErrorKind kind, const Token* t, std::string msg) const {
    ParseError e;
    e.kind = kind;
    if (t) e.token = *t;
    e.message = std::move(msg);
    return e;
}

// ---------------- top-level ----------------
std::unique_ptr<Program> Parser::parse_program() {
    auto program = std::make_unique<Program>();
    while (!is_at_end()) {
        StmtPtr s = parse_declaration();
        if (s) program->items.push_back(std::move(s));
        else break;
    }
    return program;
}

// --------------- declarations & statements ----------------
StmtPtr Parser::parse_declaration() {
    // function declaration if lexeme 'fn' or type name T_FN
    if (checkEither("fn") || checkEither("T_FN")) {
        auto f = parse_function_decl();
        return StmtPtr(f.release());
    }
    // var/let (if your language uses let/const)
    if (checkEither("let") || checkEither("const") || checkEither("T_LET") || checkEither("T_CONST")) {
        // simple var decl
        advance(); // consume let/const
        if (!checkEither("T_IDENTIFIER") && !std::isalpha((unsigned char)peek().value[0])) {
            throw make_error(ParseErrorKind::ExpectedIdentifier, &peek());
        }
        std::string name = advance().value;
        std::optional<std::string> typeName;
        if (matchEither(":") || matchEither("T_COLON")) {
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek());
            typeName = advance().value;
        }
        std::optional<ExprPtr> init;
        if (matchEither("=") || matchEither("T_ASSIGN")) {
            init = parse_expression();
        }
        // optional semicolon
        if (matchEither(";") || matchEither("T_SEMICOLON")) {
            return std::make_unique<VarDecl>(name, typeName, std::move(init));
        } else {
            // treat newline/no-semicolon as okay for now
            return std::make_unique<VarDecl>(name, typeName, std::move(init));
        }
    }

    // otherwise statement
    return parse_statement();
}

std::unique_ptr<FuncDecl> Parser::parse_function_decl() {
    // expects: fn name ( params ) [: ret] { body }
    // consume 'fn'
    advance();
    if (!checkEither("T_IDENTIFIER") && !std::isalpha((unsigned char)peek().value[0])) {
        throw make_error(ParseErrorKind::ExpectedIdentifier, &peek());
    }
    std::string name = advance().value;
    expectEither("(", ParseErrorKind::FailedToFindToken);
    std::vector<std::pair<std::string,std::string>> params;
    if (!checkEither(")")) {
        do {
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedIdentifier, &peek());
            std::string pname = advance().value;
            expectEither(":", ParseErrorKind::FailedToFindToken);
            if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek());
            std::string ptype = advance().value;
            params.emplace_back(pname, ptype);
        } while (matchEither(",") || matchEither("T_COMMA"));
    }
    expectEither(")", ParseErrorKind::FailedToFindToken);
    std::optional<std::string> retType;
    if (matchEither(":") || matchEither("T_COLON")) {
        if (!checkEither("T_IDENTIFIER")) throw make_error(ParseErrorKind::ExpectedTypeToken, &peek());
        retType = advance().value;
    }
    // parse body as block
    auto body = parse_block();
    return std::make_unique<FuncDecl>(name, params, retType, std::move(body));
}

std::unique_ptr<BlockStmt> Parser::parse_block() {
    // expect '{'
    if (!matchEither("{") && !matchEither("T_LBRACE")) {
        throw make_error(ParseErrorKind::FailedToFindToken, &peek(), "Expected '{' for block");
    }
    auto block = std::make_unique<BlockStmt>();
    while (!checkEither("}") && !checkEither("T_RBRACE") && !is_at_end()) {
        block->statements.push_back(parse_declaration());
    }
    expectEither("}", ParseErrorKind::FailedToFindToken);
    return block;
}

StmtPtr Parser::parse_statement() {
    if (matchEither("return") || matchEither("T_RETURN")) return parse_return_stmt();
    if (matchEither("if") || matchEither("T_IF")) return parse_if_stmt();
    if (matchEither("while") || matchEither("T_WHILE")) return parse_while_stmt();
    if (matchEither("{") || matchEither("T_LBRACE")) {
        // we consumed '{', but parse_block expects to consume it; step back one pos
        --pos_;
        return parse_block();
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
        // allow missing semicolon but it's okay
    }
    return std::make_unique<ReturnStmt>(std::move(maybe));
}

StmtPtr Parser::parse_if_stmt() {
    // 'if' already consumed by caller
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);
    StmtPtr thenBranch;
    if (checkEither("{") || checkEither("T_LBRACE")) thenBranch = parse_block();
    else thenBranch = parse_statement();
    std::optional<StmtPtr> elseBranch;
    if (matchEither("else") || matchEither("T_ELSE")) {
        if (checkEither("{") || checkEither("T_LBRACE")) elseBranch = parse_block();
        else elseBranch = parse_statement();
    }
    // For simplicity, wrap then/else into a block: return block with them inside
    auto b = std::make_unique<BlockStmt>();
    b->statements.push_back(std::move(thenBranch));
    if (elseBranch) b->statements.push_back(std::move(*elseBranch));
    return b;
}

StmtPtr Parser::parse_while_stmt() {
    // 'while' consumed by caller
    expectEither("(", ParseErrorKind::FailedToFindToken);
    ExprPtr cond = parse_expression();
    expectEither(")", ParseErrorKind::FailedToFindToken);
    StmtPtr body;
    if (checkEither("{") || checkEither("T_LBRACE")) body = parse_block();
    else body = parse_statement();
    auto b = std::make_unique<BlockStmt>();
    b->statements.push_back(std::move(body));
    return b;
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
        auto right = parse_assignment();
        return std::make_unique<BinaryExpr>(std::move(left), op, std::move(right));
    }
    return left;
}

int Parser::prefix_bp(const std::string &op) const {
    if (op == "!" || op == "T_NOT" || op == "-" ) return 15;
    return 0;
}
std::pair<int,int> Parser::infix_bp(const std::string &op) const {
    if (op == "*" || op == "/" || op == "%") return {12,12};
    if (op == "+" || op == "-") return {11,11};
    if (op == "==" || op == "!=") return {8,8};
    if (op == "<" || op == ">" || op == "<=" || op == ">=") return {9,9};
    if (op == "&&") return {4,4};
    if (op == "||") return {3,3};
    if (op == "(") return {20,19}; // function call
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
    Token t = peek();
    ExprPtr left;
    std::string tlex = t.value;
    std::string tname = lexUtil_.tokenTypeToString(t.type);

    // prefix unary by lexeme or type name
    if (prefix_bp(tlex) > 0 || prefix_bp(tname) > 0) {
        Token op = advance();
        int bp = prefix_bp(op.value);
        ExprPtr rhs = parse_pratt(bp);
        left = std::make_unique<UnaryExpr>(op, std::move(rhs));
    } else {
        left = parse_primary();
    }

    while (true) {
        if (is_at_end()) break;
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
        ExprPtr right = parse_pratt(rbp + 0); // left-assoc default
        left = std::make_unique<BinaryExpr>(std::move(left), op, std::move(right));
    }

    return left;
}

ExprPtr Parser::parse_primary() {
    const Token &t = peek();
    if (is_at_end()) throw make_error(ParseErrorKind::UnexpectedEOF, &t);

    // Number literal detection by lexeme
    if (!t.value.empty() && isNumberLexeme(t.value[0] ? t.value : std::string())) {
        Token tok = advance();
        return std::make_unique<LiteralExpr>(tok.value);
    }
    // more robust number detection:
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
