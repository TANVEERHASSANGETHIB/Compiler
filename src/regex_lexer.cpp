#include "regex_lexer.h"
#include <regex>
#include <iostream>

using namespace std;

static vector<pair<regex, TokenType>> tokenPatterns = {
    {regex("^fn"), T_FUNCTION}, {regex("^int"), T_INT}, {regex("^float"), T_FLOAT},
    {regex("^string"), T_STRING}, {regex("^bool"), T_BOOL}, {regex("^return"), T_RETURN},
    {regex("^if"), T_IF}, {regex("^else"), T_ELSE}, {regex("^for"), T_FOR},
    {regex("^while"), T_WHILE}, {regex("^break"), T_BREAK}, {regex("^continue"), T_CONTINUE},
    {regex("^true|^false"), T_BOOLLIT},
    {regex("^[0-9][a-zA-Z0-9_]*"), T_INVALID_IDENTIFIER},
    {regex("^[a-zA-Z_][a-zA-Z0-9_]*[^a-zA-Z0-9_\\s;{}()\\[\\],=+\\-*/%&|^~<>?:.\"]"), T_INVALID_IDENTIFIER},
    {regex("^[a-zA-Z_][a-zA-Z0-9_]*"), T_IDENTIFIER},
    {regex("^[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?"), T_FLOATLIT},
    {regex("^[0-9]+\\.[0-9]+"), T_FLOATLIT},
    {regex("^0[xX][0-9a-fA-F]+"), T_INTLIT},
    {regex("^[0-9]+"), T_INTLIT},
    {regex("^\"([^\"\\\\]|\\\\.)*\""), T_STRINGLIT},
    {regex("^=="), T_EQUALSOP}, {regex("^="), T_ASSIGNOP},
    {regex("^\\+\\+"), T_INCREMENT}, {regex("^\\+="), T_PLUS_ASSIGN},
    {regex("^\\+"), T_PLUS}, {regex("^-"), T_MINUS},
    {regex("^\\*"), T_MULT}, {regex("^/"), T_DIV},
    {regex("^%"), T_MOD}, {regex("^<="), T_LTE},
    {regex("^>="), T_GTE}, {regex("^<"), T_LT},
    {regex("^>"), T_GT}, {regex("^!="), T_NEQ},
    {regex("^&&"), T_AND}, {regex("^\\|\\|"), T_OR},
    {regex("^!"), T_NOT},
    {regex("^&"), T_BITAND}, {regex("^\\|"), T_BITOR},
    {regex("^\\^"), T_BITXOR}, {regex("^~"), T_BITNOT},
    {regex("^<<"), T_LEFTSHIFT}, {regex("^>>"), T_RIGHTSHIFT},
    {regex("^\\("), T_PARENL}, {regex("^\\)"), T_PARENR},
    {regex("^\\{"), T_BRACEL}, {regex("^\\}"), T_BRACER},
    {regex("^\\["), T_BRACKL}, {regex("^\\]"), T_BRACKR},
    {regex("^,"), T_COMMA}, {regex("^;"), T_SEMICOLON},
    {regex("^:"), T_COLON}, {regex("^\\?"), T_QUESTION},
    {regex("^\\."), T_DOT},
    {regex("^//[^\n]*"), T_COMMENT},
};

vector<Token> RegexLexer::tokenize(const string &src) {
    vector<Token> tokens;
    string code = src;
    int line = 1, column = 1;
    smatch match;

    while (!code.empty()) {
        if (regex_search(code, match, regex("^\\s+"))) {
            for (char c : match.str()) {
                if (c == '\n') { line++; column = 1; }
                else { column++; }
            }
            code = match.suffix().str();
            continue;
        }

        if (regex_search(code, match, regex("^/\\*"))) {
            code = match.suffix().str();
            size_t end_pos = code.find("*/");
            if (end_pos == string::npos) {
                cerr << "Error: Unclosed multi-line comment at line " << line << ", column " << column << endl;
                break;
            }
            string comment_content = code.substr(0, end_pos);
            for (char c : comment_content) {
                if (c == '\n') { line++; column = 1; }
                else { column++; }
            }
            code = code.substr(end_pos + 2);
            column += 2;
            continue;
        }

        bool matched = false;

        for (auto &tp : tokenPatterns) {
            if (regex_search(code, match, tp.first)) {
                string token_value = match.str();

                if (tp.second == T_COMMENT) {
                    for (char c : token_value) {
                        if (c == '\n') { line++; column = 1; }
                        else { column++; }
                    }
                    code = match.suffix().str();
                    matched = true;
                    break;
                }

                if (tp.second == T_INVALID_IDENTIFIER) {
                    cerr << "Error: Invalid identifier '" << token_value
                         << "' at line " << line << ", column " << column << endl;
                }

                tokens.push_back({tp.second, token_value, line, column});

                for (char c : token_value) {
                    if (c == '\n') { line++; column = 1; }
                    else { column++; }
                }

                code = match.suffix().str();
                matched = true;
                break;
            }
        }

        if (!matched) {
            cerr << "Error: Unknown token '" << code.substr(0, 1)
                 << "' at line " << line << ", column " << column << endl;
            tokens.push_back({T_UNKNOWN, string(1, code[0]), line, column});
            column++;
            code.erase(0, 1);
        }
    }

    tokens.push_back({T_EOF, "", line, column});
    return tokens;
}

string RegexLexer::tokenTypeToString(TokenType type) {
    switch(type) {
        case T_FUNCTION: return "T_FUNCTION";
        case T_INT: return "T_INT";
        case T_FLOAT: return "T_FLOAT";
        case T_STRING: return "T_STRING";
        case T_BOOL: return "T_BOOL";
        case T_RETURN: return "T_RETURN";
        case T_IF: return "T_IF";
        case T_ELSE: return "T_ELSE";
        case T_FOR: return "T_FOR";
        case T_WHILE: return "T_WHILE";
        case T_BREAK: return "T_BREAK";
        case T_CONTINUE: return "T_CONTINUE";
        case T_IDENTIFIER: return "T_IDENTIFIER";
        case T_INTLIT: return "T_INTLIT";
        case T_FLOATLIT: return "T_FLOATLIT";
        case T_STRINGLIT: return "T_STRINGLIT";
        case T_BOOLLIT: return "T_BOOLLIT";
        case T_ASSIGNOP: return "T_ASSIGNOP";
        case T_EQUALSOP: return "T_EQUALSOP";
        case T_PLUS: return "T_PLUS";
        case T_MINUS: return "T_MINUS";
        case T_MULT: return "T_MULT";
        case T_DIV: return "T_DIV";
        case T_MOD: return "T_MOD";
        case T_LT: return "T_LT";
        case T_GT: return "T_GT";
        case T_LTE: return "T_LTE";
        case T_GTE: return "T_GTE";
        case T_NEQ: return "T_NEQ";
        case T_AND: return "T_AND";
        case T_OR: return "T_OR";
        case T_NOT: return "T_NOT";
        case T_BITAND: return "T_BITAND";
        case T_BITOR: return "T_BITOR";
        case T_BITXOR: return "T_BITXOR";
        case T_BITNOT: return "T_BITNOT";
        case T_LEFTSHIFT: return "T_LEFTSHIFT";
        case T_RIGHTSHIFT: return "T_RIGHTSHIFT";
        case T_PARENL: return "T_PARENL";
        case T_PARENR: return "T_PARENR";
        case T_BRACEL: return "T_BRACEL";
        case T_BRACER: return "T_BRACER";
        case T_BRACKL: return "T_BRACKL";
        case T_BRACKR: return "T_BRACKR";
        case T_COMMA: return "T_COMMA";
        case T_SEMICOLON: return "T_SEMICOLON";
        case T_COLON: return "T_COLON";
        case T_QUESTION: return "T_QUESTION";
        case T_DOT: return "T_DOT";
        case T_COMMENT: return "T_COMMENT";
        case T_UNKNOWN: return "T_UNKNOWN";
        case T_EOF: return "T_EOF";
        case T_INVALID_IDENTIFIER: return "T_INVALID_IDENTIFIER";
        case T_INCREMENT: return "T_INCREMENT";
        case T_PLUS_ASSIGN: return "T_PLUS_ASSIGN";
        default: return "UNKNOWN";
    }
}
