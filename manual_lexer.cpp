#include <iostream>
#include <vector>
#include <string>
#include <set>

using namespace std;

enum TokenType {
    T_FUNCTION, T_INT, T_FLOAT, T_STRING, T_BOOL, T_RETURN,
    T_IF, T_ELSE, T_FOR, T_WHILE, T_BREAK, T_CONTINUE,
    T_IDENTIFIER, T_INTLIT, T_FLOATLIT, T_STRINGLIT, T_BOOLLIT,
    T_ASSIGNOP, T_EQUALSOP, T_PLUS, T_MINUS, T_MULT, T_DIV, T_MOD,
    T_LT, T_GT, T_LTE, T_GTE, T_NEQ,
    T_AND, T_OR, T_NOT,
    T_BITAND, T_BITOR, T_BITXOR, T_BITNOT, T_LEFTSHIFT, T_RIGHTSHIFT,
    T_PARENL, T_PARENR, T_BRACEL, T_BRACER, T_BRACKL, T_BRACKR,
    T_COMMA, T_SEMICOLON, T_COLON, T_QUESTION, T_DOT,
    T_COMMENT, T_UNKNOWN, T_EOF,
    T_INVALID_IDENTIFIER, T_INCREMENT, T_PLUS_ASSIGN
};

struct Token {
    TokenType type;
    string value;
    int line;
    int column;
};

// List of keywords for comparison
const set<string> keywords = {
    "fn", "int", "float", "string", "bool", "return",
    "if", "else", "for", "while", "break", "continue",
    "true", "false"
};

vector<Token> tokenize(const string &src) {
    vector<Token> tokens;
    int pos = 0;
    int line = 1;
    int column = 1;

    while (pos < src.size()) {
        char c = src[pos];

        // Skip whitespace
        if (isspace(c)) {
            if (c == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            pos++;
            continue;
        }

        // Handle single-line comments
        if (pos + 1 < src.size() && src[pos] == '/' && src[pos + 1] == '/') {
            while (pos < src.size() && src[pos] != '\n') {
                pos++;
                column++;
            }
            continue;
        }

        // Handle multi-line comments
        if (pos + 1 < src.size() && src[pos] == '/' && src[pos + 1] == '*') {
            int start_line = line;
            int start_column = column;
            pos += 2;
            column += 2;
            while (pos + 1 < src.size() && !(src[pos] == '*' && src[pos + 1] == '/')) {
                if (src[pos] == '\n') {
                    line++;
                    column = 1;
                } else {
                    column++;
                }
                pos++;
            }
            if (pos + 1 >= src.size()) {
                cerr << "Error: Unclosed multi-line comment at line " << start_line << ", column " << start_column << endl;
                break;
            }
            pos += 2;
            column += 2;
            continue;
        }

        // Handle string literals
        if (c == '"') {
            string value = "\"";
            int start_line = line;
            int start_column = column;
            pos++;
            column++;
            bool escaped = false;
            while (pos < src.size() && (escaped || src[pos] != '"')) {
                if (src[pos] == '\n') {
                    line++;
                    column = 1;
                } else {
                    column++;
                }
                if (src[pos] == '\\' && !escaped) {
                    escaped = true;
                } else {
                    escaped = false;
                }
                value += src[pos];
                pos++;
            }
            if (pos >= src.size()) {
                cerr << "Error: Unclosed string literal at line " << start_line << ", column " << start_column << endl;
                break;
            }
            value += '"';
            pos++;
            column++;
            tokens.push_back({T_STRINGLIT, value, start_line, start_column});
            continue;
        }

        // Handle identifiers and keywords
        if (isalpha(c) || c == '_') {
            string value;
            int start_line = line;
            int start_column = column;
            bool invalid = false;
            while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) {
                value += src[pos];
                pos++;
                column++;
            }
            // Check for invalid characters after valid identifier characters
            if (pos < src.size() && !isspace(src[pos]) && !ispunct(src[pos])) {
                invalid = true;
                while (pos < src.size() && !isspace(src[pos]) && !ispunct(src[pos])) {
                    value += src[pos];
                    pos++;
                    column++;
                }
            }
            if (invalid) {
                cerr << "Error: Invalid identifier '" << value << "' at line " << start_line << ", column " << start_column << endl;
                tokens.push_back({T_INVALID_IDENTIFIER, value, start_line, start_column});
            } else if (keywords.count(value)) {
                TokenType type;
                if (value == "fn") type = T_FUNCTION;
                else if (value == "int") type = T_INT;
                else if (value == "float") type = T_FLOAT;
                else if (value == "string") type = T_STRING;
                else if (value == "bool") type = T_BOOL;
                else if (value == "return") type = T_RETURN;
                else if (value == "if") type = T_IF;
                else if (value == "else") type = T_ELSE;
                else if (value == "for") type = T_FOR;
                else if (value == "while") type = T_WHILE;
                else if (value == "break") type = T_BREAK;
                else if (value == "continue") type = T_CONTINUE;
                else if (value == "true" || value == "false") type = T_BOOLLIT;
                else type = T_IDENTIFIER; // Should not happen due to keywords set
                tokens.push_back({type, value, start_line, start_column});
            } else {
                tokens.push_back({T_IDENTIFIER, value, start_line, start_column});
            }
            continue;
        }

        // Handle invalid identifiers starting with digits
        if (isdigit(c)) {
            string value;
            int start_line = line;
            int start_column = column;
            bool is_float = false;
            bool is_hex = false;
            // Check for hexadecimal
            if (c == '0' && pos + 1 < src.size() && (src[pos + 1] == 'x' || src[pos + 1] == 'X')) {
                is_hex = true;
                value += src[pos];
                value += src[pos + 1];
                pos += 2;
                column += 2;
                while (pos < src.size() && (isdigit(src[pos]) || (tolower(src[pos]) >= 'a' && tolower(src[pos]) <= 'f'))) {
                    value += src[pos];
                    pos++;
                    column++;
                }
                tokens.push_back({T_INTLIT, value, start_line, start_column});
                continue;
            }
            // Collect digits
            while (pos < src.size() && isdigit(src[pos])) {
                value += src[pos];
                pos++;
                column++;
            }
            // Check for float
            if (pos < src.size() && src[pos] == '.') {
                is_float = true;
                value += src[pos];
                pos++;
                column++;
                while (pos < src.size() && isdigit(src[pos])) {
                    value += src[pos];
                    pos++;
                    column++;
                }
                // Check for exponent
                if (pos < src.size() && (src[pos] == 'e' || src[pos] == 'E')) {
                    value += src[pos];
                    pos++;
                    column++;
                    if (pos < src.size() && (src[pos] == '+' || src[pos] == '-')) {
                        value += src[pos];
                        pos++;
                        column++;
                    }
                    while (pos < src.size() && isdigit(src[pos])) {
                        value += src[pos];
                        pos++;
                        column++;
                    }
                }
            }
            // Check for invalid identifier (digits followed by letters)
            if (pos < src.size() && (isalpha(src[pos]) || src[pos] == '_')) {
                while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) {
                    value += src[pos];
                    pos++;
                    column++;
                }
                cerr << "Error: Invalid identifier '" << value << "' at line " << start_line << ", column " << start_column << endl;
                tokens.push_back({T_INVALID_IDENTIFIER, value, start_line, start_column});
                continue;
            }
            tokens.push_back({is_float ? T_FLOATLIT : T_INTLIT, value, start_line, start_column});
            continue;
        }

        // Handle operators and punctuation
        string value(1, c);
        TokenType type = T_UNKNOWN;
        int start_line = line;
        int start_column = column;

        if (pos + 1 < src.size()) {
            string two_chars = src.substr(pos, 2);
            if (two_chars == "==") { type = T_EQUALSOP; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "<=") { type = T_LTE; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == ">=") { type = T_GTE; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "!=") { type = T_NEQ; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "&&") { type = T_AND; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "||") { type = T_OR; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "++") { type = T_INCREMENT; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "+=") { type = T_PLUS_ASSIGN; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == "<<") { type = T_LEFTSHIFT; value = two_chars; pos += 2; column += 2; }
            else if (two_chars == ">>") { type = T_RIGHTSHIFT; value = two_chars; pos += 2; column += 2; }
            else {
                // Single-character operators and punctuation
                switch (c) {
                    case '=': type = T_ASSIGNOP; break;
                    case '+': type = T_PLUS; break;
                    case '-': type = T_MINUS; break;
                    case '*': type = T_MULT; break;
                    case '/': type = T_DIV; break;
                    case '%': type = T_MOD; break;
                    case '<': type = T_LT; break;
                    case '>': type = T_GT; break;
                    case '!': type = T_NOT; break;
                    case '&': type = T_BITAND; break;
                    case '|': type = T_BITOR; break;
                    case '^': type = T_BITXOR; break;
                    case '~': type = T_BITNOT; break;
                    case '(': type = T_PARENL; break;
                    case ')': type = T_PARENR; break;
                    case '{': type = T_BRACEL; break;
                    case '}': type = T_BRACER; break;
                    case '[': type = T_BRACKL; break;
                    case ']': type = T_BRACKR; break;
                    case ',': type = T_COMMA; break;
                    case ';': type = T_SEMICOLON; break;
                    case ':': type = T_COLON; break;
                    case '?': type = T_QUESTION; break;
                    case '.': type = T_DOT; break;
                    default:
                        cerr << "Error: Unknown token at line " << line << ", column " << column 
                             << " -> '" << c << "'" << endl;
                        break;
                }
                pos++;
                column++;
            }
        } else {
            switch (c) {
                case '=': type = T_ASSIGNOP; break;
                case '+': type = T_PLUS; break;
                case '-': type = T_MINUS; break;
                case '*': type = T_MULT; break;
                case '/': type = T_DIV; break;
                case '%': type = T_MOD; break;
                case '<': type = T_LT; break;
                case '>': type = T_GT; break;
                case '!': type = T_NOT; break;
                case '&': type = T_BITAND; break;
                case '|': type = T_BITOR; break;
                case '^': type = T_BITXOR; break;
                case '~': type = T_BITNOT; break;
                case '(': type = T_PARENL; break;
                case ')': type = T_PARENR; break;
                case '{': type = T_BRACEL; break;
                case '}': type = T_BRACER; break;
                case '[': type = T_BRACKL; break;
                case ']': type = T_BRACKR; break;
                case ',': type = T_COMMA; break;
                case ';': type = T_SEMICOLON; break;
                case ':': type = T_COLON; break;
                case '?': type = T_QUESTION; break;
                case '.': type = T_DOT; break;
                default:
                    cerr << "Error: Unknown token at line " << line << ", column " << column 
                         << " -> '" << c << "'" << endl;
                    break;
            }
            pos++;
            column++;
        }
        tokens.push_back({type, value, start_line, start_column});
    }

    tokens.push_back({T_EOF, "", line, column});
    return tokens;
}

string tokenTypeToString(TokenType type) {
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

int main() {
    string program = R"(
        fn int my_fn(int x, float y) {
            // This is a comment
            string my_str = "Hello \"world\"!\n\tEscaped";
            bool my_bool = true;
            int 123abc; // Invalid identifier
            float aap = .25;
            int my@var; // Invalid identifier
            int result = x * y + 10;
            
            if (x <= 40 && y != 0) {
                for (int i = 0; i < 10; i++) {
                    result += i & 0xFF;
                }
            }
            
            /* Multi-line
               comment */
            return result << 2;
        }
    )";

    vector<Token> tokens = tokenize(program);
    for (auto &t : tokens) {
        if (t.type != T_COMMENT) {
            cout << "Token(" << tokenTypeToString(t.type) << ", \"" << t.value 
                 << "\") at line " << t.line << ", column " << t.column << endl;
        }
    }
    
    return 0;
}