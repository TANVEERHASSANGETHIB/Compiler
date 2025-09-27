// main.cpp (updated)
#include <iostream>
#include <string>
#include <vector>
#include "regex_lexer.h"
#include "parser.h"

using namespace std;

void printTokens(const vector<Token>& tokens, RegexLexer& lexer) {
    for (const auto& token : tokens) {
        // tokenTypeToString should be safe for valid token.type as provided by lexer
        cout << lexer.tokenTypeToString(token.type)
             << " (" << token.value << ")"
             << " at line " << token.line
             << ", column " << token.column << "\n";
    }
}

int main() {
    string code;
    cout << "Enter code (type 'exit' to quit):\n";

    while (true) {
        cout << "\n> ";
        if (!std::getline(cin, code)) break;
        if (code == "exit") break;

        cout << "\n--- Regex Lexer ---\n";
        RegexLexer regexLexer;
        vector<Token> regexTokens = regexLexer.tokenize(code);

        if (regexTokens.empty()) {
            cout << "(lexer produced no tokens)\n";
            continue;
        }

        // Print token stream returned by lexer
        printTokens(regexTokens, regexLexer);

        cout << "\n--- Parser / AST ---\n";
        try {
            // Pass exact token vector from lexer to parser.
            // Do NOT fabricate an EOF token here; rely on the lexer to produce correct EOF if it does.
            Parser parser(regexTokens);
            auto program = parser.parse_program();

            if (program) {
                program->print();
            } else {
                cout << "Parser produced no program AST.\n";
            }

        } catch (const ParseError &e) {
            // Print detailed parse error info
            cout << "Parse error: ";
            if (!e.message.empty()) cout << e.message;
            else {
                // fallback by kind
                switch (e.kind) {
                    case ParseErrorKind::UnexpectedEOF: cout << "Unexpected EOF"; break;
                    case ParseErrorKind::FailedToFindToken: cout << "Failed to find token"; break;
                    case ParseErrorKind::ExpectedTypeToken: cout << "Expected type token"; break;
                    case ParseErrorKind::ExpectedIdentifier: cout << "Expected identifier"; break;
                    case ParseErrorKind::UnexpectedToken: cout << "Unexpected token"; break;
                    case ParseErrorKind::ExpectedFloatLit: cout << "Expected float literal"; break;
                    case ParseErrorKind::ExpectedIntLit: cout << "Expected int literal"; break;
                    case ParseErrorKind::ExpectedStringLit: cout << "Expected string literal"; break;
                    case ParseErrorKind::ExpectedBoolLit: cout << "Expected bool literal"; break;
                    case ParseErrorKind::ExpectedExpr: cout << "Expected expression"; break;
                    default: cout << "Unknown parse error"; break;
                }
            }
            if (e.token) {
                cout << " at token '" << e.token->value << "', line " << e.token->line << ", column " << e.token->column;
            }
            cout << "\n";
        } catch (const std::exception &ex) {
            cout << "Exception while parsing: " << ex.what() << "\n";
        } catch (...) {
            cout << "Unknown error occurred during parsing.\n";
        }
    }

    cout << "\nExiting Lexer+Parser Program.\n";
    return 0;
}
