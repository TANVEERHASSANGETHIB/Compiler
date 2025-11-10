#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <streambuf>
#include "regex_lexer.h"
#include "parser.h"
#include "scope_errors.h"

using namespace std;

void printTokens(const vector<Token>& tokens, RegexLexer& lexer) {
    for (const auto& token : tokens) {
        cout << lexer.tokenTypeToString(token.type)
             << " (" << token.value << ")"
             << " at line " << token.line
             << ", column " << token.column << "\n";
    }
}

int main(int argc, char** argv) {
    // Determine input filename
    string filename;
    if (argc >= 2) {
        filename = argv[1];
    } else {
        filename = "C:/Users/Admin/OneDrive/Desktop/compil/Compiler/src/input.txt"; // default
    }

    // Read entire file into a string
    ifstream ifs(filename, ios::in | ios::binary);
    if (!ifs) {
        cout << "Failed to open input file: " << filename << "\n";
        cout << "Usage: " << (argc > 0 ? argv[0] : "lexer_app") << " [input-file.txt]\n";
        return 1;
    }

    string code;
    // Read whole file
    code.assign((istreambuf_iterator<char>(ifs)), istreambuf_iterator<char>());
    ifs.close();

    if (code.empty()) {
        cout << "Input file is empty: " << filename << "\n";
        return 0;
    }

    cout << "\n--- Regex Lexer ---\n";
    RegexLexer regexLexer;
    vector<Token> regexTokens = regexLexer.tokenize(code);

    if (regexTokens.empty()) {
        cout << "(lexer produced no tokens)\n";
        return 0;
    }

    // Print token stream returned by lexer
    printTokens(regexTokens, regexLexer);

    cout << "\n--- Parser / AST ---\n";
    try {
        // Pass exact token vector from lexer to parser.
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
    } catch (const ScopeError &e) {
        cout << "Scope error: " << e.what() << "\n";
    } catch (const std::exception &ex) {
        cout << "Exception while parsing: " << ex.what() << "\n";
    } catch (...) {
        cout << "Unknown error occurred during parsing.\n";
    }

    cout << "\nDone. Parsed file: " << filename << "\n";
    return 0;
}