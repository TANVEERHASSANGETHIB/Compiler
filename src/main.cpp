#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <streambuf>
#include "regex_lexer.h"
#include "parser.h"
#include "scope_errors.h"
#include "type_checker.h"

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
    string filename;
    if (argc >= 2) {
        filename = argv[1];
    } else {
        filename = "input.txt";
    }

    ifstream ifs(filename, ios::in | ios::binary);
    if (!ifs) {
        cout << "Failed to open input file: " << filename << "\n";
        cout << "Usage: " << (argc > 0 ? argv[0] : "compiler") << " [input-file.txt]\n";
        return 1;
    }

    string code;
    code.assign((istreambuf_iterator<char>(ifs)), istreambuf_iterator<char>());
    ifs.close();

    if (code.empty()) {
        cout << "Input file is empty: " << filename << "\n";
        return 0;
    }

    cout << "--- Lexer / Tokenization ---\n";
    RegexLexer regexLexer;
    vector<Token> regexTokens = regexLexer.tokenize(code);

    if (regexTokens.empty()) {
        cout << "(lexer produced no tokens)\n";
        return 0;
    }

    printTokens(regexTokens, regexLexer);

    cout << "\n--- Parser / AST ---\n";
    Parser parser(regexTokens);
    auto program = parser.parse_program();

    vector<ParseError> parse_errors = parser.get_errors();
    if (!parse_errors.empty()) {
        cout << "Parse errors:\n";
        for (const auto& error : parse_errors) {
            cout << "  Error: " << error.message;
            if (error.token) {
                cout << " at line " << error.token->line << ", column " << error.token->column;
            }
            cout << "\n";
        }
    }

    if (program) {
        program->print();
    } else {
        cout << "Parser produced no program AST.\n";
    }

    if (!parse_errors.empty()) {
        cout << "Stopping due to parse errors\n";
        return 1;
    }

    cout << "\n--- Type Checking ---\n";
    TypeChecker type_checker(parser.get_scope_stack());
    bool type_check_passed = type_checker.check(program.get());
    
    vector<TypeCheckException> type_errors = type_checker.get_errors();
    if (!type_errors.empty()) {
        cout << "Type check errors:\n";
        for (const auto& error : type_errors) {
            cout << "  Error: " << error.what();
            if (error.token) {
                cout << " at line " << error.token->line << ", column " << error.token->column;
            }
            cout << "\n";
        }
    }

    if (type_check_passed) {
        cout << "Type checking passed!\n";
    } else {
        cout << "Type checking failed!\n";
    }

    cout << "\nDone. Parsed file: " << filename << "\n";
    return type_check_passed ? 0 : 1;
}