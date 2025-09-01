#include <iostream>
#include <string>
#include <vector>
#include "manual_lexer.h"
#include "regex_lexer.h"

using namespace std;

void printTokens(const vector<Token>& tokens, RegexLexer& lexer) {
    for (const auto& token : tokens) {
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
        getline(cin, code);

        if (code == "exit") break;

        cout << "\n--- Manual Lexer ---\n";
        ManualLexer manualLexer(code);
        vector<Token> manualTokens = manualLexer.tokenize();
        for (const auto& token : manualTokens) {
            cout << token.value << " at line " << token.line << ", column " << token.column << "\n";
        }

        cout << "\n--- Regex Lexer ---\n";
        RegexLexer regexLexer;
        vector<Token> regexTokens = regexLexer.tokenize(code);
        printTokens(regexTokens, regexLexer);
    }

    cout << "\nExiting Lexer Program.\n";
    return 0;
}
