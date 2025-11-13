#ifndef REGEX_LEXER_H
#define REGEX_LEXER_H

#include <string>
#include <vector>
#include "base_lexer.h"

using namespace std;

class RegexLexer {
public:
    RegexLexer() = default;
    vector<Token> tokenize(const string &src);
    string tokenTypeToString(TokenType type);
};

#endif