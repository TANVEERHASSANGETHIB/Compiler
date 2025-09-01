#ifndef MANUAL_LEXER_H
#define MANUAL_LEXER_H

#include "base_lexer.h"
#include <vector>
#include <string>

class ManualLexer {
private:
       string source;
public:
    ManualLexer(const    string &src) : source(src) {}
       vector<Token> tokenize(); // method version
};

   vector<Token> tokenize(const    string &src); // keep free function if needed

#endif
