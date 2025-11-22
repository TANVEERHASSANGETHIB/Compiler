#ifndef BASE_LEXER_H
#define BASE_LEXER_H

#include <string>
#include <vector>

using namespace std;

enum TokenType {
  
    T_FUNCTION, T_INT, T_FLOAT, T_STRING, T_BOOL, T_RETURN,
    T_IF, T_ELSE, T_FOR, T_WHILE, T_BREAK, T_CONTINUE, T_LET, T_CONST,
    
    T_IDENTIFIER, T_INTLIT, T_FLOATLIT, T_STRINGLIT, T_BOOLLIT,
    
    T_ASSIGNOP, T_EQUALSOP, T_PLUS, T_MINUS, T_MULT, T_DIV, T_MOD,
    T_LT, T_GT, T_LTE, T_GTE, T_NEQ,
    T_AND, T_OR, T_NOT,
    T_BITAND, T_BITOR, T_BITXOR, T_BITNOT, 
    T_LEFTSHIFT, T_RIGHTSHIFT,
    T_INCREMENT, T_DECREMENT,
    T_PLUS_ASSIGN, T_MINUS_ASSIGN, T_MULT_ASSIGN, T_DIV_ASSIGN, T_MOD_ASSIGN,
    T_AND_ASSIGN, T_OR_ASSIGN, T_XOR_ASSIGN, T_LEFTSHIFT_ASSIGN, T_RIGHTSHIFT_ASSIGN,
    
    T_PARENL, T_PARENR, T_BRACEL, T_BRACER, T_BRACKL, T_BRACKR,
    T_COMMA, T_SEMICOLON, T_COLON, T_QUESTION, T_DOT, T_ARROW,
    
    
    T_COMMENT, T_UNKNOWN, T_EOF, T_INVALID_IDENTIFIER,T_VOID,

};

struct Token {
    TokenType type;
    string value;
    int line;
    int column;
    
    Token(TokenType t = T_UNKNOWN, string v = "", int l = 0, int c = 0) 
        : type(t), value(v), line(l), column(c) {}
};

string tokenTypeToString(TokenType type);

#endif