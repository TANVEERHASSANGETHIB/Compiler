#ifndef SCOPE_H
#define SCOPE_H

#include <string>
#include <unordered_map>
#include <vector>
#include <memory>
#include "base_lexer.h"

using namespace std;

enum class SymbolType {
    VARIABLE,
    FUNCTION,
    PARAMETER
};

struct Symbol {
    string name;
    SymbolType type;
    TokenType dataType;
    vector<TokenType> paramTypes;
    int declarationLine;
    int declarationColumn;

    Symbol() : name(""), type(SymbolType::VARIABLE), dataType(T_INT), declarationLine(0), declarationColumn(0) {}
   
    Symbol(const string& n, SymbolType t, TokenType dt = T_INT, int line = 0, int col = 0)
        : name(n), type(t), dataType(dt), declarationLine(line), declarationColumn(col) {}
   
    Symbol(const string& n, SymbolType t, TokenType dt, const vector<TokenType>& params, int line = 0, int col = 0)
        : name(n), type(t), dataType(dt), paramTypes(params), declarationLine(line), declarationColumn(col) {}
};

class Scope {
private:
    unordered_multimap<string, Symbol> symbols;
    Scope* parent;
    string scopeName;
    int level;

public:
    Scope(Scope* p = nullptr, const string& name = "");
    bool addSymbol(const string& name, SymbolType type, TokenType dataType = T_INT, int line = 0, int col = 0);
    bool addSymbol(const string& name, SymbolType type, TokenType dataType, const vector<TokenType>& paramTypes, int line = 0, int col = 0);
    bool addFunction(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line = 0, int col = 0);
    Symbol* lookup(const string& name);
    const Symbol* lookup(const string& name) const;
    Symbol* lookupCurrent(const string& name);
    const Symbol* lookupCurrent(const string& name) const;
    const Symbol* lookupFunction(const string& name, const vector<TokenType>& argTypes) const;
    bool functionExists(const string& name, const vector<TokenType>& paramTypes) const;
    Scope* getParent() const { return parent; }
    int getLevel() const { return level; }
    const string& getName() const { return scopeName; }
    const unordered_multimap<string, Symbol>& getSymbols() const { return symbols; }
    void clear() { symbols.clear(); }
};

class ScopeStack {
private:
    vector<unique_ptr<Scope>> scopes;
    Scope* current;

public:
    ScopeStack();
    ~ScopeStack() = default;
    ScopeStack(ScopeStack&&) noexcept = default;
    ScopeStack& operator=(ScopeStack&&) noexcept = default;
    ScopeStack(const ScopeStack&) = delete;
    ScopeStack& operator=(const ScopeStack&) = delete;
   
    void enterScope(const string& name = "");
    void exitScope();
    bool addSymbol(const string& name, SymbolType type, TokenType dataType = T_INT, int line = 0, int col = 0);
    bool addFunction(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line = 0, int col = 0);
    Symbol* lookup(const string& name);
    const Symbol* lookup(const string& name) const;
    Symbol* lookupCurrent(const string& name);
    const Symbol* lookupCurrent(const string& name) const;
    const Symbol* lookupFunction(const string& name, const vector<TokenType>& argTypes) const;
    Scope* getCurrentScope() const { return current; }
    bool isGlobalScope() const { return current && current->getLevel() == 0; }
    void clear();
};

#endif