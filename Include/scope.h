#ifndef SCOPE_H
#define SCOPE_H

#include <string>
#include <unordered_map>
#include <vector>
#include <memory>
#include "base_lexer.h"

enum class SymbolType {
    VARIABLE,
    FUNCTION,
    PARAMETER
};

struct Symbol {
    std::string name;
    SymbolType type;
    TokenType dataType; // For type checking later (T_INT, T_FLOAT, etc.)
    
    // Default constructor
    Symbol() : name(""), type(SymbolType::VARIABLE), dataType(T_INT) {}
    
    // Parameterized constructor
    Symbol(const std::string& n, SymbolType t, TokenType dt = T_INT) 
        : name(n), type(t), dataType(dt) {}
};

class Scope {
private:
    std::unordered_map<std::string, Symbol> symbols;
    Scope* parent;
    std::string scopeName;
    int level;

public:
    Scope(Scope* p = nullptr, const std::string& name = "");
    bool addSymbol(const std::string& name, SymbolType type, TokenType dataType = T_INT);
    Symbol* lookup(const std::string& name);
    const Symbol* lookup(const std::string& name) const;
    Symbol* lookupCurrent(const std::string& name);
    const Symbol* lookupCurrent(const std::string& name) const;
    Scope* getParent() const { return parent; }
    int getLevel() const { return level; }
    const std::string& getName() const { return scopeName; }
    const std::unordered_map<std::string, Symbol>& getSymbols() const { return symbols; }
};

class ScopeStack {
private:
    std::vector<std::unique_ptr<Scope>> scopes;
    Scope* current;

public:
    ScopeStack();
    ~ScopeStack();
    
    void enterScope(const std::string& name = "");
    void exitScope();
    bool addSymbol(const std::string& name, SymbolType type, TokenType dataType = T_INT);
    Symbol* lookup(const std::string& name);
    const Symbol* lookup(const std::string& name) const;
    Symbol* lookupCurrent(const std::string& name);
    const Symbol* lookupCurrent(const std::string& name) const;
    Scope* getCurrentScope() const { return current; }
    bool isGlobalScope() const { return current && current->getLevel() == 0; }
};

#endif