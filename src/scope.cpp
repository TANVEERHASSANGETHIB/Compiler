#include "scope.h"
#include <stdexcept>
#include <iostream>
using namespace std;

Scope::Scope(Scope* p, const string& name)
    : parent(p), scopeName(name), level(p ? p->level + 1 : 0) {}

// Fixed: Added line and col parameters
bool Scope::addSymbol(const string& name, SymbolType type, TokenType dataType, int line, int col) {
    if (type != SymbolType::FUNCTION) {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return false;
        }
    }
    symbols.emplace(name, Symbol(name, type, dataType, line, col));
    return true;
}

// Fixed: Added line and col parameters
bool Scope::addSymbol(const string& name, SymbolType type, TokenType dataType, const vector<TokenType>& paramTypes, int line, int col) {
    if (type == SymbolType::FUNCTION) {
        if (functionExists(name, paramTypes)) {
            return false;
        }
    } else {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return false;
        }
    }
   
    symbols.emplace(name, Symbol(name, type, dataType, paramTypes, line, col));
    return true;
}

// Fixed: Added line and col parameters
bool Scope::addFunction(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line, int col) {
    return addSymbol(name, SymbolType::FUNCTION, returnType, paramTypes, line, col);
}

Symbol* Scope::lookup(const string& name) {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return &it->second;
    }
    return nullptr;
}

const Symbol* Scope::lookup(const string& name) const {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return &it->second;
    }
    return nullptr;
}

Symbol* Scope::lookupCurrent(const string& name) {
    auto it = symbols.find(name);
    return it != symbols.end() ? &it->second : nullptr;
}

const Symbol* Scope::lookupCurrent(const string& name) const {
    auto it = symbols.find(name);
    return it != symbols.end() ? &it->second : nullptr;
}

const Symbol* Scope::lookupFunction(const string& name, const vector<TokenType>& argTypes) const {
    auto range = symbols.equal_range(name);
    for (auto it = range.first; it != range.second; ++it) {
        if (it->second.type == SymbolType::FUNCTION && it->second.paramTypes == argTypes) {
            return &it->second;
        }
    }
    return nullptr;
}

bool Scope::functionExists(const string& name, const vector<TokenType>& paramTypes) const {
    auto range = symbols.equal_range(name);
    for (auto it = range.first; it != range.second; ++it) {
        if (it->second.type == SymbolType::FUNCTION && it->second.paramTypes == paramTypes) {
            return true;
        }
    }
    return false;
}

ScopeStack::ScopeStack() {
    scopes.push_back(make_unique<Scope>(nullptr, "global"));
    current = scopes.back().get();
}

void ScopeStack::enterScope(const string& name) {
    scopes.push_back(make_unique<Scope>(current, name));
    current = scopes.back().get();
}

void ScopeStack::exitScope() {
    if (current->getParent()) {
        current = current->getParent();
        scopes.pop_back();
    }
}

// Fixed: Added line and col parameters
bool ScopeStack::addSymbol(const string& name, SymbolType type, TokenType dataType, int line, int col) {
    return current->addSymbol(name, type, dataType, line, col);
}

// Fixed: Added line and col parameters
bool ScopeStack::addFunction(const string& name, TokenType returnType, const vector<TokenType>& paramTypes, int line, int col) {
    return current->addFunction(name, returnType, paramTypes, line, col);
}

Symbol* ScopeStack::lookup(const string& name) {
    Scope* scope = current;
    while (scope) {
        Symbol* symbol = scope->lookupCurrent(name);
        if (symbol) return symbol;
        scope = scope->getParent();
    }
    return nullptr;
}

const Symbol* ScopeStack::lookup(const string& name) const {
    const Scope* scope = current;
    while (scope) {
        const Symbol* symbol = scope->lookupCurrent(name);
        if (symbol) return symbol;
        scope = scope->getParent();
    }
    return nullptr;
}

Symbol* ScopeStack::lookupCurrent(const string& name) {
    return current->lookupCurrent(name);
}

const Symbol* ScopeStack::lookupCurrent(const string& name) const {
    return current->lookupCurrent(name);
}

const Symbol* ScopeStack::lookupFunction(const string& name, const vector<TokenType>& argTypes) const {
    const Scope* scope = current;
    while (scope) {
        const Symbol* symbol = scope->lookupFunction(name, argTypes);
        if (symbol) return symbol;
        scope = scope->getParent();
    }
    return nullptr;
}

void ScopeStack::clear() {
    scopes.clear();
    scopes.push_back(make_unique<Scope>(nullptr, "global"));
    current = scopes.back().get();
}