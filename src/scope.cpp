#include "scope.h"
#include <stdexcept>

Scope::Scope(Scope* p, const std::string& name) 
    : parent(p), scopeName(name), level(p ? p->level + 1 : 0) {}

bool Scope::addSymbol(const std::string& name, SymbolType type, TokenType dataType) {
    if (symbols.find(name) != symbols.end()) {
        return false; // Symbol already exists in this scope
    }
    symbols[name] = Symbol(name, type, dataType);
    return true;
}

Symbol* Scope::lookup(const std::string& name) {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return &it->second;
    }
    return nullptr;
}

const Symbol* Scope::lookup(const std::string& name) const {
    auto it = symbols.find(name);
    if (it != symbols.end()) {
        return &it->second;
    }
    return nullptr;
}

Symbol* Scope::lookupCurrent(const std::string& name) {
    auto it = symbols.find(name);
    return it != symbols.end() ? &it->second : nullptr;
}

const Symbol* Scope::lookupCurrent(const std::string& name) const {
    auto it = symbols.find(name);
    return it != symbols.end() ? &it->second : nullptr;
}

// ScopeStack implementation
ScopeStack::ScopeStack() {
    // Start with global scope
    scopes.push_back(std::make_unique<Scope>(nullptr, "global"));
    current = scopes.back().get();
}

ScopeStack::~ScopeStack() = default;

void ScopeStack::enterScope(const std::string& name) {
    scopes.push_back(std::make_unique<Scope>(current, name));
    current = scopes.back().get();
}

void ScopeStack::exitScope() {
    if (current->getParent()) {
        current = current->getParent();
        scopes.pop_back();
    }
}

bool ScopeStack::addSymbol(const std::string& name, SymbolType type, TokenType dataType) {
    return current->addSymbol(name, type, dataType);
}

Symbol* ScopeStack::lookup(const std::string& name) {
    Scope* scope = current;
    while (scope) {
        Symbol* symbol = scope->lookupCurrent(name);
        if (symbol) return symbol;
        scope = scope->getParent();
    }
    return nullptr;
}

const Symbol* ScopeStack::lookup(const std::string& name) const {
    const Scope* scope = current;
    while (scope) {
        const Symbol* symbol = scope->lookupCurrent(name);
        if (symbol) return symbol;
        scope = scope->getParent();
    }
    return nullptr;
}

Symbol* ScopeStack::lookupCurrent(const std::string& name) {
    return current->lookupCurrent(name);
}

const Symbol* ScopeStack::lookupCurrent(const std::string& name) const {
    return current->lookupCurrent(name);
}