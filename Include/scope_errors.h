#ifndef SCOPE_ERRORS_H
#define SCOPE_ERRORS_H

#include <stdexcept>
#include <string>

using namespace std;

enum class ScopeErrorType {
    UndeclaredVariableAccessed,
    UndefinedFunctionCalled,
    VariableRedefinition,
    FunctionPrototypeRedefinition,
    InvalidSymbolType
};

class ScopeError : public runtime_error {
private:
    ScopeErrorType errorType;
    string identifier;

public:
    ScopeError(ScopeErrorType type, const string& id)
        : runtime_error(createMessage(type, id)), errorType(type), identifier(id) {}

    ScopeErrorType getErrorType() const { return errorType; }
    const string& getIdentifier() const { return identifier; }

private:
    static string createMessage(ScopeErrorType type, const string& id) {
        switch (type) {
            case ScopeErrorType::UndeclaredVariableAccessed:
                return "Undeclared variable accessed: " + id;
            case ScopeErrorType::UndefinedFunctionCalled:
                return "Undefined function called: " + id;
            case ScopeErrorType::VariableRedefinition:
                return "Variable redefinition: " + id;
            case ScopeErrorType::FunctionPrototypeRedefinition:
                return "Function prototype redefinition: " + id;
            case ScopeErrorType::InvalidSymbolType:
                return "Invalid symbol type: " + id;
            default:
                return "Scope error: " + id;
        }
    }
};

#endif