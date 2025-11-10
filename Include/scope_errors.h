#ifndef SCOPE_ERRORS_H
#define SCOPE_ERRORS_H

#include <stdexcept>
#include <string>

enum class ScopeErrorType {
    UndeclaredVariableAccessed,
    UndefinedFunctionCalled,
    VariableRedefinition,
    FunctionPrototypeRedefinition
};

class ScopeError : public std::runtime_error {
private:
    ScopeErrorType errorType;
    std::string identifier;

public:
    ScopeError(ScopeErrorType type, const std::string& id)
        : std::runtime_error(createMessage(type, id)), errorType(type), identifier(id) {}

    ScopeErrorType getErrorType() const { return errorType; }
    const std::string& getIdentifier() const { return identifier; }

private:
    static std::string createMessage(ScopeErrorType type, const std::string& id) {
        switch (type) {
            case ScopeErrorType::UndeclaredVariableAccessed:
                return "Undeclared variable accessed: " + id;
            case ScopeErrorType::UndefinedFunctionCalled:
                return "Undefined function called: " + id;
            case ScopeErrorType::VariableRedefinition:
                return "Variable redefinition: " + id;
            case ScopeErrorType::FunctionPrototypeRedefinition:
                return "Function prototype redefinition: " + id;
            default:
                return "Scope error: " + id;
        }
    }
};

#endif