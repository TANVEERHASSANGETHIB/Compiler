
## Custom Lexer for a C-like Language

This project implements a lexical analyzer (lexer) for a simple C-like programming language. The lexer processes source code and breaks it down into a sequence of tokens such as keywords, identifiers, literals, operators, and punctuation symbols. It also detects invalid tokens and provides useful error messages with line and column information.

---

## Features

- **Keyword recognition:** Supports keywords like `fn`, `int`, `float`, `string`, `bool`, `if`, `else`, `for`, `while`, `return`, and more.
- **Literals:** Supports integer, floating-point (including scientific notation), string literals (with escape sequences), and boolean literals (`true` and `false`).
- **Operators:** Handles arithmetic, logical, relational, bitwise, and assignment operators.
- **Identifiers:** Validates identifiers with standard naming rules and detects invalid identifiers (e.g., starting with a digit or containing illegal characters).
- **Comments:** Supports both single-line (`// ...`) and multi-line (`/* ... */`) comments.
- **Error reporting:** Provides detailed error messages with line and column numbers for invalid tokens and unclosed comments.
- **Position tracking:** Tracks line and column positions for every token for precise diagnostics.

---

## Usage

1. **Compile the lexer**

   Use a C++17 compatible compiler to build the lexer. For example, with `g++`:

   ```bash
   g++ -std=c++17 -o lexer lexer.cpp

2. **Run the lexer**

   The lexer currently reads a hardcoded sample program inside the `main()` function and outputs the list of tokens with their types and positions.

   ```bash
   ./lexer
   ```

3. **Modify input**

   Replace the `program` string in `main()` with your source code to tokenize different inputs.

---

## Code Structure

* **Token types:** Defined as an enumeration for easy identification.
* **Token pattern list:** A vector of regex patterns paired with token types, checked in order to find matches.
* **Tokenizer function:** Processes input string, skipping whitespace and comments, matching tokens with regexes, and recording tokens along with line and column info.
* **Error handling:** Prints errors to `stderr` for invalid identifiers, unknown tokens, and unclosed comments.
* **Main driver:** Contains a sample source program and prints tokens after tokenization.

---

## Example

Given this source snippet:

```c
fn int my_fn(int x, float y) {
    // This is a comment
    string greeting = "Hello, world!";
    bool flag = true;
    int 123abc; // Invalid identifier
}
```

The lexer output includes tokens for keywords, identifiers, literals, and reports an error for the invalid identifier `123abc`.

---

## Future Improvements

* Enhance invalid identifier detection for more complex cases.
* Add support for nested or documentation comments.
* Improve performance by implementing a state machine-based lexer.
* Support file input/output for easier testing.
* Add more detailed error recovery mechanisms.

---

## License

This project is open-source and free to use under the MIT License.

---

## Contact

For questions, suggestions, or contributions, feel free to open an issue or submit a pull request.

---

Thank you for exploring this lexer project!

```
