CXX = g++
CXXFLAGS = -std=c++17 -Wall -Iinclude

# Add all source files including the new scope.cpp
SRC = src/main.cpp src/manual_lexer.cpp src/regex_lexer.cpp src/parser.cpp src/scope.cpp src/type_checker.cpp
OBJ = $(SRC:.cpp=.o)
TARGET = build/lexer_app.exe

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJ)
	@if not exist build mkdir build
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

src/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	del /Q src\*.o 2>nul || exit 0
	del /Q build\*.exe 2>nul || exit 0