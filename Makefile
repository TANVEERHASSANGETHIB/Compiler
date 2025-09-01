CXX = g++
CXXFLAGS = -std=c++17 -Wall -Iinclude
SRC = src/main.cpp src/manual_lexer.cpp src/regex_lexer.cpp
OBJ = $(SRC:.cpp=.o)
TARGET = build/lexer_app

all: $(TARGET)

$(TARGET): $(OBJ)
	@mkdir -p build
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

src/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf src/*.o build
