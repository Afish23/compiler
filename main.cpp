#include <iostream>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <string>
#include <stdexcept>
#include <iomanip>
#include <sstream>
using namespace std;

// 枚举类型定义
enum TokenType { K, D, I, C1, C2, CT, ST };

// 定义状态枚举
enum State {
    START,
    IN_IDENT,
    IN_NUM,
    IN_HEX,
    IN_FLOAT,
    IN_EXP,
    IN_CHAR,
    IN_STRING,
    IN_ERROR
};

// 预定义符号表（支持多字符运算符）
const unordered_map<string, int> DELIMITERS = {
    {"-", 1}, {"/",2 }, {"(",3 }, {")",4 }, {"==", 5}, {"<=", 6},
    {"<", 7}, {"+", 8}, {"*", 9}, {">", 10}, {"=", 11}, {",", 12},
    {";", 13}, {"++", 14}, {"{", 15}, {"}", 16}, {"%", 17},{"^", 18},
    {"&", 19}, {"!", 20}
};

const unordered_map<string, int> KEYWORDS = {
    {"program", 1}, {"var", 2},{"integer", 3},{"real", 4}, {"char", 5}, {"begin", 6},
    {"end", 7}, {"const", 8}, {"if", 9}, {"else", 10}, {"while", 11}, {"do", 12},
    {"type", 11}, {"procedure", 12}, {"record", 13}, {"boolean", 14}, {"array", 15}
};

// 符号表管理类
class SymbolTable {
private:
    unordered_map<string, int> symbolMap;
    vector<string> symbolList;

public:
    int addSymbol(const string& symbol) {
        auto it = symbolMap.find(symbol);
        if (it != symbolMap.end()) {
            return it->second;
        }

        int index = symbolList.size();
        symbolMap[symbol] = index;
        symbolList.push_back(symbol);
        return index;
    }

    const string& getSymbol(int index) const {
        if (index < 0 || index >= static_cast<int>(symbolList.size())) {
            throw out_of_range("Invalid symbol index");
        }
        return symbolList[index];
    }

    const vector<string>& getAllSymbols() const {
        return symbolList;
    }

    void clear() {
        symbolMap.clear();
        symbolList.clear();
    }

    bool empty() const {
        return symbolList.empty();
    }
};

// Token结构体
struct Token {
    TokenType type;
    int code;
    string value;

    Token(TokenType t, int c, string v = "")
        : type(t), code(c), value(std::move(v)) {}
};

// 词法分析器类
class Lexer {
private:
    SymbolTable idTable;      // 标识符表
    SymbolTable constIntTable;  // 整型常量表
    SymbolTable constFloatTable; // 浮点型常量表
    SymbolTable constCharTable;  // 字符常量表
    SymbolTable constStringTable; // 字符串常量表

    string input;
    size_t pos;
    State state;
    string buffer;
    bool hasError;

    char currentChar() const {
        return (pos < input.size()) ? input[pos] : '\0';
    }


    void nextChar() {
        if (pos < input.size()) pos++;
    }

    void processIdentifier(vector<Token>& tokens) {
        while (isalnum(currentChar()) || currentChar() == '_') {
            buffer += currentChar();
            nextChar();
        }

        if (KEYWORDS.count(buffer)) {
            Token new_token(TokenType::K, KEYWORDS.at(buffer), buffer);
            tokens.push_back(new_token);
        }
        else {
            int id = idTable.addSymbol(buffer);
            //tokens.emplace_back(TokenType::I, id + 1, buffer);
            Token new_token(TokenType::I, id + 1, buffer);
            tokens.push_back(new_token);
        }

        buffer.clear();
        state = State::START;
    }

    void processNumber(vector<Token>& tokens) {
        bool isHex = false;//十六进制数标识
        bool isFloat = false;//浮点数标识
        bool hasExp = false;//科学计数法标识

        // 检查16进制前缀
        if (buffer == "0" && (currentChar() == 'x' || currentChar() == 'X')) {
            isHex = true;
            buffer += currentChar();
            nextChar();
        }

        while (true)
        {
            char c = currentChar();

            if (isHex) {
                if (isxdigit(c)) {//检查十六进制前缀后的数值部分是否合法
                    buffer += c;
                    nextChar();
                }
                else {
                    break;
                }
            }
            else {//整数与浮点数识别
                if (isdigit(c)) {
                    buffer += c;
                    nextChar();
                }
                else if (c == '.') {
                    if (isFloat) {//已被识别为浮点数的情况下又识别到一个小数点
                        hasError = true;
                        buffer += c;
                        nextChar();
                        break;
                    }
                    isFloat = true;//标识为浮点数
                    buffer += c;
                    nextChar();
                }
                else if (c == 'e' || c == 'E') {
                    if (hasExp) {//已被识别为科学计数法的情况下又识别到一个e
                        hasError = true;
                        buffer += c;
                        nextChar();
                        break;
                    }
                    isFloat = true;//标识为科学计数法及浮点数
                    hasExp = true;
                    buffer += c;
                    nextChar();

                    // 处理指数符号
                    if (currentChar() == '+' || currentChar() == '-') {
                        buffer += currentChar();
                        nextChar();
                        if (!isalpha(currentChar()))//若为科学计数法，加减号后需有数字
                        {
                            hasError = true;
                            break;
                        }
                    }
                }
                else {
                    break;
                }
            }
        }

        // 检查非法后缀
        if (isalpha(currentChar())) {
            hasError = true;
            buffer += currentChar();
            nextChar();
        }

        if (hasError) {
            throw runtime_error("Invalid number format: " + buffer);
        }

        // 添加到相应的常量表
        if (isFloat || hasExp) {
            int id = constFloatTable.addSymbol(buffer);
            tokens.emplace_back(TokenType::C2, id + 1, buffer);
        }
        else if (isHex) {
            // 转换16进制为10进制
            unsigned int value;
            stringstream ss;
            ss << hex << buffer.substr(2); // 去掉0x前缀
            ss >> value;
            string decStr = to_string(value);
            int id = constIntTable.addSymbol(decStr);
            tokens.emplace_back(TokenType::C1, id + 1, decStr);
        }
        else {
            int id = constIntTable.addSymbol(buffer);
            tokens.emplace_back(TokenType::C1, id + 1, buffer);
        }

        buffer.clear();
        state = State::START;
    }

    void processCharLiteral(vector<Token>& tokens) {
        nextChar(); // 跳过开始的单引号

        if (currentChar() == '\'') { // 空字符情况
            hasError = true;
            throw runtime_error("Empty character literal");
        }

        buffer = currentChar(); // 只取一个字符
        nextChar();

        if (currentChar() != '\'') {
            hasError = true;
            throw runtime_error("Unclosed character literal");
        }
        nextChar(); // 跳过结束的单引号

        int id = constCharTable.addSymbol(buffer);
        tokens.emplace_back(TokenType::CT, id + 1, buffer);

        buffer.clear();
        state = State::START;
    }

    // 修改后的字符串常量处理
    void processStringLiteral(vector<Token>& tokens) {
        nextChar(); // 跳过开始的双引号

        while (currentChar() != '"' && currentChar() != '\0') {
            buffer += currentChar();
            nextChar();
        }

        if (currentChar() != '"') {
            hasError = true;
            throw runtime_error("Unclosed string literal");
        }
        nextChar(); // 跳过结束的双引号

        // 空字符串是允许的
        int id = constStringTable.addSymbol(buffer);
        tokens.emplace_back(TokenType::ST, id + 1, buffer);

        buffer.clear();
        state = State::START;
    }

    void processOperator(vector<Token>& tokens) {
        // 贪心匹配最长界符
        string longestMatch;
        size_t maxLen = 0;

        // 检查所有可能的界符长度（1-2个字符）
        for (size_t len = 1; len <= 2 && pos + len <= input.size(); ++len) {
            string potentialOp = input.substr(pos, len);
            if (DELIMITERS.count(potentialOp)) {
                if (len > maxLen) {
                    maxLen = len;
                    longestMatch = potentialOp;
                }
            }
        }

        if (maxLen > 0) {
            tokens.emplace_back(TokenType::D, DELIMITERS.at(longestMatch), longestMatch);
            pos += maxLen;
        }
        else {
            hasError = true;
            string op(1, currentChar());
            throw runtime_error("Unknown operator: " + op);
        }
    }

public:
    Lexer() : pos(0), state(State::START), hasError(false) {}

    bool hasErrors() const { return hasError; }

    vector<Token> analyze(const string& inputStr) {
        input = inputStr;
        pos = 0;
        state = State::START;
        buffer.clear();
        hasError = false;
        idTable.clear();
        constIntTable.clear();
        constFloatTable.clear();
        constCharTable.clear();
        constStringTable.clear();

        vector<Token> tokens;

        try {
            while (pos <= input.size()) {
                char c = currentChar();

                switch (state) {
                case State::START:
                    if (isspace(c)) {
                        nextChar();
                    }
                    else if (isalpha(c) || c == '_') {
                        buffer = c;
                        state = State::IN_IDENT;
                        nextChar();
                    }
                    else if (isdigit(c)) {
                        buffer = c;
                        state = State::IN_NUM;
                        nextChar();
                    }
                    else if (c == '\'') {
                        state = State::IN_CHAR;
                        buffer.clear();
                        nextChar(); // 先跳过单引号
                    }
                    else if (c == '"') {
                        state = State::IN_STRING;
                        buffer.clear();
                        nextChar(); // 先跳过双引号
                    }
                    else if (c == '\0') {
                        pos++;
                    }
                    else {
                        processOperator(tokens);
                    }
                    break;

                case State::IN_IDENT:
                    processIdentifier(tokens);
                    break;

                case State::IN_NUM:
                    processNumber(tokens);
                    break;

                case State::IN_CHAR:
                    if (currentChar() == '\'') { // 遇到结束引号
                        if (buffer.empty()) {
                            hasError = true;
                            throw runtime_error("Empty character literal");
                        }
                        if (buffer.size() > 1) {
                            hasError = true;
                            throw runtime_error("Character literal too long");
                        }

                        int id = constCharTable.addSymbol(buffer);
                        tokens.emplace_back(TokenType::CT, id + 1, buffer);
                        buffer.clear();
                        nextChar(); // 跳过结束引号
                        state = State::START;
                    }
                    else if (currentChar() == '\0') {
                        hasError = true;
                        throw runtime_error("Unclosed character literal");
                    }
                    else {
                        buffer += currentChar();
                        nextChar();
                    }
                    break;

                case State::IN_STRING:
                    if (currentChar() == '"') { // 遇到结束引号
                        int id = constStringTable.addSymbol(buffer);
                        tokens.emplace_back(TokenType::ST, id + 1, buffer);
                        buffer.clear();
                        nextChar(); // 跳过结束引号
                        state = State::START;
                    }
                    else if (currentChar() == '\0') {
                        hasError = true;
                        throw runtime_error("Unclosed string literal");
                    }
                    else {
                        buffer += currentChar();
                        nextChar();
                    }
                    break;

                case State::IN_ERROR:
                    throw runtime_error("Lexer entered error state");
                }
            }
        }
        catch (const exception& e) {
            hasError = true;
            return {};
        }

        return tokens;
    }

    const SymbolTable& getIdentifierTable() const { return idTable; }
    const SymbolTable& getConstIntTable() const { return constIntTable; }
    const SymbolTable& getConstFloatTable() const { return constFloatTable; }
    const SymbolTable& getConstCharTable() const { return constCharTable; }
    const SymbolTable& getConstStringTable() const { return constStringTable; }
};

void printResults(const vector<Token>& tokens, const Lexer& lexer) {
    if (lexer.hasErrors()) {
        cout << "ERROR" << endl;
        return;
    }

    cout << "Token :";
    for (const auto& token : tokens) {
        switch (token.type) {
        case TokenType::K: cout << "(K " << token.code << ")"; break;
        case TokenType::D: cout << "(P " << token.code << ")"; break;
        case TokenType::I: cout << "(I " << token.code << ")"; break;
        case TokenType::C1: cout << "(C1 " << token.code << ")"; break;
        case TokenType::C2: cout << "(C2 " << token.code << ")"; break;
        case TokenType::CT: cout << "(CT " << token.code << ")"; break;
        case TokenType::ST: cout << "(ST " << token.code << ")"; break;
        }
    }
    cout << endl;

    auto printTable = [](const string& name, const SymbolTable& table) {
        cout << name << " :";
        for (size_t i = 0; i < table.getAllSymbols().size(); ++i) {
            cout << table.getSymbol(i) << " ";
        }
        cout << endl;
    };

    printTable("I", lexer.getIdentifierTable());
    printTable("C1", lexer.getConstIntTable());
    printTable("C2", lexer.getConstFloatTable());
    printTable("CT", lexer.getConstCharTable());
    printTable("ST", lexer.getConstStringTable());
}

int main() {
    string input;
    getline(cin, input);

    Lexer lexer;
    auto tokens = lexer.analyze(input);
    printResults(tokens, lexer);

    return 0;
}