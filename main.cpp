#include <iostream>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <string>
#include <stdexcept>
#include <iomanip>
#include <sstream>
#include <fstream>
#include "SymbolTablesUtils.h"

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
    {",", 1}, {":",2 }, {";",3 }, {":=",4 }, {"*", 5}, {"/", 6},
    {"+", 7}, {"-", 8}, {".", 9}, {"(", 10}, {")", 11}, {"{", 12},
    {"}", 13}, {"[", 14}, {"]", 15}, {">", 16}, {"<", 17}, {">=", 18},
    {"<=", 19}, {"=", 20}, {"..", 21}, {"<>", 22}
};

const unordered_map<string, int> KEYWORDS = {
    {"program", 1}, {"var", 2},{"integer", 3},{"real", 4}, {"char", 5}, {"begin", 6},
    {"end", 7}, {"const", 8}, {"if", 9}, {"else", 10}, {"while", 11}, {"do", 12},
    {"type", 13}, {"procedure", 14}, {"record", 15}, {"boolean", 16}, {"array", 17},
    {"and", 18}, {"or", 19}, {"then", 20}, {"not", 21}, {"true", 22}, {"false", 23},
    {"of", 24}, {"div", 25}, {"mod", 26}, {"return", 27}, {"packed", 28}, {"longint", 29},
    {"string",30}
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
        : type(t), code(c), value(std::move(v)) {
    }
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
                        if (hasError)cout << "1" << "\n";
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
                        if (hasError)cout << "2" << "\n";
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
            if (hasError)cout << "3" << "\n";
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

        while (currentChar() != '\'' && currentChar() != '\0') {
            buffer += currentChar();
            nextChar();
        }

        if (currentChar() != '\'') {
            hasError = true;
            if (hasError) cout << "4" << "\n";
            throw runtime_error("Unclosed character/string literal");
        }
        nextChar(); // 跳过结束的单引号

        // 判断是字符还是字符串
        if (buffer.size() == 1) {
            int id = constCharTable.addSymbol(buffer);
            tokens.emplace_back(TokenType::CT, id + 1, buffer);
        }
        else {
            int id = constStringTable.addSymbol(buffer);
            tokens.emplace_back(TokenType::ST, id + 1, buffer);
        }

        buffer.clear();
        state = State::START;
    }

    // 修改后的字符串常量处理
    void processStringLiteral(vector<Token>& tokens) {

        while (currentChar() != '"' && currentChar() != '\0') {
            buffer += currentChar();
            nextChar();
        }

        if (currentChar() != '"') {
            hasError = true;
            if (hasError)cout << "6" << "\n";
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
            if (hasError)cout << "7" << "\n";
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
        /*buffer.clear();
        hasError = false;
        idTable.clear();
        constIntTable.clear();
        constFloatTable.clear();
        constCharTable.clear();
        constStringTable.clear();*/

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
                    processCharLiteral(tokens);  // 统一使用处理函数
                    break;

                case State::IN_STRING:
                    processStringLiteral(tokens); // 统一使用处理函数
                    break;

                case State::IN_ERROR:
                    throw runtime_error("Lexer entered error state");
                }
            }
        }
        catch (const exception& e) {
            hasError = true;
            if (hasError)cout << "12" << "\n";
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

//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？
//此处开始为语法分析的函数部分？

// 语法分析器类（添加语义分析功能）
class PascalParser {
private:
    // 关键字编码常量
    static constexpr int KW_PROGRAM = 1;
    static constexpr int KW_VAR = 2;
    static constexpr int KW_INTEGER = 3;
    static constexpr int KW_REAL = 4;
    static constexpr int KW_CHAR = 5;
    static constexpr int KW_BEGIN = 6;
    static constexpr int KW_END = 7;
    static constexpr int KW_CONST = 8;
    static constexpr int KW_IF = 9;
    static constexpr int KW_ELSE = 10;
    static constexpr int KW_WHILE = 11;
    static constexpr int KW_DO = 12;
    static constexpr int KW_TYPE = 13;
    static constexpr int KW_PROCEDURE = 14;
    static constexpr int KW_RECORD = 15;
    static constexpr int KW_BOOLEAN = 16;
    static constexpr int KW_ARRAY = 17;
    static constexpr int KW_AND = 18;
    static constexpr int KW_OR = 19;
    static constexpr int KW_THEN = 20;
    static constexpr int KW_NOT = 21;
    static constexpr int KW_TRUE = 22;
    static constexpr int KW_FALSE = 23;
    static constexpr int KW_OF = 24;
    static constexpr int KW_DIV = 25;
    static constexpr int KW_MOD = 26;
    static constexpr int KW_RETURN = 27;
    static constexpr int KW_PACKED = 28;
    static constexpr int KW_LONGINT = 29;
    static constexpr int KW_STRING = 30;

    // 修复点：将界符常量改为 static constexpr
    static constexpr int P_COMMA = 1;
    static constexpr int P_COLON = 2;
    static constexpr int P_SEMICOLON = 3;
    static constexpr int P_ASSIGN = 4;      // :=
    static constexpr int P_STAR = 5;        // *
    static constexpr int P_SLASH = 6;       // /
    static constexpr int P_PLUS = 7;        // +
    static constexpr int P_MINUS = 8;       // -
    static constexpr int P_DOT = 9;         // .
    static constexpr int P_LPAREN = 10;     // (
    static constexpr int P_RPAREN = 11;     // )
    static constexpr int P_LBRACE = 12;     // {
    static constexpr int P_RBRACE = 13;     // }
    static constexpr int P_LBRACKET = 14;   // [
    static constexpr int P_RBRACKET = 15;   // ]
    static constexpr int P_GREATER = 16;    // >
    static constexpr int P_LESS = 17;       // <
    static constexpr int P_GREATER_EQUAL = 18; // >=
    static constexpr int P_LESS_EQUAL = 19; // <=
    static constexpr int P_EQUAL = 20;      // =
    static constexpr int P_DOTDOT = 21;
    static constexpr int P_NOT_EQUAL = 22;

    vector<Token> tokens;
    size_t current_token_index;
    int current_line;

    // 语义分析新增成员 ===========================================
    // 符号表结构：[作用域层级][变量名] -> 类型信息
    unordered_map<int, unordered_map<string, string>> symbolTable;
    int current_scope = 0; // 当前作用域层级

    // 类型映射表
    const unordered_map<int, string> typeMap = {
        {KW_INTEGER, "integer"},
        {KW_REAL, "real"},
        {KW_CHAR, "char"},
        {KW_BOOLEAN, "boolean"},
        {KW_LONGINT, "longint"},
        {KW_STRING, "string"},
    };
    // ===========================================================

public:
    PascalParser(const vector<Token>& tokens)
        : tokens(tokens), current_token_index(0), current_line(1) {}

    void parse() {
        parseProgram();
        match(END_OF_INPUT, "end of input");
        cout << "Syntax and semantic analysis completed successfully!" << endl;
    }

private:
    static const TokenType END_OF_INPUT = static_cast<TokenType>(-1);

    const Token& currentToken() const {
        if (current_token_index < tokens.size()) {
            return tokens[current_token_index];
        }
        static Token eof{ END_OF_INPUT, -1, "" };
        return eof;
    }

    void advance() {
        if (current_token_index < tokens.size()) {
            current_token_index++;
        }
    }

    void syntaxError(const string& message) {
        cerr << "Syntax error" << ": " << message;
        if (currentToken().type != END_OF_INPUT) {
            cerr << " (Found: ";
            switch (currentToken().type) {
            case K: cerr << "KEYWORD:" << currentToken().code; break;
            case D: cerr << "DELIMITER:" << currentToken().code; break;
            case I: cerr << "IDENTIFIER:" << currentToken().value; break;
            case C1: cerr << "INTEGER:" << currentToken().value; break;
            case C2: cerr << "REAL:" << currentToken().value; break;
            case CT: cerr << "CHAR:" << currentToken().value; break;
            case ST: cerr << "STRING:" << currentToken().value; break;
            default: cerr << "UNKNOWN";
            }
            cerr << ")";
        }
        cerr << endl;
        throw runtime_error("Syntax analysis failed");
    }

    // 语义分析错误报告 ===========================================
    void semanticError(const string& message) {
        cerr << "Semantic error at line " << current_line << ": " << message << endl;
        throw runtime_error("Semantic analysis failed");
    }
    // ===========================================================

    void match(TokenType expected_type, const string& description, int expected_code = -1) {
        if (currentToken().type == expected_type) {
            if (expected_code == -1 || currentToken().code == expected_code) {
                advance();
                return;
            }
        }
        syntaxError("Expected " + description);
    }

    void matchKeyword(int keywordCode) {
        match(K, "keyword " + to_string(keywordCode), keywordCode);
    }

    void matchDelimiter(int delimiterCode) {
        match(D, "delimiter " + to_string(delimiterCode), delimiterCode);
    }

    void matchIdentifier() {
        match(I, "identifier");
    }

    void matchNumber() {
        if (currentToken().type == C1 || currentToken().type == C2) {
            advance();
        }
        else {
            syntaxError("Expected number constant");
        }
    }

    // 常量声明解析（添加符号表记录）
    void parseConstDeclarations() {
        if (currentToken().type == K && currentToken().code == KW_CONST) {
            matchKeyword(KW_CONST);
            do {
                string constName = currentToken().value;
                matchIdentifier();
                matchDelimiter(P_EQUAL);
                string constType = parseConstant(); // 解析常量值并返回类型
                matchDelimiter(P_SEMICOLON);

                // 类型表填写
                TypeCode tcode = TypeCode::NONE;
                if (constType == "integer") tcode = TypeCode::INT;
                else if (constType == "real") tcode = TypeCode::REAL;
                else if (constType == "char") tcode = TypeCode::CHAR;
                else if (constType == "boolean") tcode = TypeCode::BOOL;
                // ...扩展

                int typIdx = insertType(tcode);


                // 添加到符号表

                symbolTable[current_scope][constName] = constType;
            } while (currentToken().type == I);
        }
    }

    void parseProgram() {
        matchKeyword(KW_PROGRAM);
        string progName = currentToken().value;
        matchIdentifier(); // 程序名

        // --- 加入符号表 ---
        int typIdx = insertType(TypeCode::NONE); // 程序本身类型一般无具体类型
        insertSymbol(progName, typIdx, CatCode::FUNC); // 类型和类别你可以自定义
        symbolTable[current_scope][progName] = "program"; // 语义分析表也记一下


        // 跳过program行后的分号（Pascal允许）
        if (currentToken().type == D && currentToken().code == P_SEMICOLON)
            advance();
        parseVarDeclarations();
        // 允许var后有多余分号
        while (currentToken().type == D && currentToken().code == P_SEMICOLON) advance();
        parseProcedureDeclarations();
        parseMainBlock();
    }

    // 语义分析：解析类型声明
    void parseTypeDeclarations() {
        if (currentToken().type == K && currentToken().code == KW_TYPE) {
            matchKeyword(KW_TYPE);
            do {
                string typeName = currentToken().value;
                matchIdentifier();
                matchDelimiter(P_EQUAL);
                string baseType = parseType(); // 记录类型定义
                matchDelimiter(P_SEMICOLON);


                // 类型表
                TypeCode tcode = TypeCode::NONE;
                if (baseType == "integer") tcode = TypeCode::INT;
                // ...扩展

                int typIdx = insertType(tcode);

                // 符号表，类别为 CatCode::TYPE
                insertSymbol(typeName, typIdx, CatCode::TYPE);
                symbolTable[current_scope][typeName] = baseType;

            } while (currentToken().type == I);
        }
    }


    vector<string> parseIdentifierList2() {
        vector<string> names;
        names.push_back(currentToken().value);
        matchIdentifier();
        while (currentToken().type == D && currentToken().code == P_COMMA) {
            advance();
            names.push_back(currentToken().value);
            matchIdentifier();
        }
        return names;
    }

    string parseType2() {
        if (currentToken().type == K) {
            int code = currentToken().code;
            if (code == KW_INTEGER) { advance(); return "integer"; }
            if (code == KW_REAL) { advance(); return "real"; }
            if (code == KW_CHAR) { advance(); return "char"; }
            if (code == KW_BOOLEAN) { advance(); return "boolean"; }
        }
        else if (currentToken().type == I) {
            string s = currentToken().value;
            advance();
            return s;
        }
        syntaxError("Expected type keyword or identifier");
        return "";
    }



    // 语义分析：变量声明处理（符号表填充）
    void parseVarDeclarations() {
        if (currentToken().type == K && currentToken().code == KW_VAR) {
            matchKeyword(KW_VAR);
            do {
                // 获取标识符列表
                vector<string> identifiers;
                identifiers.push_back(currentToken().value);
                matchIdentifier();

                while (currentToken().type == D && currentToken().code == P_COMMA) {
                    advance();
                    identifiers.push_back(currentToken().value);
                    matchIdentifier();
                }

                matchDelimiter(P_COLON);
                string varType = parseType(); // 返回类型字符串
                matchDelimiter(P_SEMICOLON);


                // === 填写类型表 ===
                TypeCode tcode = TypeCode::NONE;
                if (varType == "integer") tcode = TypeCode::INT;
                else if (varType == "real") tcode = TypeCode::REAL;
                else if (varType == "char") tcode = TypeCode::CHAR;
                else if (varType == "boolean") tcode = TypeCode::BOOL;
                // ... 其他类型

                int typIdx = insertType(tcode);

                // === 填写符号表 ===
                for (const auto& id : identifiers) {
                    insertSymbol(id, typIdx, CatCode::VAR);  // 写全局符号表（用于打印）
                    symbolTable[current_scope][id] = varType; // 写本地symbolTable供语义分析查找
                }


            } while (currentToken().type == I);
        }
    }

    // 语义分析：解析类型标识符
    string parseType() {
        if (currentToken().type != K) {
            syntaxError("Expected type keyword");
        }

        int typeCode = currentToken().code;
        advance(); // 消耗类型关键字

        // 检查是否为有效类型
        if (typeMap.find(typeCode) == typeMap.end()) {
            semanticError("Invalid type specified");
        }

        return typeMap.at(typeCode);
    }

    void parseIdentifierList() {
        matchIdentifier();
        while (currentToken().type == D && currentToken().code == P_COMMA) {
            advance();
            matchIdentifier();
        }
    }

    void parseProcedureDeclarations() {
        while (currentToken().type == K && currentToken().code == KW_PROCEDURE) {
            parseProcedureDeclaration();
        }
    }

    // 语义分析：处理过程声明（作用域管理）
    void parseProcedureDeclaration() {
        matchKeyword(KW_PROCEDURE);
        string procName = currentToken().value;
        matchIdentifier();

        // 进入新作用域
        enterScope();

        matchDelimiter(P_LPAREN);

        if (currentToken().type != D || currentToken().code != P_RPAREN) {
            parseParameterList();
        }

        matchDelimiter(P_RPAREN);
        matchDelimiter(P_SEMICOLON);
        matchKeyword(KW_BEGIN);
        parseFunctionBody();
        matchKeyword(KW_END);
        matchDelimiter(P_SEMICOLON);

        // 退出当前作用域
        exitScope();
    }

    // 语义分析：进入新作用域
    void enterScope() {
        current_scope++;
        symbolTable[current_scope] = unordered_map<string, string>();
    }

    // 语义分析：退出作用域
    void exitScope() {
        if (current_scope > 0) {
            symbolTable.erase(current_scope);
            current_scope--;
        }
    }

    // 语义分析：解析参数列表
    void parseParameterList() {
        while (true) {
            // 检查是否以右括号结束
            if (currentToken().type == D && currentToken().code == P_RPAREN) {
                return;
            }

            // 检查是否有 var 关键字（引用参数）
            bool isReference = false;
            if (currentToken().type == K && currentToken().code == KW_VAR) {
                isReference = true;
                advance(); // 跳过 'var'
            }

            // 获取标识符列表
            vector<string> identifiers;
            identifiers.push_back(currentToken().value);
            matchIdentifier();

            while (currentToken().type == D && currentToken().code == P_COMMA) {
                advance();
                identifiers.push_back(currentToken().value);
                matchIdentifier();
            }

            matchDelimiter(P_COLON);
            string paramType = parseType(); // 获取参数类型

            // 添加参数到符号表，标记引用类型
            for (const auto& id : identifiers) {
                symbolTable[current_scope][id] = isReference ? "ref " + paramType : paramType;
            }

            // 检查参数分隔符
            if (currentToken().type == D && currentToken().code == P_SEMICOLON) {
                advance(); // 跳过 ';'
            }
            else {
                break; // 参数列表结束
            }
        }
    }

    // 函数体解析
    void parseFunctionBody() {
        parseStatementList();
    }

    void parseReturnStatement() {
        matchKeyword(KW_RETURN);
        parseExpression();
    }

    void parseMainBlock() {
        matchKeyword(KW_BEGIN);
        parseStatementList();

        // 检查是否已经到达点号（程序结束）
        if (currentToken().type == D && currentToken().code == P_DOT) {
            return; // 点号将在parseProgram中处理
        }

        matchKeyword(KW_END);
        matchDelimiter(P_DOT);
    }

    // 语义分析：语句列表处理
    void parseStatementList() {
        while (true) {
            if (currentToken().type == K) {
                if (currentToken().code == KW_END || currentToken().code == KW_ELSE) {
                    break; // 结束语句列表
                }

                switch (currentToken().code) {
                case KW_BEGIN:
                    parseCompoundStatement();
                    break;
                case KW_IF:
                    parseIfStatement();
                    break;
                case KW_WHILE:
                    parseWhileStatement();
                    break;
                case KW_RETURN:
                    parseReturnStatement();
                    break;
                default:
                    // 可能是赋值语句
                    parseAssignment();
                    break;
                }
            }
            else if (currentToken().type == I) {
                parseAssignment(); // 赋值语句
            }
            else {
                break; // 其他情况结束语句列表
            }

            // 语句分隔符
            if (currentToken().type == D && currentToken().code == P_SEMICOLON) {
                advance(); // 消耗分号

                // 检查分号后是否还有语句
                if (currentToken().type == K &&
                    (currentToken().code == KW_END || currentToken().code == KW_ELSE)) {
                    break; // 分号后是结束关键字，结束语句列表
                }
            }
            else {
                // 缺少分号，但下一个token是语句的开始
                if (currentToken().type == I ||
                    (currentToken().type == K &&
                        (currentToken().code == KW_BEGIN ||
                            currentToken().code == KW_IF ||
                            currentToken().code == KW_WHILE ||
                            currentToken().code == KW_RETURN))) {
                    // 报告警告但继续解析
                    const string s = "Warning: Missing semicolon at line ";
                    syntaxError(s);
                    /* cerr << "Warning: Missing semicolon at line " << current_line << endl;*/
                }
                else {
                    // 不是语句的开始，可能是块结束
                    break;
                }
            }
        }
    }

    // 语义分析：赋值语句类型检查
    void parseAssignment() {
        string varName = currentToken().value;
        parseVariable();

        if (!isVariableDeclared(varName)) {
            semanticError("Undeclared variable: " + varName);
        }

        matchDelimiter(P_ASSIGN);
        string exprType = parseExpression();

        // 获取变量声明的基础类型（跳过ref）
        string varBaseType = getBaseType(getVariableType(varName));
        string exprBaseType = getBaseType(exprType);

        // 使用基础类型进行比较
        if (!isTypeCompatible(varBaseType, exprBaseType)) {
            semanticError("Type mismatch: Cannot assign " + exprType +
                " to " + varBaseType + " variable '" + varName + "'");
        }
    }
    // 语义分析：解析变量（包括数组访问等）
    void parseVariable() {
        string varName = currentToken().value;
        matchIdentifier();
        parseSuffix(); // 处理数组下标等后缀
    }

    // 语义分析：处理后缀（数组访问等）
    void parseSuffix() {
        if (currentToken().type == D && currentToken().code == P_LBRACKET) {
            advance(); // 跳过 '['
            parseExpression(); // 解析下标表达式
            matchDelimiter(P_RBRACKET); // 匹配 ']'
        }
    }

    // 语义分析：表达式类型推导
    string parseExpression() {
        string leftType = parseSimpleExpression();

        // 关系运算符
        if (currentToken().type == D &&
            (currentToken().code == P_EQUAL ||
                currentToken().code == P_NOT_EQUAL ||
                currentToken().code == P_LESS ||
                currentToken().code == P_LESS_EQUAL ||
                currentToken().code == P_GREATER ||
                currentToken().code == P_GREATER_EQUAL)) {

            Token op = currentToken();
            advance();
            string rightType = parseSimpleExpression();

            // 关系运算要求两边类型兼容
            if (!isTypeCompatible(leftType, rightType)) {
                semanticError("Type mismatch in relational expression");
            }

            // 关系表达式的结果总是布尔类型
            return "boolean";
        }

        return leftType;
    }

    // 语义分析：简单表达式类型推导
    string parseSimpleExpression() {
        string leftType = parseTerm();

        // 加减运算符
        while (currentToken().type == D &&
            (currentToken().code == P_PLUS ||
                currentToken().code == P_MINUS ||
                currentToken().code == KW_OR)) {

            Token op = currentToken();
            advance();
            string rightType = parseTerm();

            // 检查运算符适用性
            if (op.code == KW_OR) {
                if (leftType != "boolean" || rightType != "boolean") {
                    semanticError("OR operator requires boolean operands");
                }
                leftType = "boolean"; // 结果类型
            }
            else {
                if (!isNumeric(leftType) || !isNumeric(rightType)) {
                    semanticError("Arithmetic operator requires numeric operands");
                }

                // 类型提升：整数+实数→实数
                if (leftType == "real" || rightType == "real") {
                    leftType = "real";
                }
            }
        }

        return leftType;
    }

    // 语义分析：项类型推导
    string parseTerm() {
        string leftType = parseFactor();

        // 乘除运算符
        while (currentToken().type == D &&
            (currentToken().code == P_STAR ||
                currentToken().code == P_SLASH ||
                currentToken().code == KW_DIV ||
                currentToken().code == KW_MOD ||
                currentToken().code == KW_AND)) {

            Token op = currentToken();
            advance();
            string rightType = parseFactor();

            // 检查运算符适用性
            if (op.code == KW_AND) {
                if (leftType != "boolean" || rightType != "boolean") {
                    semanticError("AND operator requires boolean operands");
                }
                leftType = "boolean"; // 结果类型
            }
            else {
                if (!isNumeric(leftType) || !isNumeric(rightType)) {
                    semanticError("Arithmetic operator requires numeric operands");
                }

                // 类型提升：整数*实数→实数
                if (leftType == "real" || rightType == "real") {
                    leftType = "real";
                }
            }
        }

        return leftType;
    }

    // 语义分析：因子类型推导
    string parseFactor() {
        if (currentToken().type == I) {
            string varName = currentToken().value;
            matchIdentifier();
            parseSuffix();

            // 返回变量声明类型
            return getVariableType(varName);
        }
        else if (currentToken().type == C1) {
            advance();
            return "integer";
        }
        else if (currentToken().type == C2) {
            advance();
            return "real";
        }
        else if (currentToken().type == CT) {
            advance();
            return "char";
        }
        else if (currentToken().type == K) {
            if (currentToken().code == KW_TRUE || currentToken().code == KW_FALSE) {
                advance();
                return "boolean";
            }
            else if (currentToken().code == KW_NOT) {
                advance();
                string exprType = parseFactor();
                if (exprType != "boolean") {
                    semanticError("NOT operator requires boolean operand");
                }
                return "boolean";
            }
            else if (currentToken().code == P_LPAREN) {
                advance();
                string exprType = parseExpression();
                matchDelimiter(P_RPAREN);
                return exprType;
            }
        }
        else if (currentToken().type == ST) {
            advance();
            return "string";
        }
        syntaxError("Unexpected token in factor");
        return ""; // 避免编译警告
    }


    // 语义分析辅助函数 ==========================================

    // 检查变量是否声明
    bool isVariableDeclared(const string& name) {
        // 从当前作用域向全局作用域查找
        for (int scope = current_scope; scope >= 0; --scope) {
            if (symbolTable.find(scope) != symbolTable.end() &&
                symbolTable[scope].find(name) != symbolTable[scope].end()) {
                return true;
            }
        }
        return false;
    }

    // 获取变量类型
    string getVariableType(const string& name) {
        for (int scope = current_scope; scope >= 0; --scope) {
            if (symbolTable.find(scope) != symbolTable.end()) {
                auto it = symbolTable[scope].find(name);
                if (it != symbolTable[scope].end()) {
                    return it->second;
                }
            }
        }
        semanticError("Variable not found: " + name);
        return ""; // 避免编译警告
    }

    // 语义分析：类型兼容性检查（跳过ref前缀）
    bool isTypeCompatible(const string& targetType, const string& sourceType) {
        // 获取基础类型（忽略ref前缀）
        string baseTarget = getBaseType(targetType);
        string baseSource = getBaseType(sourceType);

        // 相同类型直接兼容
        if (baseTarget == baseSource) return true;

        // 特殊规则：整数可赋值给实数
        if (baseTarget == "real" && baseSource == "integer") return true;

        // 特殊规则：字符可赋值给字符串
        if (baseTarget == "string" && baseSource == "char") return true;

        // 布尔类型兼容性
        if (baseTarget == "boolean" && baseSource == "boolean") return true;

        return false;
    }

    // 语义分析：获取基础类型（忽略ref前缀）
    string getBaseType(const string& type) {
        if (type.find("ref ") == 0) {
            return type.substr(4); // 跳过"ref "前缀
        }
        return type;
    }

    // 检查是否为数值类型
    bool isNumeric(const string& type) {
        return type == "integer" || type == "real";
    }

    // 解析常量值并返回类型
    string parseConstant() {
        if (currentToken().type == C1) {
            matchNumber();
            return "integer";
        }
        else if (currentToken().type == C2) {
            matchNumber();
            return "real";
        }
        else if (currentToken().type == CT) {
            advance();
            return "char";
        }
        else if (currentToken().type == K &&
            (currentToken().code == KW_TRUE || currentToken().code == KW_FALSE)) {
            advance();
            return "boolean";
        }
        else {
            syntaxError("Expected constant value");
            return ""; // 避免编译警告
        }
    }

    // 其他语句解析（简化实现）
    void parseCompoundStatement() {
        matchKeyword(KW_BEGIN);
        parseStatementList();
        matchKeyword(KW_END);
    }

    void parseIfStatement() {
        matchKeyword(KW_IF);
        parseExpression();
        matchKeyword(KW_THEN);
        parseStatement();
        if (currentToken().type == K && currentToken().code == KW_ELSE) {
            matchKeyword(KW_ELSE);
            parseStatement();
        }
    }

    void parseWhileStatement() {
        matchKeyword(KW_WHILE);
        parseExpression();
        matchKeyword(KW_DO);
        parseStatement();
    }

    void parseStatement() {
        if (currentToken().type == K) {
            switch (currentToken().code) {
            case KW_BEGIN:
                parseCompoundStatement();
                break;
            case KW_IF:
                parseIfStatement();
                break;
            case KW_WHILE:
                parseWhileStatement();
                break;
            case KW_RETURN:
                parseReturnStatement();
                break;
            default:
                syntaxError("Unexpected keyword in statement");
            }
        }
        else if (currentToken().type == I) {
            parseAssignment();
        }
        else {
            syntaxError("Expected statement");
        }
    }
};

int main() {
    ifstream fin("input.txt");
    if (!fin.is_open()) {
        cerr << "无法打开输入文件" << endl;
        return 0;
    }

    Lexer lexer;
    vector<Token> allTokens;
    vector<Token> lineTokens;

    string line;
    while (getline(fin, line)) {
        vector<Token> lineTokens = lexer.analyze(line);
        //printResults(lineTokens, lexer);
        allTokens.insert(allTokens.end(), lineTokens.begin(), lineTokens.end());
    }

    fin.close();
    printResults(allTokens, lexer);

    try {
        PascalParser parser(allTokens);
        parser.parse();

        // === 打印符号表和类型表 ===
        printSymbolTable();
        printTypeTable();
    }
    catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }
    return 0;
}