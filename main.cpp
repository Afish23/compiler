#include <iostream>//parseExpression()返回内容不同，我的返回类型，这个返回内容
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

struct Quadruple {
    string op;     // 操作符（如 "+", ":=", "jz" 等）
    string arg1;   // 操作数1（变量、临时变量或常量）
    string arg2;   // 操作数2
    string result; // 结果变量或标签
};

class QuadrupleGenerator {
private:
    vector<Quadruple> quads;
    int tempCounter = 0;  // 临时变量计数器（如 t0, t1...）
    int labelCounter = 0; // 标签计数器（如 L0, L1...）

public:
    // 生成四元式并添加到列表
    void emit(string op, string arg1, string arg2, string result) {
        quads.push_back({ op, arg1, arg2, result });
    }

    // 生成临时变量名（用于表达式结果）
    string newTemp() {
        return "t" + to_string(tempCounter++);
    }

    // 生成标签（用于控制流）
    string newLabel() {
        return "L" + to_string(labelCounter++);
    }

    // 打印所有四元式
    void printQuads() {
        for (const auto& q : quads) {
            cout << "(" << q.op << ", " << q.arg1 << ", " << q.arg2 << ", " << q.result << ")" << endl;
        }
    }
};

// 全局四元式生成器
QuadrupleGenerator quadGen;
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

string delimToStr(int code) {
    switch (code) {
    case 4:  return ":=";
    case 5:  return "*";
    case 6:  return "/";
    case 7:  return "+";
    case 8:  return "-";
    case 16: return ">";
    case 17: return "<";
    case 18: return ">=";
    case 19: return "<=";
    case 20: return "=";
    case 22: return "<>";
    default: return "??";
    }
}


// 语法分析器类（添加语义分析功能）
class PascalParser {
private:
    string currentFieldName;
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
    unordered_map<string, string> tempVarType; // 临时变量类型表
    unordered_map<string, unordered_map<string, string>> recordFieldTypes;
    int current_scope = 0; // 当前作用域层级

    // 类型映射表
    const unordered_map<int, string> typeMap = {
    {KW_INTEGER, "integer"},
    {KW_REAL, "real"},
    {KW_CHAR, "char"},
    {KW_BOOLEAN, "boolean"},
    {KW_LONGINT, "longint"},
    {KW_STRING, "string"},
    {KW_PACKED, "packed array"},
    {KW_RECORD, "record"},

    };

    // ===========================================================

public:
    PascalParser(const vector<Token>& tokens)
        : tokens(tokens), current_token_index(0), current_line(1) {
    }

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
                string constValue = currentToken().value;
                string constType = parseConstant(); // 解析常量值并返回类型
                matchDelimiter(P_SEMICOLON);

                // 类型表填写Add commentMore actions
                TypeCode tcode = TypeCode::NONE;
                if (constType == "integer") tcode = TypeCode::INT;
                else if (constType == "real") tcode = TypeCode::REAL;
                else if (constType == "char") tcode = TypeCode::CHAR;
                else if (constType == "boolean") tcode = TypeCode::BOOL;
                else if (constType == "boolean") tcode = TypeCode::BOOL;

                // ...扩展

                int typIdx = insertType(tcode);
                //cout << constValue << " " << typIdx << endl;
                int cidx = insertConst(constValue, typIdx);
                insertSymbol(constName, typIdx, CatCode::CONST, cidx);
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

        parseConstDeclarations();


        parseTypeDeclarations();  // 新增的类型声明解析
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

                int typIdx = -1;

                // ==== 检查是否为数组类型 ====
                if (currentToken().type == K && currentToken().code == KW_ARRAY) {
                    advance(); // 跳过 array
                    matchDelimiter(P_LBRACKET);

                    int low = 0;
                    if (currentToken().type == C1) {
                        low = stoi(currentToken().value);
                        int typIdx = insertType(TypeCode::INT);
                        insertConst(currentToken().value, typIdx);
                        advance();
                    }
                    matchDelimiter(P_DOTDOT);

                    int up = 0;
                    if (currentToken().type == C1) {
                        up = stoi(currentToken().value);
                        int typIdx = insertType(TypeCode::INT);
                        insertConst(currentToken().value, typIdx);
                        advance();
                    }
                    matchDelimiter(P_RBRACKET);
                    matchKeyword(KW_OF);
                    ArrayTable arr;
                    //arr.id = currentFieldName;
                    //cout << currentFieldName << endl;
                    //cout << typeName << endl;
                    string baseType = parseType();
                    if (baseType == "integer") arr.ctp = "itp";
                    else if (baseType == "real") arr.ctp = "rtp";
                    else if (baseType == "char") arr.ctp = "ctp";
                    else if (baseType == "boolean") arr.ctp = "btp";
                    else arr.ctp = "?";
                    arr.low = low;
                    arr.up = up;
                    if (baseType == "integer") arr.clen = (up-low+1)*4;
                    else if (baseType == "real") arr.clen = (up - low + 1) * 8;
                    else if (baseType == "char") arr.clen = (up - low + 1) * 2;
                    else if (baseType == "boolean") arr.clen = (up - low + 1) * 1;
                    int arrIdx = arrayTable.size();
                    arrayTable.push_back(arr);

                    typIdx = insertType(TypeCode::ARRAY, &arrayTable[arrIdx]);
                }
                // ==== 检查是否为结构类型 ====
                else if (currentToken().type == K && currentToken().code == KW_RECORD) {
                // ---------- 关键修改区 ----------
                advance(); // 跳过 record
                unordered_map<string, string> fieldMap;
                StructTable s;
                int offset = 0;
                while (!(currentToken().type == K && currentToken().code == KW_END)) {
                    // 字段列表支持多个
                    vector<string> fieldNames;
                    fieldNames.push_back(currentToken().value);
                    matchIdentifier();
                    while (currentToken().type == D && currentToken().code == P_COMMA) {
                        advance();
                        fieldNames.push_back(currentToken().value);
                        matchIdentifier();
                    }
                    matchDelimiter(P_COLON);
                    //string fieldType = parseType();
                    TypeCode tcode = TypeCode::NONE;
                   
                    for (const auto& fieldName : fieldNames) {
                        StructField f;
                        currentFieldName = fieldName; //cout << currentFieldName << endl;
                        string fieldType = parseType();
                        f.id = fieldName;
                        if (fieldType == "integer") { f.tp = "itp"; }
                        else if (fieldType == "real") { f.tp = "rtp"; }
                        else if (fieldType == "char") { f.tp = "ctp"; }
                        else if (fieldType == "boolean") { f.tp = "btp"; }
                        else if (fieldType == "packed array of char") { f.tp = "arr"; }
                        f.off = offset;
                        int fieldLen=0;
                        if (fieldType == "integer")  fieldLen = 4;
                        else if (fieldType == "real")  fieldLen = 8;
                        else if (fieldType == "char")  fieldLen = 2;
                        else if (fieldType == "boolean")  fieldLen = 1; 
                        else if (fieldType == "packed array of char") {
                            for (const auto& arr : arrayTable) {
                                //cout << arr.id <<" "<<fieldName << endl;
                                //cout << arr.id << " " << fieldName <<" "<<1 << endl;
                                if (arr.id == fieldName) { // 用字段名查
                                    fieldLen = arr.clen;
                                    break;
                                }
                            }
                        }
                        offset+= fieldLen;
                        s.fields.push_back(f);

                        // ---------- 关键：写入 recordFieldTypes ----------
                        fieldMap[fieldName] = fieldType;
                    }
                    matchDelimiter(P_SEMICOLON);
                }
                matchKeyword(KW_END);

                int stIdx = structTable.size();
                structTable.push_back(s);
                typIdx = insertType(TypeCode::RECORD, &structTable[stIdx]);

                // ---------- 关键：写入 recordFieldTypes ----------
                recordFieldTypes[typeName] = fieldMap;
                }
                // ==== 基本类型/类型别名 ====
                else {
                    string baseType = parseType();
                    TypeCode tcode = TypeCode::NONE;
                    if (baseType == "integer") tcode = TypeCode::INT;
                    else if (baseType == "real") tcode = TypeCode::REAL;
                    else if (baseType == "char") tcode = TypeCode::CHAR;
                    else if (baseType == "boolean") tcode = TypeCode::BOOL;

                    // ...补充其它
                    typIdx = insertType(tcode);
                }

                matchDelimiter(P_SEMICOLON);

                // 符号表，类别为 CatCode::TYPE
                insertSymbol(typeName, typIdx, CatCode::TYPE);
                symbolTable[current_scope][typeName] = "type";

            } while (currentToken().type == I);
        }
    }
    string parseVariable() {
        string varName = currentToken().value;
        matchIdentifier();

        // 处理record字段访问
        while (currentToken().type == D && currentToken().code == P_DOT) {
            advance(); // 跳过点号
            varName += "." + currentToken().value; // 组合字段名
            matchIdentifier();
        }

        parseSuffix(); // 处理数组下标
        return varName;
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
            if (code == KW_PROCEDURE) { advance(); return "procedure"; }
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
                // === 填写类型表 ===Add commentMore actions
                TypeCode tcode = TypeCode::NONE;
                if (varType == "integer") tcode = TypeCode::INT;
                else if (varType == "real") tcode = TypeCode::REAL;
                else if (varType == "char") tcode = TypeCode::CHAR;
                else if (varType == "boolean") tcode = TypeCode::BOOL;
                // ... 其他类型

                int typIdx = insertType(tcode);

                // === 填写符号表 ===
                // 添加到符号表
                for (const auto& id : identifiers) {
                    insertSymbol(id, typIdx, CatCode::VAR);  // 写全局符号表（用于打印）
                    symbolTable[current_scope][id] = varType; // 写本地symbolTable供语义分析查找
                    symbolTable[current_scope][id] = varType;
                }
            } while (currentToken().type == I);
        }
    }


    // 语义分析：解析类型标识符
    // 可选参数 fieldOrTypeName，遇到数组时用于 arrayTable.id 字段存储
    string parseType(const string& fieldOrTypeName = "") {
        // 1. 用户定义类型标识符/类型别名
        if (currentToken().type == I) {
            string typeName = currentToken().value;
            advance();
            return typeName;
        }

        // 2. record
        if (currentToken().type == K && currentToken().code == KW_RECORD) {
            return parseRecordType(); // 你的原实现
        }

        // 3. packed array
        if (currentToken().type == K && currentToken().code == KW_PACKED) {
            advance();
            matchKeyword(KW_ARRAY);
            matchDelimiter(P_LBRACKET);

            int low = 0, up = 0;
            if (currentToken().type == C1) {
                low = stoi(currentToken().value);
                advance();
            }
            matchDelimiter(P_DOTDOT);
            if (currentToken().type == C1) {
                up = stoi(currentToken().value);
                advance();
            }
            matchDelimiter(P_RBRACKET);
            matchKeyword(KW_OF);

            string elementType = parseType(); // 递归，支持嵌套

            // 插入 arrayTable
            ArrayTable arr;
            arr.id = currentFieldName; // 类型名或字段名
            //cout<< currentFieldName<<endl;
            arr.low = low;
            arr.up = up;
            arr.ctp = elementType;
            if (elementType == "integer") arr.clen = (up - low + 1) * 4;
            else if (elementType == "real") arr.clen = (up - low + 1) * 8;
            else if (elementType == "char") arr.clen = (up - low + 1) * 2;
            else if (elementType == "boolean") arr.clen = (up - low + 1) * 1;
            else arr.clen = 0;
            arrayTable.push_back(arr);

            return "packed array of " + elementType;
        }

        // 4. 普通 array
        if (currentToken().type == K && currentToken().code == KW_ARRAY) {
            advance();
            matchDelimiter(P_LBRACKET);

            int low = 0, up = 0;
            if (currentToken().type == C1) {
                low = stoi(currentToken().value);
                advance();
            }
            matchDelimiter(P_DOTDOT);
            if (currentToken().type == C1) {
                up = stoi(currentToken().value);
                advance();
            }
            matchDelimiter(P_RBRACKET);
            matchKeyword(KW_OF);

            string elementType = parseType();

            ArrayTable arr;
            arr.id = fieldOrTypeName;
            arr.low = low;
            arr.up = up;
            arr.ctp = elementType;
            if (elementType == "integer") arr.clen = (up - low + 1) * 4;
            else if (elementType == "real") arr.clen = (up - low + 1) * 8;
            else if (elementType == "char") arr.clen = (up - low + 1) * 2;
            else if (elementType == "boolean") arr.clen = (up - low + 1) * 1;
            else arr.clen = 0;
            arrayTable.push_back(arr);

            return "array of " + elementType;
        }

        // 5. 基本类型
        if (currentToken().type != K) {
            syntaxError("Expected type keyword or identifier");
        }

        int typeCode = currentToken().code;
        advance();

        if (typeMap.find(typeCode) == typeMap.end()) {
            semanticError("Invalid type specified");
        }
        return typeMap.at(typeCode);
    }

    string parseRecordType(const string& recordName = "") {
        matchKeyword(KW_RECORD);

        unordered_map<string, string> fieldMap;
        while (true) {
            if (currentToken().type == K && currentToken().code == KW_END) {
                break;
            }
            // 字段列表
            vector<string> fieldNames;
            fieldNames.push_back(currentToken().value);
            matchIdentifier();
            while (currentToken().type == D && currentToken().code == P_COMMA) {
                advance();
                fieldNames.push_back(currentToken().value);
                matchIdentifier();
            }
            matchDelimiter(P_COLON);
            string fieldType = parseType();
            matchDelimiter(P_SEMICOLON);
            for (const auto& fname : fieldNames) {
                fieldMap[fname] = fieldType;
            }
        }
        matchKeyword(KW_END);

        // 记录record结构的字段到全局
        if (!recordName.empty()) {
            recordFieldTypes[recordName] = fieldMap;
        }
        return recordName.empty() ? "record" : recordName;
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
        // 这里插入过程名到主符号表Add commentMore actions
        int typIdx = insertType(TypeCode::NONE); // 或你的过程类型
        insertSymbol(procName, typIdx, CatCode::FUNC);

        // 进入新作用域
        enterScope();
        matchDelimiter(P_LPAREN);
     
        // 参数列表
        if (currentToken().type == D && currentToken().code == P_LPAREN) {
            advance();
            if (currentToken().type != D || currentToken().code != P_RPAREN) {
                parseParameterList();
            }
            matchDelimiter(P_RPAREN);
        }
        matchDelimiter(P_SEMICOLON);

        // 允许过程体内的 var 声明
        parseVarDeclarations();

        matchKeyword(KW_BEGIN);
        parseFunctionBody();
        matchKeyword(KW_END);
        matchDelimiter(P_RPAREN);
        matchDelimiter(P_SEMICOLON);

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
                int typIdx = -1;
                    if (paramType == "integer") typIdx = insertType(TypeCode::INT);
                    else if (paramType == "real") typIdx = insertType(TypeCode::REAL);
                    else if (paramType == "char") typIdx = insertType(TypeCode::CHAR);
                    else if (paramType == "boolean") typIdx = insertType(TypeCode::BOOL);

                // ... 其它类型
                insertSymbol(id, typIdx, isReference ? CatCode::VF : CatCode::VN);
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
    string getExprType(const string& name) {
        // 布尔常量
        if (name == "true" || name == "false") return "boolean";
        if (name.length() == 3 && name.front() == '\'' && name.back() == '\'') return "char";
        if (name.length() > 3 && name.front() == '\'' && name.back() == '\'') return "string";
        // 数字常量
        bool isNum = !name.empty() && (isdigit(name[0]) || (name[0] == '-' && name.size() > 1));
        if (isNum) {
            if (name.find('.') != string::npos) return "real";
            return "integer";
        }
        // 变量/临时变量
        return getVariableType(name);
    }

    // 语义分析：赋值语句类型检查
    void parseAssignment() {
        string varName = parseVariable(); // 获取完整左值
        string firstPart = varName.substr(0, varName.find('.'));
        if (!isVariableDeclared(firstPart)) {
            semanticError("Undeclared variable: " + firstPart);
        }

        matchDelimiter(P_ASSIGN);
        string rhs = parseExpression(); // 返回表达式结果（变量名、常量、或临时变量名）

        string varBaseType = getBaseType(getVariableType(varName));
        string exprBaseType = getBaseType(getExprType(rhs));

        if (!isTypeCompatible(varBaseType, exprBaseType)) {
            semanticError("Type mismatch: Cannot assign " + rhs +
                " to " + varBaseType + " variable '" + varName + "'");
        }

        quadGen.emit(":=", rhs, "", varName);
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
        string left = parseSimpleExpression();
        if (currentToken().type == D &&
            (currentToken().code == P_EQUAL ||
                currentToken().code == P_NOT_EQUAL ||
                currentToken().code == P_LESS ||
                currentToken().code == P_LESS_EQUAL ||
                currentToken().code == P_GREATER ||
                currentToken().code == P_GREATER_EQUAL)) {
            int opCode = currentToken().code;
            advance();
            string right = parseSimpleExpression();

            // 类型检查
            string leftType = getBaseType(getExprType(left));
            string rightType = getBaseType(getExprType(right));
            //cout << 1;
            //cout << leftType << " " << rightType << "\n";
            if (!isTypeCompatible(leftType, rightType)) {
                semanticError("Type mismatch in relational expression");
            }

            string temp = quadGen.newTemp();
            quadGen.emit(delimToStr(opCode), left, right, temp);
            return temp;
        }
        return left;
    }
    // 语义分析：简单表达式类型推导
    string parseSimpleExpression() {
        string left = parseTerm();
        string leftType = getBaseType(getExprType(left));

        while (currentToken().type == D &&
            (currentToken().code == P_PLUS ||
                currentToken().code == P_MINUS ||
                currentToken().code == KW_OR)) {

            int opCode = currentToken().code;
            Token op = currentToken();
            advance();

            string right = parseTerm();
            string rightType = getBaseType(getExprType(right));

            // 检查运算符适用性
            if (op.code == KW_OR) {
                if (leftType != "boolean" || rightType != "boolean") {
                    semanticError("OR operator requires boolean operands");
                }
            }
            else {
                if (!isNumeric(leftType) || !isNumeric(rightType)) {
                    semanticError("Arithmetic operator requires numeric operands");
                }
            }

            // 类型提升
            string resultType;
            if (op.code == KW_OR) {
                resultType = "boolean";
            }
            else if (leftType == "real" || rightType == "real") {
                resultType = "real";
            }
            else {
                resultType = leftType;
            }

            string temp = quadGen.newTemp();
            quadGen.emit(delimToStr(opCode), left, right, temp);
            tempVarType[temp] = resultType; // 记录临时变量类型

            left = temp;
            leftType = resultType;
        }

        return left;
    }


    // 语义分析：项类型推导
    string parseTerm() {
        string left = parseFactor();
        string leftType = getBaseType(getExprType(left));
        while (currentToken().type == D &&
            (currentToken().code == P_STAR ||
                currentToken().code == P_SLASH ||
                currentToken().code == KW_DIV ||
                currentToken().code == KW_MOD ||
                currentToken().code == KW_AND)) {

            int opCode = currentToken().code;
            Token op = currentToken();
            advance();

            string right = parseFactor();
            string rightType = getBaseType(getExprType(right));
            // 检查运算符适用性
            if (op.code == KW_AND) {
                if (leftType != "boolean" || rightType != "boolean") {
                    semanticError("AND operator requires boolean operands");
                }
            }
            else {
                if (!isNumeric(leftType) || !isNumeric(rightType)) {
                    semanticError("Arithmetic operator requires numeric operands");
                }
            }
            // 类型提升
            string resultType;
            if (op.code == KW_AND) {
                resultType = "boolean";
            }
            else if (leftType == "real" || rightType == "real") {
                resultType = "real";
            }
            else {
                resultType = leftType;
            }
            string temp = quadGen.newTemp();
            quadGen.emit(delimToStr(opCode), left, right, temp);
            tempVarType[temp] = resultType; // 记录临时变量类型

            left = temp;
            leftType = resultType;
        }
        return left;
    }

    // 语义分析：因子类型推导
    string parseFactor() {
        if (currentToken().type == I) {
            string varName = currentToken().value;
            matchIdentifier();
            return varName;
        }
        else if (currentToken().type == C1) {
            string value = currentToken().value;
            int typIdx = insertType(TypeCode::INT);
            insertConst(value, typIdx); // 插入常量表
            advance();
            return value;
        }
        else if (currentToken().type == C2) {
            string value = currentToken().value;
            int typIdx = insertType(TypeCode::REAL);
            insertConst(value, typIdx); // 插入常量表
            advance();
            return value;
        }
        else if (currentToken().type == K &&
            (currentToken().code == KW_TRUE || currentToken().code == KW_FALSE)) {
            string value = (currentToken().code == KW_TRUE) ? "true" : "false";
            advance();
            return value;
        }
        else if (currentToken().type == K && currentToken().code == KW_NOT) {
            advance();
            string val = parseFactor();
            string valType = getBaseType(getExprType(val));
            if (valType != "boolean") {
                semanticError("NOT operator requires boolean operand");
            }
            string temp = quadGen.newTemp();
            quadGen.emit("not", val, "", temp);
            return temp;
        }
        else if (currentToken().type == D && currentToken().code == P_LPAREN) {
            advance();
            string result = parseExpression();
            matchDelimiter(P_RPAREN);
            return result;
        }
        else if (currentToken().type == CT || currentToken().type == ST) {
            string value = "'" + currentToken().value + "'";
            advance();
            return value;
        }
        else {
            syntaxError("Unexpected token in factor");
            return "";
        }
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
        vector<string> fields = split(name, '.');
        string type = ""; // 变量/record类型
        if (tempVarType.count(name)) return tempVarType[name];
        // 1. 查找根变量类型
        for (int scope = current_scope; scope >= 0; --scope) {
            if (symbolTable.find(scope) != symbolTable.end()) {
                auto it = symbolTable[scope].find(fields[0]);
                if (it != symbolTable[scope].end()) {
                    type = it->second;
                    break;
                }
            }
        }
        if (type.empty()) {
            semanticError("Variable not found: " + fields[0]);
            return "";
        }

        // 2. 递归查找record字段类型
        for (size_t i = 1; i < fields.size(); ++i) {
            // 查找record字段定义
            if (recordFieldTypes.find(type) != recordFieldTypes.end()) {
                auto& fmap = recordFieldTypes[type];
                if (fmap.find(fields[i]) != fmap.end()) {
                    type = fmap[fields[i]];
                }
                else {
                    semanticError("Field not found: " + fields[i] + " in record " + type);
                    return "";
                }
            }
            else {
                semanticError("Type is not a record: " + type);
                return "";
            }
        }
        return type;
    }


    // 语义分析：类型兼容性检查（跳过ref前缀）
    bool isTypeCompatible(const string& targetType, const string& sourceType) {
        // 获取基础类型（忽略ref前缀）
        string baseTarget = getBaseType(targetType);
        string baseSource = getBaseType(sourceType);

        // 1. 相同类型直接兼容
        if (baseTarget == baseSource) return true;

        // 2. 特殊规则：整数可赋值给实数
        if (baseTarget == "real" && baseSource == "integer") return true;

        // 3. 特殊规则：字符可赋值给字符串
        if (baseTarget == "string" && baseSource == "char") return true;

        // 4. packed array of char 与 string 兼容
        if (baseTarget.find("packed array of char") != string::npos &&
            baseSource == "string") {
            return true;
        }
        if (baseTarget.find("array of char") != string::npos &&
            baseSource == "string") {
            return true;
        }
        if (baseTarget.find("array of integer") != string::npos &&
            baseSource == "integer") {
            return true;
        }
        if (baseTarget.find("packed array of integer") != string::npos &&
            baseSource == "integer") {
            return true;
        }
        // 5. 用户定义类型比较
        if (baseTarget == baseSource) return true; // 相同类型名

        // 6. 检查类型别名是否指向相同类型
        string resolvedTarget = resolveTypeAlias(baseTarget);
        string resolvedSource = resolveTypeAlias(baseSource);
        if (resolvedTarget == resolvedSource) {
            return true;
        }
        if (baseTarget == "longint" && baseSource == "integer") return true;
        return false;
    }
    //解析类别名
    string resolveTypeAlias(const string& typeName) {
        // 检查全局作用域中的类型别名
        if (symbolTable[0].find(typeName) != symbolTable[0].end()) {
            return symbolTable[0][typeName];
        }
        return typeName;
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

        // 处理可选的end前分号
        if (currentToken().type == D && currentToken().code == P_SEMICOLON) {
            advance();
        }

        matchKeyword(KW_END);
    }

    void parseIfStatement() {
        matchKeyword(KW_IF);
        string condition = parseExpression(); // 生成条件表达式
        string elseLabel = quadGen.newLabel();
        string endLabel = quadGen.newLabel();

        quadGen.emit("if", condition, "", elseLabel); // 条件为假跳转到else

        matchKeyword(KW_THEN);
        parseStatement(); // then分支

        quadGen.emit("el", "", "", endLabel); // then分支结束跳转到end

        quadGen.emit("label", elseLabel, "", ""); // else分支入口

        if (currentToken().type == K && currentToken().code == KW_ELSE) {
            matchKeyword(KW_ELSE);
            parseStatement(); // else分支
        }
        quadGen.emit("ie", "", "", "");
        quadGen.emit("label", endLabel, "", ""); // if语句结束

    }

    void parseWhileStatement() {
        matchKeyword(KW_WHILE);
        string beginLabel = quadGen.newLabel();
        quadGen.emit("wh", "", "", "");
        quadGen.emit("label", beginLabel, "", "");
        string condition = parseExpression();
        string endLabel = quadGen.newLabel();
        quadGen.emit("do", condition, "", endLabel);
        matchKeyword(KW_DO);
        parseStatement();

        quadGen.emit("jmp", "", "", beginLabel);
        quadGen.emit("we", "", "", "");
        quadGen.emit("label", endLabel, "", "");

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

    string getTypeDefinition(const string& typeName) {
        for (int scope = current_scope; scope >= 0; --scope) {
            if (symbolTable.count(scope) && symbolTable[scope].count(typeName)) {
                return symbolTable[scope][typeName];
            }
        }
        semanticError("Type not defined: " + typeName);
        return "";
    }

    bool isTypeDefined(const string& typeName) {
        for (int scope = current_scope; scope >= 0; --scope) {
            if (symbolTable.count(scope) && symbolTable[scope].count(typeName)) {
                return true;
            }
        }
        return false;
    }

    vector<string> split(const string& str, char delim) {
        vector<string> result;
        stringstream ss(str);
        string item;
        while (getline(ss, item, delim)) {
            result.push_back(item);
        }
        return result;
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

        printSymbolTable();
        printConstTable();
        printArrayTable();
        printStructTable();
    }
    catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }
    quadGen.printQuads();
    return 0;
}