#include <iostream>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <string>
#include <stdexcept>
#include <iomanip>
#include <sstream>
#include <fstream> 
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
    {"}", 13}, {"[", 14}, {"]", 15}
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

class PascalParser {
private:
    // 关键字编码常量 - 使用您提供的映射表
    const int KW_PROGRAM = 1;
    const int KW_VAR = 2;
    const int KW_INTEGER = 3;
    const int KW_REAL = 4;
    const int KW_CHAR = 5;
    const int KW_BEGIN = 6;
    const int KW_END = 7;
    const int KW_PROCEDURE = 14; // 使用procedure代替function
    const int KW_RETURN = 100;   // 自定义编码 (原表中无return)

    // 界符编码常量 - 使用您提供的映射表
    const int P_COMMA = 1;
    const int P_COLON = 2;
    const int P_SEMICOLON = 3;
    const int P_ASSIGN = 4;     // :=
    const int P_STAR = 5;       // *
    const int P_SLASH = 6;      // /
    const int P_PLUS = 7;       // +
    const int P_MINUS = 8;      // -
    const int P_DOT = 9;        // .
    const int P_LPAREN = 10;    // (
    const int P_RPAREN = 11;    // )
    const int P_LBRACE = 12;    // {
    const int P_RBRACE = 13;    // }
    const int P_LBRACKET = 14;  // [
    const int P_RBRACKET = 15;  // ]

    vector<Token> tokens;
    size_t current_token_index;
    int current_line; // 当前行号（用于错误报告）

public:
    PascalParser(const vector<Token>& tokens)
        : tokens(tokens), current_token_index(0), current_line(1) {
    }

    // 主解析函数
    void parse() {
        parseProgram();
        match(END_OF_INPUT, "end of input");
        cout << "Syntax analysis completed successfully!" << endl;
    }

private:
    // 伪枚举值用于匹配文件结束
    static const TokenType END_OF_INPUT = static_cast<TokenType>(-1);

    // 获取当前token
    const Token& currentToken() const {
        if (current_token_index < tokens.size()) {
            return tokens[current_token_index];
        }
        static Token eof{ END_OF_INPUT, -1, "" };
        return eof;
    }

    // 前进到下一个token
    void advance() {
        if (current_token_index < tokens.size()) {
            current_token_index++;
        }
    }

    // 语法错误处理
    void syntaxError(const string& message) {
        cerr << "Syntax error: " << message;

        if (currentToken().type != END_OF_INPUT) {
            cerr << " (Found: ";
            switch (currentToken().type) {
            case K:
                cerr << "KEYWORD:" << currentToken().code;
                break;
            case D:
                cerr << "DELIMITER:" << currentToken().code;
                break;
            case I:
                cerr << "IDENTIFIER:" << currentToken().value;
                break;
            case C1:
                cerr << "INTEGER:" << currentToken().value;
                break;
            case C2:
                cerr << "REAL:" << currentToken().value;
                break;
            case CT:
                cerr << "CHAR:" << currentToken().value;
                break;
            case ST:
                cerr << "STRING:" << currentToken().value;
                break;
            default:
                cerr << "UNKNOWN";
            }
            cerr << ")";
        }
        cerr << endl;
        throw runtime_error("Syntax analysis failed");
    }

    // 通用匹配函数
    void match(TokenType expected_type, const string& description, int expected_code = -1) {
        if (currentToken().type == expected_type) {
            if (expected_code == -1 || currentToken().code == expected_code) {
                advance();
                return;
            }
        }
        syntaxError("Expected " + description);
    }

    // 匹配关键字
    void matchKeyword(int keywordCode) {
        match(K, "keyword " + to_string(keywordCode), keywordCode);
    }

    // 匹配界符
    void matchDelimiter(int delimiterCode) {
        match(D, "delimiter " + to_string(delimiterCode), delimiterCode);
    }

    // 匹配标识符
    void matchIdentifier() {
        match(I, "identifier");
    }

    // 匹配数字常量
    void matchNumber() {
        if (currentToken().type == C1 || currentToken().type == C2) {
            advance();
        }
        else {
            syntaxError("Expected number constant");
        }
    }

    // 解析程序
    void parseProgram() {
        matchKeyword(KW_PROGRAM);
        matchIdentifier(); // 程序名
        parseVarDeclarations();
        parseProcedureDeclarations();
        parseMainBlock();
    }

    // 解析变量声明
    void parseVarDeclarations() {
        if (currentToken().type == K && currentToken().code == KW_VAR) {
            matchKeyword(KW_VAR);
            parseIdentifierList();
            matchDelimiter(P_COLON); // 匹配冒号
            parseType();
            matchDelimiter(P_SEMICOLON); // 匹配分号
        }
    }

    // 解析标识符列表
    void parseIdentifierList() {
        matchIdentifier();
        while (currentToken().type == D && currentToken().code == P_COMMA) {
            advance(); // 跳过逗号
            matchIdentifier();
        }
    }

    // 解析类型
    void parseType() {
        if (currentToken().type == K) {
            int typeCode = currentToken().code;
            if (typeCode == KW_INTEGER || typeCode == KW_REAL || typeCode == KW_CHAR) {
                advance();
            }
            else {
                syntaxError("Expected type keyword (integer/real/char)");
            }
        }
        else {
            syntaxError("Expected type keyword");
        }
    }

    // 解析过程/函数声明
    void parseProcedureDeclarations() {
        while (currentToken().type == K && currentToken().code == KW_PROCEDURE) {
            parseProcedureDeclaration();
        }
    }

    // 解析单个过程声明
    void parseProcedureDeclaration() {
        matchKeyword(KW_PROCEDURE);
        matchIdentifier(); // 过程名
        matchDelimiter(P_LPAREN); // (
        parseParameterList();
        matchDelimiter(P_RPAREN); // )
        matchDelimiter(P_SEMICOLON); // ;
        matchKeyword(KW_BEGIN); // begin
        parseFunctionBody();
        matchKeyword(KW_END); // end
        matchDelimiter(P_SEMICOLON); // 添加这行 ✅ 匹配过程声明结束的分号
    }

    // 解析参数列表
    void parseParameterList() {
        if (currentToken().type == I) {
            parseIdentifierList();
            matchDelimiter(P_COLON); // :
            parseType();

            while (currentToken().type == D && currentToken().code == P_SEMICOLON) {
                advance(); // 跳过分号
                parseIdentifierList();
                matchDelimiter(P_COLON); // :
                parseType();
            }
        }
    }

    // 解析函数体
    void parseFunctionBody() {
        if (currentToken().type == K && currentToken().code == KW_RETURN) {
            parseReturnStatement();
        }
        else {
            syntaxError("Function body must contain a return statement");
        }
    }

    // 解析返回语句
    void parseReturnStatement() {
        matchKeyword(KW_RETURN);
        parseExpression();
    }

    // 解析主程序块
    void parseMainBlock() {
        matchKeyword(KW_BEGIN); // begin
        parseStatementList();
        matchKeyword(KW_END); // end
        matchDelimiter(P_DOT); // .
    }

    // 解析语句列表
    void parseStatementList() {
        while (currentToken().type == I ||
            (currentToken().type == K && currentToken().code == KW_RETURN)) {
            parseStatement();
            matchDelimiter(P_SEMICOLON); // ;
        }
    }

    // 解析语句
    void parseStatement() {
        if (currentToken().type == I) {
            // 前瞻一个token
            if (current_token_index + 1 < tokens.size()) {
                const Token& lookahead = tokens[current_token_index + 1];
                if (lookahead.type == D && lookahead.code == P_ASSIGN) {
                    parseAssignment();
                }
                else if (lookahead.type == D && lookahead.code == P_LPAREN) {
                    parseFunctionCall();
                }
                else {
                    syntaxError("Invalid statement");
                }
            }
            else {
                syntaxError("Unexpected end of tokens");
            }
        }
        else if (currentToken().type == K && currentToken().code == KW_RETURN) {
            parseReturnStatement();
        }
        else {
            syntaxError("Expected identifier or return statement");
        }
    }

    // 解析赋值语句
    void parseAssignment() {
        matchIdentifier(); // 变量名
        matchDelimiter(P_ASSIGN); // :=
        parseExpression();
    }

    // 解析函数调用
    void parseFunctionCall() {
        matchIdentifier(); // 函数名
        matchDelimiter(P_LPAREN); // (
        parseArgumentList();
        matchDelimiter(P_RPAREN); // )
    }

    // 解析参数列表
    void parseArgumentList() {
        parseExpression();
        while (currentToken().type == D && currentToken().code == P_COMMA) {
            advance(); // 跳过逗号
            parseExpression();
        }
    }

    // 解析表达式
    void parseExpression() {
        parseSimpleExpression();
        // 可以扩展关系运算符等
    }

    // 解析简单表达式
    void parseSimpleExpression() {
        parseTerm();
        while (currentToken().type == D &&
            (currentToken().code == P_PLUS || currentToken().code == P_MINUS)) {
            advance(); // 跳过+或-
            parseTerm();
        }
    }

    // 解析项
    void parseTerm() {
        parseFactor();
        while (currentToken().type == D &&
            (currentToken().code == P_STAR || currentToken().code == P_SLASH)) {
            advance(); // 跳过*或/
            parseFactor();
        }
    }

    // 解析因子
    void parseFactor() {
        if (currentToken().type == I) {
            // 前瞻一个token
            if (current_token_index + 1 < tokens.size()) {
                const Token& lookahead = tokens[current_token_index + 1];
                if (lookahead.type == D && lookahead.code == P_LPAREN) {
                    parseFunctionCall();
                }
                else {
                    matchIdentifier();
                }
            }
            else {
                matchIdentifier();
            }
        }
        else if (currentToken().type == C1 || currentToken().type == C2) {
            matchNumber();
        }
        else if (currentToken().type == D && currentToken().code == P_LPAREN) {
            advance(); // 跳过(
            parseExpression();
            matchDelimiter(P_RPAREN); // )
        }
        else if (currentToken().type == D &&
            (currentToken().code == P_PLUS ||
                currentToken().code == P_MINUS)) {
            // 处理一元运算符
            advance(); // 跳过+或-
            parseFactor();
        }
        else {
            syntaxError("Expected identifier, number, or '('");
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
    }
    catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }

    fin.close();
    printResults(allTokens, lexer);
    return 0;
}