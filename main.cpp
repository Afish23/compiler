#include <iostream>
#include <vector>
#include <unordered_map>
#include <cctype>
#include <string>
#include <stdexcept>
#include <iomanip>
#include <sstream>
using namespace std;

// ö�����Ͷ���
enum TokenType { K, D, I, C1, C2, CT, ST };

// ����״̬ö��
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

// Ԥ������ű�֧�ֶ��ַ��������
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

// ���ű������
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

// Token�ṹ��
struct Token {
    TokenType type;
    int code;
    string value;

    Token(TokenType t, int c, string v = "")
        : type(t), code(c), value(std::move(v)) {}
};

// �ʷ���������
class Lexer {
private:
    SymbolTable idTable;      // ��ʶ����
    SymbolTable constIntTable;  // ���ͳ�����
    SymbolTable constFloatTable; // �����ͳ�����
    SymbolTable constCharTable;  // �ַ�������
    SymbolTable constStringTable; // �ַ���������

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
        bool isHex = false;//ʮ����������ʶ
        bool isFloat = false;//��������ʶ
        bool hasExp = false;//��ѧ��������ʶ

        // ���16����ǰ׺
        if (buffer == "0" && (currentChar() == 'x' || currentChar() == 'X')) {
            isHex = true;
            buffer += currentChar();
            nextChar();
        }

        while (true)
        {
            char c = currentChar();

            if (isHex) {
                if (isxdigit(c)) {//���ʮ������ǰ׺�����ֵ�����Ƿ�Ϸ�
                    buffer += c;
                    nextChar();
                }
                else {
                    break;
                }
            }
            else {//�����븡����ʶ��
                if (isdigit(c)) {
                    buffer += c;
                    nextChar();
                }
                else if (c == '.') {
                    if (isFloat) {//�ѱ�ʶ��Ϊ���������������ʶ��һ��С����
                        hasError = true;
                        buffer += c;
                        nextChar();
                        break;
                    }
                    isFloat = true;//��ʶΪ������
                    buffer += c;
                    nextChar();
                }
                else if (c == 'e' || c == 'E') {
                    if (hasExp) {//�ѱ�ʶ��Ϊ��ѧ���������������ʶ��һ��e
                        hasError = true;
                        buffer += c;
                        nextChar();
                        break;
                    }
                    isFloat = true;//��ʶΪ��ѧ��������������
                    hasExp = true;
                    buffer += c;
                    nextChar();

                    // ����ָ������
                    if (currentChar() == '+' || currentChar() == '-') {
                        buffer += currentChar();
                        nextChar();
                        if (!isalpha(currentChar()))//��Ϊ��ѧ���������Ӽ��ź���������
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

        // ���Ƿ���׺
        if (isalpha(currentChar())) {
            hasError = true;
            buffer += currentChar();
            nextChar();
        }

        if (hasError) {
            throw runtime_error("Invalid number format: " + buffer);
        }

        // ��ӵ���Ӧ�ĳ�����
        if (isFloat || hasExp) {
            int id = constFloatTable.addSymbol(buffer);
            tokens.emplace_back(TokenType::C2, id + 1, buffer);
        }
        else if (isHex) {
            // ת��16����Ϊ10����
            unsigned int value;
            stringstream ss;
            ss << hex << buffer.substr(2); // ȥ��0xǰ׺
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
        nextChar(); // ������ʼ�ĵ�����

        if (currentChar() == '\'') { // ���ַ����
            hasError = true;
            throw runtime_error("Empty character literal");
        }

        buffer = currentChar(); // ֻȡһ���ַ�
        nextChar();

        if (currentChar() != '\'') {
            hasError = true;
            throw runtime_error("Unclosed character literal");
        }
        nextChar(); // ���������ĵ�����

        int id = constCharTable.addSymbol(buffer);
        tokens.emplace_back(TokenType::CT, id + 1, buffer);

        buffer.clear();
        state = State::START;
    }

    // �޸ĺ���ַ�����������
    void processStringLiteral(vector<Token>& tokens) {
        nextChar(); // ������ʼ��˫����

        while (currentChar() != '"' && currentChar() != '\0') {
            buffer += currentChar();
            nextChar();
        }

        if (currentChar() != '"') {
            hasError = true;
            throw runtime_error("Unclosed string literal");
        }
        nextChar(); // ����������˫����

        // ���ַ����������
        int id = constStringTable.addSymbol(buffer);
        tokens.emplace_back(TokenType::ST, id + 1, buffer);

        buffer.clear();
        state = State::START;
    }

    void processOperator(vector<Token>& tokens) {
        // ̰��ƥ������
        string longestMatch;
        size_t maxLen = 0;

        // ������п��ܵĽ�����ȣ�1-2���ַ���
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
                        nextChar(); // ������������
                    }
                    else if (c == '"') {
                        state = State::IN_STRING;
                        buffer.clear();
                        nextChar(); // ������˫����
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
                    if (currentChar() == '\'') { // ������������
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
                        nextChar(); // ������������
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
                    if (currentChar() == '"') { // ������������
                        int id = constStringTable.addSymbol(buffer);
                        tokens.emplace_back(TokenType::ST, id + 1, buffer);
                        buffer.clear();
                        nextChar(); // ������������
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