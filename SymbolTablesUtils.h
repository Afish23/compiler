#pragma once
#include "SymbolTables.h"
#include <iostream>
#include <iomanip>
#include <unordered_map>

using namespace std;

unordered_map<string, int> arrayTypeAlias; // 类型名对应 arrayTable 下标

// 查找类型名对应的 sizeTable 长度
int findSizeByTypeName(const std::string& typeName) {
    for (const auto& entry : sizeTable) {
        if (entry.name == typeName) return entry.size;
    }
    return 0;
}

// 按 typIdx 返回类型长度，结构体/数组类型查 sizeTable
int getTypeLen(int typIdx) {
    if (typIdx < 0 || typIdx >= typeTable.size()) return 1;
    const auto& t = typeTable[typIdx];
    switch (t.tval) {
    case TypeCode::INT:   return 4;
    case TypeCode::REAL:  return 8;
    case TypeCode::CHAR:  return 1;
    case TypeCode::BOOL:  return 1;
    case TypeCode::ARRAY:
    case TypeCode::RECORD:
        for (const auto& entry : sizeTable)
            if (entry.typeIdx == typIdx) return entry.size;
        return 0;
    default:              return 1;
    }
}

// 插入类型表并返回下标
int insertType(TypeCode tval, void* tpoint = nullptr, const std::string& name = "") {
    for (int i = 0; i < typeTable.size(); ++i) {
        if (typeTable[i].tval == tval &&
            typeTable[i].tpoint == tpoint &&
            typeTable[i].name == name)
        {
            return i;
        }
    }
    typeTable.emplace_back(tval, tpoint, name);
    return static_cast<int>(typeTable.size()) - 1;
}

int insertSize(const string& name, int size, int   typeIdx) {
    sizeTable.push_back({ name, size, typeIdx });
    return sizeTable.size() -    1;
}

// 用于常量/类型等手动指定区距
int insertSymbol(const string& name, int typIdx, CatCode cat, int addr) {
    symbolTable.push_back({ name, typIdx, cat, addr });
    return (int)symbolTable.size() - 1;
}

// 用于类型/字段等指定类型名（hhh）但不分配区距
int insertSymbol(const string& name, int typIdx, CatCode cat, const string& hhh, bool forTypeOrFieldOnly) {
    // 用法: insertSymbol("Books", booksIdx, CatCode::TYPE, "Books", true);
    symbolTable.push_back({ name, typIdx, cat, 0, hhh });
    return (int)symbolTable.size() - 1;
}

// 变量/数组/结构体变量插入，typeName可为空
int insertSymbol(const string& name, int typIdx, CatCode cat, const string& typeName = "") {
    static int currentAddr = 0;
    int len = 0;
    if (!typeName.empty()) len = findSizeByTypeName(typeName);
    if (len == 0) len = getTypeLen(typIdx);
    int addr = currentAddr;
    symbolTable.push_back({ name, typIdx, cat, addr, typeName });
    if (cat == CatCode::VAR) currentAddr += len;
    return (int)symbolTable.size() - 1;
}

int insertConst(const string& value, int typeIdx) {
    for (int i = 0; i < constTable.size(); ++i)
        if (constTable[i].value == value && constTable[i].typeIdx == typeIdx)
            return i;
    constTable.push_back({ value, typeIdx });
    return constTable.size() - 1;
}

// 打印类型表
void printTypeTable() {
    cout << "TypeTable:" << endl
        << left << setw(5) << "idx"
        << setw(6) << "tval"
        << setw(10) << "tpoint" << endl;
    for (int i = 0; i < typeTable.size(); ++i) {
        cout << left << setw(5) << i;
        switch (typeTable[i].tval) {
        case TypeCode::INT: cout << setw(6) << "i"; break;
        case TypeCode::REAL: cout << setw(6) << "r"; break;
        case TypeCode::CHAR: cout << setw(6) << "c"; break;
        case TypeCode::BOOL: cout << setw(6) << "b"; break;
        case TypeCode::ARRAY: cout << setw(6) << "a"; break;
        case TypeCode::RECORD: cout << setw(6) << "d"; break;
        default: cout << setw(6) << "?";
        }
        cout << setw(10) << typeTable[i].tpoint << endl;
    }
}

// 打印符号表
void printSymbolTable() {
    cout << endl;
    cout << "语法分析表格部分" << endl;
    cout << "SYNBL:" << endl;
    cout << left
        << setw(15) << "NAME"
        << setw(10) << "TYP"
        << setw(6) << "CAT"
        << setw(8) << "ADDR" << endl;
    for (auto& e : symbolTable) {
        cout << left << setw(15) << e.name;
        // TYP列
        if (!e.hhh.empty() && (e.cat == CatCode::VAR || e.cat == CatCode::TYPE)) {
            cout << setw(10) << e.hhh;
        }
        else {
            switch (typeTable[e.typ].tval) {
            case TypeCode::INT: cout << setw(10) << "itp"; break;
            case TypeCode::REAL: cout << setw(10) << "rtp"; break;
            case TypeCode::CHAR: cout << setw(10) << "ctp"; break;
            case TypeCode::BOOL: cout << setw(10) << "btp"; break;
            case TypeCode::ARRAY: cout << setw(10) << "a"; break;
            case TypeCode::RECORD: cout << setw(10) << "d"; break;
            default: cout << setw(10) << "?";
            }
        }
        // CAT列
        switch (e.cat) {
        case CatCode::FUNC: cout << setw(6) << "f"; break;
        case CatCode::CONST: cout << setw(6) << "c"; break;
        case CatCode::TYPE: cout << setw(6) << "t"; break;
        case CatCode::FIELD: cout << setw(6) << "d"; break;
        case CatCode::VAR: cout << setw(6) << "v"; break;
        case CatCode::VN: cout << setw(6) << "vn"; break;
        case CatCode::VF: cout << setw(6) << "vf"; break;
        case CatCode::PROCEDURE: cout << setw(6) << "p"; break;
        default: cout << setw(6) << "?";
        }
        // ADDR列
        if (e.cat == CatCode::CONST) {
            cout << setw(8) << "CONSL:" << e.addr << endl;
        }
        else if (e.cat == CatCode::TYPE || e.cat == CatCode::FIELD) {
            cout << setw(8) << "Length" << endl;
        }
        else if (e.cat == CatCode::FUNC || e.cat == CatCode::PROCEDURE) {
            cout << setw(8) << "PFINFL" << endl;
        }
        else {
            cout << setw(8) << e.addr+4 << endl;
        }
    }
    cout << endl;
}

void printConstTable() {
    cout << "CONSL:" << endl;
    for (int i = 0; i < constTable.size(); ++i) {
        cout << i << ": " << constTable[i].value << endl;
    }
    cout << endl;
}

void printArrayTable() {
    cout << "AINFL:" << endl;
    cout << left << setw(5) << "ID"
        << setw(10) << "LOW"
        << setw(10) << "UP"
        << setw(10) << "CTP"
        << setw(10) << "CLEN" << endl;
    for (int i = 0; i < arrayTable.size(); ++i) {
        const auto& arr = arrayTable[i];
        cout << left << setw(5) << i
            << setw(10) << arr.low
            << setw(10) << arr.up
            << setw(10) << arr.ctp
            << setw(10) << arr.clen << endl;
    }
    cout << endl;
}

void printStructTable() {
    cout << "RINFL:" << endl;
    cout << left << setw(12) << "ID"
        << setw(8) << "OFF"
        << setw(8) << "TP" << endl;
    for (int i = 0; i < structTable.size(); ++i) {
        for (const auto& field : structTable[i].fields) {
            cout << left << setw(12) << field.id
                << setw(8) << field.off
                << setw(8) << field.tp << endl;
        }
    }
    cout << endl;
}

void printSizeTable() {
    cout << "Length:" << endl;
    for (int i = 0; i < sizeTable.size(); ++i) {
        cout << i << ": " << sizeTable[i].name << " = " << sizeTable[i].size << endl;
    }
    cout << endl;
}

void printFunctionTable() {
    cout << "PFINFL:\n";
    cout << std::left
        << setw(12) << "NAME"
        << setw(7) << "LEVEL"
        << setw(7) << "OFF"
        << setw(7) << "FN"
        << setw(8) << "ENTRY"
        << setw(8) << "PARAM" << endl;
    for (const auto& f : functionTable) {
        cout << left
            << setw(12) << f.name
            << setw(7) << f.level
            << setw(7) << f.off
            << setw(7) << f.fn
            << setw(8) << f.entry
            << setw(8) << "PARAM:" << f.param
            << endl;
    }
    cout << endl;
}

void printParaTable() {
    std::cout << "PARAM:\n";
    std::cout << std::left
        << std::setw(12) << "NAME"
        << std::setw(6) << "TYP"
        << std::setw(6) << "CAT"
        << std::setw(8) << "ADDR" << std::endl;
    for (const auto& p : paramTable) {
        std::cout << std::left << std::setw(12) << p.name;
        if (p.typ < 0 || p.typ >= (int)typeTable.size()) {
            std::cout << std::setw(6) << "?";
        }
        else {
            switch (typeTable[p.typ].tval) {
            case TypeCode::INT:    std::cout << std::setw(6) << "itp"; break;
            case TypeCode::REAL:   std::cout << std::setw(6) << "rtp"; break;
            case TypeCode::CHAR:   std::cout << std::setw(6) << "ctp"; break;
            case TypeCode::BOOL:   std::cout << std::setw(6) << "btp"; break;
            case TypeCode::ARRAY:  std::cout << std::setw(6) << "a"; break;
            case TypeCode::RECORD: std::cout << std::setw(6) << "d"; break;
            default:               std::cout << std::setw(6) << "?";
            }
        }
        std::cout << std::setw(6) << p.cat
            << "(1," << p.addr << ")"
            << std::endl;
    }
    std::cout << std::endl;
}
void syncParamAddrToSymbolTable() {
    for (const auto& param : paramTable) {
        for (auto& sym : symbolTable) {
            // 只同步参数符号，VN/VF类别
            if (sym.name == param.name && (sym.cat == CatCode::VN || sym.cat == CatCode::VF)) {
                sym.addr = param.addr+4; // 直接同步
            }
        }
    }
}