#pragma once
#include "SymbolTables.h"
#include <iostream>
#include <iomanip> // 放在文件开头

// 插入类型表并返回下标
int insertType(TypeCode tval, void* tpoint = nullptr) {
    typeTable.emplace_back(tval, tpoint);
    return (int)typeTable.size() - 1;
}

// 插入符号表并返回下标
int insertSymbol(const string& name, int typIdx, CatCode cat, int addr = -1) {
    symbolTable.push_back({ name, typIdx, cat, addr });
    return (int)symbolTable.size() - 1;
}

// 打印类型表
void printTypeTable() {
    cout << "TypeTable:" << endl
        << left << setw(5) << "idx"
        << setw(6) << "tva"
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
    cout << left
        << setw(15) << "NAME"
        << setw(6) << "TYP"
        << setw(6) << "CAT"
        << setw(8) << "ADDR" << endl;
    for (auto& e : symbolTable) {
        cout << left << setw(15) << e.name
            << setw(6) << e.typ;
        switch (e.cat) {
        case CatCode::FUNC: cout << setw(6) << "f"; break;
        case CatCode::CONST: cout << setw(6) << "c"; break;
        case CatCode::TYPE: cout << setw(6) << "t"; break;
        case CatCode::FIELD: cout << setw(6) << "d"; break;
        case CatCode::VAR: cout << setw(6) << "v"; break;
        case CatCode::VN: cout << setw(6) << "vn"; break;
        case CatCode::VF: cout << setw(6) << "vf"; break;
        default: cout << setw(6) << "?";
        }
        cout << setw(8) << e.addr << endl;
    }
}