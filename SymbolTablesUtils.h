#pragma once
#include "SymbolTables.h"
#include <iostream>

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
    cout << "TypeTable: idx | tval | tpoint" << endl;
    for (int i = 0; i < typeTable.size(); ++i) {
        cout << i << " | ";
        switch (typeTable[i].tval) {
        case TypeCode::INT: cout << "i"; break;
        case TypeCode::REAL: cout << "r"; break;
        case TypeCode::CHAR: cout << "c"; break;
        case TypeCode::BOOL: cout << "b"; break;
        case TypeCode::ARRAY: cout << "a"; break;
        case TypeCode::RECORD: cout << "d"; break;
        default: cout << "?";
        }
        cout << " | " << typeTable[i].tpoint << endl;
    }
}

// 打印符号表
void printSymbolTable() {
    cout << "NAME\tTYP\tCAT\tADDR" << endl;
    for (auto& e : symbolTable) {
        cout << e.name << "\t" << e.typ << "\t";
        switch (e.cat) {
        case CatCode::FUNC: cout << "f"; break;
        case CatCode::CONST: cout << "c"; break;
        case CatCode::TYPE: cout << "t"; break;
        case CatCode::FIELD: cout << "d"; break;
        case CatCode::VAR: cout << "v"; break;
        case CatCode::VN: cout << "vn"; break;
        case CatCode::VF: cout << "vf"; break;
        default: cout << "?";
        }
        cout << "\t" << e.addr << endl;
    }
}