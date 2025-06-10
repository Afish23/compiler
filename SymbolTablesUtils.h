#pragma once
#include "SymbolTables.h"
#include <iostream>
#include <iomanip> // 放在文件开头
int getTypeLen(int typIdx) {
    if (typIdx < 0 || typIdx >= typeTable.size()) return 1;
    const auto& t = typeTable[typIdx];
    switch (t.tval) {
    case TypeCode::INT:   return 4;
    case TypeCode::REAL:  return 8;
    case TypeCode::CHAR:  return 1;
    case TypeCode::BOOL:  return 1;
        // 复杂类型如数组、结构体可自行设定
    case TypeCode::ARRAY: return 0; // 或递归计算
    case TypeCode::RECORD:return 0; // 或递归计算
    default:              return 1;
    }
}


// 插入类型表并返回下标
int insertType(TypeCode tval, void* tpoint = nullptr) {
    typeTable.emplace_back(tval, tpoint);
    return (int)typeTable.size() - 1;
}

int insertSymbol(const string& name, int typIdx, CatCode cat) {
    static int currentAddr = 0;
    int len = getTypeLen(typIdx);
    int addr = currentAddr;
    symbolTable.push_back({ name, typIdx, cat, addr });
    currentAddr += len;
    return (int)symbolTable.size() - 1;
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
    cout << left
        << setw(15) << "NAME"
        << setw(6) << "TYP"
        << setw(6) << "CAT"
        << setw(8) << "ADDR" << endl;
    for (auto& e : symbolTable) {
        cout << left << setw(15) << e.name;
        if (e.cat == CatCode::FUNC || e.cat == CatCode::PROCEDURE) {
            cout << setw(6) << "f";
        }
        else {
            switch (typeTable[e.typ].tval) {
            case TypeCode::INT: cout << setw(6) << "itp"; break;
            case TypeCode::REAL: cout << setw(6) << "rtp"; break;
            case TypeCode::CHAR: cout << setw(6) << "ctp"; break;
            case TypeCode::BOOL: cout << setw(6) << "btp"; break;
            case TypeCode::ARRAY: cout << setw(6) << "a"; break;
            case TypeCode::RECORD: cout << setw(6) << "d"; break;
            default: cout << setw(6) << "?";
            }
        }
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
        cout << setw(8) << e.addr << endl;
    }
}
void printStructTable() {
    cout << "StructTable: "<<endl<<"idx | field#  | id | off | tp" << endl;
    for (int i = 0; i < structTable.size(); ++i) {
        for (int j = 0; j < structTable[i].fields.size(); ++j) {
            cout << setw(9) << i << " | "
                << setw(6) << j << " | "
                << setw(6) << structTable[i].fields[j].id << " | "
                << setw(4) << structTable[i].fields[j].off << " | "
                << setw(3) << structTable[i].fields[j].tp << endl;
        }
    }
}