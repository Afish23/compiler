#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
using namespace std;

//=== 类型表 ===
enum class TypeCode { INT, REAL, CHAR, BOOL, ARRAY, RECORD, NONE };

struct ArrayTable;
struct StructTable;

struct TypeTableEntry {
    TypeCode tval;             // 类型代码
    void* tpoint;              // 指针：基本类型为空，数组型指向 ArrayTable，结构型指向 StructTable

    TypeTableEntry(TypeCode code = TypeCode::NONE, void* ptr = nullptr)
        : tval(code), tpoint(ptr) {
    }
};
extern vector<TypeTableEntry> typeTable;



//=== 数组表 ===
struct ArrayTable {
    string id;
    int low = 0;                // 下界，默认为0
    int up;                     // 上界
    string ctp;                    // 成分类型指针(类型表下标)
    int clen;                   // 成分类型长度
};

//=== 结构(记录)表 ===
struct StructField {
    string id;        // 域名
    int off;          // 区距
    string tp;           // 类型表下标
};

struct StructTable {
    vector<StructField> fields;
};



//=== 符号表条目 ===
enum class CatCode { FUNC, CONST, TYPE, FIELD, VAR, VN, VF, NONE, PROCEDURE };

struct SymbolTableEntry {
    string name;        // 标识符名
    int typ;            // 指向类型表的下标
    CatCode cat;        // 类别
    int addr;           // 指针：函数/变量/常量等的表下标
};

extern vector<SymbolTableEntry> symbolTable;



extern vector<ArrayTable> arrayTable;
extern vector<StructTable> structTable;
//函数表
struct FuncTableEntry {
    int level;      // 层次号
    int off;        // 区距
    int fn;         // 形参个数
    int param;      // 指向参数表symbolTable的起始下标
    int entry;      // 入口地址
};
extern vector<FuncTableEntry> funcTable;

// 常量表
struct ConstTableEntry {
    string value; // 常量的字面值
    int typeIdx;       // 类型表下标
};
extern vector<ConstTableEntry> constTable;
