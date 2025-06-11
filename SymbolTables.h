#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
using namespace std;
//unordered_map<string, int> name2level;
//=== 类型表 ===
enum class TypeCode { INT, REAL, CHAR, BOOL, ARRAY, RECORD, NONE };

struct ArrayTable;
struct StructTable;

struct TypeTableEntry {
    TypeCode tval;             // 类型代码
    void* tpoint;              // 指针：基本类型为空，数组型指向 ArrayTable，结构型指向 StructTable
    string name;

    TypeTableEntry(TypeCode code = TypeCode::NONE, void* ptr = nullptr, const string& nm = "")
        : tval(code), tpoint(ptr), name(nm) {
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

struct lengthTable {
    int id;
    int length;
};



struct AddrValue {
    enum AddrKind { AR, CONST_TABLE, LENGTH_TABLE, PFINFL, OTHER } kind;
    int level;        // 活动记录层数
    int offset;       // 在该层的偏移
    int index;        // 指向常量表、长度表等下标
    std::string tag;  // 其他说明

    AddrValue() : kind(OTHER), level(-1), offset(-1), index(-1), tag("") {}
    static AddrValue makeAR(int level, int offset) {
        AddrValue addr; addr.kind = AR; addr.level = level; addr.offset = offset; return addr;
    }
    static AddrValue makeConst(int idx) {
        AddrValue addr; addr.kind = CONST_TABLE; addr.index = idx; return addr;
    }
    static AddrValue makeLength() {
        AddrValue addr; addr.kind = LENGTH_TABLE; return addr;
    }
    static AddrValue makeTag(const std::string& t) {
        AddrValue addr; addr.kind = PFINFL; addr.tag = t; return addr;
    }
};

//=== 符号表条目 ===
enum class CatCode { FUNC, CONST, TYPE, FIELD, VAR, VN, VF, NONE, PROCEDURE };

struct SymbolTableEntry {
    string name;        // 标识符名
    int typ;            // 指向类型表的下标
    CatCode cat;        // 类别
    
    int addr;
    string hhh;// 指针：函数/变量/常量等的表下标
};

extern vector<SymbolTableEntry> symbolTable;

extern vector<lengthTable> lenTable;

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

struct FunctionEntry {
    string name; // 函数名(附加)
    int level;    // 作用域层级
    int off;      // 偏移量
    int fn;       // 参数个数
    int entry;    // 入口地址(保留字段)
    int param;    // 参数表指针(比如参数表的起始下标)
};
extern vector<FunctionEntry> functionTable;

struct ParaEntry {
    string name;  // 参数名
    int typ;   // 类型缩写或类型指针
    string cat;   // vn:赋值形参, vf:换名形参
    int addr;          // 偏移量或参数指针
};

extern vector<ParaEntry> paramTable;

struct SizeTableEntry {
    string name;    // 类型名或变量名
    int size;       // 字节数
    int typeIdx;    // 类型表下标
};

extern vector<SizeTableEntry> sizeTable;