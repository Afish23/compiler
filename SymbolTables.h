#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>
using namespace std;

//=== ���ͱ� ===
enum class TypeCode { INT, REAL, CHAR, BOOL, ARRAY, RECORD, NONE };

struct ArrayTable;
struct StructTable;

struct TypeTableEntry {
    TypeCode tval;             // ���ʹ���
    void* tpoint;              // ָ�룺��������Ϊ�գ�������ָ�� ArrayTable���ṹ��ָ�� StructTable

    TypeTableEntry(TypeCode code = TypeCode::NONE, void* ptr = nullptr)
        : tval(code), tpoint(ptr) {
    }
};
extern vector<TypeTableEntry> typeTable;



//=== ����� ===
struct ArrayTable {
    string id;
    int low = 0;                // �½磬Ĭ��Ϊ0
    int up;                     // �Ͻ�
    string ctp;                    // �ɷ�����ָ��(���ͱ��±�)
    int clen;                   // �ɷ����ͳ���
};

//=== �ṹ(��¼)�� ===
struct StructField {
    string id;        // ����
    int off;          // ����
    string tp;           // ���ͱ��±�
};

struct StructTable {
    vector<StructField> fields;
};



//=== ���ű���Ŀ ===
enum class CatCode { FUNC, CONST, TYPE, FIELD, VAR, VN, VF, NONE, PROCEDURE };

struct SymbolTableEntry {
    string name;        // ��ʶ����
    int typ;            // ָ�����ͱ���±�
    CatCode cat;        // ���
    int addr;           // ָ�룺����/����/�����ȵı��±�
};

extern vector<SymbolTableEntry> symbolTable;



extern vector<ArrayTable> arrayTable;
extern vector<StructTable> structTable;
//������
struct FuncTableEntry {
    int level;      // ��κ�
    int off;        // ����
    int fn;         // �βθ���
    int param;      // ָ�������symbolTable����ʼ�±�
    int entry;      // ��ڵ�ַ
};
extern vector<FuncTableEntry> funcTable;

// ������
struct ConstTableEntry {
    string value; // ����������ֵ
    int typeIdx;       // ���ͱ��±�
};
extern vector<ConstTableEntry> constTable;
