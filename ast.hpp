#ifndef __AST_HPP__
#define __AST_HPP__

#include <vector>
#include <string>
#include <map>
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/TargetSelect.h"

using namespace llvm;

extern void yyerror(std::string s);

class ExprAST{
public:
	virtual Value* codegen() const = 0;
	virtual ~ExprAST() {}
};

class NumberExprAST : public ExprAST {
public:
	NumberExprAST(double val)
		: _val(val)
	{}
	Value* codegen() const;
private:
	double _val;
};

class INumberExprAST : public ExprAST {
public:
	INumberExprAST(int val)
		: _val(val)
	{}
	Value* codegen() const;
private:
	double _val;
};

class BoolExprAST : public ExprAST {
public:
	BoolExprAST(int val)
		: _val(val)
	{}
	Value* codegen() const;
private:
	int _val;
};

class VariableExprAST : public ExprAST {
public:
	VariableExprAST(std::string name)
		: _name(name)
	{}
	std::string getName() const;
	Value* codegen() const;
private:
	std::string _name;
};

class CallExprAST : public ExprAST {
public:
	CallExprAST(std::string name, std::vector<ExprAST*> *exps)
		: _name(name), _exps(exps)
	{}
	~CallExprAST() {
		for (auto e : *_exps)
			delete e;
		delete _exps;
	}
	Value* codegen() const;
private:
	CallExprAST(const CallExprAST&);
	CallExprAST operator=(const CallExprAST&);
	std::string _name;
	std::vector<ExprAST*> *_exps;
};

class ReturnExprAST : public ExprAST {
public:
	ReturnExprAST(ExprAST* expr)
		: _expr(expr)
	{}
	~ReturnExprAST() {
		delete _expr;
	}
	Value* codegen() const;
private:
	ExprAST* _expr;
private:
};

class UnExprAST : public ExprAST {
public:
	UnExprAST(int op, ExprAST* expr)
		: _op(op), _expr(expr)
	{}
	~UnExprAST() {
		delete _expr;
	}
	Value* codegen() const;
private:
	int _op;
	ExprAST* _expr;
};

class BinExprAST : public ExprAST {
public:
	BinExprAST(int op, ExprAST* lhs, ExprAST* rhs)
		: _op(op), _lhs(lhs), _rhs(rhs)
	{}
	~BinExprAST() {
		delete _lhs;
		delete _rhs;
	}
	Value* codegen() const;
private:
	BinExprAST(const BinExprAST&);
	BinExprAST operator=(const BinExprAST&);
	int _op;
	ExprAST* _lhs;
	ExprAST* _rhs;
};

class DeclarationExprAST : public ExprAST {
public:
	DeclarationExprAST( std::string name, ExprAST *expr)
		: _type(nullptr), _name(name), _expr(expr)
	{}
	~DeclarationExprAST() {
		delete _expr;
	}
	void setType(Type* type);
	Value* codegen() const;
private:
	DeclarationExprAST(const DeclarationExprAST&);
	DeclarationExprAST operator=(const DeclarationExprAST&);
	Type* _type;
	std::string _name;
	ExprAST *_expr;
};

class DeclarationArrayExprAST : public ExprAST {
public:
	DeclarationArrayExprAST(Type* type, std::vector<DeclarationExprAST*> *exprs) 
		: _type(type), _declarations(exprs)
	{
		for (auto e : *_declarations) {
			e->setType(_type);
		}
	}
	~DeclarationArrayExprAST() {
		for (auto e : *_declarations)
			delete e;
		delete _declarations;
	}
	Value* codegen() const;
private:
	DeclarationArrayExprAST(const DeclarationArrayExprAST&);
	DeclarationArrayExprAST operator=(const DeclarationArrayExprAST&);
	Type* _type;
	std::vector<DeclarationExprAST*> *_declarations;
};

class BlockExprAST : public ExprAST {
public:
	BlockExprAST(std::vector<ExprAST*> *commands)
		: _commands(commands)
	{}
	~BlockExprAST() {
		for (auto e :* _commands)
			delete e;
		delete _commands;
	}
	Value* codegen() const;
private:
	BlockExprAST(const BlockExprAST&);
	BlockExprAST operator=(const BlockExprAST&);
	std::vector<ExprAST*> *_commands;
};

class IfExprAST : public ExprAST {
public:
	IfExprAST(ExprAST* cond, ExprAST* then_branch, ExprAST* else_branch = nullptr)
		: _cond(cond), _then_branch(then_branch), _else_branch(else_branch)
	{}
	~IfExprAST() {
		delete _cond;
		delete _then_branch;
		delete _else_branch;
	}
	Value* codegen() const;
private:
	IfExprAST(const IfExprAST&);
	IfExprAST operator=(const IfExprAST&);
	ExprAST* _cond;
	ExprAST* _then_branch;
	ExprAST* _else_branch;
};

class ForExprAST : public ExprAST {
public:
	ForExprAST(ExprAST* start, ExprAST* cond, ExprAST* end, ExprAST* body)
		: _start(start), _cond(cond), _end(end), _body(body)
	{}
	~ForExprAST() {
		delete _start;
		delete _cond;
		delete _end;
		delete _body;
	}
	Value* codegen() const;
private:
	ForExprAST(const ForExprAST&);
	ForExprAST operator=(const ForExprAST&);
	ExprAST* _start;
	ExprAST* _cond;
	ExprAST* _end;
	ExprAST* _body;
};

class WhileExprAST : public ExprAST {
public:
	WhileExprAST(ExprAST* cond, ExprAST* body)
		: _cond(cond), _body(body)
	{}
	~WhileExprAST() {
		delete _cond;
		delete _body;
	}
	Value* codegen() const;
private:
	WhileExprAST(const WhileExprAST&);
	WhileExprAST operator=(const WhileExprAST&);
	ExprAST* _cond;
	ExprAST* _body;
};

class EmptyExprAST : public ExprAST {
public:
	Value* codegen() const;
};

class FunctionAST {
public:
	FunctionAST(std::string name, Type* ret, std::vector<std::pair<std::string, Type*>> *types , std::vector<ExprAST*> *definition)
		: _name(name), _ret(ret), _types(types), _definition(definition)
	{}
	~FunctionAST() {
		for (auto e : *_definition)
			delete e;
		delete _definition;
		delete _types;
	}
	Function* codegen() const;
private:
	FunctionAST(const FunctionAST&);
	FunctionAST operator=(const FunctionAST&);
	std::string _name;
	Type* _ret;
	std::vector<std::pair<std::string, Type*>> *_types;
	std::vector<ExprAST*> *_definition;
};

void initializeModule();
AllocaInst* createEntryBlockAlloca(Function* theFunction, const std::string& name, Type* type);

#endif
