#include "ast.hpp"
#include <iostream>

std::unique_ptr<Module> theModule;
LLVMContext theContext;
IRBuilder<> builder(theContext);
std::map<std::string, AllocaInst*> namedValues;

Value* NumberExprAST::codegen() const {
	return ConstantFP::get(theContext, APFloat(_val));
}

Value* INumberExprAST::codegen() const {
	return ConstantInt::get(theContext, APInt(32, _val));
}

Value* BoolExprAST::codegen() const {
	return ConstantInt::get(theContext, APInt(1, _val));
}

Value* VariableExprAST::codegen() const {
	AllocaInst* alloca = namedValues[_name];
	if (alloca == nullptr) {
		std::string msg = "Variable ";
		msg += _name;
		msg += " is not declared";
		yyerror(msg);
	}

	return builder.CreateLoad(alloca, _name);
}

std::string VariableExprAST::getName() const {
	return _name;
}

Value* CallExprAST::codegen() const {
	Function *f = theModule->getFunction(_name);
	if (!f) {
		std::string msg = "Function ";
		msg += _name;
		msg += " is not defined!";
		yyerror(msg);
	}
	FunctionType* ft = f->getFunctionType();
	if (f->arg_size() != _exps->size()) {
		std::string msg = "Call of function ";
		msg += _name;
		msg += " is not regular!";
		yyerror(msg);
	}
	std::vector<Value*> argV;
	int j = 0;
	for (auto e : *_exps) {
		Value *tmp = e->codegen();

		Type* type = tmp->getType();
		Type* param_type = ft->getParamType(j);
		j++;

		if (param_type == Type::getDoubleTy(theContext) && type == Type::getInt32Ty(theContext)) {
			tmp = builder.CreateSIToFP(tmp, Type::getDoubleTy(theContext), "tmpconvert");
		} else if (param_type == Type::getInt32Ty(theContext) && type == Type::getDoubleTy(theContext)) {
			tmp = builder.CreateFPToSI(tmp, Type::getInt32Ty(theContext), "tmpconvert");
		} else if (param_type != type) {
			yyerror("Incompatible types!");
		}
		argV.push_back(tmp);
		if (!argV.back())
			return nullptr;
	}
	if (f->getReturnType() == Type::getVoidTy(theContext)) {
		return builder.CreateCall(f, argV);
	} else {
		return builder.CreateCall(f, argV, "calltmp");
	}
}

Value* ReturnExprAST::codegen() const {
	if (Value* ret = _expr->codegen()) {
		Type* ret_type = builder.getCurrentFunctionReturnType();
		if (ret->getType() != ret_type) {
			if (ret_type == Type::getDoubleTy(theContext) && ret->getType() != Type::getInt1Ty(theContext)) {
				ret = builder.CreateSIToFP(ret, Type::getDoubleTy(theContext), "tmpconvert");
			} else if (ret_type == Type::getInt32Ty(theContext) && ret->getType() != Type::getInt1Ty(theContext)) {
				ret = builder.CreateFPToSI(ret, Type::getInt32Ty(theContext), "tmpconvert");
			} else if (ret_type == Type::getInt1Ty(theContext) && ret->getType() != Type::getDoubleTy(theContext)) {
				ret->mutateType(Type::getInt1Ty(theContext));
			} else if (ret_type == Type::getInt1Ty(theContext) && ret->getType() == Type::getDoubleTy(theContext)) {
				ret = builder.CreateFPToSI(ret, Type::getInt1Ty(theContext), "tmpconvert");
			} else {
				yyerror("Incompatible types!");
			}
		}

		return builder.CreateRet(ret);
	}
	return nullptr;
}

Value* UnExprAST::codegen() const {
	switch(_op) {
		case '-': {
			Value* exprV = _expr->codegen();
			if (!exprV) {
				return nullptr;
			}
			if (exprV->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			return builder.CreateNeg(exprV, "tmpneg");
		}
		case '!': {
			Value* exprV = _expr->codegen();
			if (!exprV) {
				return nullptr;
			}
			if (exprV->getType() == Type::getDoubleTy(theContext)) {
				exprV = builder.CreateFCmpUNE(exprV, ConstantFP::get(theContext, APFloat(0.0)), "tmpcmp");
			} else if (exprV->getType() == Type::getInt32Ty(theContext)) {
				exprV = builder.CreateICmpNE(exprV, ConstantInt::get(theContext, APInt(32, 0)), "tmpcmp");
			} else if (exprV->getType() != Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			return builder.CreateNot(exprV, "tmpnot");
		}
		case 256: { // increment
			VariableExprAST* var = dynamic_cast<VariableExprAST*>(_expr);
			std::string name = var->getName();
			Value* alloca = namedValues[name];
			if (!alloca) {
				std::string msg = "Undeclared variable ";
				msg += name;
				msg += "!";
				yyerror(msg);
			}

			Value* tmp = _expr->codegen();

			if (tmp->getType() != Type::getInt32Ty(theContext)) {
				yyerror("Incompatible types!");
			}

			tmp = builder.CreateAdd(tmp, ConstantInt::get(theContext, APInt(32, 1)), "tmpadd");
			return builder.CreateStore(tmp, alloca);
		}
		case 257: { //decrement
			VariableExprAST* var = dynamic_cast<VariableExprAST*>(_expr);
			std::string name = var->getName();
			Value* alloca = namedValues[name];
			if (!alloca) {
				std::string msg = "Undeclared variable ";
				msg += name;
				msg += "!";
				yyerror(msg);
			}

			Value* tmp = _expr->codegen();

			if (tmp->getType() != Type::getInt32Ty(theContext)) {
				yyerror("Incompatible types!");
			}

			tmp = builder.CreateSub(tmp, ConstantInt::get(theContext, APInt(32, 1)), "tmpsub");
			return builder.CreateStore(tmp, alloca);
		}
		default:
			return nullptr;
	}
}

Value* BinExprAST::codegen() const {
	if (_op == '=') {
		Value *r = _rhs->codegen();
		VariableExprAST* lhs = dynamic_cast<VariableExprAST*>(_lhs);
		if (!lhs) {
			return nullptr;
		}
		std::string name = lhs->getName();
		Value* alloca = namedValues[name];
		if (!alloca) {
			std::string msg = "Undeclared variable ";
			msg += name;
			msg += "!";
			yyerror(msg);
		}

		return builder.CreateStore(r, alloca);
	}
	Value *l = _lhs->codegen();
	Value *r = _rhs->codegen();
	if (!l || !r) {
		return nullptr;
	}
	switch(_op) {
		case '+': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFAdd(l, r, "tmpfadd");
			} else {
				return builder.CreateAdd(l, r, "tmpadd");
			}
		}
		case '-': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFSub(l, r, "tmpfsub");
			} else {
				return builder.CreateSub(l, r, "tmpsub");
			}
		}
		case '*': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFMul(l, r, "tmpfmul");
			} else {
				return builder.CreateMul(l, r, "tmpmul");
			}
		}
		case '/': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFDiv(l, r, "tmpfdiv");
			} else {
				return builder.CreateSDiv(l, r, "tmpdiv");
			}
		}
		case '%': { // works only on int32
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext) ||
				l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFRem(l, r, "tmpfrem");
			} else {
				return builder.CreateSRem(l, r, "tmprem");
			}
		}
		case '>': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpUGT(l, r, "tmpfgt");
			} else {
				return builder.CreateICmpSGT(l, r, "tmpgt");
			}
		}
		case '<': {
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpULT(l, r, "tmpflt");
			} else {
				return builder.CreateICmpSLT(l, r, "tmplt");
			}
		}
		case 256: { //<= token
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpULE(l, r, "tmpfle");
			} else {
				return builder.CreateICmpSLE(l, r, "tmple");
			}
		}
		case 257: { // >= token
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpUGE(l, r, "tmpfge");
			} else {
				return builder.CreateICmpSGE(l, r, "tmpge");
			}
		}
		case 258: { //== token
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpUEQ(l, r, "tmpfeq");
			} else {
				return builder.CreateICmpEQ(l, r, "tmpeq");
			}
		}
		case 259: { //!= token
			if (l->getType() == Type::getInt1Ty(theContext) || r->getType() == Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (l->getType() == Type::getDoubleTy(theContext) || r->getType() == Type::getDoubleTy(theContext)) {
				if (l->getType() != Type::getDoubleTy(theContext)) {
					l = builder.CreateSIToFP(l, Type::getDoubleTy(theContext), "tmpconvert");
				} else if (r->getType() != Type::getDoubleTy(theContext)) {
					r = builder.CreateSIToFP(r, Type::getDoubleTy(theContext), "tmpconvert");
				}
				return builder.CreateFCmpUNE(l, r, "tmpfne");
			} else {
				return builder.CreateICmpNE(l, r, "tmpne");
			}
		}
		case 260: { //and token
			if (l->getType() == Type::getDoubleTy(theContext)) {
				l = builder.CreateFPToSI(l, Type::getInt32Ty(theContext), "tmpconvert");
				l = builder.CreateICmpNE(l, ConstantInt::get(theContext, APInt(32, 0)), "tmpconvert");
			} else if (l->getType() == Type::getInt32Ty(theContext)) {
				l = builder.CreateICmpNE(l, ConstantInt::get(theContext, APInt(32, 0)), "tmpcmp");
			} else if (l->getType() != Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (r->getType() == Type::getDoubleTy(theContext)) {
				r = builder.CreateFPToSI(r, Type::getInt32Ty(theContext), "tmpconvert");
				r = builder.CreateICmpNE(r, ConstantInt::get(theContext, APInt(32, 0)), "tmpconvert");
			} else if (r->getType() == Type::getInt32Ty(theContext)) {
				r = builder.CreateICmpNE(r, ConstantInt::get(theContext, APInt(32, 0)), "tmpcmp");
			} else if (r->getType() != Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			return builder.CreateAnd(l, r, "tmpand");
		}
		case 261: { //or token
			if (l->getType() == Type::getDoubleTy(theContext)) {
				l = builder.CreateFPToSI(l, Type::getInt32Ty(theContext), "tmpconvert");
				l = builder.CreateICmpNE(l, ConstantInt::get(theContext, APInt(32, 0)), "tmpconvert");
			} else if (l->getType() == Type::getInt32Ty(theContext)) {
				l = builder.CreateICmpNE(l, ConstantInt::get(theContext, APInt(32, 0)), "tmpcmp");
			} else if (l->getType() != Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			if (r->getType() == Type::getDoubleTy(theContext)) {
				r = builder.CreateFPToSI(r, Type::getInt32Ty(theContext), "tmpconvert");
				r = builder.CreateICmpNE(r, ConstantInt::get(theContext, APInt(32, 0)), "tmpconvert");
			} else if (r->getType() == Type::getInt32Ty(theContext)) {
				r = builder.CreateICmpNE(r, ConstantInt::get(theContext, APInt(32, 0)), "tmpcmp");
			} else if (r->getType() != Type::getInt1Ty(theContext)) {
				yyerror("Incompatible types!");
			}
			return builder.CreateOr(l, r, "tmpor");
		}
		default:
			return nullptr;
	}
}

Value* DeclarationExprAST::codegen() const {
	AllocaInst* alloca = namedValues[_name];
	if (alloca != nullptr) {
		std::string msg = "Variable ";
		msg += _name;
		msg += " already declared!";
		yyerror(msg);
	}

	if (!_expr) {
		yyerror("No value for this declaration!");
	}

	Value* val = _expr->codegen();

	if (_type != val->getType()) {
		if (_type == Type::getDoubleTy(theContext) && val->getType() != Type::getInt1Ty(theContext)) {
			val = builder.CreateSIToFP(val, Type::getDoubleTy(theContext), "tmpconvert");
		} else if (_type == Type::getInt32Ty(theContext) && val->getType() != Type::getInt1Ty(theContext)) {
			val = builder.CreateFPToSI(val, Type::getInt32Ty(theContext), "tmpconvert");
		} else if (_type == Type::getInt1Ty(theContext) && val->getType() != Type::getDoubleTy(theContext)) {
			val->mutateType(Type::getInt1Ty(theContext));
		} else if (_type == Type::getInt1Ty(theContext) && val->getType() == Type::getDoubleTy(theContext)) {
			val = builder.CreateFPToSI(val, Type::getInt1Ty(theContext), "tmpconvert");
		} else {
			yyerror("Incompatible types!");
		}
	}
	alloca = builder.CreateAlloca(_type, 0, _name.c_str());
	namedValues[_name] = alloca;
	return builder.CreateStore(val, alloca);
}

void DeclarationExprAST::setType(Type* type) {
	_type = type;
	if (!_expr) {
		if (_type == Type::getDoubleTy(theContext)) {
			_expr = new NumberExprAST(0);
		} else if (_type == Type::getInt32Ty(theContext)) {
			_expr = new INumberExprAST(0);
		} else {
			_expr = new BoolExprAST(0);
		}
	}
}

Value* DeclarationArrayExprAST::codegen() const {
	Value* v;
	for (auto e : *_declarations) {
		v = e->codegen();
		if (!v) {
			return nullptr;
		}
	}

	return v;
}

Value* BlockExprAST::codegen() const {
	Value* v;
	for (auto e : *_commands) {
		v = e->codegen();
		if (!v) {
			return nullptr;
		}
	}

	return v;
}

Value* IfExprAST::codegen() const {
	Value *condV = _cond->codegen();
	if (!condV) {
		return nullptr;
	}

	Type* condT = condV->getType();
	if (condT != Type::getInt1Ty(theContext)) {
		if (condT == Type::getDoubleTy(theContext)) {
			condV = builder.CreateFPToSI(condV, Type::getInt1Ty(theContext), "tmpconvert");
			if (!condV) {
				return nullptr;
			}
		}
		else  if (condT != Type::getInt1Ty(theContext)){
			condV->mutateType(Type::getInt1Ty(theContext));
		}
	}

	Value* tmp = builder.CreateICmpNE(condV, ConstantInt::get(theContext, APInt(1, 0)), "ifcond");

	Function *theFunction = builder.GetInsertBlock()->getParent();

	BasicBlock *thenBB = BasicBlock::Create(theContext, "then", theFunction);
	BasicBlock *mergeBB = BasicBlock::Create(theContext, "ifcont");
	if (!_else_branch) {
		builder.CreateCondBr(tmp, thenBB, mergeBB);
		builder.SetInsertPoint(thenBB);
		Value *thenV = _then_branch->codegen();
		if (!thenV) {
			return nullptr;
		}
		builder.CreateBr(mergeBB);
		thenBB = builder.GetInsertBlock();

		theFunction->getBasicBlockList().push_back(mergeBB);

		builder.SetInsertPoint(mergeBB);

		return thenV;

	} else {
		BasicBlock* elseBB = BasicBlock::Create(theContext, "else");
		builder.CreateCondBr(tmp, thenBB, elseBB);
		builder.SetInsertPoint(thenBB);
		Value *thenV = _then_branch->codegen();
		if (!thenV) {
			return nullptr;
		}
		builder.CreateBr(mergeBB);
		thenBB = builder.GetInsertBlock();

		theFunction->getBasicBlockList().push_back(elseBB);
		builder.SetInsertPoint(elseBB);

		Value* elseV = _else_branch->codegen();
		if (!elseV) {
			return nullptr;
		}
		builder.CreateBr(mergeBB);
		elseBB = builder.GetInsertBlock();

		theFunction->getBasicBlockList().push_back(mergeBB);
		builder.SetInsertPoint(mergeBB);

		return thenV;;
	}
}

Value* ForExprAST::codegen() const {
	Function* theFunction = builder.GetInsertBlock()->getParent();
	Value* startV = _start->codegen();
	if (!startV) {
		return nullptr;
	}

	BasicBlock* loopBB = BasicBlock::Create(theContext, "loop", theFunction);
	builder.CreateBr(loopBB);
	builder.SetInsertPoint(loopBB);

	Value* condV = _cond->codegen();
	if (!condV) {
		return nullptr;
	}

	condV = builder.CreateICmpNE(condV, ConstantInt::get(theContext, APInt(1, 0)), "loopcond");
	
	BasicBlock* bodyBB = BasicBlock::Create(theContext, "loop_body");
	BasicBlock* afterLoopBB = BasicBlock::Create(theContext, "after_loop");
	builder.CreateCondBr(condV, bodyBB, afterLoopBB);

	theFunction->getBasicBlockList().push_back(bodyBB);
	builder.SetInsertPoint(bodyBB);

	if (!_body->codegen()) {
		return nullptr;
	}

	if (!_end->codegen()) {
		return nullptr;
	}

	builder.CreateBr(loopBB);

	theFunction->getBasicBlockList().push_back(afterLoopBB);
	builder.SetInsertPoint(afterLoopBB);

	return ConstantInt::get(theContext, APInt(1, 0));
}

Value* WhileExprAST::codegen() const {
	Function* theFunction = builder.GetInsertBlock()->getParent();

	BasicBlock* loopBB = BasicBlock::Create(theContext, "loop", theFunction);
	builder.CreateBr(loopBB);

	builder.SetInsertPoint(loopBB);

	Value* condV = _cond->codegen();
	if (!condV) {
		return nullptr;
	}

	condV = builder.CreateICmpNE(condV, ConstantInt::get(theContext, APInt(1, 0)), "loopcond");

	BasicBlock* bodyBB = BasicBlock::Create(theContext, "loop_body");
	BasicBlock* afterLoopBB = BasicBlock::Create(theContext, "after_loop");
	builder.CreateCondBr(condV, bodyBB, afterLoopBB);

	theFunction->getBasicBlockList().push_back(bodyBB);
	builder.SetInsertPoint(bodyBB);

	if (!_body->codegen()) {
		return nullptr;
	}

	builder.CreateBr(loopBB);

	theFunction->getBasicBlockList().push_back(afterLoopBB);
	builder.SetInsertPoint(afterLoopBB);

	return ConstantInt::get(theContext, APInt(1, 0));
}

Value* EmptyExprAST::codegen() const {
	return ConstantInt::get(theContext, APInt(1,0));
}

Function* FunctionAST::codegen() const {
	if (theModule->getFunction(_name)) {
		std::string msg = "Function with name ";
		msg += _name;
		msg += " already defined!";
		yyerror(msg);
	}
	std::vector<Type*> tmp;
	for (auto e : *_types) {
		tmp.push_back(e.second);
	}
	FunctionType* fp = FunctionType::get(_ret, tmp, false);
	Function *f = Function::Create(fp, Function::ExternalLinkage, _name, theModule.get());
	std::vector<std::string> tmp1;
	for (auto e : *_types) {
		tmp1.push_back(e.first);
	}
	int j = 0;
	for (auto &a : f->args()) {
		a.setName(tmp1[j++]);
	}
	BasicBlock *bb = BasicBlock::Create(theContext, "entry", f);

	builder.SetInsertPoint(bb);
	namedValues.clear();
	for (auto &a: f->args()) {
		AllocaInst* alloca = createEntryBlockAlloca(f, a.getName(), a.getType());
		namedValues[a.getName()] = alloca;
		builder.CreateStore(&a, alloca);
	}

	Value* ret = nullptr;
	for (auto e : *_definition) {
		ret = e->codegen();
	}

	if (ret) {
		Type* ret_type = f->getReturnType();
		if (ret_type == Type::getDoubleTy(theContext)) {
			builder.CreateRet(ConstantFP::get(theContext, APFloat(0.0)));
		} else if (ret_type == Type::getInt1Ty(theContext)) {
			builder.CreateRet(ConstantInt::get(theContext, APInt(1,0)));
		} else if (ret_type == Type::getInt32Ty(theContext)) {
			builder.CreateRet(ConstantInt::get(theContext, APInt(32,0)));
		} else {
			builder.CreateRetVoid();
		}

		verifyFunction(*f);
		namedValues.clear();
		return f;
	} else {
		f->eraseFromParent();
		namedValues.clear();
		return nullptr;
	}
}

void initializeModule() {
	theModule = make_unique<Module>("my module", theContext);
}

AllocaInst* createEntryBlockAlloca(Function* theFunction, const std::string& name, Type* type) {
  IRBuilder<> b(&theFunction->getEntryBlock(), theFunction->getEntryBlock().begin());
  return b.CreateAlloca(type, 0, name.c_str());
}
