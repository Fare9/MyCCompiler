
#include "mycc/CodeGen/llvm/LLVMIRGen.hpp"

using namespace mycc::codegen::llvmbackend;

/**
 *
 * @param Ctx
 * @param ModuleName
 */
LLVMIRGenerator::LLVMIRGenerator(llvm::LLVMContext &Ctx, llvm::StringRef ModuleName) :
    Ctx(Ctx),
    Module(std::make_unique<llvm::Module>(ModuleName, Ctx)),
    Builder(Ctx) {
}


void LLVMIRGenerator::generateGlobals(const Scope &symbols) {
    for (const auto &[Name, Decl] : symbols.getSymbols()) {
        if (Decl.isFunction()) {
            generateFunction(reinterpret_cast<const FunctionDeclaration&>(Decl.decl), Decl);
        } else {
            generateGlobalVar(reinterpret_cast<const VarDeclaration&>(Decl.decl), Decl);
        }
    }
}

void LLVMIRGenerator::generateFunction(const FunctionDeclaration &Func, const SymbolEntry& Entry) {
    // We first need to build the FunctionType, for that we will
    // need the return type, and the parameter types.
    // As we are now using only `int` we can assign the types directly.
    std::vector<llvm::Type*> ParamTypes(Func.getArgs().size(), llvm::Type::getInt32Ty(Ctx));
    auto *RetType = llvm::Type::getInt32Ty(Ctx);
    auto *FnTy = llvm::FunctionType::get(RetType, ParamTypes, false);

    // Now create the function in the module
    auto *LLVMFunc = llvm::Function::Create(FnTy, // Type of the function
        getLinkage(Entry), // linkage for that function
        Func.getName(),
        Module.get());
    // Name the parameters using the names from the AST
    unsigned i = 0;
    const auto& FuncArgs = Func.getArgs();
    for (auto &Arg : LLVMFunc->args()) {
        Arg.setName(FuncArgs[i]->getName());
        i++;
    }

    // If it is only a declaration we stop here
    if (Func.isDeclaration()) return;

    for (const auto &Item : Func) {
        generateBlockItem(Item);
    }
}

void LLVMIRGenerator::generateGlobalVar(const VarDeclaration &Var, const SymbolEntry& Entry) const {
    // As it is a global declaration, we can retrieve the
    // StaticAttr
    auto *StaticAttr = Entry.getStaticAttr();
    // We take the linkage type from LLVM (using the one from the Symbol)
    auto linkage = getLinkage(Entry);
    // We now only support `int` but we will expand it with a type
    // in the variable
    auto *Ty = llvm::Type::getInt32Ty(Ctx);

    // We need an optional constant value. It will be 0 if the
    // initial value is tentative, or we can have another value
    llvm::Constant * Init = nullptr;
    if (StaticAttr->init == InitialValue::Tentative) {
        Init = llvm::ConstantInt::get(Ty, 0);
    } else if (StaticAttr->init == InitialValue::Initial) {
        Init = llvm::ConstantInt::get(Ty, StaticAttr->value.value());
    }
    // If it is external, no value is given, Init is kept null

    // Create the GlobalVariable, the constructor taking Module as
    // the first parameter adds the global variable to the Module.
    new llvm::GlobalVariable(
        *Module,
        Ty,
        false,      // isConstant?
        linkage,
        Init,
        Var.getVar()->getName()
        );
}

llvm::Value *LLVMIRGenerator::generateConditionalExpr(const ConditionalExpr &Op) {
    // Generate the expression that represents the condition
    auto *Cond = generateExpression(*Op.getCondition());
    // We need it as a boolean expression
    auto *CondBool = Builder.CreateICmpEQ(Cond, llvm::ConstantInt::get(Cond->getType(), 0));

    // We need to create blocks to represent each part of the conditional
    auto *Func = Builder.GetInsertBlock()->getParent(); // Function where blocks will be
    auto *ThenBB = llvm::BasicBlock::Create(Ctx, "cond.then", Func);
    auto *ElseBB = llvm::BasicBlock::Create(Ctx, "cond.else", Func);
    auto *MergeBB = llvm::BasicBlock::Create(Ctx, "cond.end", Func);

    Builder.CreateCondBr(CondBool, ThenBB, ElseBB);

    // Then Code
    Builder.SetInsertPoint(ThenBB);
    auto *ThenVal = generateExpression(*Op.getLeft());
    Builder.CreateBr(MergeBB);
    ThenBB = Builder.GetInsertBlock(); // update in case the generateExpression added its own blocks

    // Else
    Builder.SetInsertPoint(ElseBB);
    auto *ElseVal = generateExpression(*Op.getRight());
    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock();

    // Merge with Phi both values
    Builder.SetInsertPoint(MergeBB);
    auto *Phi = Builder.CreatePHI(ThenVal->getType(), 2);
    Phi->addIncoming(ThenVal, ThenBB);
    Phi->addIncoming(ElseVal, ElseBB);

    return Phi;
}

llvm::Value *LLVMIRGenerator::generateFunctionCallExpr(const FunctionCallExpr &Call) {
    // Retrieve the function from the module
    auto *Callee = Module->getFunction(Call.getIdentifier());
    // Generate the arguments for the call
    std::vector<llvm::Value*> Args;
    for (const auto & Arg : Call.getArgs()) {
        Args.emplace_back(generateExpression(*Arg));
    }
    // Return the function
    return Builder.CreateCall(Callee, Args);
}


llvm::Type *generateBuiltInType(const mycc::BuiltinType* T, llvm::LLVMContext& Ctx) {
    if (T->getBuiltinKind() == mycc::BuiltinType::Int)
        return llvm::Type::getInt32Ty(Ctx);
    if (T->getBuiltinKind() == mycc::BuiltinType::Void)
        return llvm::Type::getVoidTy(Ctx);
    return nullptr;
}

llvm::Type *LLVMIRGenerator::getLLVMType(const Type *T) const {
    if (T->getKind() == Type::TK_Builtin)
        return generateBuiltInType(reinterpret_cast<const mycc::BuiltinType*>(T), Ctx);
    return nullptr;
}

llvm::GlobalValue::LinkageTypes LLVMIRGenerator::getLinkage(const SymbolEntry &Entry) {
    return Entry.isGlobal() ?
        llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::InternalLinkage;
}

llvm::AllocaInst *LLVMIRGenerator::createEntryBlockAlloca(llvm::Function *F,
                                             const llvm::StringRef Name,
                                             llvm::Type *Ty) {
    llvm::IRBuilder<> TmpBuilder(&F->getEntryBlock(), F->getEntryBlock().begin());
    return TmpBuilder.CreateAlloca(Ty, nullptr, Name);
}
