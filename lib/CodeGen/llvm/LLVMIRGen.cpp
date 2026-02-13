
#include "mycc/CodeGen/llvm/LLVMIRGen.hpp"

using namespace mycc::codegen::llvmbackend;

LLVMIRGenerator::LLVMIRGenerator(llvm::LLVMContext &Ctx, llvm::StringRef ModuleName) :
    Ctx(Ctx),
    Module(std::make_unique<llvm::Module>(ModuleName, Ctx)),
    Builder(Ctx) {
}

void LLVMIRGenerator::generateIR(const Program &ASTProgram, const Scope &symbols) {
    generateGlobals(symbols);
}

void LLVMIRGenerator::print(llvm::raw_ostream &OS) const {
    Module->print(OS, nullptr);
}

void LLVMIRGenerator::generateGlobals(const Scope &symbols) {
    // Pass 1: declare all functions and global variables (signatures only)
    for (const auto &[Name, Decl] : symbols.getSymbols()) {
        if (Decl.isFunction()) {
            auto &Func = *std::get<FunctionDeclaration *>(Decl.decl);
            std::vector<llvm::Type*> ParamTypes(Func.getArgs().size(), llvm::Type::getInt32Ty(Ctx));
            auto *RetType = llvm::Type::getInt32Ty(Ctx);
            auto *FnTy = llvm::FunctionType::get(RetType, ParamTypes, false);
            auto *LLVMFunc = llvm::Function::Create(FnTy, getLinkage(Decl), Func.getName(), Module.get());
            unsigned i = 0;
            for (auto &Arg : LLVMFunc->args())
                Arg.setName(Func.getArgs()[i++]->getName());
        } else {
            generateGlobalVar(*std::get<VarDeclaration *>(Decl.decl), Decl);
        }
    }

    // Pass 2: generate function bodies
    for (const auto &[Name, Decl] : symbols.getSymbols()) {
        if (Decl.isFunction()) {
            generateFunction(*std::get<FunctionDeclaration *>(Decl.decl), Decl);
        }
    }
}

void LLVMIRGenerator::generateFunction(const FunctionDeclaration &Func, const SymbolEntry& Entry) {
    // If it is only a declaration, it was already created in pass 1
    if (Func.isDeclaration()) return;

    // Retrieve the function created in pass 1
    auto *LLVMFunc = Module->getFunction(Func.getName());

    // Create entry block and set insertion point
    auto *EntryBB = llvm::BasicBlock::Create(Ctx, "entry", LLVMFunc);
    Builder.SetInsertPoint(EntryBB);

    // Create allocas for parameters and store argument values
    NamedValues.clear();
    for (auto &Arg : LLVMFunc->args()) {
        auto *Alloca = createEntryBlockAlloca(LLVMFunc, Arg.getName(), Arg.getType());
        Builder.CreateStore(&Arg, Alloca);
        NamedValues[Arg.getName()] = Alloca;
    }

    for (const auto &Item : Func) {
        generateBlockItem(Item);
    }

    // If the last block has no terminator, add an implicit return
    if (!Builder.GetInsertBlock()->getTerminator()) {
        if (LLVMFunc->getReturnType()->isVoidTy())
            Builder.CreateRetVoid();
        else
            Builder.CreateRet(llvm::ConstantInt::get(LLVMFunc->getReturnType(), 0));
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

void LLVMIRGenerator::generateBlockItem(const BlockItem &Item) {
    if (std::holds_alternative<Statement*>(Item))
        generateStatement(*std::get<Statement*>(Item));
    else if (std::holds_alternative<VarDeclaration*>(Item))
        generateDeclaration(*std::get<VarDeclaration*>(Item));
}

void LLVMIRGenerator::generateStatement(const Statement &Stmt) {
    switch (Stmt.getKind()) {
        case Statement::SK_Return:
            generateReturnStmt(dynamic_cast<const ReturnStatement &>(Stmt));
            break;
        case Statement::SK_Expression: {
            auto &ExprStmt = dynamic_cast<const ExpressionStatement &>(Stmt);
            generateExpression(*ExprStmt.getExpr());
            break;
        }
        case Statement::SK_If:
            generateIfStmt(dynamic_cast<const IfStatement &>(Stmt));
            break;
        case Statement::SK_Compound:
            generateCompoundStmt(dynamic_cast<const CompoundStatement &>(Stmt));
            break;
        case Statement::SK_While:
            generateWhileStmt(dynamic_cast<const WhileStatement &>(Stmt));
            break;
        case Statement::SK_DoWhile:
            generateDoWhileStmt(dynamic_cast<const DoWhileStatement &>(Stmt));
            break;
        case Statement::SK_For:
            generateForStmt(dynamic_cast<const ForStatement &>(Stmt));
            break;
        case Statement::SK_Switch:
            generateSwitchStmt(dynamic_cast<const SwitchStatement &>(Stmt));
            break;
        case Statement::SK_Break:
            Builder.CreateBr(BreakBlocks.back());
            break;
        case Statement::SK_Continue:
            Builder.CreateBr(ContinueBlocks.back());
            break;
        case Statement::SK_Label: {
            auto &Label = dynamic_cast<const LabelStatement &>(Stmt);
            auto *Func = Builder.GetInsertBlock()->getParent();
            auto *LabelBB = llvm::BasicBlock::Create(Ctx, Label.getLabel(), Func);
            LabelBlocks[Label.getLabel()] = LabelBB;
            if (!Builder.GetInsertBlock()->getTerminator())
                Builder.CreateBr(LabelBB);
            Builder.SetInsertPoint(LabelBB);
            break;
        }
        case Statement::SK_Goto: {
            auto &Goto = dynamic_cast<const GotoStatement &>(Stmt);
            auto it = LabelBlocks.find(Goto.getLabel());
            if (it != LabelBlocks.end()) {
                Builder.CreateBr(it->second);
            } else {
                // Forward reference: create the block now, label will find it later
                auto *Func = Builder.GetInsertBlock()->getParent();
                auto *LabelBB = llvm::BasicBlock::Create(Ctx, Goto.getLabel(), Func);
                LabelBlocks[Goto.getLabel()] = LabelBB;
                Builder.CreateBr(LabelBB);
            }
            break;
        }
        case Statement::SK_Null:
            break;
        case Statement::SK_Case:
        case Statement::SK_Default:
            // Handled by generateSwitchStmt
            break;
    }
}

void LLVMIRGenerator::generateReturnStmt(const ReturnStatement &Stmt) {
    if (Stmt.getRetVal()) {
        auto * RetValue = generateExpression(*Stmt.getRetVal());
        Builder.CreateRet(RetValue);
    } else {
        Builder.CreateRetVoid();
    }
}

void LLVMIRGenerator::generateIfStmt(const IfStatement &Stmt) {
    auto *CondVal = generateExpression(*Stmt.getCondition());
    auto *Func = Builder.GetInsertBlock()->getParent();

    auto *ThenBB = llvm::BasicBlock::Create(Ctx, "if.then", Func);
    llvm::BasicBlock *ElseBB = Stmt.getElseSt() ? ElseBB = llvm::BasicBlock::Create(Ctx, "if.else", Func) : nullptr;
    auto *EndBB = llvm::BasicBlock::Create(Ctx, "if.end", Func);


    auto* Cond= Builder.CreateICmpNE(CondVal, llvm::ConstantInt::get(CondVal->getType(), 0));

    if (Stmt.getElseSt()) {
        Builder.CreateCondBr(Cond, ThenBB, ElseBB);
    }
    else {
        Builder.CreateCondBr(Cond, ThenBB, EndBB);
    }

    // Create Then body
    Builder.SetInsertPoint(ThenBB);
    generateStatement(*Stmt.getThenSt());
    if (!Builder.GetInsertBlock()->getTerminator())
        Builder.CreateBr(EndBB);

    // Create the Else body
    if (Stmt.getElseSt()) {
        Builder.SetInsertPoint(ElseBB);
        generateStatement(*Stmt.getElseSt());
        if (!Builder.GetInsertBlock()->getTerminator())
            Builder.CreateBr(EndBB);
    }

    Builder.SetInsertPoint(EndBB);
}

void LLVMIRGenerator::generateCompoundStmt(const CompoundStatement &Stmt) {
    for (const auto & S : Stmt) {
        if (std::holds_alternative<Statement*>(S)) {
            generateStatement(*std::get<Statement*>(S));
        } else if (std::holds_alternative<VarDeclaration*>(S)) {
            generateDeclaration(*std::get<VarDeclaration*>(S));
        }
    }
}

void LLVMIRGenerator::generateWhileStmt(const WhileStatement &Stmt) {
    auto * Func = Builder.GetInsertBlock()->getParent();

    auto *WhileHeader = llvm::BasicBlock::Create(Ctx, "while.header", Func);
    auto *WhileBody = llvm::BasicBlock::Create(Ctx, "while.body", Func);
    auto *EndBr = llvm::BasicBlock::Create(Ctx, "while.end", Func);

    ContinueBlocks.emplace_back(WhileHeader);
    BreakBlocks.emplace_back(EndBr);

    // Generate the header
    Builder.CreateBr(WhileHeader);
    Builder.SetInsertPoint(WhileHeader);
    auto * CondVal = generateExpression(*Stmt.getCondition());
    auto* Cond= Builder.CreateICmpNE(CondVal, llvm::ConstantInt::get(CondVal->getType(), 0));
    Builder.CreateCondBr(Cond, WhileBody, EndBr);

    // Generate the body
    Builder.SetInsertPoint(WhileBody);
    generateStatement(*Stmt.getBody());
    if (!Builder.GetInsertBlock()->getTerminator())
        Builder.CreateBr(WhileHeader);

    Builder.SetInsertPoint(EndBr);

    ContinueBlocks.pop_back();
    BreakBlocks.pop_back();
}

void LLVMIRGenerator::generateDoWhileStmt(const DoWhileStatement &Stmt) {
    auto * Func = Builder.GetInsertBlock()->getParent();

    auto * DoWhileBody = llvm::BasicBlock::Create(Ctx, "dowhile.body", Func);
    auto * DoWhileComparison = llvm::BasicBlock::Create(Ctx, "dowhile.comp", Func);
    auto * DoWhileEnd = llvm::BasicBlock::Create(Ctx, "dowhile.end", Func);

    ContinueBlocks.push_back(DoWhileComparison);
    BreakBlocks.push_back(DoWhileEnd);

    // Generate first the body
    Builder.CreateBr(DoWhileBody);
    Builder.SetInsertPoint(DoWhileBody);
    generateStatement(*Stmt.getBody());
    if (!Builder.GetInsertBlock()->getTerminator())
        Builder.CreateBr(DoWhileComparison);

    // Generate the comparison
    Builder.SetInsertPoint(DoWhileComparison);
    auto * CondVal = generateExpression(*Stmt.getCondition());
    auto * Cond = Builder.CreateICmpNE(CondVal, llvm::ConstantInt::get(CondVal->getType(), 0));
    Builder.CreateCondBr(Cond, DoWhileBody, DoWhileEnd);

    // Finally...
    Builder.SetInsertPoint(DoWhileEnd);

    ContinueBlocks.pop_back();
    BreakBlocks.pop_back();
}

void LLVMIRGenerator::generateForStmt(const ForStatement &Stmt) {
    auto * Func = Builder.GetInsertBlock()->getParent();

    auto * ForVarCheck = llvm::BasicBlock::Create(Ctx, "for.check", Func);
    auto * ForBody = llvm::BasicBlock::Create(Ctx, "for.body", Func);
    auto * ForPost = llvm::BasicBlock::Create(Ctx, "for.post", Func);
    auto * ForEnd = llvm::BasicBlock::Create(Ctx, "for.end", Func);

    ContinueBlocks.emplace_back(ForPost);
    BreakBlocks.emplace_back(ForEnd);

    // Generate the init clause (declaration, expression, or nothing)
    const auto &Init = Stmt.getInit();
    if (std::holds_alternative<VarDeclaration *>(Init))
        generateDeclaration(*std::get<VarDeclaration *>(Init));
    else if (std::holds_alternative<Expr *>(Init))
        generateExpression(*std::get<Expr *>(Init));

    Builder.CreateBr(ForVarCheck);

    // Generate the condition check
    Builder.SetInsertPoint(ForVarCheck);
    if (Stmt.getCondition()) {
        auto *CondVal = generateExpression(*Stmt.getCondition());
        auto *Cond = Builder.CreateICmpNE(CondVal, llvm::ConstantInt::get(CondVal->getType(), 0));
        Builder.CreateCondBr(Cond, ForBody, ForEnd);
    } else {
        // for(;;) — unconditional loop
        Builder.CreateBr(ForBody);
    }

    // Generate the body
    Builder.SetInsertPoint(ForBody);
    generateStatement(*Stmt.getBody());
    if (!Builder.GetInsertBlock()->getTerminator())
        Builder.CreateBr(ForPost);

    // Generate Post
    Builder.SetInsertPoint(ForPost);
    if (Stmt.getPost()) {
        generateExpression(*Stmt.getPost());
    }
    Builder.CreateBr(ForVarCheck);

    Builder.SetInsertPoint(ForEnd);

    ContinueBlocks.pop_back();
    BreakBlocks.pop_back();
}

void LLVMIRGenerator::generateSwitchStmt(const SwitchStatement &Stmt) {
    auto *CondVal = generateExpression(*Stmt.get_condition());
    auto *Func = Builder.GetInsertBlock()->getParent();

    auto *BreakBB = llvm::BasicBlock::Create(Ctx, "switch.end", Func);
    auto *DefaultBB = BreakBB; // default falls to break if no default: label

    auto *StmtBody = dynamic_cast<CompoundStatement *>(Stmt.get_body());
    const auto &Items = StmtBody->getBlock();

    BreakBlocks.emplace_back(BreakBB);

    // Structures to collect case info
    // we have a similar structure in our IR generator
    struct CaseInfo {
        llvm::ConstantInt *Value;
        llvm::BasicBlock *BB;
    };
    std::vector<CaseInfo> Cases;

    // Map from BlockItem index to the BasicBlock it starts
    std::unordered_map<size_t, llvm::BasicBlock *> BlockForIndex;

    // Pass 1: scan block items, create a BasicBlock for each case/default
    for (size_t i = 0; i < Items.size(); i++) {
        // We just care about statements
        if (!std::holds_alternative<Statement *>(Items[i]))
            continue;

        auto *S = std::get<Statement *>(Items[i]);

        // For Case statements
        if (S->getKind() == Statement::SK_Case) {
            auto *CS = dynamic_cast<CaseStatement *>(S);
            // The values in the Case statement can only be integer
            auto &CaseVal = dynamic_cast<const IntegerLiteral &>(*CS->getValue());
            // create a constant value
            auto *ConstVal = llvm::ConstantInt::get(Ctx, CaseVal.getValue());
            // create a case block
            auto *CaseBB = llvm::BasicBlock::Create(Ctx, "switch.case", Func);
            // add this information to the structures
            Cases.push_back({ConstVal, CaseBB});
            BlockForIndex[i] = CaseBB;
        } else if (S->getKind() == Statement::SK_Default) {
            // for default, just create a block
            DefaultBB = llvm::BasicBlock::Create(Ctx, "switch.default", Func);
            BlockForIndex[i] = DefaultBB;
        }
    }

    // Create the switch instruction
    auto *Switch = Builder.CreateSwitch(
        CondVal,                    // generated condition value
        DefaultBB,                  // default block (it can be null)
        Cases.size());     // Num of cases generated
    // Add the cases to the Switch instruction
    for (auto &CI : Cases) {
        // provides value and block
        Switch->addCase(CI.Value, CI.BB);
    }

    // Pass 2: emit the body, setting insert point when we hit case/default
    for (size_t i = 0; i < Items.size(); i++) {
        // If this index has a case/default block, transition to it
        // we just need to set the block as the new insertion point
        auto it = BlockForIndex.find(i);
        if (it != BlockForIndex.end()) {
            // If the previous block has no terminator, fall through
            if (!Builder.GetInsertBlock()->getTerminator())
                Builder.CreateBr(it->second);
            Builder.SetInsertPoint(it->second);
            continue; // the case/default statement itself is just a label
        }

        // Generate regular block items
        generateBlockItem(Items[i]);
    }

    // Close the last case block if it has no terminator (implicit fallthrough to break)
    if (!Builder.GetInsertBlock()->getTerminator())
        Builder.CreateBr(BreakBB);

    // Finally, set the insertion point to the BreakBB
    Builder.SetInsertPoint(BreakBB);

    BreakBlocks.pop_back();
}

void LLVMIRGenerator::generateDeclaration(const VarDeclaration &Decl) {
    std::optional<StorageClass> storageClass = Decl.getStorageClass();
    // A extern local just reference a global defined elsewhere
    if (storageClass.has_value() && *storageClass == StorageClass::SC_Extern) {
        // These NamedValues will fall through the Module->getGlobalVariable()
        // or it's already declared, nothing to allocate
        return;
    }

    // Static locals become global variables with internal linkage
    if (storageClass.has_value() && *storageClass == StorageClass::SC_Static) {
        auto *Ty = llvm::Type::getInt32Ty(Ctx);
        // Use the unique name (e.g. "func.x.1") to avoid collisions
        auto GlobalName = Decl.getUniqueName().value_or(Decl.getVar()->getName().str());

        // Static local initializers must be compile-time constants
        llvm::Constant *Init = llvm::ConstantInt::get(Ty, 0);
        if (Decl.getExpr()) {
            auto &InitVal = dynamic_cast<const IntegerLiteral &>(*Decl.getExpr());
            Init = llvm::ConstantInt::get(Ty, InitVal.getValue());
        }

        auto *GV = new llvm::GlobalVariable(
            *Module, Ty, false,
            llvm::GlobalValue::InternalLinkage,
            Init, GlobalName);
        // Register with the original variable name so getVarAddress finds it
        NamedValues[Decl.getVar()->getName()] = GV;
        return;
    }

    // allocate space for the variable
    auto *Func = Builder.GetInsertBlock()->getParent();
    auto *Alloca = createEntryBlockAlloca(Func, Decl.getVar()->getName(), llvm::Type::getInt32Ty(Ctx));
    NamedValues[Decl.getVar()->getName()] = Alloca;

    if (Decl.getExpr() != nullptr) {
        auto *Expr = generateExpression(*Decl.getExpr());
        Builder.CreateStore(Expr, Alloca);
    }
}

llvm::Value *LLVMIRGenerator::generateExpression(const Expr &E) {
    switch (E.getKind()) {
        case Expr::Ek_Int:
            return generateIntLiteral(dynamic_cast<const IntegerLiteral &>(E));
        case Expr::Ek_Var:
            return generateVarExpr(dynamic_cast<const Var &>(E));
        case Expr::Ek_UnaryOperator:
            return generateUnaryExpr(dynamic_cast<const UnaryOperator &>(E));
        case Expr::Ek_BinaryOperator:
            return generateBinaryExpr(dynamic_cast<const BinaryOperator &>(E));
        case Expr::Ek_AssignmentOperator:
            return generateAssignmentExpr(dynamic_cast<const AssignmentOperator &>(E));
        case Expr::Ek_PrefixOperator:
            return generatePrefixExpr(dynamic_cast<const PrefixOperator &>(E));
        case Expr::Ek_PostfixOperator:
            return generatePostfixExpr(dynamic_cast<const PostfixOperator &>(E));
        case Expr::Ek_ConditionalOperator:
            return generateConditionalExpr(dynamic_cast<const ConditionalExpr &>(E));
        case Expr::Ek_FunctionCallOperator:
            return generateFunctionCallExpr(dynamic_cast<const FunctionCallExpr &>(E));
    }
    llvm_unreachable("Unknown expression kind");
}

llvm::Value *LLVMIRGenerator::generateIntLiteral(const IntegerLiteral &Lit) const {
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), Lit.getValue().getSExtValue());
}

llvm::Value *LLVMIRGenerator::generateVarExpr(const Var &V) {
    // retrieve the address of that variable
    auto *Addr = getVarAddress(V);
    // load its value from the address
    return Builder.CreateLoad(llvm::Type::getInt32Ty(Ctx), Addr);
}

llvm::Value *LLVMIRGenerator::generateUnaryExpr(const UnaryOperator &Op) {
    auto *Expr = generateExpression(*Op.getExpr());

    llvm::Value* NewValue = nullptr;
    switch (Op.getOperatorKind()) {
        case UnaryOperator::UopK_Complement:
            NewValue = Builder.CreateNot(Expr);
            break;
        case UnaryOperator::UopK_Negate:
            NewValue = Builder.CreateNeg(Expr);
            break;
        case UnaryOperator::UopK_Not:
            auto *Cmp = Builder.CreateICmpEQ(Expr, llvm::ConstantInt::get(Expr->getType(), 0));
            NewValue = Builder.CreateZExt(Cmp, llvm::Type::getInt32Ty(Ctx));
            break;
    }

    return NewValue;
}

llvm::Value *LLVMIRGenerator::generateLogicalAnd(const BinaryOperator &Op) {
    auto *Func = Builder.GetInsertBlock()->getParent();
    auto *RhsBB = llvm::BasicBlock::Create(Ctx, "and.rhs", Func);
    auto *EndBB = llvm::BasicBlock::Create(Ctx, "and.end", Func);

    auto *LHS = generateExpression(*Op.getLeft());
    auto *LHSBool = Builder.CreateICmpNE(LHS, llvm::ConstantInt::get(LHS->getType(), 0));
    auto *LHSBlock = Builder.GetInsertBlock();
    Builder.CreateCondBr(LHSBool, RhsBB, EndBB);

    Builder.SetInsertPoint(RhsBB);
    auto *RHS = generateExpression(*Op.getRight());
    auto *RHSBool = Builder.CreateICmpNE(RHS, llvm::ConstantInt::get(RHS->getType(), 0));
    auto *RHSBlock = Builder.GetInsertBlock();
    Builder.CreateBr(EndBB);

    Builder.SetInsertPoint(EndBB);
    auto *Phi = Builder.CreatePHI(llvm::Type::getInt1Ty(Ctx), 2);
    Phi->addIncoming(llvm::ConstantInt::getFalse(Ctx), LHSBlock);
    Phi->addIncoming(RHSBool, RHSBlock);
    return Builder.CreateZExt(Phi, llvm::Type::getInt32Ty(Ctx));
}

llvm::Value *LLVMIRGenerator::generateLogicalOr(const BinaryOperator &Op) {
    auto *Func = Builder.GetInsertBlock()->getParent();
    auto *RhsBB = llvm::BasicBlock::Create(Ctx, "or.rhs", Func);
    auto *EndBB = llvm::BasicBlock::Create(Ctx, "or.end", Func);

    auto *LHS = generateExpression(*Op.getLeft());
    auto *LHSBool = Builder.CreateICmpNE(LHS, llvm::ConstantInt::get(LHS->getType(), 0));
    auto *LHSBlock = Builder.GetInsertBlock();
    Builder.CreateCondBr(LHSBool, EndBB, RhsBB);

    Builder.SetInsertPoint(RhsBB);
    auto *RHS = generateExpression(*Op.getRight());
    auto *RHSBool = Builder.CreateICmpNE(RHS, llvm::ConstantInt::get(RHS->getType(), 0));
    auto *RHSBlock = Builder.GetInsertBlock();
    Builder.CreateBr(EndBB);

    Builder.SetInsertPoint(EndBB);
    auto *Phi = Builder.CreatePHI(llvm::Type::getInt1Ty(Ctx), 2);
    Phi->addIncoming(llvm::ConstantInt::getTrue(Ctx), LHSBlock);
    Phi->addIncoming(RHSBool, RHSBlock);
    return Builder.CreateZExt(Phi, llvm::Type::getInt32Ty(Ctx));
}

llvm::Value *LLVMIRGenerator::generateBinaryExpr(const BinaryOperator &Op) {
    // Short-circuit operators need their own control flow
    if (Op.getOperatorKind() == BinaryOperator::Bok_And)
        return generateLogicalAnd(Op);
    if (Op.getOperatorKind() == BinaryOperator::Bok_Or)
        return generateLogicalOr(Op);

    auto *LHS = generateExpression(*Op.getLeft());
    auto *RHS = generateExpression(*Op.getRight());

    llvm::Value *NewValue = nullptr;
    switch (Op.getOperatorKind()) {
        case BinaryOperator::BoK_Add:
            NewValue = Builder.CreateAdd(LHS, RHS);
            break;
        case BinaryOperator::BoK_Subtract:
            NewValue = Builder.CreateSub(LHS, RHS);
            break;
        case BinaryOperator::BoK_Multiply:
            NewValue = Builder.CreateMul(LHS, RHS);
            break;
        case BinaryOperator::BoK_Divide:
            NewValue = Builder.CreateSDiv(LHS, RHS);
            break;
        case BinaryOperator::BoK_Remainder:
            NewValue = Builder.CreateSRem(LHS, RHS);
            break;
        case BinaryOperator::BoK_LeftShift:
            NewValue = Builder.CreateShl(LHS, RHS);
            break;
        case BinaryOperator::BoK_RightShift:
            NewValue = Builder.CreateAShr(LHS, RHS);
            break;
        case BinaryOperator::BoK_BitwiseAnd:
            NewValue = Builder.CreateAnd(LHS, RHS);
            break;
        case BinaryOperator::BoK_BitwiseXor:
            NewValue = Builder.CreateXor(LHS, RHS);
            break;
        case BinaryOperator::BoK_BitwiseOr:
            NewValue = Builder.CreateOr(LHS, RHS);
            break;
        case BinaryOperator::BoK_LowerThan:
            NewValue = Builder.CreateZExt(Builder.CreateICmpSLT(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        case BinaryOperator::BoK_LowerEqual:
            NewValue = Builder.CreateZExt(Builder.CreateICmpSLE(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        case BinaryOperator::BoK_GreaterThan:
            NewValue = Builder.CreateZExt(Builder.CreateICmpSGT(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        case BinaryOperator::BoK_GreaterEqual:
            NewValue = Builder.CreateZExt(Builder.CreateICmpSGE(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        case BinaryOperator::Bok_Equal:
            NewValue = Builder.CreateZExt(Builder.CreateICmpEQ(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        case BinaryOperator::Bok_NotEqual:
            NewValue = Builder.CreateZExt(Builder.CreateICmpNE(LHS, RHS), llvm::Type::getInt32Ty(Ctx));
            break;
        default:
            llvm_unreachable("Unexpected binary operator kind");
    }

    return NewValue;
}

llvm::Value *LLVMIRGenerator::generateAssignmentExpr(const AssignmentOperator &Op) {
    // retrieve the address of the variable
    // In the future a `generateLValueExpr` could be included to
    // obtain the address of an LValue
    auto *assignedVar = dynamic_cast<const Var*>(Op.getLeft());
    auto *Addr = getVarAddress(*assignedVar);
    auto *RHS = generateExpression(*Op.getRight());
    Builder.CreateStore(RHS, Addr);
    return RHS;
}

llvm::Value *LLVMIRGenerator::generatePrefixExpr(const PrefixOperator &Op) {
    // Get address of the variable
    auto *VarExpr = dynamic_cast<const Var*>(Op.getExpr());
    auto *Addr = getVarAddress(*VarExpr);

    auto *OldVal = Builder.CreateLoad(llvm::Type::getInt32Ty(Ctx), Addr);

    llvm::Value* NewValue;
    if (Op.getOperatorKind() == PrefixOperator::POK_PreIncrement) {
        // ++Var
        NewValue = Builder.CreateAdd(OldVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), 1));
    } else {
        // --Var
        NewValue = Builder.CreateSub(OldVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), 1));
    }
    Builder.CreateStore(NewValue, Addr);

    return NewValue;
}

llvm::Value *LLVMIRGenerator::generatePostfixExpr(const PostfixOperator &Op) {
    // Get address of the variable
    auto *VarExpr = dynamic_cast<const Var*>(Op.getExpr());
    auto *Addr = getVarAddress(*VarExpr);

    auto *OldVal = Builder.CreateLoad(llvm::Type::getInt32Ty(Ctx), Addr);

    llvm::Value* NewValue;
    if (Op.getOperatorKind() == PostfixOperator::POK_PostIncrement) {
        // Var++
        NewValue = Builder.CreateAdd(OldVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), 1));
    } else {
        // Var--
        NewValue = Builder.CreateSub(OldVal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(Ctx), 1));
    }

    // Store the new value to the variable
    Builder.CreateStore(NewValue, Addr);

    return OldVal;
}

llvm::Value *LLVMIRGenerator::generateConditionalExpr(const ConditionalExpr &Op) {
    // Generate the expression that represents the condition
    auto *Cond = generateExpression(*Op.getCondition());
    // We need it as a boolean expression
    auto *CondBool = Builder.CreateICmpNE(Cond, llvm::ConstantInt::get(Cond->getType(), 0));

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
        return generateBuiltInType(dynamic_cast<const BuiltinType*>(T), Ctx);
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

llvm::Value *LLVMIRGenerator::getVarAddress(const Var& V) {
    return NamedValues[V.getName()];
}
