#include "mycc/CodeGen/IRGen.hpp"

#include <set>

#include "llvm/Support/Casting.h"

using namespace mycc;
using namespace mycc::codegen;

void IRGenerator::generateIR(const Program &ASTProgram, const Scope& symbols) {
    // Store symbol table for lookups during expression generation
    Symbols = &symbols;

    // Convert each AST function to IR function
    for (const auto& decl : ASTProgram) {
        if (const auto *ASTFunc = std::get_if<FunctionDeclaration *>(&decl)) {
            ir::Function *IRFunc = generateFunction(**ASTFunc);
            IRProg.add_function(IRFunc);
        }
    }

    // After processing the AST, convert symbol table entries to static variables
    // Important: Process AST first, then symbol table (matters in Chapter 16+)
    convertSymbolsToTacky(symbols);
}

void IRGenerator::convertSymbolsToTacky(const Scope& symbols) {
    std::set<std::string> StaticVars;

    for (const auto& [name, entry] : symbols.getSymbols()) {
        // Skip if not a static variable (functions and local vars are skipped)
        if (!entry.isStaticAttr()) {
            continue;
        }

        const StaticAttr* staticAttr = entry.getStaticAttr();

        // Skip if NoInitializer (extern declaration, defined elsewhere)
        if (staticAttr->init == InitialValue::NoInitializer) {
            continue;
        }

        // Determine initial value
        int initValue = 0;
        if (staticAttr->init == InitialValue::Initial && staticAttr->value.has_value()) {
            initValue = static_cast<int>(staticAttr->value.value());
        }
        // Tentative definitions get initialized to 0 (already default)

        // Create the StaticVariable and add to program (avoid duplicates)
        if (StaticVars.insert(name.str()).second) {
            auto* staticVar = new ir::StaticVariable(name, staticAttr->global, initValue);
            IRProg.add_static_variable(staticVar);
        }
    }
}

ir::Function *IRGenerator::generateFunction(const FunctionDeclaration &ASTFunc) {
    // Set current function name for static local name generation
    CurrentFunctionName = ASTFunc.getName().str();

    // Create IR function arguments first
    ir::Args args;

    // Track function parameters and variables declared at function level
    std::vector<std::string> funcDeclaredVars;

    // Handle function parameters - rename them and create ParameterOp objects
    auto *nonConstFunc = const_cast<FunctionDeclaration*>(&ASTFunc);
    for (size_t i = 0, e = nonConstFunc->getArgs().size(); i < e; i++) {
        Var *param = nonConstFunc->getArg(i);
        if (!param) break;

        StringRef paramName = param->getName();
        std::string uniqueName = generateUniqueVarName(paramName);

        // Create ParameterOp with the unique name
        auto *paramOp = new ir::ParameterOp(uniqueName);
        args.push_back(paramOp);

        // Push to rename stack so references to this parameter use the unique name
        VariableRenameStack[paramName].push_back(uniqueName);
        funcDeclaredVars.push_back(paramName.str());
    }

    // Now create the IR function with the parameters
    ir::InstList instructions;
    auto *IRFunc = new ir::Function(instructions, ASTFunc.getName(), args, ASTFunc.isGlobal());

    bool containsReturn = false;

    if (ASTFunc.hasBody()) {
        for (const BlockItem &Item: ASTFunc) {
            if (std::holds_alternative<VarDeclaration *>(Item)) {
                VarDeclaration *Decl = std::get<VarDeclaration *>(Item);
                funcDeclaredVars.push_back(Decl->getVar()->getName().str());
            }
            containsReturn |= generateBlockItem(Item, IRFunc);
        }

        if (!containsReturn) {
            ir::Int *RetVal = Ctx.createInt(llvm::APSInt::get(0));
            ir::Ret *RetInst = Ctx.createRet(RetVal);
            IRFunc->add_instruction(RetInst);
        }
    }

    exitScope(funcDeclaredVars);

    return IRFunc;
}


bool IRGenerator::generateBlockItem(const BlockItem &Item, ir::Function *IRFunc) {
    if (std::holds_alternative<Statement *>(Item)) {
        Statement *Stmt = std::get<Statement *>(Item);
        generateStatement(*Stmt, IRFunc);
        if (Stmt->getKind() == Statement::SK_Return)
            return true;
    } else if (std::holds_alternative<VarDeclaration *>(Item)) {
        VarDeclaration *Decl = std::get<VarDeclaration *>(Item);
        generateDeclaration(*Decl, IRFunc);
    }
    return false;
}

void IRGenerator::generateStatement(const Statement &Stmt, ir::Function *IRFunc) {
    switch (Stmt.getKind()) {
        case Statement::SK_Return: {
            generateReturnStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Expression: {
            const auto &ExprStmt = dynamic_cast<const ExpressionStatement &>(Stmt);
            // Generate the expression
            generateExpression(*ExprStmt.getExpr(), IRFunc);
            break;
        }
        case Statement::SK_If: {
            generateIfStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Label: {
            generateLabelStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Goto: {
            generateGotoStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Null:
            break;
        case Statement::SK_Compound: {
            generateCompoundStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Break: {
            const auto &BreakStmt = dynamic_cast<const BreakStatement &>(Stmt);
            auto *label =
                    Ctx.getOrCreateLabel(std::string(BreakStmt.get_label()), true);
            IRFunc->add_instruction(Ctx.createJump(label));
            break;
        }
        case Statement::SK_Continue: {
            const auto &ContinueStmt = dynamic_cast<const ContinueStatement &>(Stmt);
            auto *label =
                    Ctx.getOrCreateLabel(std::string(ContinueStmt.get_label()), true);
            IRFunc->add_instruction(Ctx.createJump(label));
            break;
        }
        case Statement::SK_While: {
            generateWhileStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_DoWhile: {
            generateDoWhileStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_For: {
            generateForStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Switch: {
            generateSwitchStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Case: {
            generateCaseStmt(Stmt, IRFunc);
            break;
        }
        case Statement::SK_Default: {
            generateDefaultStmt(Stmt, IRFunc);
            break;
        }
    }
}

void IRGenerator::generateDeclaration(const VarDeclaration &Decl, ir::Function *IRFunc) {
    const auto *left = Decl.getVar();
    StringRef originalName = left->getName();

    // For extern declarations at block scope, map to the global variable name
    // (no unique name, no initialization - it refers to a global defined elsewhere)
    if (Decl.getStorageClass().has_value() &&
        Decl.getStorageClass().value() == StorageClass::SC_Extern) {
        // Push the original name (global variable) onto the rename stack
        VariableRenameStack[originalName].push_back(originalName.str());
        // Track this as an extern variable (refers to global defined elsewhere)
        ExternVariables.insert(originalName.str());
        return;
    }

    // For static local declarations, use the unique name stored in the declaration
    // Static locals are initialized at compile time (in data segment), not at runtime
    if (Decl.getStorageClass().has_value() &&
        Decl.getStorageClass().value() == StorageClass::SC_Static) {
        if (Decl.getUniqueName().has_value()) {
            // Push the global unique name onto the rename stack
            VariableRenameStack[originalName].push_back(Decl.getUniqueName().value());
        }
        // Don't generate copy instruction - initialization is in data segment
        return;
    }

    // Generate unique name for this variable
    std::string uniqueName = generateUniqueVarName(originalName);

    // Push the unique name onto the rename stack
    VariableRenameStack[originalName].push_back(uniqueName);

    // If there's an initializer expression, generate the assignment
    if (Decl.getExpr() != nullptr) {
        const auto *right = Decl.getExpr();

        // First emit the right part of the assignment
        auto *result = generateExpression(*right, IRFunc);

        // Create IR variable with the unique name
        auto *varop = Ctx.getOrCreateVar(uniqueName);
        IRFunc->add_instruction(Ctx.createCopy(result, varop));
    }
}

void IRGenerator::generateReturnStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &RetStmt = dynamic_cast<const ReturnStatement &>(Stmt);

    // Generate IR for the return value expression
    ir::Value *RetVal = generateExpression(*RetStmt.getRetVal(), IRFunc);

    // Create return instruction
    ir::Ret *RetInst = Ctx.createRet(RetVal);
    IRFunc->add_instruction(RetInst);
}

void IRGenerator::generateIfStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &IfStmt = dynamic_cast<const IfStatement &>(Stmt);
    // generate conditional code
    auto result = generateExpression(*IfStmt.getCondition(), IRFunc);
    // generate labels
    auto *else_label = Ctx.getOrCreateLabel("else_label");
    auto *end_label = Ctx.getOrCreateLabel("end_label");
    if (IfStmt.getElseSt() != nullptr)
        IRFunc->add_instruction(Ctx.createJZ(result, else_label));
    else
        IRFunc->add_instruction(Ctx.createJZ(result, end_label));
    // generate the code of `then`
    generateStatement(*IfStmt.getThenSt(), IRFunc);
    if (IfStmt.getElseSt() != nullptr) {
        // generate a JUMP to END label from the IF statement
        IRFunc->add_instruction(Ctx.createJump(end_label));
        // Now generate else
        IRFunc->add_instruction(else_label);
        generateStatement(*IfStmt.getElseSt(), IRFunc);
    }
    // Now we generate the END label
    IRFunc->add_instruction(end_label);
}

void IRGenerator::generateLabelStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &Label = dynamic_cast<const LabelStatement &>(Stmt);
    const auto ILabel = Ctx.getOrCreateLabel(Label.getLabel().str(), true);
    IRFunc->add_instruction(ILabel);
}

void IRGenerator::generateGotoStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &Goto = dynamic_cast<const GotoStatement &>(Stmt);
    const auto ILabel = Ctx.getOrCreateLabel(Goto.getLabel().str(), true);
    IRFunc->add_instruction(Ctx.createJump(ILabel));
}

void IRGenerator::generateCompoundStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &Compound = dynamic_cast<const CompoundStatement &>(Stmt);

    // Track variables declared in this compound statement
    std::vector<std::string> declaredVars;

    // Process each block item
    for (const BlockItem &Item: Compound) {
        // If it's a variable declaration, track it
        if (std::holds_alternative<VarDeclaration *>(Item)) {
            VarDeclaration *Decl = std::get<VarDeclaration *>(Item);
            declaredVars.push_back(Decl->getVar()->getName().str());
        }
        generateBlockItem(Item, IRFunc);
    }

    // Exit scope - pop all variables declared in this compound statement
    exitScope(declaredVars);
}

void IRGenerator::generateWhileStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &While = dynamic_cast<const WhileStatement &>(Stmt);

    std::string while_start = std::string(While.get_label()) + "_start";
    std::string while_continue = std::string(While.get_label()) + "_continue";
    std::string while_end = std::string(While.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(while_start, true);
    auto label_continue = Ctx.getOrCreateLabel(while_continue, true);
    auto label_end = Ctx.getOrCreateLabel(while_end, true);

    // Place start label
    IRFunc->add_instruction(label_start);
    IRFunc->add_instruction(label_continue);

    // Evaluate condition
    auto result = generateExpression(*While.getCondition(), IRFunc);

    // Jump to end if condition is false (zero)
    IRFunc->add_instruction(Ctx.createJZ(result, label_end));

    // Generate loop body
    generateStatement(*While.getBody(), IRFunc);

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);
}

void IRGenerator::generateDoWhileStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &DoWhile = dynamic_cast<const DoWhileStatement &>(Stmt);

    std::string do_while_start = std::string(DoWhile.get_label()) + "_start";
    std::string do_while_continue = std::string(DoWhile.get_label()) + "_continue";
    std::string do_while_end = std::string(DoWhile.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(do_while_start, true);
    auto label_continue = Ctx.getOrCreateLabel(do_while_continue, true);
    auto label_end = Ctx.getOrCreateLabel(do_while_end, true);

    // Place label start
    IRFunc->add_instruction(label_start);

    // Generate the body of the do/while
    generateStatement(*DoWhile.getBody(), IRFunc);

    // In opposite to While, the label continue must jump to the expression check
    IRFunc->add_instruction(label_continue);

    // Now generate the expression
    auto result = generateExpression(*DoWhile.getCondition(), IRFunc);

    // Jump to end if condition is false (zero)
    IRFunc->add_instruction(Ctx.createJZ(result, label_end));

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);
}

void IRGenerator::generateForStmt(const Statement &Stmt, ir::Function *IRFunc) {
    const auto &For = dynamic_cast<const ForStatement &>(Stmt);

    std::string for_label_start = std::string(For.get_label()) + "_start";
    std::string for_label_continue = std::string(For.get_label()) + "_continue";
    std::string for_end = std::string(For.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(for_label_start, true);
    auto label_continue = Ctx.getOrCreateLabel(for_label_continue, true);
    auto label_end = Ctx.getOrCreateLabel(for_end, true);

    // Track variable declared in for-loop init (if any)
    std::vector<std::string> forDeclaredVars;

    // Now we generate a declaration or an expression
    if (std::holds_alternative<VarDeclaration *>(For.getInit())) {
        auto *decl = std::get<VarDeclaration *>(For.getInit());
        forDeclaredVars.push_back(decl->getVar()->getName().str());
        generateDeclaration(*decl, IRFunc);
    } else if (std::holds_alternative<Expr *>(For.getInit())) {
        auto *expr = std::get<Expr *>(For.getInit());
        generateExpression(*expr, IRFunc);
    }

    // Place start label
    IRFunc->add_instruction(label_start);

    // Evaluate condition
    if (For.getCondition() != nullptr) {
        auto result = generateExpression(*For.getCondition(), IRFunc);

        // Jump to end if condition is false (zero)
        IRFunc->add_instruction(Ctx.createJZ(result, label_end));
    }
    // Generate loop body
    generateStatement(*For.getBody(), IRFunc);

    // Different to While and DoWhile, in For we need
    // to set the label_continue, right before the
    // post-processing in the update of the variables.
    IRFunc->add_instruction(label_continue);

    // Generate update of the variables
    if (For.getPost() != nullptr) {
        generateExpression(*For.getPost(), IRFunc);
    }

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);

    // Exit scope for any variable declared in for-loop init
    exitScope(forDeclaredVars);
}

void IRGenerator::generateSwitchStmt(const Statement& Stmt, ir::Function* IRFunc) {
    const auto & Switch = dynamic_cast<const SwitchStatement&>(Stmt);

    // Collect all cases and the default value
    std::vector<CaseInfo> cases;
    std::string defaultLabel;
    bool hasDefault = false;
    collectSwitchCases(Switch.get_body(), cases, defaultLabel, hasDefault);

    // Get break label for switch statement
    std::string switch_end = std::string(Switch.get_break_label());
    auto* label_end = Ctx.getOrCreateLabel(switch_end, true);

    // Evaluate the switch condition once
    auto * switchValue = generateExpression(*Switch.get_condition(), IRFunc);

    // Now generate the comparisons for each case
    // also generate the jump, so we later just need
    // to generate the labels
    for (const auto & caseInfo : cases) {
        // Evaluate the case constant
        auto * caseValue = generateExpression(*caseInfo.value, IRFunc);

        // Compare the result using the case from the switch, and the case
        auto * cmp = Ctx.createICmpOp(
            dynamic_cast<ir::Operand*>(switchValue),
            dynamic_cast<ir::Operand*>(caseValue),
            ir::ICmpOp::eq);
        // add the comparison instruction
        IRFunc->add_instruction(cmp);

        // Jump to the case label if equal
        auto* caseLabel = Ctx.getOrCreateLabel(caseInfo.label, true);
        IRFunc->add_instruction(Ctx.createJNZ(cmp->getDestination(), caseLabel));
    }

    // no case matched, jump to default or end
    if (hasDefault) {
        auto * defLabel = Ctx.getOrCreateLabel(defaultLabel, true);
        IRFunc->add_instruction(Ctx.createJump(defLabel));
    } else {
        IRFunc->add_instruction(Ctx.createJump(label_end));
    }

    // generate the body of the switch, it contains the case/default labels
    // and also the statements, case/default only generates the labels
    generateStatement(*Switch.get_body(), IRFunc);

    // End label
    IRFunc->add_instruction(label_end);
}

void IRGenerator::generateCaseStmt(const Statement &Stmt, ir::Function *IRFunc) const {
    auto &caseStmt = dynamic_cast<const CaseStatement &>(Stmt);
    std::string caseLabel = std::string(caseStmt.get_label());
    IRFunc->add_instruction(Ctx.getOrCreateLabel(caseLabel, true));
}

void IRGenerator::generateDefaultStmt(const Statement &Stmt, ir::Function *IRFunc) const {
    auto &defaultStmt = dynamic_cast<const DefaultStatement &>(Stmt);
    std::string defaultLabel = std::string(defaultStmt.get_label());
    IRFunc->add_instruction(Ctx.getOrCreateLabel(defaultLabel, true));
}

ir::Value *IRGenerator::generateExpression(const Expr &Expr, ir::Function *IRFunc) {
    switch (Expr.getKind()) {
        case Expr::Ek_Var:
            return generateVarExpression(dynamic_cast<const Var &>(Expr), IRFunc);
        case Expr::Ek_AssignmentOperator:
            return generateAssignmentExpression(dynamic_cast<const AssignmentOperator &>(Expr), IRFunc);
        case Expr::Ek_Int:
            return generateIntExpression(dynamic_cast<const IntegerLiteral &>(Expr), IRFunc);
        case Expr::Ek_UnaryOperator:
            return generateUnaryExpression(dynamic_cast<const UnaryOperator &>(Expr), IRFunc);
        case Expr::Ek_BinaryOperator:
            return generateBinaryExpression(dynamic_cast<const BinaryOperator &>(Expr), IRFunc);
        case Expr::Ek_PrefixOperator:
            return generatePrefixExpression(dynamic_cast<const PrefixOperator &>(Expr), IRFunc);
        case Expr::Ek_PostfixOperator:
            return generatePostfixExpression(dynamic_cast<const PostfixOperator &>(Expr), IRFunc);
        case Expr::Ek_ConditionalOperator:
            return generateConditionalExpression(dynamic_cast<const ConditionalExpr &>(Expr), IRFunc);
        case Expr::Ek_FunctionCallOperator:
            return generateFunctionCallExpression(dynamic_cast<const FunctionCallExpr &>(Expr), IRFunc);
    }

    return nullptr; // Should not reach here
}

ir::Value *IRGenerator::generateVarExpression(const Var &var, ir::Function *IRFunc) {
    StringRef originalName = var.getName();

    // First check the rename stack to get the current binding for this name
    std::string irName = getIRName(originalName);

    // If the IR name equals the original name, it might be a global/static variable
    // (either because there's no local, or because an extern declaration is in scope)
    if (irName == originalName.str()) {
        // Check if it's a known extern variable (defined in another translation unit)
        if (ExternVariables.count(irName)) {
            return Ctx.getOrCreateStaticVar(originalName);
        }

        // Check for file-scope static variable (stored with original name)
        if (Symbols) {
            if (const SymbolEntry* entry = Symbols->lookupEntry(originalName)) {
                if (entry->isStaticAttr()) {
                    // File-scope static - use original name
                    return Ctx.getOrCreateStaticVar(originalName);
                }
            }
        }
    }

    // Check if irName is a static local variable (from rename stack)
    // Static locals have unique names like "functionName.varName" or "functionName.varName.1"
    if (Symbols) {
        if (const SymbolEntry* entry = Symbols->lookupEntry(irName)) {
            if (entry->isStaticAttr()) {
                // Static local - use the unique name from rename stack
                return Ctx.getOrCreateStaticVar(irName);
            }
        }
    }

    // Local variable - use renamed name from the stack
    return Ctx.getOrCreateVar(irName);
}

ir::Value *IRGenerator::generateAssignmentExpression(const AssignmentOperator &assignment, ir::Function *IRFunc) {
    const auto *left = assignment.getLeft();
    const auto *right = assignment.getRight();
    // First we emit the right part of the assignment
    auto *result = generateExpression(*right, IRFunc);
    // Now we create a copy that we include in functions
    auto *varop = generateExpression(*left, IRFunc);
    IRFunc->add_instruction(Ctx.createCopy(result, varop));
    return varop;
}

ir::Value *IRGenerator::generateIntExpression(const IntegerLiteral &IntLit, ir::Function *IRFunc) const {
    // Create integer constant in IR
    return Ctx.createInt(IntLit.getValue());
}

ir::Value *IRGenerator::generateUnaryExpression(const UnaryOperator &UnaryOp, ir::Function *IRFunc) {
    ir::UnaryOp::UnaryOpKind Kind;
    switch (UnaryOp.getOperatorKind()) {
        case UnaryOperator::UnaryOperatorKind::UopK_Negate:
            Kind = ir::UnaryOp::UnaryOpKind::Neg;
            break;
        case UnaryOperator::UnaryOperatorKind::UopK_Complement:
            Kind = ir::UnaryOp::UnaryOpKind::Complement;
            break;
        case UnaryOperator::UnaryOperatorKind::UopK_Not:
            Kind = ir::UnaryOp::UnaryOpKind::Not;
            break;
    }
    ir::Value *source = generateExpression(*UnaryOp.getExpr(), IRFunc);
    ir::UnaryOp *UnOp = Ctx.createUnaryOp(dynamic_cast<ir::Operand *>(source), Kind);
    if (IRFunc != nullptr)
        IRFunc->add_instruction(UnOp);
    return UnOp->getDestination();
}

ir::Value *IRGenerator::generateBinaryExpression(const BinaryOperator &BinaryOp, ir::Function *IRFunc) {
    ir::BinaryOp::BinaryOpKind Kind = ir::BinaryOp::BinaryOpKind::none;
    ir::ICmpOp::CmpOpKind CmpKind = ir::ICmpOp::CmpOpKind::none;

    switch (BinaryOp.getOperatorKind()) {
        case BinaryOperator::BoK_Add:
            Kind = ir::BinaryOp::BinaryOpKind::Add;
            break;
        case BinaryOperator::BoK_Subtract:
            Kind = ir::BinaryOp::BinaryOpKind::Sub;
            break;
        case BinaryOperator::BoK_Multiply:
            Kind = ir::BinaryOp::BinaryOpKind::Mul;
            break;
        case BinaryOperator::BoK_Divide:
            Kind = ir::BinaryOp::BinaryOpKind::Div;
            break;
        case BinaryOperator::BoK_Remainder:
            Kind = ir::BinaryOp::BinaryOpKind::Rem;
            break;
        case BinaryOperator::BoK_BitwiseAnd:
            Kind = ir::BinaryOp::BinaryOpKind::And;
            break;
        case BinaryOperator::BoK_BitwiseOr:
            Kind = ir::BinaryOp::BinaryOpKind::Or;
            break;
        case BinaryOperator::BoK_BitwiseXor:
            Kind = ir::BinaryOp::BinaryOpKind::Xor;
            break;
        case BinaryOperator::BoK_LeftShift:
            Kind = ir::BinaryOp::BinaryOpKind::Sal;
            break;
        case BinaryOperator::BoK_RightShift:
            Kind = ir::BinaryOp::BinaryOpKind::Sar;
            break;
        case BinaryOperator::BoK_LowerThan:
            CmpKind = ir::ICmpOp::lt;
            break;
        case BinaryOperator::BoK_LowerEqual:
            CmpKind = ir::ICmpOp::le;
            break;
        case BinaryOperator::BoK_GreaterThan:
            CmpKind = ir::ICmpOp::gt;
            break;
        case BinaryOperator::BoK_GreaterEqual:
            CmpKind = ir::ICmpOp::ge;
            break;
        case BinaryOperator::Bok_Equal:
            CmpKind = ir::ICmpOp::eq;
            break;
        case BinaryOperator::Bok_NotEqual:
            CmpKind = ir::ICmpOp::neq;
            break;
        case BinaryOperator::Bok_And:
        case BinaryOperator::Bok_Or:
        case BinaryOperator::BoK_None:
        default:
            break;

    }
    // According to C standard the subexpressions of the same operation
    // are usually unsequenced, they can be evaluated in any order.

    // We will generate left and right before they are used because
    // we use `generateExpression` for them, that will add instructions
    // to the IR Function.

    if (Kind != ir::BinaryOp::BinaryOpKind::none) {
        ir::Value *left = generateExpression(*BinaryOp.getLeft(), IRFunc);
        ir::Value *right = generateExpression(*BinaryOp.getRight(), IRFunc);

        ir::BinaryOp *BinOp = Ctx.createBinaryOp(dynamic_cast<ir::Operand *>(left),
                                                 dynamic_cast<ir::Operand *>(right), Kind);
        if (IRFunc != nullptr)
            IRFunc->add_instruction(BinOp);
        return BinOp->getDestination();
    }
    if (CmpKind != ir::ICmpOp::CmpOpKind::none) {
        ir::Value *left = generateExpression(*BinaryOp.getLeft(), IRFunc);
        ir::Value *right = generateExpression(*BinaryOp.getRight(), IRFunc);

        ir::ICmpOp *CmpOp = Ctx.createICmpOp(dynamic_cast<ir::Operand *>(left),
                                             dynamic_cast<ir::Operand *>(right), CmpKind);
        if (IRFunc != nullptr)
            IRFunc->add_instruction(CmpOp);
        return CmpOp->getDestination();
    }

    // Short-Circuiting for && instruction
    if (BinaryOp.getOperatorKind() == BinaryOperator::Bok_And) {
        auto *false_label = Ctx.getOrCreateLabel("false_label");
        auto *end_label = Ctx.getOrCreateLabel("end_label");
        auto *result = Ctx.createReg();
        auto *temp_left = Ctx.createReg();
        auto *temp_right = Ctx.createReg();
        auto *val_1 = Ctx.createInt(llvm::APSInt(llvm::APInt(32, abs(1))));
        auto *val_0 = Ctx.createInt(llvm::APSInt(32, abs(0)));

        ir::Value *left = generateExpression(*BinaryOp.getLeft(), IRFunc);

        if (IRFunc != nullptr) {
            auto *mov_left = Ctx.createCopy(left, temp_left);
            IRFunc->add_instruction(mov_left);

            auto *jump_if_zero_left = Ctx.createJZ(temp_left, false_label);
            IRFunc->add_instruction(jump_if_zero_left);

            ir::Value *right = generateExpression(*BinaryOp.getRight(), IRFunc);
            auto *mov_right = Ctx.createCopy(right, temp_right);
            IRFunc->add_instruction(mov_right);

            auto *jump_if_zero_right = Ctx.createJZ(temp_right, false_label);
            IRFunc->add_instruction(jump_if_zero_right);

            auto *set_result_to_1 = Ctx.createCopy(val_1, result);
            IRFunc->add_instruction(set_result_to_1);

            auto *jump_end = Ctx.createJump(end_label);
            IRFunc->add_instruction(jump_end);
            auto *set_result_to_0 = Ctx.createCopy(val_0, result);
            IRFunc->add_instruction(false_label);

            IRFunc->add_instruction(set_result_to_0);
            IRFunc->add_instruction(end_label);
        }

        return result;
    }

    // Short-circuiting for || instruction
    if (BinaryOp.getOperatorKind() == BinaryOperator::Bok_Or) {
        auto *true_label = Ctx.getOrCreateLabel("true_label");
        auto *end_label = Ctx.getOrCreateLabel("end_label");
        auto *result = Ctx.createReg();
        auto *temp_left = Ctx.createReg();
        auto *temp_right = Ctx.createReg();
        auto *val_1 = Ctx.createInt(llvm::APSInt(llvm::APInt(32, abs(1))));
        auto *val_0 = Ctx.createInt(llvm::APSInt(32, abs(0)));

        ir::Value *left = generateExpression(*BinaryOp.getLeft(), IRFunc);

        if (IRFunc != nullptr) {
            auto *mov_left = Ctx.createCopy(left, temp_left);
            IRFunc->add_instruction(mov_left);

            auto *jump_if_not_zero_left = Ctx.createJNZ(temp_left, true_label);
            IRFunc->add_instruction(jump_if_not_zero_left);

            ir::Value *right = generateExpression(*BinaryOp.getRight(), IRFunc);

            auto *mov_right = Ctx.createCopy(right, temp_right);
            IRFunc->add_instruction(mov_right);

            auto *jump_if_not_zero_right = Ctx.createJNZ(temp_right, true_label);
            IRFunc->add_instruction(jump_if_not_zero_right);

            auto *set_result_to_0 = Ctx.createCopy(val_0, result);
            IRFunc->add_instruction(set_result_to_0);

            auto *jump_end = Ctx.createJump(end_label);
            IRFunc->add_instruction(jump_end);

            auto *set_result_to_1 = Ctx.createCopy(val_1, result);
            IRFunc->add_instruction(true_label);

            IRFunc->add_instruction(set_result_to_1);
            IRFunc->add_instruction(end_label);
        }

        return result;
    }

    return nullptr;
}

ir::Value *IRGenerator::generatePrefixExpression(const PrefixOperator &PrefixOp, ir::Function *IRFunc) {
    // Get the variable being operated on
    ir::Value *var = generateExpression(*PrefixOp.getExpr(), IRFunc);
    auto *one = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 1)));

    ir::BinaryOp *increment_instruction = nullptr;

    // For prefix operators, we first update the variable, then return its new value
    switch (PrefixOp.getOperatorKind()) {
        case PrefixOperator::POK_PreIncrement:
            increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand *>(var),
                                                       one, ir::BinaryOp::Add);
            break;
        case PrefixOperator::POK_PreDecrement:
            increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand *>(var),
                                                       one, ir::BinaryOp::Sub);
            break;
    }

    if (IRFunc != nullptr && increment_instruction != nullptr) {
        IRFunc->add_instruction(increment_instruction);
        // Copy the result back to the variable
        IRFunc->add_instruction(Ctx.createCopy(increment_instruction->getDestination(), var));
    }

    return var; // Return the updated variable
}

ir::Value *IRGenerator::generatePostfixExpression(const PostfixOperator &PostfixOp, ir::Function *IRFunc) {
    // Get the variable being operated on
    ir::Value *var = generateExpression(*PostfixOp.getExpr(), IRFunc);
    auto *one = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 1)));

    // For postfix operators, we save the old value, update the variable, then return the old value
    auto *old_value = Ctx.createReg();
    ir::BinaryOp *increment_instruction = nullptr;

    if (IRFunc != nullptr) {
        // Save the current value
        IRFunc->add_instruction(Ctx.createCopy(var, old_value));
    }

    switch (PostfixOp.getOperatorKind()) {
        case PostfixOperator::POK_PostIncrement:
            increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand *>(var),
                                                       one, ir::BinaryOp::Add);
            break;
        case PostfixOperator::POK_PostDecrement:
            increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand *>(var),
                                                       one, ir::BinaryOp::Sub);
            break;
    }

    if (IRFunc != nullptr && increment_instruction != nullptr) {
        IRFunc->add_instruction(increment_instruction);
        // Copy the result back to the variable
        IRFunc->add_instruction(Ctx.createCopy(increment_instruction->getDestination(), var));
    }

    return old_value; // Return the old value (before increment/decrement)
}

ir::Value *IRGenerator::generateConditionalExpression(const ConditionalExpr &cond_expr, ir::Function *IRFunc) {
    // Results that will be returned
    ir::Value *Result, *temp1, *temp2;
    // labels
    auto *e2_label = Ctx.getOrCreateLabel("e2_label");
    auto *end_label = Ctx.getOrCreateLabel("end_label");
    // Result must be a register
    Result = Ctx.createReg();

    // first generate the conditional statement
    auto *cond_result = generateExpression(*cond_expr.getCondition(), IRFunc);
    // If met, jump to the e2
    IRFunc->add_instruction(Ctx.createJZ(cond_result, e2_label));
    // now we generate the code for the first statement, and obtain
    // the result
    temp1 = generateExpression(*cond_expr.getLeft(), IRFunc);
    IRFunc->add_instruction(Ctx.createMov(temp1, Result));
    IRFunc->add_instruction(Ctx.createJump(end_label));
    // now we generate the second statement
    IRFunc->add_instruction(e2_label);
    temp2 = generateExpression(*cond_expr.getRight(), IRFunc);
    IRFunc->add_instruction(Ctx.createMov(temp2, Result));
    // finally the end label
    IRFunc->add_instruction(end_label);
    return Result;
}

ir::Value* IRGenerator::generateFunctionCallExpression(const FunctionCallExpr& FuncCallExpr, ir::Function* IRFunc) {
    std::vector<ir::Operand*> params;
    for (auto * arg : FuncCallExpr.getArgs()) {
        params.emplace_back(reinterpret_cast<ir::Operand *>(generateExpression(*arg, IRFunc)));
    }
    auto * invoke = Ctx.createInvoke(FuncCallExpr.getIdentifier(), params);
    IRFunc->add_instruction(invoke);
    return invoke->getResult();
}

void IRGenerator::collectSwitchCases(
    const Statement *stmt,
    std::vector<CaseInfo> &cases,
    std::string &defaultLabel,
    bool &hasDefault) {
    if (!stmt) return;

    switch (stmt->getKind()) {
        case Statement::SK_Case: {
            const auto *caseStmt = dynamic_cast<const CaseStatement *>(stmt);
            CaseInfo info;
            info.value = caseStmt->getValue();
            info.label = std::string(caseStmt->get_label());
            cases.push_back(info);
            break;
        }
        case Statement::SK_Default: {
            const auto *defaultStmt = dynamic_cast<const DefaultStatement *>(stmt);
            defaultLabel = std::string(defaultStmt->get_label());
            hasDefault = true;
            break;
        }

        case Statement::SK_Compound: {
            // Recursively scan compound statement's children
            const auto *compound = dynamic_cast<const CompoundStatement *>(stmt);
            for (const auto &item: *compound) {
                if (std::holds_alternative<Statement *>(item)) {
                    collectSwitchCases(std::get<Statement *>(item), cases, defaultLabel, hasDefault);
                }
            }
            break;
        }

        case Statement::SK_If: {
            // Cases can appear inside if statements within switch
            const auto *ifStmt = dynamic_cast<const IfStatement *>(stmt);
            collectSwitchCases(ifStmt->getThenSt(), cases, defaultLabel, hasDefault);
            if (ifStmt->getElseSt()) {
                collectSwitchCases(ifStmt->getElseSt(), cases, defaultLabel, hasDefault);
            }
            break;
        }

        // For loops, while, etc. - cases could be inside
        case Statement::SK_While: {
            const auto *whileStmt = dynamic_cast<const WhileStatement *>(stmt);
            collectSwitchCases(whileStmt->getBody(), cases, defaultLabel, hasDefault);
            break;
        }

        case Statement::SK_DoWhile: {
            const auto *doWhile = dynamic_cast<const DoWhileStatement *>(stmt);
            collectSwitchCases(doWhile->getBody(), cases, defaultLabel, hasDefault);
            break;
        }

        case Statement::SK_For: {
            const auto *forStmt = dynamic_cast<const ForStatement *>(stmt);
            collectSwitchCases(forStmt->getBody(), cases, defaultLabel, hasDefault);
            break;
        }

        // Other statements don't contain cases
        default:
            break;
    }
}

// Variable renaming helper methods

std::string IRGenerator::generateUniqueVarName(StringRef originalName) {
    return originalName.str() + "_" + std::to_string(VariableCounter++);
}

std::string IRGenerator::getIRName(StringRef originalName) {
    auto it = VariableRenameStack.find(originalName);
    if (it != VariableRenameStack.end() && !it->second.empty()) {
        return it->second.back();
    }
    // If not found in rename stack, use original name
    return originalName.str();
}

void IRGenerator::enterScope() {
    // Scopes are tracked implicitly by the rename stack
    // No explicit action needed here for now
}

void IRGenerator::exitScope(const std::vector<std::string> &declaredVars) {
    // Pop variables that were declared in this scope from the rename stack
    for (const std::string &varName : declaredVars) {
        auto it = VariableRenameStack.find(varName);
        if (it != VariableRenameStack.end() && !it->second.empty()) {
            it->second.pop_back();
            if (it->second.empty()) {
                VariableRenameStack.erase(it);
            }
        }
    }
}