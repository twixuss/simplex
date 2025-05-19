#include "c2simplex.h"
#include <clang-c/Index.h>

inline void append(StringBuilder &builder, CXCursorKind kind) {
    switch (kind) {
        #define x(name) case name: return append(builder, #name);
        x(CXCursor_UnexposedDecl )
        x(CXCursor_StructDecl )
        x(CXCursor_UnionDecl )
        x(CXCursor_ClassDecl )
        x(CXCursor_EnumDecl )
        x(CXCursor_FieldDecl )
        x(CXCursor_EnumConstantDecl )
        x(CXCursor_FunctionDecl )
        x(CXCursor_VarDecl )
        x(CXCursor_ParmDecl )
        x(CXCursor_ObjCInterfaceDecl )
        x(CXCursor_ObjCCategoryDecl )
        x(CXCursor_ObjCProtocolDecl )
        x(CXCursor_ObjCPropertyDecl )
        x(CXCursor_ObjCIvarDecl )
        x(CXCursor_ObjCInstanceMethodDecl )
        x(CXCursor_ObjCClassMethodDecl )
        x(CXCursor_ObjCImplementationDecl )
        x(CXCursor_ObjCCategoryImplDecl )
        x(CXCursor_TypedefDecl )
        x(CXCursor_CXXMethod )
        x(CXCursor_Namespace )
        x(CXCursor_LinkageSpec )
        x(CXCursor_Constructor )
        x(CXCursor_Destructor )
        x(CXCursor_ConversionFunction )
        x(CXCursor_TemplateTypeParameter )
        x(CXCursor_NonTypeTemplateParameter )
        x(CXCursor_TemplateTemplateParameter )
        x(CXCursor_FunctionTemplate )
        x(CXCursor_ClassTemplate )
        x(CXCursor_ClassTemplatePartialSpecialization )
        x(CXCursor_NamespaceAlias )
        x(CXCursor_UsingDirective )
        x(CXCursor_UsingDeclaration )
        x(CXCursor_TypeAliasDecl )
        x(CXCursor_ObjCSynthesizeDecl )
        x(CXCursor_ObjCDynamicDecl )
        x(CXCursor_CXXAccessSpecifier )
        x(CXCursor_ObjCSuperClassRef )
        x(CXCursor_ObjCProtocolRef )
        x(CXCursor_ObjCClassRef )
        x(CXCursor_TypeRef )
        x(CXCursor_CXXBaseSpecifier )
        x(CXCursor_TemplateRef )
        x(CXCursor_NamespaceRef )
        x(CXCursor_MemberRef )
        x(CXCursor_LabelRef )
        x(CXCursor_OverloadedDeclRef )
        x(CXCursor_VariableRef )
        x(CXCursor_FirstInvalid )
        x(CXCursor_NoDeclFound )
        x(CXCursor_NotImplemented )
        x(CXCursor_LastInvalid )
        x(CXCursor_UnexposedExpr )
        x(CXCursor_DeclRefExpr )
        x(CXCursor_MemberRefExpr )
        x(CXCursor_CallExpr )
        x(CXCursor_ObjCMessageExpr )
        x(CXCursor_BlockExpr )
        x(CXCursor_IntegerLiteral )
        x(CXCursor_FloatingLiteral )
        x(CXCursor_ImaginaryLiteral )
        x(CXCursor_StringLiteral )
        x(CXCursor_CharacterLiteral )
        x(CXCursor_ParenExpr )
        x(CXCursor_UnaryOperator )
        x(CXCursor_ArraySubscriptExpr )
        x(CXCursor_BinaryOperator )
        x(CXCursor_CompoundAssignOperator )
        x(CXCursor_ConditionalOperator )
        x(CXCursor_CStyleCastExpr )
        x(CXCursor_CompoundLiteralExpr )
        x(CXCursor_InitListExpr )
        x(CXCursor_AddrLabelExpr )
        x(CXCursor_StmtExpr )
        x(CXCursor_GenericSelectionExpr )
        x(CXCursor_GNUNullExpr )
        x(CXCursor_CXXStaticCastExpr )
        x(CXCursor_CXXDynamicCastExpr )
        x(CXCursor_CXXReinterpretCastExpr )
        x(CXCursor_CXXConstCastExpr )
        x(CXCursor_CXXFunctionalCastExpr )
        x(CXCursor_CXXTypeidExpr )
        x(CXCursor_CXXBoolLiteralExpr )
        x(CXCursor_CXXNullPtrLiteralExpr )
        x(CXCursor_CXXThisExpr )
        x(CXCursor_CXXThrowExpr )
        x(CXCursor_CXXNewExpr )
        x(CXCursor_CXXDeleteExpr )
        x(CXCursor_UnaryExpr )
        x(CXCursor_ObjCStringLiteral )
        x(CXCursor_ObjCEncodeExpr )
        x(CXCursor_ObjCSelectorExpr )
        x(CXCursor_ObjCProtocolExpr )
        x(CXCursor_ObjCBridgedCastExpr )
        x(CXCursor_PackExpansionExpr )
        x(CXCursor_SizeOfPackExpr )
        x(CXCursor_LambdaExpr )
        x(CXCursor_ObjCBoolLiteralExpr )
        x(CXCursor_ObjCSelfExpr )
        x(CXCursor_OMPArraySectionExpr )
        x(CXCursor_ObjCAvailabilityCheckExpr )
        x(CXCursor_FixedPointLiteral )
        x(CXCursor_OMPArrayShapingExpr )
        x(CXCursor_OMPIteratorExpr )
        x(CXCursor_CXXAddrspaceCastExpr )
        x(CXCursor_ConceptSpecializationExpr )
        x(CXCursor_RequiresExpr )
        x(CXCursor_CXXParenListInitExpr )
        x(CXCursor_UnexposedStmt )
        x(CXCursor_LabelStmt )
        x(CXCursor_CompoundStmt )
        x(CXCursor_CaseStmt )
        x(CXCursor_DefaultStmt )
        x(CXCursor_IfStmt )
        x(CXCursor_SwitchStmt )
        x(CXCursor_WhileStmt )
        x(CXCursor_DoStmt )
        x(CXCursor_ForStmt )
        x(CXCursor_GotoStmt )
        x(CXCursor_IndirectGotoStmt )
        x(CXCursor_ContinueStmt )
        x(CXCursor_BreakStmt )
        x(CXCursor_ReturnStmt )
        x(CXCursor_AsmStmt )
        x(CXCursor_ObjCAtTryStmt )
        x(CXCursor_ObjCAtCatchStmt )
        x(CXCursor_ObjCAtFinallyStmt )
        x(CXCursor_ObjCAtThrowStmt )
        x(CXCursor_ObjCAtSynchronizedStmt )
        x(CXCursor_ObjCAutoreleasePoolStmt )
        x(CXCursor_ObjCForCollectionStmt )
        x(CXCursor_CXXCatchStmt )
        x(CXCursor_CXXTryStmt )
        x(CXCursor_CXXForRangeStmt )
        x(CXCursor_SEHTryStmt )
        x(CXCursor_SEHExceptStmt )
        x(CXCursor_SEHFinallyStmt )
        x(CXCursor_MSAsmStmt )
        x(CXCursor_NullStmt )
        x(CXCursor_DeclStmt )
        x(CXCursor_OMPParallelDirective )
        x(CXCursor_OMPSimdDirective )
        x(CXCursor_OMPForDirective )
        x(CXCursor_OMPSectionsDirective )
        x(CXCursor_OMPSectionDirective )
        x(CXCursor_OMPSingleDirective )
        x(CXCursor_OMPParallelForDirective )
        x(CXCursor_OMPParallelSectionsDirective )
        x(CXCursor_OMPTaskDirective )
        x(CXCursor_OMPMasterDirective )
        x(CXCursor_OMPCriticalDirective )
        x(CXCursor_OMPTaskyieldDirective )
        x(CXCursor_OMPBarrierDirective )
        x(CXCursor_OMPTaskwaitDirective )
        x(CXCursor_OMPFlushDirective )
        x(CXCursor_SEHLeaveStmt )
        x(CXCursor_OMPOrderedDirective )
        x(CXCursor_OMPAtomicDirective )
        x(CXCursor_OMPForSimdDirective )
        x(CXCursor_OMPParallelForSimdDirective )
        x(CXCursor_OMPTargetDirective )
        x(CXCursor_OMPTeamsDirective )
        x(CXCursor_OMPTaskgroupDirective )
        x(CXCursor_OMPCancellationPointDirective )
        x(CXCursor_OMPCancelDirective )
        x(CXCursor_OMPTargetDataDirective )
        x(CXCursor_OMPTaskLoopDirective )
        x(CXCursor_OMPTaskLoopSimdDirective )
        x(CXCursor_OMPDistributeDirective )
        x(CXCursor_OMPTargetEnterDataDirective )
        x(CXCursor_OMPTargetExitDataDirective )
        x(CXCursor_OMPTargetParallelDirective )
        x(CXCursor_OMPTargetParallelForDirective )
        x(CXCursor_OMPTargetUpdateDirective )
        x(CXCursor_OMPDistributeParallelForDirective )
        x(CXCursor_OMPDistributeParallelForSimdDirective )
        x(CXCursor_OMPDistributeSimdDirective )
        x(CXCursor_OMPTargetParallelForSimdDirective )
        x(CXCursor_OMPTargetSimdDirective )
        x(CXCursor_OMPTeamsDistributeDirective )
        x(CXCursor_OMPTeamsDistributeSimdDirective )
        x(CXCursor_OMPTeamsDistributeParallelForSimdDirective )
        x(CXCursor_OMPTeamsDistributeParallelForDirective )
        x(CXCursor_OMPTargetTeamsDirective )
        x(CXCursor_OMPTargetTeamsDistributeDirective )
        x(CXCursor_OMPTargetTeamsDistributeParallelForDirective )
        x(CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective )
        x(CXCursor_OMPTargetTeamsDistributeSimdDirective )
        x(CXCursor_BuiltinBitCastExpr )
        x(CXCursor_OMPMasterTaskLoopDirective )
        x(CXCursor_OMPParallelMasterTaskLoopDirective )
        x(CXCursor_OMPMasterTaskLoopSimdDirective )
        x(CXCursor_OMPParallelMasterTaskLoopSimdDirective )
        x(CXCursor_OMPParallelMasterDirective )
        x(CXCursor_OMPDepobjDirective )
        x(CXCursor_OMPScanDirective )
        x(CXCursor_OMPTileDirective )
        x(CXCursor_OMPCanonicalLoop )
        x(CXCursor_OMPInteropDirective )
        x(CXCursor_OMPDispatchDirective )
        x(CXCursor_OMPMaskedDirective )
        x(CXCursor_OMPUnrollDirective )
        x(CXCursor_OMPMetaDirective )
        x(CXCursor_OMPGenericLoopDirective )
        x(CXCursor_OMPTeamsGenericLoopDirective )
        x(CXCursor_OMPTargetTeamsGenericLoopDirective )
        x(CXCursor_OMPParallelGenericLoopDirective )
        x(CXCursor_OMPTargetParallelGenericLoopDirective )
        x(CXCursor_OMPParallelMaskedDirective )
        x(CXCursor_OMPMaskedTaskLoopDirective )
        x(CXCursor_OMPMaskedTaskLoopSimdDirective )
        x(CXCursor_OMPParallelMaskedTaskLoopDirective )
        x(CXCursor_OMPParallelMaskedTaskLoopSimdDirective )
        x(CXCursor_OMPErrorDirective )
        x(CXCursor_OMPScopeDirective )
        x(CXCursor_TranslationUnit )
        x(CXCursor_UnexposedAttr )
        x(CXCursor_IBActionAttr )
        x(CXCursor_IBOutletAttr )
        x(CXCursor_IBOutletCollectionAttr )
        x(CXCursor_CXXFinalAttr )
        x(CXCursor_CXXOverrideAttr )
        x(CXCursor_AnnotateAttr )
        x(CXCursor_AsmLabelAttr )
        x(CXCursor_PackedAttr )
        x(CXCursor_PureAttr )
        x(CXCursor_ConstAttr )
        x(CXCursor_NoDuplicateAttr )
        x(CXCursor_CUDAConstantAttr )
        x(CXCursor_CUDADeviceAttr )
        x(CXCursor_CUDAGlobalAttr )
        x(CXCursor_CUDAHostAttr )
        x(CXCursor_CUDASharedAttr )
        x(CXCursor_VisibilityAttr )
        x(CXCursor_DLLExport )
        x(CXCursor_DLLImport )
        x(CXCursor_NSReturnsRetained )
        x(CXCursor_NSReturnsNotRetained )
        x(CXCursor_NSReturnsAutoreleased )
        x(CXCursor_NSConsumesSelf )
        x(CXCursor_NSConsumed )
        x(CXCursor_ObjCException )
        x(CXCursor_ObjCNSObject )
        x(CXCursor_ObjCIndependentClass )
        x(CXCursor_ObjCPreciseLifetime )
        x(CXCursor_ObjCReturnsInnerPointer )
        x(CXCursor_ObjCRequiresSuper )
        x(CXCursor_ObjCRootClass )
        x(CXCursor_ObjCSubclassingRestricted )
        x(CXCursor_ObjCExplicitProtocolImpl )
        x(CXCursor_ObjCDesignatedInitializer )
        x(CXCursor_ObjCRuntimeVisible )
        x(CXCursor_ObjCBoxable )
        x(CXCursor_FlagEnum )
        x(CXCursor_ConvergentAttr )
        x(CXCursor_WarnUnusedAttr )
        x(CXCursor_WarnUnusedResultAttr )
        x(CXCursor_AlignedAttr )
        x(CXCursor_PreprocessingDirective )
        x(CXCursor_MacroDefinition )
        x(CXCursor_MacroExpansion )
        x(CXCursor_InclusionDirective )
        x(CXCursor_ModuleImportDecl )
        x(CXCursor_TypeAliasTemplateDecl )
        x(CXCursor_StaticAssert )
        x(CXCursor_FriendDecl )
        x(CXCursor_ConceptDecl )
        x(CXCursor_OverloadCandidate )
        #undef x
    }
    return append_format(builder, "CXCursorKind({})", (u32)kind);
}

// Callback function to handle diagnostics
static void handleDiagnostic(CXTranslationUnit tu, CXDiagnosticSet diagnostics) {
    unsigned int numDiagnostics = clang_getNumDiagnosticsInSet(diagnostics);
    for (unsigned int i = 0; i < numDiagnostics; i++) {
        CXDiagnostic diagnostic = clang_getDiagnosticInSet(diagnostics, i);
        CXString string = clang_formatDiagnostic(diagnostic, clang_defaultDiagnosticDisplayOptions());
        const char *message = clang_getCString(string);
        printf("%s\n", message);
        clang_disposeString(string);
    }
}

String location(CXCursor cursor) {
    CXSourceRange range = clang_getCursorExtent(cursor);
    CXSourceLocation start = clang_getRangeStart(range);
    CXSourceLocation end = clang_getRangeEnd(range);

    unsigned start_line, start_column, end_line, end_column;
    clang_getSpellingLocation(start, NULL, &start_line, &start_column, NULL);
    clang_getSpellingLocation(end, NULL, &end_line, &end_column, NULL);

    CXString cursor_spelling = clang_getCursorSpelling(cursor);
    return as_utf8(as_span(clang_getCString(cursor_spelling)));
}

CXTranslationUnit tu;

// Callback function to handle AST nodes
static enum CXChildVisitResult visitNode(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    switch (clang_getCursorKind(cursor)) {
        case CXCursor_MacroDefinition: {
            break;
            auto name = clang_getCString(clang_getCursorDisplayName(cursor));
            auto extent = clang_getCursorExtent(cursor);
            CXToken *tokens;
            unsigned numTokens;
            clang_tokenize(tu, extent, &tokens, &numTokens);


            if (numTokens > 1) {
                print("const {} = ", name);
                for (u32 i = 1; i < numTokens; ++i) {
                    print(clang_getCString(clang_getTokenSpelling(tu, tokens[i])));
                }
                println();
            }
            break;
        }
        case CXCursor_StructDecl: {
            auto s = clang_getCursorType(cursor);
            println("const {} = struct {{", clang_getCString(clang_getCursorSpelling(cursor)));
            for (int i = 0; i < clang_getNumArgTypes(clang_getCursorType(cursor)); ++i) {
                auto aname = clang_getCString(clang_getCursorSpelling(clang_Cursor_getArgument(cursor, i)));
                auto atype = clang_getCString(clang_getTypeSpelling(clang_getArgType(clang_getCursorType(cursor), i)));
                printf("    %s: %s", aname, atype);
            }
            println("}");
            break;
        }
        case CXCursor_TypedefDecl: {
            auto t = as_span(clang_getCString(clang_getTypeSpelling(clang_getCanonicalType(clang_getCursorType(cursor)))));

            auto skip = [&](Span<char> prefix) {
                if (starts_with(t, prefix))
                    t = t.skip(prefix.count);
            };

            skip("struct "s);
            skip("union "s);
            skip("enum "s);

            println("const {} = {}", clang_getCString(clang_getTypedefName(clang_getCursorType(cursor))), t);
            break;
        }
        case CXCursor_FunctionDecl: {
            break;
            auto fname = clang_getCString(clang_getCursorSpelling(cursor));
            printf("fn %s(", fname);
            for (int i = 0; i < clang_getNumArgTypes(clang_getCursorType(cursor)); ++i) {
                if (i)
                    printf(", ");
                
                auto aname = clang_getCString(clang_getCursorSpelling(clang_Cursor_getArgument(cursor, i)));
                auto atype = clang_getCString(clang_getTypeSpelling(clang_getArgType(clang_getCursorType(cursor), i)));

                printf("%s: %s", aname, atype);
            }
            printf(") => #extern\n");
            break;
        }
        default:
            //println(clang_getCursorKind(cursor));
            break;
    }
    return CXChildVisit_Continue;
}

void c2simplex(String source) {
    // Create a clang index
    CXIndex index = clang_createIndex(0, 0);

    // Parse a C file
    Span<char const *> args{
    //    "-E"
    };
    tu = clang_parseTranslationUnit(index, "F:/projects/simplex/tmp/windows.c", args.data, args.count, NULL, 0, CXTranslationUnit_DetailedPreprocessingRecord);
    CXTranslationUnit tu2 = clang_parseTranslationUnit(index, "F:/projects/simplex/tmp/windows.c", args.data, args.count, NULL, 0, CXTranslationUnit_None);

    // Get diagnostics
    CXDiagnosticSet diagnostics = clang_getDiagnosticSetFromTU(tu);
    handleDiagnostic(tu, diagnostics);
    diagnostics = clang_getDiagnosticSetFromTU(tu2);
    handleDiagnostic(tu2, diagnostics);

    // Visit AST nodes
    clang_visitChildren(clang_getTranslationUnitCursor(tu), visitNode, NULL);
    clang_visitChildren(clang_getTranslationUnitCursor(tu2), visitNode, NULL);

    // Dispose of the translation unit and index
    clang_disposeTranslationUnit(tu);
    clang_disposeTranslationUnit(tu2);
    clang_disposeIndex(index);
}
