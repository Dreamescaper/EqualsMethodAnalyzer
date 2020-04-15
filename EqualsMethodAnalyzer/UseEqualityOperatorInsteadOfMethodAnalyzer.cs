using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace EqualsMethodAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class UseEqualityOperatorInsteadOfMethodAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "EMA0001";

        private static readonly DiagnosticDescriptor DiagnosticDescriptor = new DiagnosticDescriptor(
                DiagnosticId,
                "Use == operator instead of Equals method",
                "Use == operator instead of Equals method for built-in types",
                "Style",
                DiagnosticSeverity.Warning,
                true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; }
            = ImmutableArray.Create(DiagnosticDescriptor);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterOperationAction(AnalyzeOperation, OperationKind.Invocation);
        }

        private void AnalyzeOperation(OperationAnalysisContext context)
        {
            if (!(context.Operation is IInvocationOperation invocationOperation))
                return;

            if (invocationOperation.TargetMethod?.Name == nameof(object.Equals)
                && invocationOperation.Arguments.Length == 1
                && invocationOperation.Instance != null
                && IsBuiltInType(invocationOperation.Instance.Type))
            {
                context.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptor, invocationOperation.Syntax.GetLocation()));
            }
        }

        private static bool IsBuiltInType(ITypeSymbol typeSymbol)
        {
            switch (typeSymbol.SpecialType)
            {
                case SpecialType.System_Boolean:
                case SpecialType.System_Char:
                case SpecialType.System_SByte:
                case SpecialType.System_Byte:
                case SpecialType.System_Int16:
                case SpecialType.System_UInt16:
                case SpecialType.System_Int32:
                case SpecialType.System_UInt32:
                case SpecialType.System_Int64:
                case SpecialType.System_UInt64:
                case SpecialType.System_Decimal:
                case SpecialType.System_Single:
                case SpecialType.System_Double:
                case SpecialType.System_String:
                case SpecialType.System_DateTime:
                    return true;
            }

            if (typeSymbol.BaseType?.SpecialType == SpecialType.System_Enum)
                return true;

            if (typeSymbol is INamedTypeSymbol namedTypeSymbol
                && namedTypeSymbol.IsGenericType
                && namedTypeSymbol.TypeArguments.Length == 1
                && namedTypeSymbol.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T)
            {
                return IsBuiltInType(namedTypeSymbol.TypeArguments[0]);
            }

            return false;
        }
    }
}
