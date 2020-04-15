using System.Collections.Immutable;
using System.Composition;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace EqualsMethodAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp), Shared]
    public class UseEqualityOperatorInsteadOfMethodCodeFixProvider : CodeFixProvider
    {
        private const string Title = "Use == operator";

        public UseEqualityOperatorInsteadOfMethodCodeFixProvider()
        {
        }

        public override ImmutableArray<string> FixableDiagnosticIds { get; }
            = ImmutableArray.Create(UseEqualityOperatorInsteadOfMethodAnalyzer.DiagnosticId);

        public override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var diagnostic = context.Diagnostics[0];


            if (!(root.FindNode(context.Span) is InvocationExpressionSyntax invocationExpressionSyntax))
                return;

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: Title,
                    createChangedDocument: _ => ReplaceWithOperator(context.Document, root, invocationExpressionSyntax),
                    equivalenceKey: Title),
                diagnostic);
        }

        private Task<Document> ReplaceWithOperator(Document document, SyntaxNode root, InvocationExpressionSyntax invocationExpressionSyntax)
        {
            ExpressionSyntax nodeToReplace = invocationExpressionSyntax;

            var left = ((MemberAccessExpressionSyntax)invocationExpressionSyntax.Expression).Expression;
            var right = invocationExpressionSyntax.ArgumentList.Arguments[0].Expression;

            var parentUnaryExpression = invocationExpressionSyntax.Parent as PrefixUnaryExpressionSyntax;

            var operatorKind = SyntaxKind.EqualsExpression;
            if (parentUnaryExpression?.IsKind(SyntaxKind.LogicalNotExpression) == true)
            {
                operatorKind = SyntaxKind.NotEqualsExpression;
                nodeToReplace = parentUnaryExpression;
            }

            ExpressionSyntax operatorExpression = SyntaxFactory.BinaryExpression(operatorKind, left, right);

            if (nodeToReplace.Parent is PrefixUnaryExpressionSyntax)
                operatorExpression = SyntaxFactory.ParenthesizedExpression(operatorExpression);

            operatorExpression = operatorExpression
                .WithLeadingTrivia(invocationExpressionSyntax.GetLeadingTrivia())
                .WithTrailingTrivia(invocationExpressionSyntax.GetTrailingTrivia());

            var newRoot = root.ReplaceNode(nodeToReplace, operatorExpression);

            return Task.FromResult(document.WithSyntaxRoot(newRoot));
        }
    }
}
