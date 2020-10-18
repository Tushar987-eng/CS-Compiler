namespace Compiler.CodeAnalysis.Syntax
{
    public enum SyntaxKind
    {

        
        BadToken,
        EndOffileToken,

        NumberToken,
        WhitespaceToken,
        PlusToken,
        MinusToken,
        StarToken,
        SlashToken,
        OpenParenthesisToken,
        CloseParenthesisToken,
        IdentifierToken,


        BinaryExpression,
        UnaryExpression,
        NameExpression,
        LiteralExpression,
        AssignmentExpression,
        ParenthesizedExpression,


        TrueKeyword,
        FalseKeyword,

        
        BangToken,
        AmpersandAmpersandToken,
        PipePipeToken,
        EqualsEqualsToken,
        EqualsToken,
        BangEqualsToken,
    }
}