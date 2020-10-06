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
        IdetifierToken,


        BinaryExpression,
        UnaryExpression,
        LiteralExpression,
        ParenthesizedExpression,


        TrueKeyword,
        FalseKeyword,

        
        BangToken,
        AmpersandAmpersandToken,
        PipePipeToken,
        EqualsEqualsToken,
        BangEqualsToken,
    }
}