using System;
using System.Collections.Generic;
using System.Linq;

using Compiler.CodeAnalysis;
using Compiler.CodeAnalysis.Binding;
using Compiler.CodeAnalysis.Syntax;
namespace Compiler
{
    internal static class Program {
        private static void Main () {

            var showTree = false;
            var variables = new Dictionary<VariableSymbol , object>();
            Console.WriteLine("Commands:");
            Console.WriteLine("#cls: To clear the screen");
            Console.WriteLine("#showParseTree: To show/hide Parse Trees\n");

            while (true) {
                Console.Write ("-> ");
                var line = Console.ReadLine ();
                if (string.IsNullOrWhiteSpace (line))
                    return;
                
                if(line == "#showParseTree")
                {
                    showTree = !showTree;
                    Console.WriteLine(showTree ? "Showing Parse Trees" : "Not showing Parse trees");
                    continue;
                }
                else if(line  == "#cls")
                {
                    Console.Clear();
                    continue;
                }

                var syntaxTree = SyntaxTree.Parse(line);
                var compilation = new Compilation(syntaxTree);
                var result = compilation.Evaluate(variables);

                var diagnostics = result.Diagnostics;
                
                if(showTree)
                {
                    Console.ForegroundColor = ConsoleColor.DarkGray; 
                    PrettyPrint(syntaxTree.Root);
                    Console.ResetColor();
                }

                if (!diagnostics.Any())
                {
                    Console.WriteLine(result.Value);
                }
                else
                {

                    foreach (var diagnostic in diagnostics)
                    {
                        Console.WriteLine();
                        
                        Console.ForegroundColor = ConsoleColor.DarkRed;
                        Console.WriteLine(diagnostic);
                        Console.ResetColor();
                        
                        var prefix = line.Substring(0,diagnostic.Span.Start);
                        var error = line.Substring(diagnostic.Span.Start,diagnostic.Span.Length);
                        var suffix = line.Substring(diagnostic.Span.End);
                        
                        Console.Write("    ");
                        Console.Write(prefix);
                        
                        Console.ForegroundColor = ConsoleColor.DarkRed;
                        Console.WriteLine(error);
                        Console.ResetColor();
                        
                        Console.Write(suffix);
                        
                        Console.WriteLine();
                    }
                    
                    Console.WriteLine();
                }
            }
        }

        static void PrettyPrint(SyntaxNode node, string indent = "", bool isLast = true)
        {
            var marker = isLast ? "└──" : "├──";

            Console.Write(indent);
            Console.Write(marker);
            Console.Write(node.Kind);

            if(node is SyntaxToken t && t.Value != null)
            {
                Console.Write(" ");
                Console.Write(t.Value);
            }

            Console.WriteLine();

            indent += isLast ? "   " : "│  " ;

            var lastChild  = node.GetChildren().LastOrDefault();

            foreach(var child in node.GetChildren())
                PrettyPrint(child, indent, child == lastChild);
        }
    }
} 