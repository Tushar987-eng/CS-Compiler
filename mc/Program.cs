using System;
using System.Collections.Generic;
using System.Linq;

using Compiler.CodeAnalysis.Binding;
using Compiler.CodeAnalysis.Syntax;
using Compiler.CodeAnalysis;

namespace Compiler 
{
    internal static class Program {
        private static void Main () {

            var showTree = false;
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
                var binder = new Binder();
                var boundExpression = binder.BindExpression(syntaxTree.Root);

                var diagnostics = syntaxTree.Diagnostics.Concat(binder.Diagnostics).ToArray();
                
                if(showTree)
                {
                    Console.ForegroundColor = ConsoleColor.DarkGray; 
                    PrettyPrint(syntaxTree.Root);
                    Console.ResetColor();
                }

                if (!diagnostics.Any())
                {
                    var e = new Evaluator(boundExpression);
                    var result = e.Evaluate(); 
                    Console.WriteLine(result);
                }
                else
                {
                    Console.ForegroundColor = ConsoleColor.DarkRed;

                    foreach (var diagnostic in diagnostics)
                        Console.WriteLine(diagnostic);

                    Console.ResetColor();
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
   
