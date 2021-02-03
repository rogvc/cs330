using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace RudimentaryInterpreter
{
    internal class Interpreter
    {
        internal static Expression Parse(in string input)
        {
            Expression response;
            var expression = "";
            try
            {
                if (input[0].Equals('('))
                {
                    expression = input.Substring(1, input.Length - 2); // Skips the opening and closing parenthesis
                }
                else
                {
                    expression = input;
                }

                var nextTokenString = "";
                var nextToken = ExtractNextToken(expression, out nextTokenString);

                string leftHandSide;
                string rightHandSide;

                switch (nextToken)
                {
                    case Token.Number:
                        response = new Number(Double.Parse(nextTokenString));
                        break;
                    case Token.Symbol:
                        response = new Symbol(nextTokenString);
                        break;
                    case Token.Plus:
                        RemoveNextToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Addition(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case Token.Minus:
                        RemoveNextToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Subtraction(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case Token.Asterisk:
                        RemoveNextToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Multiplication(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case Token.ForwardSlash:
                        RemoveNextToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Division(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case Token.With:

                    // break;
                    default:
                        response = new Symbol("Syntax Error");
                        break;
                }
            }
            catch (Exception e)
            {
                response = new Symbol(e.ToString());
            }

            return response;
        }

        private static void ExtractExpressionSides(in string expression, out string leftHandSide, out string rightHandSide)
        {
            var subExpression = expression;

            var alteredChars = 0;
            if (subExpression[0].Equals('('))
            {
                leftHandSide = subExpression.Substring(0, subExpression.LastIndexOf(')') + 1);
                alteredChars = leftHandSide.Length;
            }
            else
            {
                ExtractNextToken(subExpression, out leftHandSide);
                alteredChars = leftHandSide.Length;
            }

            subExpression = subExpression.Substring(++alteredChars);

            if (subExpression[0].Equals('('))
            {
                rightHandSide = subExpression.Substring(0, subExpression.LastIndexOf(')') + 1);
                alteredChars = rightHandSide.Length;
            }
            else
            {
                ExtractNextToken(subExpression, out rightHandSide);
                alteredChars = rightHandSide.Length;
            }
        }

        private static void RemoveNextToken(ref string expression)
        {
            var backupExpression = expression;
            try
            {
                var nextWhitespace = IndexOfNextWhitespace(expression);
                if (nextWhitespace != -1)
                {
                    expression = expression.Substring(nextWhitespace + 1);
                }
            }
            catch (Exception)
            {
                expression = backupExpression;
            }
        }

        private enum Token
        {
            Number,
            Symbol,
            With,
            Plus,
            Minus,
            Asterisk,
            ForwardSlash,
            EOF,
            Error,
        }

        private static Token ExtractNextToken(in string expression, out string tokenString)
        {
            Token response;
            try
            {
                var nextWhitespace = IndexOfNextWhitespace(expression);
                if (nextWhitespace == -1)
                {
                    tokenString = expression;
                    response = ParseNextToken(tokenString);
                }
                else
                {
                    tokenString = expression.Substring(0, nextWhitespace);
                    response = ParseNextToken(tokenString);
                }
            }
            catch (Exception)
            {
                response = Token.Error;
                tokenString = expression;
            }

            return response;
        }

        private static Token ParseNextToken(in string value)
        {
            Token response;
            if (value.Equals("with"))
            {
                response = Token.With;
            }
            else if (value.Equals("+"))
            {
                response = Token.Plus;
            }
            else if (value.Equals("-"))
            {
                response = Token.Minus;
            }
            else if (value.Equals("*"))
            {
                response = Token.Asterisk;
            }
            else if (value.Equals("/"))
            {
                response = Token.ForwardSlash;
            }
            else
            {
                try
                {
                    Double.Parse(value);
                    response = Token.Number;
                }
                catch (Exception)
                {
                    response = Token.Symbol;
                }
            }

            return response;
        }

        private static int IndexOfNextWhitespace(in string expression)
        {
            int response = -1;

            try
            {
                for (var i = 0; i < expression.Length; i++)
                {
                    if (Char.IsWhiteSpace(expression[i]))
                    {
                        response = i;
                        break;
                    }
                }
            }
            catch (Exception)
            {
                response = -1;
            }

            return response;
        }

    }
}
