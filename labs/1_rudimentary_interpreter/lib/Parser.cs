using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace RudimentaryInterpreter
{
    internal class Parser
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
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide, true);
                        response = new Substitution(new Symbol(leftHandSide), Parse(rightHandSide));
                        break;
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

        private static void ExtractExpressionSides(in string expression, out string leftHandSide, out string rightHandSide, bool isWithOperator = false)
        {
            var subExpression = expression;

            var alteredChars = 0;
            if (isWithOperator)
            {
                leftHandSide = subExpression.Substring(subExpression.IndexOf('['));
                leftHandSide = leftHandSide.Split(')', 100)[0];
                alteredChars = leftHandSide.Length + "with()".Length;
            }
            else if (subExpression[0].Equals('('))
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
                if (expression[0].Equals('('))
                {
                    tokenString = expression.Substring(0, expression.LastIndexOf(')') + 1);
                    response = ParseNextToken(tokenString.Substring(1));
                }
                else
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
            if (value.Equals("+"))
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
                    if (IsWithOperator(value))
                    {
                        response = Token.With;
                    }
                    else
                    {
                        response = Token.Symbol;
                    }

                }
            }

            return response;
        }

        private static bool IsWithOperator(string value)
        {
            bool response = false;
            try
            {
                response = value.Substring(0, 4).Equals("with");
            }
            catch (Exception)
            {
                response = false;
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