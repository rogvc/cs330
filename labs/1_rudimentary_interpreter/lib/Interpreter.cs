using System;

namespace RudimentaryInterpreter
{
    internal class Interpreter
    {
        internal static Expression Interpret(in string toInterpret)
        {
            Expression response;

            var command = toInterpret.Substring(0, toInterpret.IndexOf('(')).ToLower();
            var input = toInterpret.Substring(command.Length);

            switch (command)
            {
                case "parse":
                    response = Parse(input);
                    break;
                case "calc":
                    input = input.Substring(1, input.LastIndexOf(')') - 1);
                    response = Calculate(input);
                    break;
                default:
                    response = new Symbol("Syntax Error");
                    break;
            }

            return response;
        }

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
                var nextToken = ExtractNextParsingToken(expression, out nextTokenString);

                string leftHandSide;
                string rightHandSide;

                switch (nextToken)
                {
                    case ParsingToken.Number:
                        response = new Number(Double.Parse(nextTokenString));
                        break;
                    case ParsingToken.Symbol:
                        response = new Symbol(nextTokenString);
                        break;
                    case ParsingToken.Plus:
                        RemoveNextParsingToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Addition(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case ParsingToken.Minus:
                        RemoveNextParsingToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Subtraction(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case ParsingToken.Asterisk:
                        RemoveNextParsingToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Multiplication(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case ParsingToken.ForwardSlash:
                        RemoveNextParsingToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Division(Parse(leftHandSide), Parse(rightHandSide));
                        break;
                    case ParsingToken.With:
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

        internal static Expression Calculate(in string input)
        {
            Expression response;

            try
            {
                var expression = input;

                if (input.Contains('('))
                {
                    var command = input.Substring(0, input.IndexOf('(')).ToLower();


                    if (command.Equals("parse"))
                    {
                        expression = input.Substring(command.Length);
                        expression = Parse(expression).ToString();
                    }
                }


                var nextTokenString = "";
                var nextToken = ExtractNextOperationToken(expression, out nextTokenString);

                var leftHandSide = "";
                var rightHandSide = "";
                switch (nextToken)
                {
                    case ParsingToken.Number:
                        response = new Number(Double.Parse(nextTokenString));
                        break;
                    case ParsingToken.Symbol:
                        response = new Symbol(nextTokenString);
                        break;
                    case ParsingToken.Plus:
                        RemoveNextOperationToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Number(Calculate(leftHandSide).GetValue() + Calculate(rightHandSide).GetValue());
                        break;
                    case ParsingToken.Minus:
                        RemoveNextOperationToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Number(Calculate(leftHandSide).GetValue() - Calculate(rightHandSide).GetValue());
                        break;
                    case ParsingToken.Asterisk:
                        RemoveNextOperationToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Number(Calculate(leftHandSide).GetValue() * Calculate(rightHandSide).GetValue());
                        break;
                    case ParsingToken.ForwardSlash:
                        RemoveNextOperationToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        response = new Number(Calculate(leftHandSide).GetValue() / Calculate(rightHandSide).GetValue());
                        break;
                    case ParsingToken.With:
                        RemoveNextOperationToken(ref expression);
                        ExtractExpressionSides(expression, out leftHandSide, out rightHandSide);
                        var parameters = leftHandSide.Split(' ', 100);
                        rightHandSide = rightHandSide.Replace(parameters[0], parameters[1]);
                        response = new Number(Calculate(rightHandSide).GetValue());
                        break;
                    default:
                        response = new Symbol("Syntax Error");
                        break;
                }
            }
            catch (Exception)
            {
                response = new Symbol("Syntax Error");
            }

            return response;
        }

        private static void ExtractExpressionSides(in string expression, out string leftHandSide, out string rightHandSide, bool isWithOperator = false)
        {
            var subExpression = expression;

            var alteredChars = 0;
            if (subExpression.StartsWith('['))
            {
                leftHandSide = subExpression.Substring(1, subExpression.IndexOf(']') - 1);
                alteredChars = leftHandSide.Length + "[]".Length;
            }
            else if (IsAnOperation(expression))
            {
                leftHandSide = subExpression.Substring(0, subExpression.IndexOf(')') + 1);
                alteredChars = leftHandSide.Length;
            }
            else if (isWithOperator)
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
                ExtractNextParsingToken(subExpression, out leftHandSide);
                alteredChars = leftHandSide.Length;
            }

            rightHandSide = subExpression.Substring(++alteredChars);

            // if (subExpression[0].Equals('('))
            // {
            //     rightHandSide = subExpression.Substring(0, subExpression.LastIndexOf(')') + 1);
            //     alteredChars = rightHandSide.Length;
            // }
            // else
            // {
            //     ExtractNextParsingToken(subExpression, out rightHandSide);
            //     alteredChars = rightHandSide.Length;
            // }
        }

        private static void RemoveNextParsingToken(ref string expression)
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

        private enum ParsingToken
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

        private static ParsingToken ExtractNextParsingToken(in string expression, out string tokenString)
        {
            ParsingToken response;
            try
            {
                if (expression[0].Equals('('))
                {
                    tokenString = expression.Substring(0, expression.LastIndexOf(')') + 1);
                    response = ParseNextParsingToken(tokenString.Substring(1));
                }
                else
                {
                    var nextWhitespace = IndexOfNextWhitespace(expression);
                    if (nextWhitespace == -1)
                    {
                        tokenString = expression;
                        response = ParseNextParsingToken(tokenString);
                    }
                    else
                    {
                        tokenString = expression.Substring(0, nextWhitespace);
                        response = ParseNextParsingToken(tokenString);
                    }
                }
            }
            catch (Exception)
            {
                response = ParsingToken.Error;
                tokenString = expression;
            }

            return response;
        }

        private static ParsingToken ParseNextParsingToken(in string value)
        {
            ParsingToken response;
            if (value.Equals("+"))
            {
                response = ParsingToken.Plus;
            }
            else if (value.Equals("-"))
            {
                response = ParsingToken.Minus;
            }
            else if (value.Equals("*"))
            {
                response = ParsingToken.Asterisk;
            }
            else if (value.Equals("/"))
            {
                response = ParsingToken.ForwardSlash;
            }
            else
            {
                try
                {
                    Double.Parse(value);
                    response = ParsingToken.Number;
                }
                catch (Exception)
                {
                    if (IsWithOperator(value))
                    {
                        response = ParsingToken.With;
                    }
                    else
                    {
                        response = ParsingToken.Symbol;
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

        private static ParsingToken ExtractNextOperationToken(in string expression, out string tokenString)
        {
            ParsingToken response;
            try
            {
                if (expression[0].Equals('('))
                {
                    tokenString = expression.Substring(0, expression.LastIndexOf(')') + 1);
                    response = ParseNextOperationToken(tokenString.Substring(1));
                }
                else
                {

                    var nextOpenParenthesis = expression.IndexOf('(');
                    if (nextOpenParenthesis == -1)
                    {
                        tokenString = expression;
                        response = ParseNextOperationToken(tokenString);
                    }
                    else
                    {
                        tokenString = expression.Substring(0, expression.IndexOf('('));
                        response = ParseNextOperationToken(tokenString);
                    }


                }
            }
            catch (Exception)
            {
                response = ParsingToken.Error;
                tokenString = expression;
            }

            return response;
        }

        private static ParsingToken ParseNextOperationToken(in string value)
        {
            ParsingToken response;
            if (value.Equals("Addition"))
            {
                response = ParsingToken.Plus;
            }
            else if (value.Equals("Subtraction"))
            {
                response = ParsingToken.Minus;
            }
            else if (value.Equals("Multiplication"))
            {
                response = ParsingToken.Asterisk;
            }
            else if (value.Equals("Division"))
            {
                response = ParsingToken.ForwardSlash;
            }
            else
            {
                try
                {
                    Double.Parse(value);
                    response = ParsingToken.Number;
                }
                catch (Exception)
                {
                    if (IsASubstitution(value))
                    {
                        response = ParsingToken.With;
                    }
                    else
                    {
                        response = ParsingToken.Symbol;
                    }

                }
            }

            return response;
        }


        private static void RemoveNextOperationToken(ref string expression)
        {
            var backupExpression = expression;
            try
            {
                var nextOpenParenthesis = expression.IndexOf('(');
                if (nextOpenParenthesis != -1)
                {
                    expression = expression.Substring(nextOpenParenthesis + 1);
                    expression = expression.Substring(0, expression.LastIndexOf(')')); // Removes both parenthesis
                }
            }
            catch (Exception)
            {
                expression = backupExpression;
            }
        }


        private static bool IsAnOperation(string value)
            => value.Contains("Addition")
            || value.Contains("Subtraction")
            || value.Contains("Multiplication")
            || value.Contains("Division")
            || value.Contains("Substitution");

        private static bool IsASubstitution(string value)
            => value.Equals("Substitution");
    }

}
