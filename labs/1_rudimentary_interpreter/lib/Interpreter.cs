using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace RudimentaryInterpreter
{
    internal class Interpreter
    {
        internal static Expression Parse(string input)
        {
            Expression response;
            try
            {
                var subString = "";
                if (Char.IsDigit(input[0]))
                {
                    response = new Number(Double.Parse(input[0].ToString()));
                }
                else if (input[0].Equals('('))
                {
                    subString = input.Substring(1, input.Length - 2); // Skips the opening and closing parenthesis
                    response = Parse(subString);
                }
                else
                {
                    response = new Symbol("error");
                }
                // else if (input[0].Equals('(')) {
                //     response = Parse(input.Substring(1));
                // }
                // // else if (input[0].Equals('+')) {
                // //     response = 
                // // }
                // expression.ForEach(c =>
                // {
                //     if (c.Equals('('))
                //     {
                //         ParseInnerExpression();
                //     }
                // });
            }
            catch (Exception e)
            {
                response = new Symbol(e.ToString());
            }

            return response;
        }
    }
}
