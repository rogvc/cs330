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
                if (Char.IsDigit(input[0]))
                {
                    response = new Number(Double.Parse(input[0].ToString()));
                }
                else
                {
                    response = new Symbol(false);
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
