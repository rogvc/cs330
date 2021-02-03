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
                    response = Parser.Parse(input);
                    break;
                case "calc":
                default:
                    response = new Symbol("Syntax Error");
                    break;
            }

            return response;
        }
    }
}
