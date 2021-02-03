using System;

namespace RudimentaryInterpreter
{
    internal class Program
    {
        static void Main(string[] args)
        {
            var testCases = Loader.LoadJson(@"Resources\Tests.json");
            testCases.ForEach(test =>
            {
                var result = Interpreter.Interpret(test.Input);
                Console.WriteLine(test);
                Console.Write(String.Format("Result: {0} | ", result));

                if (result.ToString().Equals(test.ExpectedResult))
                {
                    Console.ForegroundColor = ConsoleColor.Green;
                    Console.WriteLine("GOOD");
                }
                else
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.WriteLine("BAD");
                }

                Console.ForegroundColor = ConsoleColor.Gray;
                Console.WriteLine();
            });
        }

    }

    public class TestCase
    {
        public string TestName { get; set; }
        public string Input { get; set; }
        public string ExpectedResult { get; set; }

        public override string ToString() => String.Format("{0}: {1} should be {2}", TestName, Input, ExpectedResult);
    }
}