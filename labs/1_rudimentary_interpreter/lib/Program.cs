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
                var result = Interpreter.Parse(test.Input);
                Console.WriteLine(test);
                Console.Write(String.Format("Result: {0}.\t", result));

                if (result.ToString().Equals(test.ExpectedResult))
                {
                    Console.ForegroundColor = ConsoleColor.Green;
                    Console.WriteLine("Good.");
                }
                else
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.WriteLine("Bad.");
                }

                Console.ForegroundColor = ConsoleColor.White;
            });
        }

    }

    public class TestCase
    {
        public string FunctionName { get; set; }
        public string Input { get; set; }
        public string ExpectedResult { get; set; }

        public override string ToString() => String.Format("{0}: {1} should be {2}", FunctionName, Input, ExpectedResult);
    }
}