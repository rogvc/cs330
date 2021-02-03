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
                Console.WriteLine(test);
                Console.WriteLine(String.Format("Result: {0}.", Interpreter.Parse(test.Input)));
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