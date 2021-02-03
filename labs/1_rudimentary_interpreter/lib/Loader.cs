using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json;

namespace RudimentaryInterpreter
{
    internal class Loader
    {
        internal static List<TestCase> LoadJson(string file)
        {
            List<TestCase> testCases;
            using (StreamReader reader = new StreamReader(file))
            {
                string json = reader.ReadToEnd();
                testCases = JsonConvert.DeserializeObject<List<TestCase>>(json);
            }

            return testCases;
        }
    }
}