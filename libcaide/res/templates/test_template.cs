using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

// Disable 'unreachable code' warning
#pragma warning disable 162

static partial class CaideTester
{
    public static bool ENABLE_CUSTOM_CHECKER = false;
    public static bool CustomCheck(TextReader input, TextReader output)
    {
        ParseInputAndOutput(input, output);

        // Implement the checker here.
        // Use static variables of this class
        //    (named the same as parameters of the solution method) for a Topcoder problem;
        // use input and output streams for a classical problem.
        // Return true if the result is correct.

        return true;
    }
}


public class Test
{
    private static string RunTest(string inputFileName, string outputFileName)
    {
        TextWriter output = new StringWriter();

        using (TextReader input = new StreamReader(inputFileName))
        {
            CaideTester.Solve(input, output);
        }

        // save program output
        string result = output.ToString();
        File.WriteAllText(outputFileName, result);
        return result;
    }


    //-----------------------------------------------------------------------------//
    private static Process Run(string exe, string args)
    {
        var psi = new ProcessStartInfo(exe, args)
        {
            UseShellExecute = false,
            WorkingDirectory = Directory.GetCurrentDirectory(),
        };
        return Process.Start(psi);
    }


    public static void Main(string[] args)
    {
        // Read path to caide executable from a file in current directory
        string testDir = ".";
        string caideExeFile = Path.Combine(testDir, "caideExe.txt");
        if (!File.Exists(caideExeFile))
        {
            testDir = Path.Combine(".caideproblem", "test");
            caideExeFile = Path.Combine(testDir, "caideExe.txt");
        }
        if (!File.Exists(caideExeFile))
        {
            throw new InvalidOperationException("Test musts be run from problem directory");
        }
        string caideExe = File.ReadAllText(caideExeFile).Trim();

        // Prepare the list of test cases in correct order; add recently created test cases too.
        Process updateTestsProcess = Run(caideExe, "update_tests");
        updateTestsProcess.WaitForExit();
        if (updateTestsProcess.ExitCode != 0)
        {
            Console.Error.WriteLine("caide update_tests returned non-zero error code " + updateTestsProcess.ExitCode);
        }

        StringWriter report = new StringWriter();

        // Process each test case described in a file in current directory
        foreach (string line in File.ReadAllLines(Path.Combine(testDir, "testList.txt")))
        {
            string[] words = line.Split(' ');
            string testName = words[0], testState = words[1];
            if (testState == "Skip")
            {
                Console.Error.WriteLine("Skipping test " + testName);
                report.WriteLine(testName + " skipped");
            }
            else if (testState == "Run")
            {
                Console.Error.WriteLine("Running test " + testName);
                string inputFile = Path.Combine(testDir, testName + ".in");

                string result = null;
                var start = DateTime.Now;
                Func<string> getDuration = () => " #time:" +
                    ((int)(DateTime.Now - start).TotalMilliseconds).ToString() + "ms";
                try
                {
                    result = RunTest(inputFile, Path.Combine(testDir, testName + ".out"));
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine("Test " + testName + " threw an exception: " + e);
                    report.WriteLine(testName + getDuration() + " failed");
                    continue;
                }

                if (CaideTester.ENABLE_CUSTOM_CHECKER)
                {
                    try
                    {
                        using (StringReader output = new StringReader(result))
                        using (StreamReader input = new StreamReader(inputFile))
                        {
                            bool ok = CaideTester.CustomCheck(input, output);
                            if (ok)
                            {
                                report.WriteLine(testName + getDuration() + " OK");
                            }
                            else
                            {
                                Console.Error.WriteLine("Test " + testName + " failed!");
                                report.WriteLine(testName + getDuration() + " failed");
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        Console.Error.WriteLine("Checker for test " + testName + " threw an exception: " + e.Message);
                        report.WriteLine(testName + getDuration() + " error");
                    }
                }
                else
                {
                    report.WriteLine(testName + getDuration() + " ran");
                }

                if (result.Length > 200)
                    result = result.Substring(0, 200) + " [...] (output truncated)\n";
                Console.Error.WriteLine(result);
            }
            else
            {
                report.WriteLine(testName + " error unknown test status");
            }
        }

        File.WriteAllText(Path.Combine(testDir, "report.txt"), report.ToString());

        // optional: evaluate tests automatically
        Process evalTestsProcess = Run(caideExe, "eval_tests");
        evalTestsProcess.WaitForExit();
        Environment.Exit(evalTestsProcess.ExitCode);
    }
}

#region Topcoder (de)serialization
namespace Caide
{
    interface TCSerializer<T>
    {
        T Deserialize(TextReader input);
        void Serialize(TextWriter output, T val);
    }

    static class S
    {
        public static TCSerializer<T[]> v<T>(TCSerializer<T> ser)
        {
            return new ArraySerializer<T>(ser);
        }
    }

    static class TCSerializeUtil
    {
        public static char PeekChar(this TextReader input)
        {
            int i = input.Peek();
            if (i < 0)
            {
                throw new IOException("Unexpected end of file");
            }
            return (char)i;
        }

        public static char ReadChar(this TextReader input)
        {
            int i = input.Read();
            if (i < 0)
            {
                throw new IOException("Unexpected end of file");
            }
            return (char)i;
        }

        public static string SkipNumber(this TextReader input)
        {
            return input.SkipWhile(c => "0123456789-.eE".IndexOf(c) >= 0);
        }

        public static void Consume(this TextReader input, char c)
        {
            if (input.ReadChar() != c)
            {
                throw new IOException("Expected char '" + c + "'");
            }
        }

        public static string SkipWhile(this TextReader input, Func<char, bool> predicate)
        {
            StringBuilder res = new StringBuilder();
            while (true)
            {
                int ci = input.Peek();
                if (ci < 0)
                {
                    return res.ToString();
                }

                char c = (char)ci;
                if (!predicate(c))
                {
                    return res.ToString();
                }

                res.Append(c);
                input.Read();
            }
        }

        public static string SkipUpTo(this TextReader input, Func<char, bool> predicate)
        {
            StringBuilder res = new StringBuilder();
            while (true)
            {
                char c = input.ReadChar();
                res.Append(c);
                if (predicate(c))
                {
                    return res.ToString();
                }
            }
        }
    }

    class IntSerializer : TCSerializer<int>
    {
        public int Deserialize(TextReader input)
        {
            input.SkipWhile(char.IsWhiteSpace);
            string s = input.SkipNumber();
            return int.Parse(s);
        }

        public void Serialize(TextWriter output, int val)
        {
            output.Write(val);
        }
    }

    class LongSerializer : TCSerializer<long>
    {
        public long Deserialize(TextReader input)
        {
            input.SkipWhile(char.IsWhiteSpace);
            string s = input.SkipNumber();
            return long.Parse(s);
        }

        public void Serialize(TextWriter output, long val)
        {
            output.Write(val);
        }
    }

    class DoubleSerializer : TCSerializer<double>
    {
        public double Deserialize(TextReader input)
        {
            input.SkipWhile(char.IsWhiteSpace);
            string s = input.SkipNumber();
            return double.Parse(s);
        }

        public void Serialize(TextWriter output, double val)
        {
            output.Write(val);
        }
    }

    class BoolSerializer : TCSerializer<bool>
    {
        public bool Deserialize(TextReader input)
        {
            input.SkipWhile(char.IsWhiteSpace);
            StringBuilder s = new StringBuilder();
            for (int i = 0; i < 4; ++i)
                s.Append(input.ReadChar());
            if (s.ToString() == "true")
                return true;
            else if (s.ToString() == "fals" && input.ReadChar() == 'e')
                return false;
            else
                throw new IOException("true or fales is expected");
        }

        public void Serialize(TextWriter output, bool val)
        {
            output.Write(val ? "true" : "false");
        }
    }

    class StringSerializer : TCSerializer<string>
    {
        public string Deserialize(TextReader input)
        {
            input.SkipWhile(char.IsWhiteSpace);
            input.Consume('"');
            string res = input.SkipWhile(c => c != '"');
            input.Consume('"');
            return res;
        }

        public void Serialize(TextWriter output, string val)
        {
            output.Write('"');
            output.Write(val);
            output.Write('"');
        }
    }

    class ArraySerializer<T> : TCSerializer<T[]>
    {
        private TCSerializer<T> elemSerializer;

        public ArraySerializer(TCSerializer<T> elemSerializer)
        {
            this.elemSerializer = elemSerializer;
        }

        public T[] Deserialize(TextReader input)
        {
            List<T> res = new List<T>();
            input.SkipWhile(char.IsWhiteSpace);
            char open = input.ReadChar();
            if (open != '{' && open != '[') // Topcoder uses {} for arrays, and LeetCode -- []
            {
                throw new IOException("{ or [ character is expected");
            }

            while (true)
            {
                input.SkipWhile(char.IsWhiteSpace);
                char c = input.PeekChar();
                if (c == '}' || c == ']')
                {
                    input.Read();
                    break;
                }
                if (res.Any())
                {
                    if (c != ',')
                    {
                        throw new IOException("A comma is expected");
                    }
                    input.Read();
                }

                input.SkipWhile(char.IsWhiteSpace);
                res.Add(elemSerializer.Deserialize(input));
            }
            return res.ToArray();
        }

        public void Serialize(TextWriter output, T[] val)
        {
            output.Write('{');
            for (int i = 0; i < val.Length; ++i)
            {
                if (i > 0)
                {
                    output.Write(',');
                }
                elemSerializer.Serialize(output, val[i]);
            }
            output.Write('}');
        }
    }
}
#endregion
