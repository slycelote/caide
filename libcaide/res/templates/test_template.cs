namespace CaideTester
{

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;


partial class TestRunner
{
    private static Process Run(string exe, params string[] args)
    {
        var sb = new StringBuilder();
        for (int i = 0; i < args.Length; ++i) {
            if (i > 0) sb.Append(' ');
            sb.Append(args[i]);
        }
        var psi = new ProcessStartInfo(exe, sb.ToString())
        {
            UseShellExecute = false,
            WorkingDirectory = Directory.GetCurrentDirectory(),
        };

        var process = Process.Start(psi);
        process.WaitForExit();
        return process;
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
        if (updateTestsProcess.ExitCode != 0)
        {
            Console.Error.WriteLine("caide update_tests returned non-zero error code " + updateTestsProcess.ExitCode);
        }

        StringWriter report = new StringWriter();

        // Process each test case described in a file in current directory.
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
                string origInputFile = Path.Combine(testDir, testName + ".in");
                string inputFile = origInputFile;
                if (IsTopcoder || IsLeetCode) {
                    inputFile = Path.Combine(testDir, "..", "..", testName + ".in");
                    Process p = Run(caideExe, "convert_test_input", origInputFile, inputFile);
                    if (p.ExitCode != 0) {
                        Console.Error.WriteLine("Test converter for test " + testName + " failed");
                        report.WriteLine(testName + " error");
                        continue;
                    }
                }

                string origOutputFile = Path.Combine(testDir, testName + ".out");
                var start = DateTime.Now;
                Func<string> getDuration = () => " #time:" +
                    ((int)(DateTime.Now - start).TotalMilliseconds).ToString() + "ms";
                try
                {
                    using (var reader = new StreamReader(inputFile))
                    using (var writer = new StreamWriter(origOutputFile))
                        Solve(reader, writer);
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine("Test " + testName + " threw an exception: " + e);
                    report.WriteLine(testName + getDuration() + " failed");
                    continue;
                }

                if (USE_CUSTOM_CHECKER)
                {
                    try
                    {
                        string origEtalonFile = Path.Combine(testDir, "..", "..", testName + ".out");
                        string etalonFile = origEtalonFile, outputFile = origOutputFile;
                        if (IsTopcoder || IsLeetCode)
                        {
                            etalonFile = Path.Combine(testDir, testName + ".plain.etalon");
                            Run(caideExe, "convert_test_output", origEtalonFile, etalonFile);
                            outputFile = Path.Combine(testDir, testName + ".plain.out");
                            Run(caideExe, "convert_test_output", origOutputFile, outputFile);
                        }

                        using (var input = new StreamReader(inputFile))
                        using (var userOutput = new StreamReader(outputFile))
                        using (var judgeOutput = new StreamReader(etalonFile))
                        {
                            string error = CustomCheck(input, userOutput, judgeOutput);
                            if (error == null)
                            {
                                report.WriteLine(testName + getDuration() + " OK");
                            }
                            else
                            {
                                Console.Error.WriteLine("Test " + testName + " failed!");
                                report.WriteLine(testName + getDuration() + " failed " + error);
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

                using (var reader = new StreamReader(origOutputFile))
                {
                    var fileLen = new FileInfo(origOutputFile).Length;
                    var len = (int)Math.Min(fileLen, 200);
                    var buf = new char[len];
                    reader.Read(buf, 0, len);
                    Console.Error.Write(new string(buf));
                    if (fileLen > len)
                        Console.Error.Write(" [...] (output truncated)");
                    Console.Error.WriteLine();
                }
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

    public static TCSerializer<T[]> V<T>(TCSerializer<T> ser)
    {
        return new ArraySerializer<T>(ser);
    }
}

interface TCSerializer<T>
{
    T Deserialize(TextReader input);
    void Serialize(TextWriter output, T val);
}

class IntSerializer : TCSerializer<int>
{
    public int Deserialize(TextReader input)
    {
        return int.Parse(input.ReadLine());
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
        return long.Parse(input.ReadLine());
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
        return double.Parse(input.ReadLine());
    }

    public void Serialize(TextWriter output, double val)
    {
        output.Write(val);
    }
}

class NullSerializer : TCSerializer<object>
{
    public object Deserialize(TextReader input)
    {
        if (input.ReadLine() != "null")
            throw new IOException("null expected");
        return null;
    }

    public void Serialize(TextWriter output, object val)
    {
        output.Write("null");
    }
}

class BoolSerializer : TCSerializer<bool>
{
    public bool Deserialize(TextReader input)
    {
        var s = input.ReadLine();
        if (s == "true")
            return true;
        else if (s == "false")
            return false;
        else
            throw new IOException("true or false is expected");
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
        return input.ReadLine();
    }

    public void Serialize(TextWriter output, string val)
    {
        output.Write('"');
        output.Write(val);
        output.WriteLine('"');
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
        var len = int.Parse(input.ReadLine());
        T[] res = new T[len];
        for (int i = 0; i < len; ++i)
        {
            res[i] = elemSerializer.Deserialize(input);
        }

        return res;
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
