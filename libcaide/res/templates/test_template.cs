#region Structures predefined in LeetCode

// Definition for singly-linked list.
public class ListNode {
    public int val;
    public ListNode next;
    public ListNode(int val=0, ListNode next=null) {
        this.val = val;
        this.next = next;
    }
}

// Definition for a binary tree node.
public class TreeNode {
    public int val;
    public TreeNode left;
    public TreeNode right;
    public TreeNode(int val=0, TreeNode left=null, TreeNode right=null) {
        this.val = val;
        this.left = left;
        this.right = right;
    }
}

#endregion

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
                string inputFile = Path.Combine(testDir, testName + ".plain.in");
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
                    report.WriteLine(testName + getDuration() + " failed " + e.Message);
                    continue;
                }

                if (USE_CUSTOM_CHECKER)
                {
                    try
                    {
                        string etalonFile = Path.Combine(testDir, testName + ".plain.etalon");
                        string outputFile = origOutputFile;
                        if (IsTopcoder || IsLeetCode)
                        {
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
                        report.WriteLine(testName + getDuration() + " error Custom checker threw an exception: " + e.Message);
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

    public static TestSerializer<T[]> A<T>(TestSerializer<T> ser)
    {
        return new ArraySerializer<T>(ser);
    }

    public static TestSerializer<IList<T>> L<T>(TestSerializer<T> ser)
    {
        return new ListSerializer<T>(ser);
    }
}

interface TestSerializer<T>
{
    T Deserialize(TextReader input);
    void Serialize(TextWriter output, T val);
}

class intSerializer : TestSerializer<int>
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

class longSerializer : TestSerializer<long>
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

class doubleSerializer : TestSerializer<double>
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

class voidSerializer : TestSerializer<object>
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

class boolSerializer : TestSerializer<bool>
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

class stringSerializer : TestSerializer<string>
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

class ListSerializer<T> : TestSerializer<IList<T>>
{
    private TestSerializer<T> elemSerializer;

    public ListSerializer(TestSerializer<T> elemSerializer)
    {
        this.elemSerializer = elemSerializer;
    }

    public IList<T> Deserialize(TextReader input)
    {
        var len = int.Parse(input.ReadLine());
        var res = new List<T>(len);
        for (int i = 0; i < len; ++i)
        {
            res.Add(elemSerializer.Deserialize(input));
        }

        return res;
    }

    public void Serialize(TextWriter output, IList<T> val)
    {
        output.Write(TestRunner.IsTopcoder ? '{' : '[');
        for (int i = 0; i < val.Count; ++i)
        {
            if (i > 0)
            {
                output.Write(',');
            }

            elemSerializer.Serialize(output, val[i]);
        }

        output.Write(TestRunner.IsTopcoder ? '}' : ']');
    }
}

class ArraySerializer<T> : TestSerializer<T[]>
{
    private ListSerializer<T> listSerializer;

    public ArraySerializer(TestSerializer<T> elemSerializer)
    {
        this.listSerializer = new ListSerializer<T>(elemSerializer);
    }

    public T[] Deserialize(TextReader input)
    {
        return listSerializer.Deserialize(input).ToArray();
    }

    public void Serialize(TextWriter output, T[] val)
    {
        listSerializer.Serialize(output, new List<T>(val));
    }
}

class ListNodeSerializer : TestSerializer<ListNode>
{
    public ListNode Deserialize(TextReader input)
    {
        int[] vals = new ArraySerializer<int>(new intSerializer()).Deserialize(input);
        ListNode head = null, tail = null;
        foreach (int i in vals)
        {
            if (head == null)
            {
                head = tail = new ListNode();
            }
            else
            {
                tail.next = new ListNode();
                tail = tail.next;
            }

            tail.val = i;
        }

        return head;
    }

    public void Serialize(TextWriter output, ListNode val)
    {
        var vals = new List<int>();
        while (val != null)
        {
            vals.Add(val.val);
            val = val.next;
        }

        new ArraySerializer<int>(new intSerializer()).Serialize(output, vals.ToArray());
    }
}

class TreeNodeSerializer : TestSerializer<TreeNode>
{
    public TreeNode Deserialize(TextReader input)
    {
        var intReader = new intSerializer();
        int length = intReader.Deserialize(input);
        if (length == 0)
            return null;
        var root = new TreeNode();

        root.val = intReader.Deserialize(input);
        var q = new Queue<TreeNode>();
        q.Enqueue(root);
        --length;
        while (length > 0) {
            TreeNode node = q.Dequeue();
            var line = input.ReadLine();
            --length;
            if (line != "null") {
                node.left = new TreeNode(int.Parse(line));
                q.Enqueue(node.left);
            }

            if (length > 0) {
                line = input.ReadLine();
                --length;
                if (line != "null") {
                    node.right = new TreeNode(int.Parse(line));
                    q.Enqueue(node.right);
                }
            }
        }

        return root;
    }

    public void Serialize(TextWriter output, TreeNode val)
    {
        output.Write('[');
        var layer = new List<TreeNode>{val};
        for (;;)
        {
            while (layer.Count > 0 && layer[layer.Count - 1] == null)
                layer.RemoveAt(layer.Count - 1);
            if (layer.Count == 0)
                break;

            var nextLayer = new List<TreeNode>();
            foreach (var node in layer)
            {
                if (node != val)
                    output.Write(',');
                if (node == null)
                    output.Write("null");
                else
                {
                    output.Write(node.val);
                    nextLayer.Add(node.left);
                    nextLayer.Add(node.right);
                }
            }

            layer = nextLayer;
        }

        output.Write(']');
    }
}

}
