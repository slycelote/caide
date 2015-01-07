using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide
{
    public class TestCase: IComparable<TestCase>, IEquatable<TestCase>
    {
        public string Name { get; set; }
        public string Input { get; set; }
        public string Output { get; set; }
        public bool IsSkipped { get; set; }
        public bool OutputIsKnown { get; set; }

        public override string ToString()
        {
            return Name;
        }

        public int CompareTo(TestCase that)
        {
            return StringComparer.CurrentCultureIgnoreCase.Compare(this.Name, that.Name);
        }

        public bool Equals(TestCase that)
        {
            return CompareTo(that) == 0;
        }

        public static List<TestCase> FromDirectory(string problemDirectory)
        {
            var res = new List<TestCase>();
            foreach (var inFile in Directory.EnumerateFiles(problemDirectory, "*.in"))
            {
                var testCase = new TestCase { Name = Path.GetFileNameWithoutExtension(inFile), Input = File.ReadAllText(inFile) };
                var outFile = Path.ChangeExtension(inFile, "out");
                var skippedFile = Path.ChangeExtension(inFile, "skip");
                if (File.Exists(outFile))
                {
                    testCase.Output = File.ReadAllText(outFile);
                    testCase.IsSkipped = false;
                    testCase.OutputIsKnown = true;
                }
                else if (File.Exists(skippedFile))
                {
                    testCase.Output = File.ReadAllText(skippedFile);
                    testCase.IsSkipped = true;
                    testCase.OutputIsKnown = true;
                }
                else
                {
                    testCase.IsSkipped = false;
                    testCase.OutputIsKnown = false;
                    var unknownOutputFile = Path.ChangeExtension(inFile, "unknown");
                    testCase.Output = File.Exists(unknownOutputFile) ? File.ReadAllText(unknownOutputFile) : "";
                }
                res.Add(testCase);
            }

            res.Sort();
            return res;
        }

        public static void WriteToDirectory(List<TestCase> testCases, string problemDirectory)
        {
            var allExtensions = new[] { "skip", "out", "unknown" };

            // Delete non-existent test cases
            foreach (var inFile in Directory.EnumerateFiles(problemDirectory, "*.in").ToList())
            {
                var testName = Path.GetFileNameWithoutExtension(inFile);
                if (!testCases.Any(t => StringComparer.CurrentCultureIgnoreCase.Equals(t.Name, testName)))
                {
                    File.Delete(inFile);
                    foreach (var ext in allExtensions)
                    {
                        var file = Path.Combine(problemDirectory, testName + "." + ext);
                        File.Delete(file);
                    }
                }
            }

            foreach (var testCase in testCases)
            {
                // write *.in file
                var inFile = Path.Combine(problemDirectory, testCase.Name + ".in");
                File.WriteAllText(inFile, testCase.Input);

                // write output to a file with appropriate extension, depending on test state
                string extension = testCase.IsSkipped ? "skip" : testCase.OutputIsKnown ? "out" : "unknown";
                var outFile = Path.Combine(problemDirectory, testCase.Name + "." + extension);
                File.WriteAllText(outFile, testCase.Output);

                // delete all other files for this test case
                var otherExtensions = allExtensions.Except(new[] { extension }).ToList();
                foreach (var ext in otherExtensions)
                {
                    var file = Path.Combine(problemDirectory, testCase.Name + "." + ext);
                    if (!StringComparer.CurrentCultureIgnoreCase.Equals(extension, Path.GetExtension(file)))
                    {
                        File.Delete(file);
                    }
                }
            }
        }


    }
}
