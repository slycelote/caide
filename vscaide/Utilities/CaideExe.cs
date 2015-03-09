using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    using MessageBox = System.Windows.Forms.MessageBox;

    internal class ReadStreamToOutputWindow
    {
        private readonly StreamReader Reader;
        private readonly CancellationToken CancelToken;
        
        internal ReadStreamToOutputWindow(StreamReader reader, CancellationToken cancelToken)
        {
            this.Reader = reader;
            this.CancelToken = cancelToken;
        }

        internal Task RunAsync()
        {
            return Task.Run(() =>
            {
                var outputWindow = Services.GeneralOutputWindow;
                try
                {
                    for (;;)
                    {
                        if (CancelToken.IsCancellationRequested)
                            return;

                        var readTask = Reader.ReadLineAsync();
                        if (readTask.Wait(1000, CancelToken))
                        {
                            outputWindow.OutputString(readTask.Result);
                        }
                    }
                }
                catch (OperationCanceledException) { }
            });
        }
    }

    public abstract class CaideExe
    {
        public const int EXIT_CODE_TIMEOUT = -4451;

        public static string Run(string[] args, bool loud = false, string solutionDir = null)
        {
            if (solutionDir == null)
            {
                solutionDir = SolutionUtilities.GetSolutionDir();
            }

            string stdout, stderr;
            int ret = CaideExe.Execute(args, solutionDir, out stdout, out stderr);
            if (ret != 0)
            {
                Logger.LogError("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr);
                if (loud)
                {
                    MessageBox.Show(string.Format("caide.exe error. Return code {0}\n{1}\n{2}", ret, stdout, stderr));
                }
                return null;
            }

            return stdout;
        }

        public static string Run(params string[] args)
        {
            return Run(args, loud: false);
        }


        public static int Execute(string[] args, string directory, out string stdout, out string stderr)
        {
            if (Paths.CaideExe == null)
            {
                stderr = "Couldn't find caide.exe";
                stdout = "";
                return 1;
            }

            var psi = new ProcessStartInfo
            {
                FileName = Paths.CaideExe,
                Arguments = CreateCommandLine(args),
                WorkingDirectory = directory,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            };

            using (var process = Process.Start(psi))
            {
                var stdOutTask = process.StandardOutput.ReadToEndAsync();
                var stdErrTask = process.StandardError.ReadToEndAsync();

                var waitTime = 30 * 1000;
                if (process.WaitForExit(waitTime))
                {
                    stdout = stdOutTask.Result;
                    stderr = stdErrTask.Result;
                    return process.ExitCode;
                }

                stdout = "";
                stderr = "";

                return EXIT_CODE_TIMEOUT;
            }
        }

        private static string CreateCommandLine(IEnumerable<string> arguments)
        {
            return string.Join(" ", arguments.Select(EscapeCommandLineArgument));
        }

        // http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx
        private static string EscapeCommandLineArgument(string arg)
        {
            var escaped = new StringBuilder();
            escaped.Append('"');

            for (int i = 0; ; ++i)
            {
                int numBackslashes = 0;

                while (i < arg.Length && arg[i] == '\\')
                {
                    ++i;
                    ++numBackslashes;
                }

                if (i == arg.Length)
                {
                    // Escape all backslashes, but let the terminating double quotation mark we add below
                    // be interpreted as a metacharacter.
                    escaped.Append('\\', numBackslashes * 2);
                    break;
                }
                else if (arg[i] == '"')
                {
                    // Escape all backslashes and the following double quotation mark.
                    escaped.Append('\\', numBackslashes * 2 + 1);
                    escaped.Append(arg[i]);
                }
                else
                {
                    // Backslashes aren't special here.
                    escaped.Append('\\', numBackslashes);
                    escaped.Append(arg[i]);
                }
            } 

            escaped.Append('"');
            return escaped.ToString();
        }

    }
}
