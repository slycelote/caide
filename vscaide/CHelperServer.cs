using slycelote.VsCaide.Utilities;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace slycelote.VsCaide
{
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1001:TypesThatOwnDisposableFieldsShouldBeDisposable")]
    public class CHelperServer
    {
        private readonly Process Caide;
        private readonly CancellationTokenSource CancelTokenSource = new CancellationTokenSource();

        public CHelperServer()
        {
            var args = new [] {"httpServer"};
            var psi = new ProcessStartInfo
            {
                FileName = Paths.CaideExe,
                Arguments = CommandLine.CreateCommandLine(args),
                WorkingDirectory = SolutionUtilities.GetSolutionDir(),
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            };
            Caide = Process.Start(psi);
            new ReadStreamToOutputWindow(Caide.StandardOutput, CancelTokenSource.Token).RunAsync();
            new ReadStreamToOutputWindow(Caide.StandardError,  CancelTokenSource.Token).RunAsync();
        }

        public void Stop()
        {
            try
            {
                CancelTokenSource.Cancel();
                Caide.StandardInput.WriteLine();
                Caide.StandardInput.Close();
                if (!Caide.WaitForExit(5000))
                    Caide.Kill();
                Caide.Close();
            }
            catch (Exception e)
            {
                Logger.LogException(e);
            }
        }
    }

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
                try
                {
                    char[] buf = new char[2048];
                    var outputWindow = Services.GeneralOutputWindow;
                    for (;;)
                    {
                        var readTask = Reader.ReadAsync(buf, 0, buf.Length);
                        readTask.Wait(CancelToken);
                        outputWindow.OutputStringThreadSafe(new string(buf, 0, readTask.Result));
                    }
                }
                catch (OperationCanceledException) { }
                catch (Exception e)
                {
                    Logger.LogException(e);
                }
            });
        }
    }
}
