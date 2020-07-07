using Microsoft.VisualStudio.Shell;
using slycelote.VsCaide.Utilities;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Task = System.Threading.Tasks.Task;

namespace slycelote.VsCaide
{
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1001:TypesThatOwnDisposableFieldsShouldBeDisposable")]
    public class CHelperServer
    {
        private readonly Process caideProcess;
        private readonly CancellationTokenSource cancelTokenSource = new CancellationTokenSource();

        public CHelperServer(AsyncPackage package)
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
            caideProcess = Process.Start(psi);
            _ = new ReadStreamToOutputWindow(package, caideProcess.StandardOutput, cancelTokenSource.Token).RunAsync();
            _ = new ReadStreamToOutputWindow(package, caideProcess.StandardError, cancelTokenSource.Token).RunAsync();
        }

        public void Stop()
        {
            try
            {
                cancelTokenSource.Cancel();
                caideProcess.StandardInput.WriteLine();
                caideProcess.StandardInput.Close();
                if (!caideProcess.WaitForExit(5000))
                    caideProcess.Kill();
                caideProcess.Close();
            }
            catch (Exception e)
            {
                Logger.LogException(e);
            }
        }
    }

    internal class ReadStreamToOutputWindow
    {
        private readonly StreamReader reader;
        private readonly CancellationToken cancelToken;
        private readonly AsyncPackage package;

        internal ReadStreamToOutputWindow(AsyncPackage package, StreamReader reader, CancellationToken cancelToken)
        {
            this.package = package;
            this.reader = reader;
            this.cancelToken = cancelToken;
        }

        internal async Task RunAsync()
        {
            try
            {
                char[] buf = new char[2048];
                var outputWindow = Services.GeneralOutputWindow;
                for (;;)
                {
                    if (cancelToken.IsCancellationRequested)
                    {
                        return;
                    }

                    int bytesRead = await reader.ReadAsync(buf, 0, buf.Length).ConfigureAwait(false);
                    await package.JoinableTaskFactory.SwitchToMainThreadAsync(cancelToken);
                    _ = outputWindow.OutputStringThreadSafe(new string(buf, 0, bytesRead));
                }
            }
            catch (OperationCanceledException) { }
            catch (Exception e)
            {
                Logger.LogException(e);
            }
        }
    }
}
