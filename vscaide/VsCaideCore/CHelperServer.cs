using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Core
{
    public class CHelperServer
    {
        private readonly Process caideProcess;
        private readonly CancellationTokenSource cancelTokenSource = new CancellationTokenSource();

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
            caideProcess = Process.Start(psi);
            _ = new ReadStreamToOutputWindow(caideProcess.StandardOutput, cancelTokenSource.Token).RunAsync();
            _ = new ReadStreamToOutputWindow(caideProcess.StandardError, cancelTokenSource.Token).RunAsync();
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

        internal ReadStreamToOutputWindow(StreamReader reader, CancellationToken cancelToken)
        {
            this.reader = reader;
            this.cancelToken = cancelToken;
        }

        internal async Task RunAsync()
        {
            try
            {
                char[] buf = new char[2048];
                var services = VsInterface.VsImplementation.Services;
                for (;;)
                {
                    if (cancelToken.IsCancellationRequested)
                    {
                        return;
                    }

                    int bytesRead = await reader.ReadAsync(buf, 0, buf.Length).ConfigureAwait(false);
                    await services.SwitchToMainThreadAsync(cancelToken);
                    _ = services.WriteToOutputWindow(new string(buf, 0, bytesRead));
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
