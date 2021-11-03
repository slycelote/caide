using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using slycelote.VsCaide.VsInterface;

namespace slycelote.VsCaide.Core
{
    public static class Logger
    {
        public static void LogError(string formatString, params object[] args)
        {
            LogMessage(formatString, args);
            VsImplementation.Services.ActivateOutputWindow();
        }

        public static void LogException(Exception e)
        {
            LogError("{0}", e.Message);
        }

        public static void LogMessage(string formatString, params object[] args)
        {
            var time = DateTime.Now.ToString("s");
            VsImplementation.Services.WriteToOutputWindow(
                "[VsCaide " + time + "] " + string.Format(formatString, args) + "\n");
        }

        [Conditional("DEBUG")]
        public static void Trace(string formatString, params object[] args)
        {
            LogMessage("[TRACE] " + formatString, args);
        }
    }
}
