using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    public static class Logger
    {
        public static void LogError(string formatString, params object[] args)
        {
            LogMessage(formatString, args);
            Services.GeneralOutputWindow.Activate();
        }

        public static void LogException(Exception e)
        {
            LogError("{0}", e.Message);
        }

        public static void LogMessage(string formatString, params object[] args)
        {
            var time = DateTime.Now.ToString("s");
            var outputWindow = Services.GeneralOutputWindow;
            _ = outputWindow.OutputStringThreadSafe("[VsCaide " + time + "] " + string.Format(formatString, args) + "\n");
        }

        [Conditional("DEBUG")]
        public static void Trace(string formatString, params object[] args)
        {
            LogMessage("[TRACE] " + formatString, args);
        }
    }
}
