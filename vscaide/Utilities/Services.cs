using EnvDTE;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    public class Services
    {
        public static DTE DTE { get { return Get<SDTE, DTE>(); } }
        public static IVsSolution Solution { get { return Get<SVsSolution, IVsSolution>(); } }
        public static IVsOutputWindowPane GeneralOutputWindow { get { return Get<SVsGeneralOutputWindowPane, IVsOutputWindowPane>(); } }
        public static IVsMonitorSelection MonitorSelection { get { return Get<SVsShellMonitorSelection, IVsMonitorSelection>(); } }

        public static IVsShell Shell { get { return Get<SVsShell, IVsShell>(); } }
        public static IVsOutputWindow OutputWindow { get { return Get<SVsOutputWindow, IVsOutputWindow>(); } }
        public static IVsFileChangeEx FileChangeEx { get { return Get<SVsFileChangeEx, IVsFileChangeEx>(); } }
        public static IVsActivityLog Log { get { return Get<SVsActivityLog, IVsActivityLog>(); } }

        private static TInterface Get<TService, TInterface>()
            where TInterface: class
        {
            return Package.GetGlobalService(typeof(TService)) as TInterface;
        }
    }
}
