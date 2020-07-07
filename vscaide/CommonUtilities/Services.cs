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
        public static async System.Threading.Tasks.Task InitializeAsync(AsyncPackage package)
        {
            DTE = await Get<DTE>(package);
            Solution = await GetAsync<SVsSolution, IVsSolution>(package);
            GeneralOutputWindow = await GetAsync<SVsGeneralOutputWindowPane, IVsOutputWindowPane>(package);
            MonitorSelection = await Get<IVsMonitorSelection>(package);
            CommandWindow = await Get<IVsCommandWindow>(package);
            Shell = await Get<IVsShell>(package);
            OutputWindow = await Get<IVsOutputWindow>(package);
            FileChangeEx = await Get<IVsFileChangeEx>(package);
            Log = await GetAsync<SVsActivityLog, IVsActivityLog>(package);
        }

        public static DTE DTE { get; private set; }
        public static IVsSolution Solution { get; private set; }
        public static IVsOutputWindowPane GeneralOutputWindow { get; private set; }
        public static IVsMonitorSelection MonitorSelection { get; private set; }
        public static IVsCommandWindow CommandWindow { get; private set; }
        public static IVsShell Shell { get; private set; }
        public static IVsOutputWindow OutputWindow { get; private set; }
        public static IVsFileChangeEx FileChangeEx { get; private set; }
        public static IVsActivityLog Log { get; private set; }

        private static async Task<TInterface> GetAsync<TServiceType, TInterface>(AsyncPackage package)
            where TInterface : class
        {
            return await package.GetServiceAsync(typeof(TServiceType)) as TInterface;
        }

        private static async Task<TInterface> Get<TInterface>(AsyncPackage package)
            where TInterface : class
        {
            return await GetAsync<TInterface, TInterface>(package);
        }
    }
}
