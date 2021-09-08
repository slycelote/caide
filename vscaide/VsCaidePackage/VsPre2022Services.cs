namespace slycelote.VsCaide
{
    using EnvDTE;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Reflection;
    using System.Threading;
    using System.Threading.Tasks;
    using slycelote.VsCaide.VsInterface;

    public class VsPre2022Services : IVsServices
    {
        public static async System.Threading.Tasks.Task<IVsServices> ConstructAsync(AsyncPackage package)
        {
            var services = new VsPre2022Services();
            services.Package = package;
            services.DTE = await GetAsync<DTE>(package);
            services.Solution = await GetAsync<SVsSolution, IVsSolution>(package);
            services.GeneralOutputWindow = await GetAsync<SVsGeneralOutputWindowPane, IVsOutputWindowPane>(package);
            services.MonitorSelection = await GetAsync<IVsMonitorSelection>(package);
            services.CommandWindow = await GetAsync<IVsCommandWindow>(package);
            services.Shell = await GetAsync<IVsShell>(package);
            services.OutputWindow = await GetAsync<IVsOutputWindow>(package);
            services.FileChangeEx = await GetAsync<IVsFileChangeEx>(package);
            services.Log = await GetAsync<SVsActivityLog, IVsActivityLog>(package);
            return services;
        }

        public int ThrowOnFailure(int hr) => ErrorHandler.ThrowOnFailure(hr);

        public dynamic SwitchToMainThreadAsync(CancellationToken cancelToken)
#pragma warning disable VSTHRD004 // Await SwitchToMainThreadAsync
            => ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(cancelToken);
#pragma warning restore VSTHRD004

        public void ThrowIfNotOnUIThread() => ThreadHelper.ThrowIfNotOnUIThread();

        public IProjectManager GetProjectManager()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string version = DTE.Version;
            if (string.IsNullOrEmpty(version))
            {
                version = "2015";
            }
            else
            {
                var parts = version.Split('.');
                if (parts.Length > 0 && parts[0] == "16")
                {
                    version = "2019";
                }
                else if (parts.Length > 0 && parts[0] == "15")
                {
                    version = "2017";
                }
                else
                {
                    version = "2015";
                }
            }

            string packageInstallationDir = Path.GetDirectoryName(typeof(VsPre2022Services).Assembly.Location);

            string versionSpecificDLL = Path.Combine(packageInstallationDir, "Vs" + version + ".dll");

            Assembly assembly = Assembly.LoadFrom(versionSpecificDLL);
            if (assembly == null)
            {
                throw new CaideException("Couldn't load assembly " + versionSpecificDLL);
            }

            Type pmType = assembly.GetType("slycelote.VsCaide.VsSpecific.ProjectManager");
            if (pmType == null)
            {
                throw new CaideException("Couldn't find type slycelote.VsCaide.VsSpecific.ProjectManager");
            }

            ConstructorInfo constructor = pmType.GetConstructor(new Type[] { typeof(DTE) });
            if (constructor == null)
            {
                throw new CaideException("Couldn't find constructor");
            }

            object obj = constructor.Invoke(new object[] { DTE });
            if (!(obj is IProjectManager))
            {
                throw new CaideException("Couldn't create a ProjectManager");
            }

            return obj as IProjectManager;
        }

        public void ActivateOutputWindow()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GeneralOutputWindow.Activate();
        }

        public int WriteToOutputWindow(string message)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return GeneralOutputWindow.OutputStringThreadSafe(message);
        }

        public string GetSolutionDir()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string solutionDir, unused;

            ErrorHandler.ThrowOnFailure(
                Solution.GetSolutionInfo(out solutionDir, out unused, out unused));
            return solutionDir;
        }

        public int CreateSolution(string location, string name)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            return Solution.CreateSolution(location, name, 0);
        }

        public void SaveSolution()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            ThrowOnFailure(
                Solution.SaveSolutionElement((uint)__VSSLNSAVEOPTIONS.SLNSAVEOPT_SaveIfDirty, null, 0));
        }

        public IEnumerable<IProject> Projects
        {
            get
            {
                ThreadHelper.ThrowIfNotOnUIThread();
                return DTE.Solution.Projects.OfType<Project>().Select(p => new ProjectImpl(p));
            }
        }

        public void RemoveProject(IProject project)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            DTE.Solution.Remove(GetProject(project));
        }

        public IProject GetProjectFromHierarchy(object value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            IVsHierarchy vsHierarchy = value as IVsHierarchy;
            if (vsHierarchy == null)
            {
                return null;
            }

            object project;
            ErrorHandler.ThrowOnFailure(
                vsHierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out project));

            return new ProjectImpl((Project)project);
        }

        public void ExecuteCommand(string name, string args = "")
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            DTE.ExecuteCommand(name, args);
        }

        private VsPre2022Services()
        { }

        private AsyncPackage Package { get; set; }
        private DTE DTE { get; set; }
        private IVsSolution Solution { get; set; }
        private IVsOutputWindowPane GeneralOutputWindow { get; set; }
        private IVsMonitorSelection MonitorSelection { get; set; }
        private IVsCommandWindow CommandWindow { get; set; }
        private IVsShell Shell { get; set; }
        private IVsOutputWindow OutputWindow { get; set; }
        private IVsFileChangeEx FileChangeEx { get; set; }
        private IVsActivityLog Log { get; set; }

        private static async Task<TInterface> GetAsync<TServiceType, TInterface>(AsyncPackage package)
            where TInterface : class
        {
            return await package.GetServiceAsync(typeof(TServiceType)) as TInterface;
        }

        private static async Task<TInterface> GetAsync<TInterface>(AsyncPackage package)
            where TInterface : class
        {
            return await GetAsync<TInterface, TInterface>(package);
        }

        private static Project GetProject(IProject project) => ((ProjectImpl)project).Project;
    }
}
