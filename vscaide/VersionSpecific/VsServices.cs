namespace slycelote.VsCaide.VsSpecific
{
    using EnvDTE;
    using Microsoft.VisualStudio;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading;
    using System.Threading.Tasks;
    using slycelote.VsCaide.VsInterface;
    using IAsyncServiceProvider = VsInterface.IAsyncServiceProvider;

    /// <summary>
    /// This is a common implementation for all Visual Studio versions.
    /// The only difference is which versions of VS SDK and VCProjectEngine the DLL references.
    /// VCProjectEngline version must be specific to each VS version.
    /// VS SDK has two versions: 15.* for VS pre-2022 and 17.* for VS 2022 and newer. 
    /// </summary>
    public class VsServices : IVsServices
    {
        private IAsyncServiceProvider Package { get; }

        private DTE DTE { get; set; }
        private IVsSolution Solution { get; set; }
        private IVsOutputWindowPane GeneralOutputWindow { get; set; }

        public VsServices(IAsyncServiceProvider package)
        {
            Package = package;
        }

        public async System.Threading.Tasks.Task InitializeAsync()
        {
            DTE = await GetAsync<DTE>();
            Solution = await GetAsync<SVsSolution, IVsSolution>();
            GeneralOutputWindow = await GetAsync<SVsGeneralOutputWindowPane, IVsOutputWindowPane>();
            /*
            MonitorSelection = await GetAsync<IVsMonitorSelection>();
            CommandWindow = await GetAsync<IVsCommandWindow>();
            Shell = await GetAsync<IVsShell>();
            OutputWindow = await GetAsync<IVsOutputWindow>();
            FileChangeEx = await GetAsync<IVsFileChangeEx>();
            Log = await GetAsync<SVsActivityLog, IVsActivityLog>();
            */
        }

        public int ThrowOnFailure(int hr) => ErrorHandler.ThrowOnFailure(hr);

        public dynamic SwitchToMainThreadAsync(CancellationToken cancelToken)
#pragma warning disable VSTHRD004 // Await SwitchToMainThreadAsync
            => ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(cancelToken);
#pragma warning restore VSTHRD004

        public void ThrowIfNotOnUIThread() => ThreadHelper.ThrowIfNotOnUIThread();

        public IProjectManager GetProjectManager()
        {
            return new ProjectManager(DTE);
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

        private async Task<TInterface> GetAsync<TServiceType, TInterface>()
            where TInterface : class
        {
            // Package is provided by the main vscaide project, which is built against old
            // (pre-2022) VS SDK. Vs2022 implementation is built against new (2022) VS SDK,
            // so requests types from that SDK. This seems to work out fine, probably because
            // the types are COM objects...
            // We do get a build warning about package downgrade, so this might break in future...
            return await Package.GetServiceAsync(typeof(TServiceType)) as TInterface;
        }

        private async Task<TInterface> GetAsync<TInterface>()
            where TInterface : class
        {
            return await GetAsync<TInterface, TInterface>();
        }

        private static Project GetProject(IProject project) => ((ProjectImpl)project).Project;
    }
}
