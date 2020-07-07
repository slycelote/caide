using System;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using Microsoft;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.Win32;
using slycelote.VsCaide.Utilities;

using SolutionEvents = Microsoft.VisualStudio.Shell.Events.SolutionEvents;
using Task = System.Threading.Tasks.Task;

namespace VsCaide
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the
    /// IVsPackage interface and uses the registration attributes defined in the framework to
    /// register itself and its components with the shell. These attributes tell the pkgdef creation
    /// utility what data to put into .pkgdef file.
    /// </para>
    /// <para>
    /// To get loaded into VS, the package must be referred by &lt;Asset Type="Microsoft.VisualStudio.VsPackage" ...&gt; in .vsixmanifest file.
    /// </para>
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [InstalledProductRegistration("#110", "#112", "2.4.2", IconResourceID = 400)] // Info on this package for Help/About
    [Guid(VsCaidePackage.PackageGuidString)]
    [SuppressMessage("StyleCop.CSharp.DocumentationRules", "SA1650:ElementDocumentationMustBeSpelledCorrectly", Justification = "pkgdef, VS and vsixmanifest are valid VS terms")]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string, PackageAutoLoadFlags.BackgroundLoad)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [ProvideToolWindow(typeof(VsCaideMainWindow), Style = VsDockStyle.Tabbed, Window = ToolWindowGuids.SolutionExplorer)]
    public sealed class VsCaidePackage : AsyncPackage, IVsSelectionEvents
    {
        /// <summary>
        /// VsCaidePackage GUID string.
        /// </summary>
        public const string PackageGuidString = "8e97a36f-88cc-49ee-8e47-df660b4c7d83";

        /// <summary>
        /// Initializes a new instance of the <see cref="VsCaidePackage"/> class.
        /// </summary>
        public VsCaidePackage()
        {
            // Inside this method you can place any initialization code that does not require
            // any Visual Studio service because at this point the package object is created but
            // not sited yet inside Visual Studio environment. The place to do all the other
            // initialization is the Initialize method.
        }

        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        /// <param name="cancellationToken">A cancellation token to monitor for initialization cancellation, which can occur when VS is shutting down.</param>
        /// <param name="progress">A provider for progress updates.</param>
        /// <returns>A task representing the async work of package initialization, or an already completed task if there is none. Do not return null from this method.</returns>
        protected override async Task InitializeAsync(CancellationToken cancellationToken, IProgress<ServiceProgressData> progress)
        {
            await JoinableTaskFactory.SwitchToMainThreadAsync();
            await Services.InitializeAsync(this);
            SolutionEvents.OnAfterCloseSolution += SolutionEvents_OnAfterCloseSolution;
            SolutionEvents.OnBeforeOpenSolution += SolutionEvents_OnBeforeOpenSolution;
            SolutionEvents.OnBeforeBackgroundSolutionLoadBegins += SolutionEvents_OnBeforeBackgroundSolutionLoadBegins;
            SolutionEvents.OnAfterBackgroundSolutionLoadComplete += SolutionEvents_OnAfterBackgroundSolutionLoadComplete;
            SolutionEvents.OnBeforeCloseProject += SolutionEvents_OnBeforeCloseProject;
            var monitorSelection = Services.MonitorSelection;
            _ = ErrorHandler.ThrowOnFailure(
                monitorSelection.AdviseSelectionEvents(this, out uint monitorSelectionCookie));

            // https://github.com/madskristensen/SolutionLoadSample
            SolutionEvents.OnAfterOpenSolution += SolutionEvents_OnAfterLoadedSolution;
            bool isSolutionLoaded = await IsSolutionLoadedAsync();
            if (isSolutionLoaded)
            {
                SolutionEvents_OnAfterLoadedSolution();
            }

            await VsCaideMainWindowCommand.InitializeAsync(this);
        }

        #endregion

        private async Task<bool> IsSolutionLoadedAsync()
        {
            await JoinableTaskFactory.SwitchToMainThreadAsync();
            var solService = await GetServiceAsync(typeof(SVsSolution)) as IVsSolution;
            Assumes.Present(solService);
            ErrorHandler.ThrowOnFailure(
                solService.GetProperty((int)__VSPROPID.VSPROPID_IsSolutionOpen,
                out object value));

            return value is bool && (bool)value;
        }

        private VsCaideMainWindowControl GetMainWindowControl()
        {
            ToolWindowPane window = this.FindToolWindow(typeof(VsCaideMainWindow), 0, true) as VsCaideMainWindow;
            return window != null ? window.Content as VsCaideMainWindowControl : null;
        }

        private void SolutionEvents_OnAfterLoadedSolution(object sender = null, EventArgs e = null)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetMainWindowControl()?.OnAfterLoadedSolution();
        }

        private void SolutionEvents_OnAfterBackgroundSolutionLoadComplete(object sender = null, EventArgs e = null)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetMainWindowControl()?.OnAfterBackgroundSolutionLoadComplete();
        }

        private void SolutionEvents_OnBeforeBackgroundSolutionLoadBegins(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetMainWindowControl()?.OnBeforeBackgroundSolutionLoadBegins();
        }

        private void SolutionEvents_OnBeforeCloseProject(object sender, Microsoft.VisualStudio.Shell.Events.CloseProjectEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (e.IsRemoved && !SolutionUtilities.IgnoreSolutionEvents)
            {
                GetMainWindowControl()?.OnBeforeCloseProject(e.Hierarchy);
            }
        }

        private void SolutionEvents_OnBeforeOpenSolution(object sender, Microsoft.VisualStudio.Shell.Events.BeforeOpenSolutionEventArgs e)
        {
            GetMainWindowControl()?.OnBeforeOpenSolution();
        }

        private void SolutionEvents_OnAfterCloseSolution(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            GetMainWindowControl()?.OnAfterCloseSolution();
        }

        #region IVsSelectionEvents
        public int OnElementValueChanged(uint elementid, object varValueOld, object varValueNew)
        {
            if (elementid != (uint)VSConstants.VSSELELEMID.SEID_StartupProject)
                return VSConstants.S_OK;

            ThreadHelper.ThrowIfNotOnUIThread();

            if (!SolutionUtilities.IgnoreSolutionEvents)
            {
                GetMainWindowControl()?.OnStartupProjectChanged(varValueNew as IVsHierarchy);
            }

            return VSConstants.S_OK;
        }

        #region Unrelated events
        public int OnSelectionChanged(IVsHierarchy pHierOld, uint itemidOld, IVsMultiItemSelect pMISOld, ISelectionContainer pSCOld, IVsHierarchy pHierNew, uint itemidNew, IVsMultiItemSelect pMISNew, ISelectionContainer pSCNew)
        {
            return VSConstants.S_OK;
        }

        public int OnCmdUIContextChanged(uint dwCmdUICookie, int fActive)
        {
            return VSConstants.S_OK;
        }
        #endregion
        #endregion
    }
}
