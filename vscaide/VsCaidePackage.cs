using System;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;

using slycelote.VsCaide.Utilities;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Shell;

namespace slycelote.VsCaide
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    ///
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the
    /// IVsPackage interface and uses the registration attributes defined in the framework to
    /// register itself and its components with the shell.
    /// </summary>

    // This attribute tells the PkgDef creation utility (CreatePkgDef.exe) that this class is
    // a package.
    [PackageRegistration(UseManagedResourcesOnly = true)]

    // This attribute is used to register the information needed to show this package
    // in the Help/About dialog of Visual Studio.
    [InstalledProductRegistration("#110", "#112", "2.4.1", IconResourceID = 400)]

    // This attribute is needed to let the shell know that this package exposes some menus.
    [ProvideMenuResource("Menus.ctmenu", 1)]

    // This attribute registers a tool window exposed by this package.
    [ProvideToolWindow( typeof(MainToolWindow),
        Window=Microsoft.VisualStudio.Shell.Interop.ToolWindowGuids.PropertyBrowser,
        Style=VsDockStyle.Tabbed
    )]

    [ProvideAutoLoad(Microsoft.VisualStudio.VSConstants.UICONTEXT.NoSolution_string)]
    [ProvideAutoLoad(Microsoft.VisualStudio.VSConstants.UICONTEXT.SolutionExists_string)]
    [Guid(GuidList.guidVsCaidePkgString)]
    public sealed class VsCaidePackage : Package, IVsShellPropertyEvents, IVsSolutionEvents, IVsSolutionLoadEvents, IVsSelectionEvents
    {
        private static VsCaidePackage Instance { get; set; }

        /// <summary>
        /// Default constructor of the package.
        /// Inside this method you can place any initialization code that does not require
        /// any Visual Studio service because at this point the package object is created but
        /// not sited yet inside Visual Studio environment. The place to do all the other
        /// initialization is the Initialize method.
        /// </summary>
        public VsCaidePackage()
        {
            Debug.WriteLine(string.Format(CultureInfo.CurrentCulture, "Entering constructor for: {0}", this.ToString()));
            Instance = null;
        }

        /// <summary>
        /// This function is called when the user clicks the menu item that shows the
        /// tool window. See the Initialize method to see how the menu item is associated to
        /// this function using the OleMenuCommandService service and the MenuCommand class.
        /// </summary>
        private void ShowToolWindow(object sender, EventArgs e)
        {
            ToolWindowPane window = LocateMainToolWindow();
            if ((null == window) || (null == window.Frame))
            {
                throw new NotSupportedException(Resources.CanNotCreateWindow);
            }
            IVsWindowFrame windowFrame = (IVsWindowFrame)window.Frame;
            Microsoft.VisualStudio.ErrorHandler.ThrowOnFailure(windowFrame.Show());
        }

        private MainToolWindow LocateMainToolWindow()
        {
            // Get the instance number 0 of this tool window. This window is single instance so this instance
            // is actually the only one.
            // The last flag is set to true so that if the tool window does not exists it will be created.
            return (MainToolWindow)this.FindToolWindow(typeof(MainToolWindow), 0, true);
        }


        /////////////////////////////////////////////////////////////////////////////
        // Overridden Package Implementation
        #region Package Members

        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            Debug.WriteLine (string.Format(CultureInfo.CurrentCulture, "Entering Initialize() of: {0}", this.ToString()));
            base.Initialize();

            if (Instance != null)
            {
                Logger.LogError("Package has already been initialized");
            }
            Instance = this;

            IVsShell shellService = Services.Shell;
            if (shellService != null)
                ErrorHandler.ThrowOnFailure(
                  shellService.AdviseShellPropertyChanges(this, out shellPropertyChangeCookie));

        }
        #endregion

        #region IVsShellPropertyEvents members

        private uint shellPropertyChangeCookie;

        public int OnShellPropertyChange(int propid, object var)
        {
            if ((int)__VSSPROPID.VSSPROPID_Zombie != propid || (bool)var == true)
                return VSConstants.S_OK;

            // when zombie state changes to false, finish package initialization
            IVsShell shellService = Services.Shell;

            if (shellService != null)
                ErrorHandler.ThrowOnFailure(
                  shellService.UnadviseShellPropertyChanges(this.shellPropertyChangeCookie));
            this.shellPropertyChangeCookie = 0;

            // Add our command handlers for menu (commands must exist in the .vsct file)
            OleMenuCommandService mcs = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if ( null != mcs )
            {
                // Create the command for the tool window
                CommandID toolwndCommandID = new CommandID(GuidList.guidVsCaideCmdSet, (int)PkgCmdIDList.cmdidMainWindow);
                MenuCommand menuToolWin = new MenuCommand(ShowToolWindow, toolwndCommandID);
                mcs.AddCommand( menuToolWin );
            }

            var solutionService = Services.Solution;
            ErrorHandler.ThrowOnFailure(
                solutionService.AdviseSolutionEvents(this, out solutionEventsCookie));

            var monitorSelection = Services.MonitorSelection;
            ErrorHandler.ThrowOnFailure(
                monitorSelection.AdviseSelectionEvents(this, out monitorSelectionCookie));

            return VSConstants.S_OK;
        }


        #endregion

        #region IVsSolutionEvents methods

        private uint solutionEventsCookie;
        public int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            LocateMainToolWindow().Control.Solution_Opened();
            return VSConstants.S_OK;
        }

        public int OnAfterCloseSolution(object pUnkReserved)
        {
            LocateMainToolWindow().Control.Solution_Closed();
            return VSConstants.S_OK;
        }

        private IVsHierarchy unloadingProjectHier = null;
        public int OnBeforeUnloadProject(IVsHierarchy pRealHierarchy, IVsHierarchy pStubHierarchy)
        {
            unloadingProjectHier = pRealHierarchy;
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseProject(IVsHierarchy pHierarchy, int fRemoved)
        {
            try
            {
                if (fRemoved != 0 && pHierarchy != unloadingProjectHier && !SolutionUtilities.IgnoreSolutionEvents)
                {
                    LocateMainToolWindow().Control.Project_Removed(pHierarchy);
                }
            }
            finally
            {
                unloadingProjectHier = null;
            }
            return VSConstants.S_OK;
        }

        #region Unrelated events
        public int OnAfterLoadProject(IVsHierarchy pStubHierarchy, IVsHierarchy pRealHierarchy)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterOpenProject(IVsHierarchy pHierarchy, int fAdded)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseSolution(object pUnkReserved)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryCloseProject(IVsHierarchy pHierarchy, int fRemoving, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryCloseSolution(object pUnkReserved, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryUnloadProject(IVsHierarchy pRealHierarchy, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }
        #endregion

        #endregion

        #region IVsSelectionEvents methods
        private uint monitorSelectionCookie;

        public int OnElementValueChanged(uint elementid, object varValueOld, object varValueNew)
        {
            if (elementid == (uint)VSConstants.VSSELELEMID.SEID_StartupProject && !SolutionUtilities.IgnoreSolutionEvents)
            {
                var newStartupProjectHierarchy = (IVsHierarchy)varValueNew;
                LocateMainToolWindow().Control.StartupProject_Changed(newStartupProjectHierarchy);
            }
            return VSConstants.S_OK;
        }

        #region Unrelated events
        public int OnCmdUIContextChanged(uint dwCmdUICookie, int fActive)
        {
            return VSConstants.S_OK;
        }

        public int OnSelectionChanged(IVsHierarchy pHierOld, uint itemidOld, IVsMultiItemSelect pMISOld, ISelectionContainer pSCOld, IVsHierarchy pHierNew, uint itemidNew, IVsMultiItemSelect pMISNew, ISelectionContainer pSCNew)
        {
            return VSConstants.S_OK;
        }
        #endregion

        #endregion

        #region IVsSolutionLoadEvents members
        public int OnAfterBackgroundSolutionLoadComplete()
        {
            try
            {
                LocateMainToolWindow().Control.AllProjects_Loaded();
            }
            catch (Exception e)
            {
                Logger.LogError("{0}", e);
            }
            return VSConstants.S_OK;
        }

        #region Unrelated events
        public int OnAfterLoadProjectBatch(bool fIsBackgroundIdleBatch)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeBackgroundSolutionLoadBegins()
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeLoadProjectBatch(bool fIsBackgroundIdleBatch)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeOpenSolution(string pszSolutionFilename)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryBackgroundLoadProjectBatch(out bool pfShouldDelayLoadToNextIdle)
        {
            pfShouldDelayLoadToNextIdle = false;
            return VSConstants.S_OK;
        }
        #endregion

        #endregion
    }
}
