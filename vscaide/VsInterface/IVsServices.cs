namespace slycelote.VsCaide.VsInterface
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;

    public interface IVsServices
    {
        IProjectManager GetProjectManager();

        int ThrowOnFailure(int hr);

        #region UI
        void ThrowIfNotOnUIThread();
        dynamic SwitchToMainThreadAsync(CancellationToken cancelToken = default);
        #endregion

        #region Output Window
        void ActivateOutputWindow();
        int WriteToOutputWindow(string message);
        #endregion

        #region Solution
        void SaveSolution();

        int CreateSolution(string location, string name);

        string GetSolutionDir();

        IEnumerable<IProject> Projects { get; }

        void RemoveProject(IProject project);

        // The argument is actually IVsHierarchy
        IProject GetProjectFromHierarchy(object value);
        #endregion

        #region DTE
        void ExecuteCommand(string name, string args = "");
        #endregion
    }
}

