namespace slycelote.VsCaide
{
    using System;
    using System.Runtime.InteropServices;
    using Microsoft.VisualStudio.Shell;
    using Microsoft.VisualStudio.Shell.Interop;
    using slycelote.VsCaide.Core;
    using slycelote.VsCaide.VsInterface;

    /// <summary>
    /// This class implements the tool window exposed by this package and hosts a user control.
    /// </summary>
    /// <remarks>
    /// In Visual Studio tool windows are composed of a frame (implemented by the shell) and a pane,
    /// usually implemented by the package implementer.
    /// <para>
    /// This class derives from the ToolWindowPane class provided from the MPF in order to use its
    /// implementation of the IVsUIElementPane interface.
    /// </para>
    /// </remarks>
    [Guid("78b0fcb0-cd7e-4561-961d-1c09e45b2145")]
    public class VsCaideMainWindow : ToolWindowPane, IWindow
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="VsCaideMainWindow"/> class.
        /// </summary>
        public VsCaideMainWindow() : base(null)
        {
            this.Caption = "VsCaide";

            // This is the user control hosted by the tool window; Note that, even if this class implements IDisposable,
            // we are not calling Dispose on this object. This is because ToolWindowPane calls Dispose on
            // the object returned by the Content property.
            this.Content = new VsCaideMainWindowControl(this);
        }

        public void Show()
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            (Frame as IVsWindowFrame)?.Show();
        }
    }
}
