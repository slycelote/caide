namespace slycelote.VsCaide.VsInterface
{
    using System;
    using System.Threading.Tasks;

    /// <summary>
    /// Like Microsoft.VisualStudio.Shell.Interop.IAsyncServiceProvider,
    /// but doesn't depend on VS SDK.
    /// </summary>
    public interface IAsyncServiceProvider
    {
        Task<object> GetServiceAsync(Type serviceType);
    }
}
