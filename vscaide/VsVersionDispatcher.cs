using System;
using System.IO;
using System.Reflection;

using slycelote.VsCaide.Utilities;
using VsInterface;

namespace slycelote.VsCaide
{
    public class VsVersionDispatcher
    {
        public static IProjectManager GetProjectManager()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            string version = Services.DTE.Version;
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

            string versionSpecificDLL = Path.Combine(Paths.PackageInstallationDir, "Vs" + version + ".dll");

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

            ConstructorInfo constructor = pmType.GetConstructor(new Type[] { });
            if (constructor == null)
            {
                throw new CaideException("Couldn't find constructor");
            }

            object obj = constructor.Invoke(new object[] { });
            if (!(obj is IProjectManager))
            {
                throw new CaideException("Couldn't create a ProjectManager");
            }

            return obj as IProjectManager;
        }
    }
}
