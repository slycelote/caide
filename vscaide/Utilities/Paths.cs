using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    public abstract class Paths
    {
        public static string PackageInstallationDir
        {
            get
            {
                return Path.GetDirectoryName(typeof(Paths).Assembly.Location);
            }
        }

        public static string CaideExe
        {
            get
            {
                return Path.Combine(PackageInstallationDir, "Resources", "caide.exe");
            }
        }

        public static string CppProjectTemplate
        {
            get
            {
                return Path.Combine(PackageInstallationDir, "Resources", "MyTemplate.vstemplate");
            }
        }

        public static string NormalizePath(string path)
        {
            return Path.GetFullPath(new Uri(path).LocalPath).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        }
    
    }
}
