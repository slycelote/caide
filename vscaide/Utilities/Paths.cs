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
                return NormalizePath(Path.Combine(Path.GetDirectoryName(typeof(Paths).Assembly.Location), ".."));
            }
        }

        private static string caideExePath;
        public static string CaideExe
        {
            get
            {
                if (caideExePath == null)
                {
                    var executables = Directory.EnumerateFiles(PackageInstallationDir, "caide.exe", SearchOption.AllDirectories).Take(1).ToList();
                    caideExePath = executables.Any() ? executables[0] : null;
                }
                return caideExePath;
            }
        }

        public static string NormalizePath(string path)
        {
            return Path.GetFullPath(new Uri(path).LocalPath).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        }
    
    }
}
