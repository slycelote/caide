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
                return Path.Combine(PackageInstallationDir, "Resources", "CppTemplate.vstemplate");
            }
        }

        public static string CSharpProjectTemplate
        {
            get
            {
                if (!cSharpTemplateProcessed)
                {
                    var csProj = Path.Combine(PackageInstallationDir, "Resources", "vscaide_cs_template.csproj");
                    var template = File.ReadAllText(csProj);
                    var newTemplate = template.Replace("CAIDE_EXE", Paths.CaideExe);
                    if (newTemplate != template)
                    {
                        File.WriteAllText(csProj, newTemplate);
                    }
                    cSharpTemplateProcessed = true;
                }
                return Path.Combine(PackageInstallationDir, "Resources", "CsTemplate.vstemplate");
            }
        }
        private static bool cSharpTemplateProcessed = false;

        public static string NormalizePath(string path)
        {
            return Path.GetFullPath(new Uri(path).LocalPath).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        }
    
    }
}
