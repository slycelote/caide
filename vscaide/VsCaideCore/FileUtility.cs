using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Core
{
    public class FileUtility
    {
        public static void DirectoryCopy(string sourceDirName, string destDirName,
            bool copySubDirs = true, Func<FileInfo, bool> fileFilter = null)
        {
            if (fileFilter == null)
                fileFilter = _ => true;

            // Get the subdirectories for the specified directory.
            DirectoryInfo dir = new DirectoryInfo(sourceDirName);

            if (!dir.Exists)
            {
                throw new DirectoryNotFoundException(
                    "Source directory does not exist or could not be found: "
                    + sourceDirName);
            }

            // If the destination directory doesn't exist, create it. 
            if (!Directory.Exists(destDirName))
            {
                Directory.CreateDirectory(destDirName);
            }

            // Get the files in the directory and copy them to the new location.
            FileInfo[] files = dir.GetFiles();
            foreach (FileInfo file in files)
            {
                if (fileFilter(file))
                {
                    string temppath = Path.Combine(destDirName, file.Name);
                    file.CopyTo(temppath, overwrite: true);
                }
            }

            // If copying subdirectories, copy them and their contents to new location. 
            if (copySubDirs)
            {
                DirectoryInfo[] dirs = dir.GetDirectories();
                foreach (DirectoryInfo subdir in dirs)
                {
                    string temppath = Path.Combine(destDirName, subdir.Name);
                    DirectoryCopy(subdir.FullName, temppath, copySubDirs, fileFilter);
                }
            }
        }

        public static void RemoveFiles(string dirName,
            bool recursive = true, Func<FileInfo, bool> fileFilter = null)
        {
            if (fileFilter == null)
                fileFilter = _ => true;

            DirectoryInfo dir = new DirectoryInfo(dirName);

            if (!dir.Exists)
            {
                return;
            }

            // Get the files in the directory and copy them to the new location.
            FileInfo[] files = dir.GetFiles();
            foreach (FileInfo file in files)
            {
                if (fileFilter(file))
                {
                    try
                    {
                        File.Delete(file.FullName);
                    }
                    catch { }
                }
            }

            if (recursive)
            {
                DirectoryInfo[] dirs = dir.GetDirectories();
                foreach (DirectoryInfo subdir in dirs)
                {
                    RemoveFiles(subdir.FullName, recursive);
                    try
                    {
                        Directory.Delete(subdir.FullName);
                    }
                    catch { }
                }
            }
        }
    }
}
