using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace VsInterface
{
    public interface IProjectManager
    {
        void CreateAndActivateCSharpProject(string selectedProblem);
        void CreateAndActivateCppProject(string selectedProblem, string language);
        void CreateCppLibProject();
        void CreateSubmissionCppProject();
        void CreateSubmissionCsProject();
    }
}
