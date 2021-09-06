namespace slycelote.VsCaide.VsInterface
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
