namespace slycelote.VsCaide
{
    using EnvDTE;
    using slycelote.VsCaide.VsInterface;

    public class ProjectImpl : IProject
    {
        internal ProjectImpl(Project project)
        {
            Project = project;
        }

        public string Name
        {
            get
            {
                Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
                return Project.Name;
            }
        }

        internal Project Project { get; }
    }
}
