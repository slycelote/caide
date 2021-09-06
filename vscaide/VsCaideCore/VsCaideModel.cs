using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace slycelote.VsCaide.Core
{
    class CaideProblem: IEquatable<CaideProblem>
    {
        public string Name { get; set; }

        public bool Equals(CaideProblem other)
        {
            return Name == other.Name;
        }

        public override bool Equals(object obj)
        {
            return obj is CaideProblem && Equals(obj as CaideProblem);
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }
    }

    // The model backed by caide state on disk.
    class VsCaideModel : NotifyPropertyChangedBase
    {
        private bool isCaideSolution = false;
        public bool IsCaideSolution
        {
            get { return isCaideSolution; }
            set { SetField(ref isCaideSolution, value); }
        }

        public ObservableCollection<string> ProgrammingLanguages { get; private set; } =
            new ObservableCollection<string>(new[] { "c++", "c#" });

        private string language = "";
        public string ProgrammingLanguage
        {
            get { return language; }
            set {
                if (value != null && !ProgrammingLanguages.Contains(value))
                    ProgrammingLanguages.Add(value);
                SetField(ref language, value);
            }
        }

        private CaideProblem selectedProblem = null;
        public CaideProblem SelectedProblem
        {
            get { return selectedProblem; }
            set { SetField(ref selectedProblem, value); }
        }

        public ObservableCollection<CaideProblem> Problems { get; private set; } = new ObservableCollection<CaideProblem>();
        public void SetProblems(IEnumerable<CaideProblem> newProblems)
        {
            var newProblemSet = new HashSet<CaideProblem>(newProblems);
            if (!newProblemSet.Contains(selectedProblem))
            {
                SelectedProblem = null;
            }

            var problemsToRemove = new HashSet<CaideProblem>();
            foreach (var existingProblem in Problems)
            {
                if (!newProblemSet.Remove(existingProblem))
                {
                    problemsToRemove.Add(existingProblem);
                }
            }

            foreach (var p in problemsToRemove)
            {
                Problems.Remove(p);
            }

            foreach (var p in newProblemSet)
            {
                Problems.Add(p);
            }
        }
    }
}
