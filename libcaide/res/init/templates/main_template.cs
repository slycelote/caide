public class Program {
    public static void Main(string[] args) {
        Solution solution = new Solution();
        using (System.IO.TextReader input =
                CaideConstants.InputFile == null ? System.Console.In :
                        new System.IO.StreamReader(CaideConstants.InputFile))
        using (System.IO.TextWriter output =
                CaideConstants.OutputFile == null ? System.Console.Out:
                        new System.IO.StreamWriter(CaideConstants.OutputFile))

            solution.solve(input, output);
    }
}

