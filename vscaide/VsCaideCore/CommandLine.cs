using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace slycelote.VsCaide.Core
{
    public class CommandLine
    {
        public static string CreateCommandLine(IEnumerable<string> arguments)
        {
            return string.Join(" ", arguments.Select(EscapeCommandLineArgument));
        }

        // http://blogs.msdn.com/b/twistylittlepassagesallalike/archive/2011/04/23/everyone-quotes-arguments-the-wrong-way.aspx
        private static string EscapeCommandLineArgument(string arg)
        {
            var escaped = new StringBuilder();
            escaped.Append('"');

            for (int i = 0; ; ++i)
            {
                int numBackslashes = 0;

                while (i < arg.Length && arg[i] == '\\')
                {
                    ++i;
                    ++numBackslashes;
                }

                if (i == arg.Length)
                {
                    // Escape all backslashes, but let the terminating double quotation mark we add below
                    // be interpreted as a metacharacter.
                    escaped.Append('\\', numBackslashes * 2);
                    break;
                }
                else if (arg[i] == '"')
                {
                    // Escape all backslashes and the following double quotation mark.
                    escaped.Append('\\', numBackslashes * 2 + 1);
                    escaped.Append(arg[i]);
                }
                else
                {
                    // Backslashes aren't special here.
                    escaped.Append('\\', numBackslashes);
                    escaped.Append(arg[i]);
                }
            } 

            escaped.Append('"');
            return escaped.ToString();
        }

    }
}
