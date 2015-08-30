using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace slycelote.VsCaide.Utilities
{
    [Serializable]
    public class CaideException: Exception
    {
        public CaideException(string message)
            : base(message)
        { }
    }
}
