namespace slycelote.VsCaide.VsInterface
{
    using System;

    [Serializable]
    public class CaideException: Exception
    {
        public CaideException(string message)
            : base(message)
        { }
    }
}
