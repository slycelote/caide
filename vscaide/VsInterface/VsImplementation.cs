namespace slycelote.VsCaide.VsInterface
{
    public static class VsImplementation
    {
        private static IVsServices _services;

        public static IVsServices Services
        {
            get => _services ?? throw new CaideException("Services singleton is not initialized");
            set => _services = value;
        }
    }
}

