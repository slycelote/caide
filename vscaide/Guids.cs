// Guids.cs
// MUST match guids.h
using System;

namespace slycelote.VsCaide
{
    static class GuidList
    {
        public const string guidVsCaidePkgString = "8e97a36f-88cc-49ee-8e47-df660b4c7d83";
        public const string guidVsCaideCmdSetString = "0f1a8efd-0e8f-47d9-8c03-16bc84b980ca";
        public const string guidToolWindowPersistanceString = "75f238bd-f9f7-44f8-ac0c-b88f9b529ad0";

        public static readonly Guid guidVsCaideCmdSet = new Guid(guidVsCaideCmdSetString);
    };
}