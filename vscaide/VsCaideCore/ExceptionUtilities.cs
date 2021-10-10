namespace slycelote.VsCaide.Core
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Runtime.CompilerServices;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;


    public static class ExceptionUtilities
    {
        public static T CatchAll<T>(T returnValueOnException, Func<T> action, 
            [CallerMemberName] string functionName = null)
        {
            try
            {
                return action();
            }
            catch (Exception e) when (IsNonFatal(e))
            {
                try
                {
                    Logger.LogError("Exception in {0}: {1}", functionName, e);
                }
                catch (Exception e2) when (IsNonFatal(e2))
                {  }

                return returnValueOnException;
            }
        }

        public static void CatchAll(Action action, [CallerMemberName] string functionName = null) => 
            CatchAll(0, () =>
            {
                action();
                return 0;
            }, functionName);

        public static async Task CatchAllAsync(Func<Task> action, [CallerMemberName] string functionName = null)
        {
            try
            {
                await action().ConfigureAwait(false);
            }
            catch (Exception e) when (IsNonFatal(e))
            {
                try
                {
                    Logger.LogError("Exception in {0}: {1}", functionName, e);
                }
                catch (Exception e2) when (IsNonFatal(e2))
                {  }
            }
        }

        public static bool IsNonFatal(Exception e)
        {
            return !(e is ThreadAbortException || e is OutOfMemoryException
                || e is StackOverflowException);
        }
    }
}
