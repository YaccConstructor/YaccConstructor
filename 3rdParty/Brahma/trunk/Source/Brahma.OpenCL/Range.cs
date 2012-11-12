using System.Collections.Generic;

namespace Brahma.OpenCL
{
    public static class Range
    {
        [KernelCallable]
        public static IEnumerable<int> Make(int startValue, int count, int step = 1)
        {
            int value = startValue;
            yield return startValue;
            for (int i = 1; i < count; i++)
            {
                value += step;
                yield return value;
            }
        }
    }
}