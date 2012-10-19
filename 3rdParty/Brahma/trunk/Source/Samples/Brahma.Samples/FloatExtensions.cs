namespace Brahma.Samples
{
    public static class FloatExtensions
    {
        public const float Eps = 0.001f;
        
        public static bool IsCloseTo(this float actual, float expected, float epsilon = Eps)
        {
            return actual * actual - expected * expected <= epsilon;
        }
    }
}
