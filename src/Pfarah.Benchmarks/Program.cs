using BenchmarkDotNet.Running;

namespace Pfarah.Benchmarks
{
    class Program
    {
        static void Main()
        {
            BenchmarkRunner.Run<ParseDouble>();
            BenchmarkRunner.Run<ParseDateTime>();
        }
    }
}
