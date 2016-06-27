using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Diagnostics.Windows;
using BenchmarkDotNet.Running;

namespace Pfarah.Benchmarks
{
    class Program
    {
        static void Main()
        {
            BenchmarkRunner.Run<ParseDouble>();
            BenchmarkRunner.Run<ParseDateTime>();
            BenchmarkRunner.Run<ParseFile>(ManualConfig.Create(DefaultConfig.Instance)
                .With(new MemoryDiagnoser()));
        }
    }
}
