using BenchmarkDotNet.Attributes;
using System.IO;

namespace Pfarah.Benchmarks
{
    public class ParseFile
    {
        private byte[] data;

        [Setup]
        public void SetupData()
        {
            data = File.ReadAllBytes(Payload);
        }

        [Params("achievements.txt")]
        public string Payload { get; set; }

        [Benchmark]
        public ParaValue ReadFile()
        {
            using (var mem = new MemoryStream(data))
            {
                return ParaValue.LoadText(mem);
            }
        }
    }
}
