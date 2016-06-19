using BenchmarkDotNet.Attributes;
using System.Text;
using System.Globalization;

namespace Pfarah.Benchmarks
{
    public class ParseDouble
    {
        private byte[] data;
        private readonly NumberStyles numStyle = NumberStyles.AllowDecimalPoint | NumberStyles.AllowLeadingSign;

        [Setup]
        public void SetupData()
        {
            data = Encoding.UTF8.GetBytes(Payload);
        }

        [Params("50", "15.3", "1.000", "0.98765", "abc")]
        public string Payload { get; set; }

        [Benchmark]
        public double? pfarahParseDouble()
        {
            return Utils.parseDouble(data, data.Length);
        }

        [Benchmark]
        public double? bclParseDouble()
        {
            double res;
            if (double.TryParse("3.0000", numStyle, CultureInfo.InvariantCulture, out res))
            {
                return res;
            }

            return null;
        }
    }
}
