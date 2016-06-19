using BenchmarkDotNet.Attributes;
using System;
using System.Globalization;
using System.Text;

namespace Pfarah.Benchmarks
{
    public class ParseDateTime
    {
        private byte[] data;

        [Setup]
        public void SetupData()
        {
            data = Encoding.UTF8.GetBytes(Payload);
        }

        [Params("2015.8.1", "99999.8.1", "1942.5.2.14", "abc")]
        public string Payload { get; set; }

        [Benchmark]
        public DateTime? pfarahParseDateTime()
        {
            return Utils.parseDate(data, data.Length);
        }

        [Benchmark]
        public DateTime? bclDateTime()
        {
            DateTime res;
            if (DateTime.TryParseExact(Payload, new string[] { "yyyy.MM.dd", "yyyy.MM.dd.HH" },
                CultureInfo.InvariantCulture, DateTimeStyles.None, out res))
            {
                return res;
            }

            return null;
        }
    }
}
