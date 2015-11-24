using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Persimmon;
using Persimmon.Dried;

namespace Persimmon.Dried.CSharp.Tests
{
    public static class CSharpTest
    {
        public static TestCase<Unit> syntaxCheck()
        {
            return Property.Default
                .Add(Syntax.Prop.ForAll(Arb.Int, i =>
                    (new Lazy<bool>(() => (i + 1) % 2 != 0)).When(i % 2 == 0)));
        }
    }
}
