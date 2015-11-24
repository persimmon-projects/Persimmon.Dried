using System;
using System.Linq;
using Persimmon.Dried.Ext;

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

        public class Foo
        {
            public int Bar { get; set; }
            public int Buz { get; set; }
        }

        public static Gen<Foo> genFoo =
            from x in Arb.Int.Gen
            from y in Arb.Int.Gen.Where(i => i != x)
            select new Foo { Bar = x, Buz = y };

        public static Arbitrary<Foo> arbFoo = Arbitrary.Create(genFoo, Shrink.Any<Foo>(), PrettyModule.Any);

        public static TestCase<Unit> querySyntaxCheck()
        {
            return Property.Default
                .Add(Syntax.Prop.ForAll(arbFoo, foo => foo.Bar != foo.Buz));
        }
    }
}
