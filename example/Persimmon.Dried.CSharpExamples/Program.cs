using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

// https://github.com/fsharp/FsCheck/blob/980fa9ec57d9899e1ef66edeaeb2bd52d7abb980/examples/FsCheck.CSharpExamples/Program.cs

namespace Persimmon.Dried.CSharpExamples
{
    public static class Extensions
    {
        public static IEnumerable<int> Insert(this IEnumerable<int> cs, int x)
        {
            var result = new List<int>(cs);
            foreach (var c in cs)
            {
                if (x <= c)
                {
                    result.Insert(result.IndexOf(c), x);
                    return result;
                }
            }
            result.Add(x);
            return result;
        }

        public static bool IsOrdered<T>(this IEnumerable<T> source)
        {
            //by Jon Skeet
            //I was too lazy to write it myself, and wondered whether a prettier 
            //solution might exist in C# than the one I had in mind.
            //Here's your answer...
            var comparer = Comparer<T>.Default;
            T previous = default(T);
            bool first = true;

            foreach (T element in source)
            {
                if (!first && comparer.Compare(previous, element) > 0)
                {
                    return false;
                }
                first = false;
                previous = element;
            }
            return true;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {

            var property = Property.Default.Callback(Runner.createConsoleReporter(100));

            //A simple example
            property
                .Add(Syntax.Prop.ForAll(Arb.Array(Arb.Int), xs => xs.Reverse().Reverse().SequenceEqual(xs)))
                .Run("RevRev");

            property
                .Add(Syntax.Prop.ForAll(Arb.Array(Arb.Int), xs => xs.Reverse().SequenceEqual(xs)))
                .Run("RevId");

            //--------Properties--------------
            property
                .Add(Syntax.Prop.ForAll(Arb.Array(Arb.Single), xs => xs.Reverse().Reverse().SequenceEqual(xs)))
                .Run("RevRevFloat");

            //conditional properties
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Array(Arb.Int), (x, xs) =>
                    new Lazy<bool>(() => xs.Insert(x).IsOrdered())
                        .When(xs.IsOrdered())))
                .Run("Insert");

            property
                .Add(Syntax.Prop.ForAll(Arb.Int, a =>
                    new Lazy<bool>(() => 1 / a == 1 / a)
                        .When(a != 0)))
                .Run("DivByZero");

            //counting trivial cases
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Array(Arb.Int), (x, xs) =>
                    new Lazy<bool>(() =>xs.Insert(x).IsOrdered())
                        .When(xs.IsOrdered())
                        .Classify(xs.Count() == 0, "trivial")))
                .Run("InsertTrivial");

            //classifying test values
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Array(Arb.Int), (x, xs) =>
                    new Lazy<bool>(() => xs.Insert(x).IsOrdered())
                    .When(xs.IsOrdered())
                    .Classify(new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail")))
                .Run("InsertClassify");

            //collecting data values
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Array(Arb.Int), (x, xs) =>
                    new Lazy<bool>(() => xs.Insert(x).IsOrdered())
                    .When(xs.IsOrdered())
                    .Collect("length " + xs.Count().ToString())))
                .Run("InsertCollect");

            //combining observations
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Array(Arb.Int), (x, xs) =>
                    new Lazy<bool>(() => xs.Insert(x).IsOrdered())
                        .When(xs.IsOrdered())
                        .Classify(new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                        .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                        .Collect("length " + xs.Count().ToString())))
                .Run("InsertCombined");

            //---labelling sub properties-----
            //hmm. Cannot express result = m + n once this way.
            property
                .Add(Syntax.Prop.ForAll(Arb.Int, Arb.Int, (m, n) =>
                    (m + n >= m).Label("result > #1") //maybe add overload with label to ForAll?
                    .And(() => (m + n >= n).Label("result > #2"))
                    .And(() => (m + n < m + n).Label("result not sum"))))
                .Run("ComplexProp");

            property
                .Add(Syntax.Prop.ForAll(Arb.Int, x =>
                    false.Label("Always false")
                    .And(() => Math.Abs(x) - x == 0))) //actually, And should start a new property, not just a new assertion...
                .Run("Label");

            //rest seem hard to express without real "And" and "Or" support

            //-------Test data generators-----------
            //can't be made generic, only in separate method?
            //Func<int[],Gen<int>> chooseFromList = xs =>
            //    from i in Gen.Choose(0,xs.Length-1)
            //    select xs[i];
            
            //var chooseBool = Gen.OneOf( Gen.Constant( true), Gen.Constant(false));

            ////no tuples in C# until BCL 4.0...can we do better now?
            //var chooseBool2 = Gen.Frequency(
            //    new WeightAndValue<Gen<bool>>(2, Gen.Constant(true)),
            //    new WeightAndValue<Gen<bool>>(1, Gen.Constant(false)));

            ////the size of test data : see matrix method

            ////generating recursive data types: not so common in C#?

            //// generating functions:
            //Prop.ForAll((Func<int, int> f, Func<int, int> g, ICollection<int> a) => {
            //                var l1 = a.Select(x => f(g(x)));
            //                var l2 = a.Select(g).Select(f);
            //                return l1.SequenceEqual(l2);
            //            }).QuickCheck();

            ////generators support select, selectmany and where
            //var gen = from x in Arb.Generate<int>()
            //          from y in Gen.Choose(5, 10)
            //          where x > 5
            //          select new { Fst = x, Snd = y };

            ////registering default arbitrary instances
            //Arb.Register<MyArbitraries>();

            //Prop.ForAll<long>(l => l + 1 > l)
            //    .QuickCheck();

            //Prop.ForAll<string>(s => true)
            //    .Check(new Configuration { Name = "Configuration Demo", MaxNbOfTest = 500 });

            //Prop.ForAll((IEnumerable<int> a, IEnumerable<int> b) => 
            //                a.Except(b).Count() <= a.Count())
            //    .QuickCheck();

            Console.ReadKey();
        }

        //public static Gen<T> Matrix<T>(Gen<T> gen)
        //{
        //    return Gen.Sized(s => gen.Resize(Convert.ToInt32(Math.Sqrt(s))));
        //}

        //public class ArbitraryLong : Arbitrary<long>
        //{
        //    public override Gen<long> Generator
        //    {
        //        get {
        //            return Gen.Sized(s => Gen.Choose(-s, s))
        //                .Select(i => Convert.ToInt64(i));
        //        }
        //    }
        //}

        //public class MyArbitraries
        //{
        //    public static Arbitrary<long> Long() { return new ArbitraryLong(); }

        //    public static Arbitrary<IEnumerable<T>> Enumerable<T>() {
        //        return Arb.Default.Array<T>().Convert(x => (IEnumerable<T>)x, x => (T[])x);
        //    }

        //    public static Arbitrary<StringBuilder> StringBuilder() {
        //        return Arb.Generate<string>().Select(x => new StringBuilder(x)).ToArbitrary();
        //    }
        //}
    }
}
