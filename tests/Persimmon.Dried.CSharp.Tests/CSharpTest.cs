﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Persimmon;
using Persimmon.Dried;

namespace Persimmon.Dried.CSharp.Tests
{
    public class CSharpTest
    {
        public TestCase<Unit> syntaxCheck()
        {
            return Property.Default
                .Add(Syntax.Prop.forAll(Arb.Int, i =>
                    (i % 2 == 0).Require(new Lazy<bool>(() =>
                        (i + 1) % 2 != 0))));
        }
    }
}
