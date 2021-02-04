[![Build Status](https://github.com/andrzejolszak/CSharpProlog/workflows/dotNet/badge.svg)](https://github.com/andrzejolszak/CSharpProlog/actions)

[![Tests](https://gist.github.com/andrzejolszak/5db25bb87d75d59fa492a7ef3365da84/raw/csharpprolog_tests.md_badge.svg)](https://gist.github.com/andrzejolszak/5db25bb87d75d59fa492a7ef3365da84)

A C# implementation of Prolog

```csharp
using System;
using Prolog;

class Program
{
    static void Main(string[] args)
    {
        var prolog = new PrologEngine();
        
        prolog.ConsultFromString("human(socrates).");
        prolog.ConsultFromString("mortal(X) :- human(X).");
        
        var solution = prolog.GetFirstSolution(query: "mortal(socrates).");
        Console.WriteLine(solution.Solved); // = "True" (Yes!)
    }
}
```
## About this fork

This is an experimental backwards-incompatible fork of jsakamoto's repository https://github.com/jsakamoto/CSharpProlog/.
The focus here is only C# library-based usage and IDE-based usage. The IDE is completely overhauled, and REPL-specific features will get gradually removed.

Further development will focus on: bringing the Prolog behavior closer to the ISO standard, code clean up and optimization, unit tests, refactoring of static mutable state, and removing non-core-prolog features (e.g. SQL, json, xml, regex, etc.).
All this will happen at the cost C# API-level backwards compatibility.

## Solution Layout
### CSProlog
Prolog Engine

### CSProlog.Core.Test
Unit Tests

### IDE
Windows Forms IDE


## For more documents

Earlier release documents can be found in [README (2007-2014).pdf](README%20(2007-2014).pdf).


### Older versions

Earlier release notes can be found in [README (2007-2014).pdf](README%20(2007-2014).pdf).

## License

[GNU LGPL v.3](LICENSE)
