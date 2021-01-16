using System;
using System.Linq;
using System.Runtime.CompilerServices;
using Prolog;
using Xunit;
using static Prolog.PrologEngine;

namespace CSPrologTest
{
    public static class TestUtils
    {
    }

    public static class PrologSourceStringExtensions
    {
        private static readonly string Dynamics = @"
:- fail_if_undefined( nofoo/1 ).
:- fail_if_undefined( undef_pred/0 ).
:- fail_if_undefined( '\='/2 ).
:- fail_if_undefined( foo/2 ).

%:- dynamic( nofoo/1 ).
%:- dynamic( undef_pred/0 ).
%:- dynamic( '\='/2 ).
%:- dynamic( foo/2 ).
";

        public static PredicateDescr CanParse(this string consult, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine();
            e.ConsultFromString(Dynamics + consult);
            return e.PredTable.Predicates.Where(x => !x.Value.IsPredefined).FirstOrDefault().Value;
        }

        public static void True(this string query, string consult = null, ExecutionDetails execDetails = null, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(execDetails);
            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(!ss.HasError && ss.Success,
                $"{query} NOT TRUE @ ln {sourceLineNumber}\n\nOUT: {ss}, \n\nERR:{ss.ErrMsg}");

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString(Dynamics + "test :- " + query + ".");
                ss = e.GetAllSolutions(null, "test", 5);

                Assert.True(!ss.HasError && ss.Success,
                    $"test NOT TRUE @ ln {sourceLineNumber}\n\nOUT: {ss}\n\nERR:{ss.ErrMsg}");
            }
        }

        public static void False(this string query, string consult = null, ExecutionDetails execDetails = null, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(execDetails);

            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(!ss.HasError && !ss.Success,
                $"{query} NOT FALSE @ ln {sourceLineNumber}\n\nOUT: {ss}\n\nERR:{ss.ErrMsg}");

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString(Dynamics + "test :- " + query + ".");

                ss = e.GetAllSolutions(null, "test", 5);

                Assert.True(!ss.HasError && !ss.Success,
                    $"{query} NOT FALSE @ ln {sourceLineNumber}\n\nOUT: {ss}\n\nERR:{ss.ErrMsg}");
            }
        }

        public static void Error(this string query, string consult = null, ExecutionDetails execDetails = null, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(execDetails);

            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(ss.HasError && !ss.Success,
                $"{query} NOT ERROR @ ln {sourceLineNumber}\n\nOUT: {ss}\n\nERR:{ss.ErrMsg}");
        }

        public static void Evaluate(this string test, string consult = null, ExecutionDetails execDetails = null)
        {
            string expectation = test.Substring(0, 3);
            string query = test.Substring(3);

            switch (expectation)
            {
                case "T: ":
                    query.True(consult, execDetails: execDetails);
                    break;

                case "F: ":
                    query.False(consult, execDetails: execDetails);
                    break;

                case "P: ":
                    query.Error(consult, execDetails: execDetails);
                    break;

                case "R: ":
                    query.Error(consult, execDetails: execDetails);
                    break;

                default:
                    throw new InvalidOperationException("Not supported expectation: " + expectation);
            }
        }
    }
}