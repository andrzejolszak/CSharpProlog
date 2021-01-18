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

        public static void True(this string query, string consult = null, bool executionDetails = true, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);
            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(!ss.HasError && ss.Success,
                $"{query} NOT TRUE @ ln {sourceLineNumber}\nOUT: {ss}, \nERR:{ss.ErrMsg}\nExecDetails:{e.ExecutionDetails?.CurrentTermHistoryString}");

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString(Dynamics + "test :- " + query + ".");
                ss = e.GetAllSolutions(null, "test", 5);

                Assert.True(!ss.HasError && ss.Success,
                    $"test NOT TRUE @ ln {sourceLineNumber}\nOUT: {ss}\nERR:{ss.ErrMsg}\nExecDetails:{e.ExecutionDetails?.CurrentTermHistoryString}");
            }
        }

        public static void False(this string query, string consult = null, bool executionDetails = true, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);

            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(!ss.HasError && !ss.Success,
                $"{query} NOT FALSE @ ln {sourceLineNumber}\nOUT: {ss}\nERR:{ss.ErrMsg}\nExecDetails:{e.ExecutionDetails?.CurrentTermHistoryString}");

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString(Dynamics + "test :- " + query + ".");

                ss = e.GetAllSolutions(null, "test", 5);

                Assert.True(!ss.HasError && !ss.Success,
                    $"{query} NOT FALSE @ ln {sourceLineNumber}\nOUT: {ss}\nERR:{ss.ErrMsg}\nExecDetails:{e.ExecutionDetails?.CurrentTermHistoryString}");
            }
        }

        public static void Error(this string query, string consult = null, bool executionDetails = true, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);

            e.ConsultFromString(Dynamics + "\n" + (consult ?? ""));

            SolutionSet ss = e.GetAllSolutions(null, query, 5);

            Assert.True(ss.HasError && !ss.Success,
                $"{query} NOT ERROR @ ln {sourceLineNumber}\nOUT: {ss}\nERR:{ss.ErrMsg}\nExecDetails:{e.ExecutionDetails?.CurrentTermHistoryString}");
        }

        public static void Evaluate(this string test, string consult = null, bool executionDetails = true)
        {
            string expectation = test.Substring(0, 3);
            string query = test.Substring(3);

            switch (expectation)
            {
                case "T: ":
                    query.True(consult, executionDetails: executionDetails);
                    break;

                case "F: ":
                    query.False(consult, executionDetails: executionDetails);
                    break;

                case "P: ":
                    query.Error(consult, executionDetails: executionDetails);
                    break;

                case "R: ":
                    query.Error(consult, executionDetails: executionDetails);
                    break;

                default:
                    throw new InvalidOperationException("Not supported expectation: " + expectation);
            }
        }
    }
}