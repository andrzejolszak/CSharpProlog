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
            consult = consult.Replace("\r\n", Environment.NewLine);
            PrologEngine e = new PrologEngine();
            e.ConsultFromString(consult + Dynamics);
            return e.PredTable.Predicates.Where(x => !x.Value.IsPredefined).FirstOrDefault().Value;
        }

        public static void True(this string query, string consult = null, bool executionDetails = true, int? solutionsCount = null, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);
            e.ConsultFromString((consult ?? "") + Dynamics);

            SolutionSet ss = e.GetAllSolutions(query, 5);

            Assert.True(!ss.HasError && ss.Success,
                $"{query} NOT TRUE @ ln {sourceLineNumber}{Environment.NewLine}OUT: {ss}, {Environment.NewLine}ERR:{ss.ErrMsg}{Environment.NewLine}ExecDetails:{e.ExecutionDetails?.CallHistoryStringWithLinesLast10}");

            if (solutionsCount.HasValue)
            {
                Assert.Equal(solutionsCount.Value, ss.SolutionsCount);
            }

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString("test :- " + query + "." + Dynamics);
                ss = e.GetAllSolutions("test", 5);

                Assert.True(!ss.HasError && ss.Success,
                    $"test NOT TRUE @ ln {sourceLineNumber}{Environment.NewLine}OUT: {ss}{Environment.NewLine}ERR:{ss.ErrMsg}{Environment.NewLine}ExecDetails:{e.ExecutionDetails?.CallHistoryStringWithLinesLast10}");
            }
        }

        public static void False(this string query, string consult = null, bool executionDetails = true, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);

            e.ConsultFromString((consult ?? "") + Dynamics);

            SolutionSet ss = e.GetAllSolutions(query, 5);

            Assert.True(!ss.HasError && !ss.Success,
                $"{query} NOT FALSE @ ln {sourceLineNumber}{Environment.NewLine}OUT: {ss}{Environment.NewLine}ERR:{ss.ErrMsg}{Environment.NewLine}ExecDetails:{e.ExecutionDetails?.CallHistoryStringWithLinesLast10}");

            if (consult == null)
            {
                e.Reset();
                e.ConsultFromString("test :- " + query + "." + Dynamics);

                ss = e.GetAllSolutions("test", 5);

                Assert.True(!ss.HasError && !ss.Success,
                    $"{query} NOT FALSE @ ln {sourceLineNumber}{Environment.NewLine}OUT: {ss}{Environment.NewLine}ERR:{ss.ErrMsg}{Environment.NewLine}ExecDetails:{e.ExecutionDetails?.CallHistoryStringWithLinesLast10}");
            }
        }

        public static void Error(this string query, string consult = null, bool executionDetails = true, [CallerLineNumber] int sourceLineNumber = 0)
        {
            PrologEngine e = new PrologEngine(executionDetails ? new ExecutionDetails() : null);

            e.ConsultFromString((consult ?? "") + Dynamics);

            SolutionSet ss = e.GetAllSolutions(query, 5);

            Assert.True(ss.HasError && !ss.Success,
                $"{query} NOT ERROR @ ln {sourceLineNumber}{Environment.NewLine}OUT: {ss}{Environment.NewLine}ERR:{ss.ErrMsg}{Environment.NewLine}ExecDetails:{e.ExecutionDetails?.CallHistoryStringWithLinesLast10}");
        }

        public static void Evaluate(this string test, string consult = null, bool executionDetails = true)
        {
            string expectation = test.Substring(0, 3);
            string query = test.Substring(3);

            switch (expectation)
            {
                case "T: ":
                    // TODO: treat as T1 in the future
                    query.True(consult, executionDetails: executionDetails);
                    break;

                case "T1:":
                    query.True(consult, executionDetails: executionDetails, solutionsCount: 1);
                    break;

                case "T2:":
                    query.True(consult, executionDetails: executionDetails, solutionsCount: 2);
                    break;

                case "T3:":
                    query.True(consult, executionDetails: executionDetails, solutionsCount: 3);
                    break;

                case "T4:":
                    query.True(consult, executionDetails: executionDetails, solutionsCount: 4);
                    break;

                case "T5:":
                    query.True(consult, executionDetails: executionDetails, solutionsCount: 5);
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