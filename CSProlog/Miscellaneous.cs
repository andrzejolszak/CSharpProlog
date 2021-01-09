/*-----------------------------------------------------------------------------------------

  C#Prolog -- Copyright (C) 2007-2015 John Pool -- j.pool@ision.nl

  This library is free software; you can redistribute it and/or modify it under the terms of
  the GNU Lesser General Public License as published by the Free Software Foundation; either 
  version 3.0 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License (http://www.gnu.org/licenses/lgpl-3.0.html), or 
  enter 'license' at the command prompt.

-------------------------------------------------------------------------------------------*/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;

namespace Prolog
{
    public partial class PrologEngine
    {        
                public class ProdConsBuffer<T>
        {
            private Queue<T> queue;
            private const int MAX_Q_SIZE = -1; // no limit
            public int Count => queue.Count;

            public ProdConsBuffer()
            {
                queue = new Queue<T>();
            }

            public void TryEnqueue(T item)
            {
                lock (this)
                {
                    if (queue.Count == MAX_Q_SIZE) Monitor.Wait(this);

                    queue.Enqueue(item);
                    Monitor.Pulse(this);
                }
            }

            public T TryDequeue()
            {
                lock (this)
                {
                    if (queue.Count == 0)
                    {
                        Monitor.Wait(this);
                    }

                    T item = queue.Dequeue();
                    Monitor.Pulse(this);

                    return item;
                }
            }


            public void Clear()
            {
                queue.Clear();
            }

            public T Peek()
            {
                return queue.Peek();
            }

            public bool Contains(T item)
            {
                return queue.Contains(item);
            }
        }
        
                public class Globals
        {
                        public static readonly string DefaultExtension = ".pl";
            


            
            

                        public static CultureInfo CI = CultureInfo.InvariantCulture;
            public static Dictionary<string, bool> ConsultedFiles = new Dictionary<string, bool>();
            //TODO reconsider the use of the statics below
            public static string ConsultFileName = null;   // file being currently consulted
            public static string ConsultModuleName = null; // name of current module (if any) in file being consulted
            public static PrologParser CurrentParser { get; set; } = null;
            public static int LineNo => (CurrentParser == null || CurrentParser.InQueryMode) ? -1 : CurrentParser.LineNo - 1;
            public static int ColNo => (CurrentParser == null) ? -1 : CurrentParser.ColNo;

            

        }
        
        
        public static class Utils
        {
            private static readonly string logFileName = "PL" + DateTime.Now.ToString("yyyy-MM-dd") + ".log";
            private static StreamWriter logFile;
            private static bool showMode = true;

            public static string AtomFromVarChar(string s)
            {
                return s.Replace("'", "''").ToAtom();
            }


            private static string WDF(string fileName)
            {
                if (fileName.IndexOf(Path.DirectorySeparatorChar) == -1)
                    return "." + fileName;

                return fileName;
            }

            public static string FileNameFromTerm(BaseTerm t, string defExt)
            {
                if (t.IsVar) return null;

                return ExtendedFileName(t.FunctorToString, defExt);
            }


            public static string DirectoryNameFromTerm(BaseTerm t)
            {
                if (!t.IsAtomOrString) return null;

                return GetFullDirectoryName(t.FunctorToString.Dequoted());
            }


            public static string GetFullDirectoryName(string wd)
            {
                try
                {
                    if (!wd.EndsWith(Path.DirectorySeparatorChar.ToString()))
                        wd = wd + Path.DirectorySeparatorChar;

                    if (wd.Length == 3 && wd.EndsWith(@":\")) return wd; // GetDirectoryName returns null for root dir

                    return Path.GetDirectoryName(wd);
                }
                catch (Exception e)
                {
                    IO.ErrorRuntime( e.Message, null, null);

                    return null;
                }
            }


            public static string FileNameFromSymbol(string s, string defExt)
            {
                return (String.IsNullOrEmpty(s) ? null : ExtendedFileName(s, defExt));
            }


            public static string ExtendedFileName(string s, string defExt)
            {
                string fileName = null;

                try
                {
                    fileName = s.Dequoted();

                    if (!Path.HasExtension(fileName))
                        fileName = Path.ChangeExtension(fileName, defExt);

                    fileName = WDF(fileName);

                    return Path.GetFullPath(fileName);
                }
                catch (Exception e)
                {
                    IO.ErrorRuntime($"Error in file name '{fileName}'\r\n{e.Message}", null, null);

                    return null;
                }
            }


            public static BaseTerm FindRegexMatches(
              OperatorTable opTable, string source, string matchPattern, string[] options)
            {
                Regex re = null;
                RegexOptions reOptions = RegexOptions.None;

                if (options != null)
                {
                    foreach (string o in options)
                        switch (o.ToLower())
                        {
                            case "ignorecase":
                                reOptions |= RegexOptions.IgnoreCase;
                                break;
                            case "multiline":
                                reOptions |= RegexOptions.Multiline;
                                break;
                            case "singleline":
                                reOptions |= RegexOptions.Singleline;
                                break;
                            case "explicitcapture":
                                reOptions |= RegexOptions.ExplicitCapture;
                                break;
                            case "cultureinvariant":
                                reOptions |= RegexOptions.CultureInvariant;
                                break;
                            default:
                                IO.ErrorRuntime($"match_regex -- unsupported option '{o}'", null, null);
                                break;
                        };
                }

                try
                {
                    re = new Regex(matchPattern, reOptions); //, RegexOptions.Multiline);
                }
                catch (Exception x)
                {
                    IO.ErrorRuntime($"Error in regular expression '{matchPattern}'\r\nMessage: {x.Message}", null, null);
                }

                Match m = re.Match(source);

                if (!m.Success) return null;

                int[] gnums = re.GetGroupNumbers();
                BaseTerm groups = new ListTerm(null);

                while (m.Success)
                {
                    for (int i = 1; i < gnums.Length; i++) // start at group 1 (0 is the fully captured match string)
                    {
                        Group g = m.Groups[gnums[i]];
                        BaseTerm captures = new ListTerm(null);
                        string groupId = re.GetGroupNames()[i];
                        int groupNo;
                        BaseTerm groupIdTerm;

                        foreach (Capture c in g.Captures)
                            captures = ((ListTerm)captures).AppendElement(new StringTerm(captures.Symbol, c.ToString()));

                        if (int.TryParse(groupId, out groupNo))
                            groupIdTerm = new DecimalTerm(captures.Symbol, groupNo);
                        else
                            groupIdTerm = new StringTerm(captures.Symbol, re.GetGroupNames()[i]);

                        groups = ((ListTerm)groups).AppendElement(
                          new OperatorTerm(captures.Symbol, opTable, ":", groupIdTerm, captures));
                    }

                    m = m.NextMatch();
                }

                if (((ListTerm)groups).ProperLength == 1)
                {
                    groups = groups.Arg(0); // groups is <groupname>:[<captures>]

                    if (groups.Arg(0) is DecimalTerm)
                        groups = groups.Arg(1); // anonymous group, just return the list of captures
                }

                return groups;
            }


            public static string Format(ListTerm lt)
            {
                if (!lt.IsProperList || lt.ProperLength != 2)
                {
                    IO.ErrorRuntime( "Format list must be a proper list of length 2", null, lt);

                    return null;
                }

                BaseTerm fstring = lt.Arg(0);
                BaseTerm args = lt.Arg(1).Arg(0);

                return Format(fstring, args);
            }


            public static string Format(BaseTerm t, BaseTerm args)
            {
                if (!(t is StringTerm))
                {
                    IO.ErrorRuntime("Improper format string", null, t);

                    return null;
                }

                return Format(t.FunctorToString, args);
            }


            public static string Format(string fmtString, BaseTerm args)
            {
                string result = fmtString;

                if (args is ListTerm)
                {
                    ListTerm lt = (ListTerm)args;

                    if (!lt.IsProperList) return null;

                    try
                    {
                        return string.Format(result, lt.ToStringArray());
                    }
                    catch
                    {
                        IO.ErrorRuntime($"Error while applying arguments to format string '{result}'", null, lt);
                    }
                }
                else
                    try
                    {
                        return string.Format(result, args.ToString().Dequoted("'").Dequoted("\""));
                    }
                    catch
                    {
                        IO.ErrorRuntime($"Error while applying arguments to format string '{result}'", null, args);
                    }

                return null;
            }

            public static void SetClipboardData(string data)
            {
            }

            public static string WrapWithMargin(string s, string margin, int lenMax)
            // Break up a string into pieces that are at most lenMax characters long, by
            // inserting spaces that are at most lenMax positions apart from each other.
            // If possible, spaces are inserted after 'separator characters'; otherwise
            // they are simply inserted at each lenMax position. Prefix a margin to the 2nd+ string.
            {
                const string separators = @" +-/*^!@():,.;=[]{}<>\";

                StringBuilder sb = new StringBuilder();
                bool first = true;
                int p = 0;
                int rem = s.Length - p;

                while (rem > lenMax)
                {
                    // get the position < lenMax of the last separator character
                    int i = s.Substring(p, lenMax).LastIndexOfAny(separators.ToCharArray(), lenMax - 1);
                    int segLen;

                    if (i == -1) segLen = lenMax; else segLen = i + 1;

                    if (first) first = false; else sb.Append(margin);
                    sb.Append(s.Substring(p, segLen));
                    sb.Append(Environment.NewLine);

                    p += segLen;
                    rem -= segLen;
                }

                if (first) first = false; else sb.Append(margin);

                if (rem != 0)
                {
                    sb.Append(s.Substring(p));
                    sb.Append(Environment.NewLine);
                }

                return sb.ToString();
            }


            public static void Assert(bool b, string s) // testing only
            {
                if (!b)
                    IO.WriteLine("Assertion violated:\r\n" + s);
                //throw new Exception ("Assertion violated:\r\n" + s);
            }


            public static void Assert(bool b, string s, params object[] o)
            {
                if (!b)
                    IO.WriteLine("Assertion violated:\r\n" + string.Format(s, o));
                //throw new Exception ("Assertion violated:\r\n" + string.Format (s, o));
            }


            public static void Check(bool b, string s)
            {
                if (!b)
                {
                    IO.Warning("Warning -- Check violated:\r\n" + s);
                }
            }


            public static void Check(bool b, string s, params object[] o)
            {
                if (!b)
                {
                    IO.Warning("Warning -- Check violated:\r\n" + string.Format(s, o));
                }
            }


            public static string ForceSpaces(string s, int lenMax)
            // Break up a string into pieces that are at most lenMax characters long, by
            // inserting spaces that are at most lenMax positions apart from each other.
            // If possible, spaces are inserted after 'separator characters'; otherwise
            // they are simply inserted at each lenMax position.
            {
                const string separators = " -/:,.;";

                if (lenMax < 2) IO.ErrorRuntime("Second argument of wrap must be > 1", null, null);

                return ForceSpaces(s, lenMax - 1, separators, 0); // 0 is current pos in separators
            }


            private static string ForceSpaces(string s, int lenMax, string separators, int i)
            {
                int len = s.Length;
                StringBuilder sb = new StringBuilder();
                string blank = Environment.NewLine;

                // special cases
                if (len <= lenMax) return s; // nothing to do

                if (i == separators.Length) // done with all separators -- now simply insert spaces
                {
                    int r = len; // rest

                    while (r > 0)
                    {
                        sb.Append(s.Substring(len - r, (r > lenMax) ? lenMax : r));
                        sb.Append(blank);
                        r -= lenMax;
                    }
                    return sb.ToString().Trim();
                }
                // end of special cases

                string[] words = s.Split(new Char[] { separators[i] }); // split, using the current separator

                for (int k = 0; k < words.Length; k++)
                {
                    string t = ForceSpaces(words[k], lenMax, separators, i + 1); // apply the next separator to each word

                    // do not re-place the separator after the last word
                    sb.Append(t + (k == words.Length - 1 ? "" : (separators[i] + blank))); // recursively handle all seps
                }

                return sb.ToString();
            }

            // Return the ISO week Number for a date. Week 1 of a year is the
            // first week of the year in which there are more than three days, i.e.
            // the week in which the first Thursday of the year lies.
            public static int WeekNo(DateTime date)
            {
                // special case: if the date is in a week that starts on December 29,
                // 30 or 31 (i.e. date day in sun..wed), then return 1
                if (date.Month == 12 && date.Day >= 29 && date.DayOfWeek <= DayOfWeek.Wednesday)
                    return 1;

                DateTime jan1 = new DateTime(date.Year, 1, 1); // January 1st
                DayOfWeek jan1Day = jan1.DayOfWeek;
                // Jan 1st is in week 1 if jan1Day is in sun..wed, since only in that case
                // there are > 3 days in the week. Calculate the start date of week 1.
                // We want to know the number of days of week 1 that are in week 1
                bool jan1inWk1 = (jan1Day <= DayOfWeek.Wednesday); // indicates whether Jan 1st is in week 1
                DateTime startWk1 =
                  jan1.Subtract(new TimeSpan((int)jan1Day - (jan1inWk1 ? 7 : 0), 0, 0, 0));
                // Calculate the Number of days between the given date and the start
                // date of week 1 and (integer) divide that by 7. This is the weekno-1.
                return 1 + (date - startWk1).Days / 7;
            }

            [DllImport("netapi32.dll")]
            private static extern short NetMessageBufferSend(IntPtr server, IntPtr recipient, IntPtr reserved, IntPtr message, int size);

            public static void SendNetBios(string server, string recipient, string text)
            {
                IntPtr srv = IntPtr.Zero, rcp = IntPtr.Zero, txt = IntPtr.Zero, res = IntPtr.Zero;

                try
                {
                    srv = Marshal.StringToBSTR(server);
                    rcp = Marshal.StringToBSTR(recipient);
                    txt = Marshal.StringToBSTR(text = $"{server}/{recipient}: {text}");

                    NetMessageBufferSend(srv, rcp, res, txt, (text.Length + 1) * 2);
                }
                catch (Exception /*engine*/)
                {
                    ;
                }
                finally
                {
                    if (srv != IntPtr.Zero)
                        Marshal.FreeBSTR(srv);
                    if (rcp != IntPtr.Zero)
                        Marshal.FreeBSTR(rcp);
                    if (txt != IntPtr.Zero)
                        Marshal.FreeBSTR(txt);
                }
            }


            // Console

            private class Constants
            {
                // Standard input, output, and Error
                internal const int STD_INPUT_HANDLE = -10;
                internal const int STD_OUTPUT_HANDLE = -11;
                internal const int STD_ERROR_HANDLE = -12;

                // Returned by GetStdHandle when an Error occurs
                internal static readonly IntPtr INVALID_HANDLE_VALUE = new IntPtr(-1);
            }

            private struct COORD
            {
                internal short X;
                internal short Y;
            }

            private struct SMALL_RECT
            {
                internal short Left;
                internal short Top;
                internal short Right;
                internal short Bottom;
            }

            private struct CONSOLE_SCREEN_BUFFER_INFO
            {
                internal COORD dwSize;
                internal COORD dwCursorPosition;
                internal ushort wAttributes;
                internal SMALL_RECT srWindow;
                internal COORD dwMaximumWindowSize;
            }

            [DllImport("kernel32.dll", SetLastError = true)]
            private static extern bool GetConsoleScreenBufferInfo(
              IntPtr hConsoleOutput,
              out CONSOLE_SCREEN_BUFFER_INFO lpConsoleScreenBufferInfo
            );

            [DllImport("kernel32.dll", SetLastError = true)]
            private static extern IntPtr GetStdHandle(
              int whichHandle
            );

            [DllImport("kernel32.dll", SetLastError = true)]
            private static extern IntPtr GetConsoleWindow();

            private static IntPtr GetHandle(int WhichHandle)
            {
                IntPtr h = GetStdHandle(WhichHandle);

                if (h == Constants.INVALID_HANDLE_VALUE)
                {
                    switch (WhichHandle)
                    {
                        case Constants.STD_INPUT_HANDLE:
                            throw new Exception("Can't get standard input handle");
                        //break;
                        case Constants.STD_OUTPUT_HANDLE:
                            throw new Exception("Can't get standard output handle");
                        //break;
                        case Constants.STD_ERROR_HANDLE:
                            throw new Exception("Can't get standard error handle");
                        //break;
                        default:
                            throw new Exception("Apparently invalid parameter to GetHandle");
                    }
                }
                return h;
            }


            public static short NumCols => 80;


            // Log file, debugging
            public static void SetShow(bool mode)
            {
                showMode = mode;
            }

            public static void WriteLogLine(bool abort, string s, params object[] pa)
            {
                if (abort)
                {
                    try
                    {
                        logFile.WriteLine(s, pa);
                        throw new Exception(String.Format(s, pa));
                    }
                    finally
                    {
                        CloseLog();
                    }
                }
                else
                {
                    IO.WriteLine(s, pa);
                    logFile.WriteLine(s, pa);
                }
            }


            public static void WriteLogLine(string s, params object[] pa)
            {
                WriteLogLine(false, s, pa);
            }


            public static void WriteLine(string s, params object[] pa)
            {
                WriteLogLine(false, s, pa);
            }


            public static void WriteLine(string s)
            {
                WriteLogLine(false, s);
            }


            public static void Show(string s, params object[] pa)
            {
                if (showMode) WriteLogLine(false, s, pa);
            }


            public static void Show(string s)
            {
                if (showMode) WriteLogLine(false, s);
            }


            public static void CloseLog()
            {
                logFile.Flush();
                logFile.Dispose();
            }
        }


        private static string Spaces(int n)
        {
            return new string(' ', n);
        }

                public class Combination // without repetition
        {
            private int k;
            public IEnumerator<ListTerm> Iterator { get; }

            public Combination(ListTerm t, int k)
            {
                BaseTermSet ts = new BaseTermSet(t);
                this.k = k;
                Iterator = CombinationsEnum(ts, k).GetEnumerator();
            }


            private IEnumerable<ListTerm> CombinationsEnum(List<BaseTerm> terms, int length)
            {
                for (int i = 0; i < terms.Count; i++)
                {
                    if (length == 1)
                        yield return new ListTerm(terms[i].Symbol, terms[i]);
                    // If > 1, return this one plus all combinations one shorter.
                    // Only use terms after the current one for the rest of the combinations
                    else
                        foreach (BaseTerm next in CombinationsEnum(terms.GetRange(i + 1, terms.Count - (i + 1)), length - 1))
                            yield return new ListTerm(terms[i].Symbol, terms[i], next);
                }
            }
        }
        
                public class Permutation
        {
            private BaseTerm[] configuration;

            public Permutation(ListTerm t)
            {
                BaseTermSet ts = new BaseTermSet(t);
                ts.Sort();
                configuration = ts.ToArray();
            }

            public bool NextPermutation()
            {
                /*
                 Knuth's method
                 1. Find the largest index j such that a[j] < a[j+1]. If no such index exists, 
                    the permutation is the last permutation.
                 2. Find the largest index l such that a[j] < a[l]. Since j+1 is such an index, 
                    l is well defined and satisfies j < l.
                 3. Swap a[j] with a[l].
                 4. Reverse the sequence from a[j+1] up to and including the final element a[n].
                */
                int maxIndex = -1;

                for (var i = configuration.Length - 2; i >= 0; i--)
                {
                    if (configuration[i].CompareTo(configuration[i + 1]) == -1)
                    {
                        maxIndex = i;
                        break;
                    }
                }

                if (maxIndex < 0) return false;

                int maxIndex2 = -1;

                for (int i = configuration.Length - 1; i >= 0; i--)
                    if (configuration[maxIndex].CompareTo(configuration[i]) == -1)
                    {
                        maxIndex2 = i;

                        break;
                    }

                var tmp = configuration[maxIndex];
                configuration[maxIndex] = configuration[maxIndex2];
                configuration[maxIndex2] = tmp;

                for (int i = maxIndex + 1, j = configuration.Length - 1; i < j; i++, j--)
                {
                    tmp = configuration[i];
                    configuration[i] = configuration[j];
                    configuration[j] = tmp;
                }

                return true;
            }

            public IEnumerator<ListTerm> GetEnumerator()
            {
                do
                {
                    yield return ListTerm.ListFromArray(configuration);
                }
                while (NextPermutation());
            }
        }
            }

    public static class Extensions
    {
        // StringBuilder
        public static void AppendPacked(this StringBuilder sb, string s)
        {
            sb.Append(s.Packed());
        }

        public static void AppendPacked(this StringBuilder sb, string s, bool mustPack)
        {
            if (mustPack)
                sb.Append(s.Packed());
            else
                sb.AppendPossiblySpaced(s);
        }

        public static void AppendLine(this StringBuilder sb, string format, params object[] args)
        {
            sb.AppendLine(string.Format(format, args));
        }

        public static void AppendPossiblySpaced(this StringBuilder sb, string s)
        {
            if (string.IsNullOrEmpty(s)) return;

            if (sb.Length == 0)
                sb.Append(s);
            else
            {
                char startChar = s[0];
                char finalChar = sb[sb.Length - 1];

                // Determine whether a space must be inserted. Rules (De Bosschere, cf. docu):
                //
                // 1. A variable or an identifier shall not be followed by an alphanumeric;
                // 2. A quoted token shall not be followed by a quote;
                // 3. A numeric token shall never be followed by a digit;
                // 4. A Special (+, -, :-, ...) shall not be followed by a Special;
                // 5. A prefix operator is separated from an open token ('(', '[', '{') by a space
                //
                // Pathologic cases:
                // - op(300, xfy, .) -> .(1,2) must be output as 1 .2 (*not* as 1 . 2, because
                //                      then the '. ' will be interpreted as end of input.
                // - op(300, xf, e10) -> e10(3.1) must be output as 3.1 e10
                // In practice (and here), numeric constants are separated from identifiers
                // (not from specials) by a space: 10is 30mod 20 => 10 is 30 mod 20
                if ((finalChar.IsSpecialAtomChar() && startChar.IsSpecialAtomChar()) ||
                     (Char.IsLetterOrDigit(finalChar) && Char.IsLetterOrDigit(startChar)) ||
                     (Char.IsDigit(finalChar) && startChar == '.') ||
                     (finalChar == '@' && startChar == '"')
                   )
                    sb.Append(' ');

                sb.Append(s);
            }
        }

        // string
        public static string Reverse(this string s)
        {
            char[] a = s.ToCharArray();
            Array.Reverse(a);

            return new string(a);
        }

        public static string Mirror(this string s)
        {
            char[] a = s.ToCharArray();
            const string pairs = "{}{[][<><()(";
            Array.Reverse(a);
            int p;

            for (int i = 0; i < a.Length; i++)
                if ((p = pairs.IndexOf(a[i])) > -1) a[i] = pairs[p + 1];

            return new string(a);
        }

        public static bool Contains(this string s, char c)
        {
            return s.IndexOf(c) >= 0;
        }

        private static Regex atomPattern = new Regex(  // \p{Ll} means Unicode lowercase letter
          @"^([+\-*/\\^<=>`~:.?@#$&]+|\p{Ll}[\w_]*|('[^']*')+)$",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        private static Regex unsignedInteger = new Regex(
          @"^(\d+)?$",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        private static Regex signedNumber = new Regex(
          @"^([+-]?((\d+\.)?\d+)((E|e)[+-]?\d+)?)$",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        private static Regex signedImagNumber = new Regex(
          @"^([+-]?((\d+\.)?\d+)((E|e)[+-]?\d+)?i?)$",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        public static bool HasUnsignedIntegerFormat(this string s)
        {
            return unsignedInteger.Match(s).Success;
        }

        public static bool HasSignedRealNumberFormat(this string s)
        {
            return signedNumber.Match(s).Success;
        }

        public static bool HasSignedImagNumberFormat(this string s)
        {
            return signedImagNumber.Match(s).Success;
        }

        private static Regex tokens = new Regex(
          // identifiers, signed numbers and sequences of non-whites, separated by whites
          @"\s*(?<token>([\p{L}_]+\d*|[+-]?((\d+\.)?\d+)((E|e)[+-]?\d+)?|\S+))\s*",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        public static string[] Tokens(this string s)
        {
            MatchCollection mc = tokens.Matches(s);
            string[] result = new string[mc.Count];

            for (int i = 0; i < mc.Count; i++)
                result[i] = mc[i].Value.Trim();

            return result;
        }

        public static string RemoveUnnecessaryAtomQuotes(this string s)
        {
            int len = s.Length;
            string a;

            // check whether the string is quoted at all
            if (len < 2 || s[0] != '\'' || s[len - 1] != '\'') return s;

            // check whether the unquoted version is an Atom
            return (atomPattern.Match((a = s.Substring(1, len - 2))).Success) ? a : s;
        }

        public static bool HasAtomFormat(this string s)
        {
            return atomPattern.Match(s).Success;
        }

        public static string ToAtom(this string s) // numbers are quoted
        {
            if (s.HasAtomFormat()) return s.RemoveUnnecessaryAtomQuotes(); // e.g. 'a' -> a

            return '\'' + s.Replace("'", "''") + '\'';
        }

        public static string ToAtomic(this string s, out TermType type) // numbers are not quoted
        {
            if (s.HasSignedRealNumberFormat())
            {
                type = TermType.Number;

                return s;
            }

            if (s.HasSignedImagNumberFormat())
            {
                type = TermType.ImagNumber;

                return s.Substring(0, s.Length - 1);
            }

            type = TermType.Atom;

            return s.ToAtom();
        }


        public static string MakeAtomic(this string s)
        {
            TermType type;

            return s.ToAtomic(out type);
        }


        public static string Dequoted(this string s) // remove ' or " quotes (if any)
        {
            if (s == null) return null;

            int len = s.Length;

            // check whether the string is quoted at all
            if (len < 2 || (s[0] != '\'' && s[0] != '"') || (s[0] != s[len - 1]))
                return s;

            return s.Substring(1, len - 2).Replace(new string(s[0], 2), new string(s[0], 1));
        }


        public static string Dequoted(this string s, string quote) // remove ' or " quotes (if any)
        {
            int len = s.Length;

            // check whether the string is quoted at all
            if (quote.Length == 0 || len < 2 || s[0] != quote[0] || s[len - 1] != quote[0])
                return s;

            return s.Substring(1, len - 2).Replace(quote + quote, quote);
        }


        public static string Enquote(this string s, char quoteChar) // enclose in ' or " quotes
        {
            string q = quoteChar.ToString();

            return (s == null) ? q + q : q + s.Replace(q, q + q) + q;
        }


        public static string EscapeDoubleQuotes(this string s) // replace " by \"
        {
            return s.Replace(@"""", @"\""");
        }


        public static string Packed(this string s)
        {
            return '(' + s + ')';
        }


        public static string AddEndDot(this string s)
        {
            string t = s.Trim();

            return (t.EndsWith(".")) ? t : t + '.';
        }


        public static string Packed(this string s, bool mustPack)
        {
            return mustPack ? '(' + s + ')' : s;
        }


        private static Regex stringLiteral = new Regex(
          @"^(?<char>(\\('|""|\\|0|a|b|f|n|r|t|v|u[0-9a-fA-F]{4}|x[0-9a-fA-F]{1,4}|U[0-9a-fA-F]{8}|.?))|[^\\])+$",
          RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        //@"\\(?:(?<h>'|""|\\|0|a|b|f|n|r|t|v)|u(?<h>[0-9a-fA-F]{4})|x(?<h>[0-9a-fA-F]{1,4})|U(?<h>[0-9a-fA-F]{8})|(?<h>.))",

        private static Regex escapedChar = new Regex(
         @"\\(?:(?<h>'|""|\\|0|a|b|f|n|r|t|v)|u(?<h>[0-9a-fA-F]{4})|x(?<h>[0-9a-fA-F]{1,4})|U(?<h>[0-9a-fA-F]{8}))",
         RegexOptions.CultureInvariant | RegexOptions.Compiled | RegexOptions.ExplicitCapture
        );

        // resolves escaped characters, cf. CSProlog.exe.config file
        public static string Unescaped(this string s)
        {
            if (string.IsNullOrEmpty(s)) return s;

            if (stringLiteral.Match(s).Success)
                return escapedChar.Replace(s,
                 match => ($"{match.Groups[1].Value.ResolveEscape()}"));

            PrologEngine.IO.ErrorRuntime(
                $"Unrecognized escape sequence in string \"{s}\"" + "\r\n(cf. CSProlog.exe.config in .exe-directory)", null, null);

            return null;
        }

        private static string ResolveEscape(this string s) // resolve escaped characters
        {
            switch (s)
            {
                case "\'":
                case "\"":
                case "\\": return s;
                case "0": return "\0";
                case "a": return "\a";
                case "b": return "\b";
                case "f": return "\f";
                case "n": return "\n";
                case "r": return "\r";
                case "t": return "\t";
                case "v": return "\v";
            }

            // ignore the / if it precedes a character that does not need an escape
            if (s.Length == 1) return s;

            // according to the regex, s now can only be a hex number
            return ((char)Int32.Parse(s, NumberStyles.HexNumber)).ToString();
        }


        public static string Repeat(this string s, int n)
        {
            return (new String('*', n)).Replace("*", s);
        }

        // Levenshtein Distance, i.e. http://www.merriampark.com/ld.htm
        public static float Levenshtein(this string a, string b)
        {
            if (string.IsNullOrEmpty(a))
                return string.IsNullOrEmpty(b) ? 0.0F : 1.0F;

            if (string.IsNullOrEmpty(b))
                return 1.0F;

            int n = a.Length;
            int m = b.Length;
            int[,] d = new int[n + 1, m + 1];

            for (int i = 0; i <= n; i++) d[i, 0] = i;

            for (int j = 0; j <= m; j++) d[0, j] = j;

            for (int i = 1; i <= n; i++)
                for (int j = 1; j <= m; j++)
                {
                    int d0 = d[i - 1, j] + 1;
                    int d1 = d[i, j - 1] + 1;
                    int d2 = d[i - 1, j - 1] + (a[i - 1] == b[j - 1] ? 0 : 1);
                    d[i, j] = Math.Min(Math.Min(d0, d1), d2);
                }

            return (float)d[n, m] / Math.Max(n, m);
        }

        // Jon Skeet -- C# In Depth, 1st edition, p.176
        public abstract class Range<T> : IEnumerable<T> where T : IComparable<T>
        {
            public Range(T start, T end)
            {
                if (start.CompareTo(end) > 0)
                    throw new ArgumentOutOfRangeException();

                this.Start = start;
                this.End = end;
            }

            public T Start { get; }

            public T End { get; }

            public bool Contains(T value)
            {
                return value.CompareTo(Start) >= 0 && value.CompareTo(End) <= 0;
            }

            public IEnumerator<T> GetEnumerator()
            {
                T value = Start;

                while (value.CompareTo(End) < 0)
                {
                    yield return value;
                    value = GetNextValue(value);
                }

                if (value.CompareTo(End) == 0)
                {
                    yield return value;
                }
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return GetEnumerator();
            }

            protected abstract T GetNextValue(T current);
        }
    }

    // Burrows–Wheeler transform -- cf. https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform

    // Code from https://gist.github.com/Lordron/5039958

    /* Main program sample

        const string str = "Better ask the way than to go astray!";

        byte[] buffer_in = Encoding.UTF8.GetBytes(str);
        byte[] buffer_out = new byte[buffer_in.Length];
        byte[] buffer_decode = new byte[buffer_in.Length];

        BWTImplementation bwt = new BWTImplementation();

        int primary_index = 0;
        bwt.bwt_encode(buffer_in, buffer_out, buffer_in.Length, ref primary_index);
        bwt.bwt_decode(buffer_out, buffer_decode, buffer_in.Length, primary_index);

        Console.WriteLine("Decoded string: {0}", Encoding.UTF8.GetString(buffer_decode));

     */

    internal class BWTImplementation
    {
        public void bwt_encode(byte[] buf_in, byte[] buf_out, int size, ref int primary_index)
        {
            int[] indices = new int[size];
            for (int i = 0; i < size; i++)
                indices[i] = i;

            Array.Sort(indices, 0, size, new BWTComparator(buf_in, size));

            for (int i = 0; i < size; i++)
                buf_out[i] = buf_in[(indices[i] + size - 1) % size];

            for (int i = 0; i < size; i++)
            {
                if (indices[i] == 1)
                {
                    primary_index = i;
                    return;
                }
            }
        }

        public void bwt_decode(byte[] buf_encoded, byte[] buf_decoded, int size, int primary_index)
        {
            byte[] F = new byte[size];
            int[] buckets = new int[0x100];
            int[] indices = new int[size];

            for (int i = 0; i < 0x100; i++)
                buckets[i] = 0;

            for (int i = 0; i < size; i++)
                buckets[buf_encoded[i]]++;

            for (int i = 0, k = 0; i < 0x100; i++)
            {
                for (int j = 0; j < buckets[i]; j++)
                {
                    F[k++] = (byte)i;
                }
            }

            for (int i = 0, j = 0; i < 0x100; i++)
            {
                while (i > F[j] && j < size - 1)
                {
                    j++;
                }
                buckets[i] = j;
            }

            for (int i = 0; i < size; i++)
                indices[buckets[buf_encoded[i]]++] = i;

            for (int i = 0, j = primary_index; i < size; i++)
            {
                buf_decoded[i] = buf_encoded[j];
                j = indices[j];
            }
        }
    }

    internal class BWTComparator : IComparer<int>
    {
        private byte[] rotlexcmp_buf = null;
        private int rottexcmp_bufsize = 0;

        public BWTComparator(byte[] array, int size)
        {
            rotlexcmp_buf = array;
            rottexcmp_bufsize = size;
        }

        public int Compare(int li, int ri)
        {
            int ac = rottexcmp_bufsize;
            while (rotlexcmp_buf[li] == rotlexcmp_buf[ri])
            {
                if (++li == rottexcmp_bufsize)
                    li = 0;
                if (++ri == rottexcmp_bufsize)
                    ri = 0;
                if (--ac <= 0)
                    return 0;
            }
            if (rotlexcmp_buf[li] > rotlexcmp_buf[ri])
                return 1;

            return -1;
        }
    }

}
