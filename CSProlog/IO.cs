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

using Serilog;
using System;
using System.Text;

namespace Prolog
{
        public abstract class BasicIo
    {
        public abstract string ReadLine();

        public abstract int ReadChar();

        public abstract void Write(string s);

        public abstract void WriteLine(string s);

        public abstract void WriteLine();

        public abstract void Clear();

        public abstract void Reset();
        
        public void Write(string s, params object[] o)
        {
            Write(string.Format(s, o));
        }

        public void WriteLine(string s, params object[] o)
        {
            WriteLine(string.Format(s, o));
        }
    }
    
    public class SilentIO : BasicIo
    {
        public override void Clear()
        {
        }

        public override int ReadChar()
        {
            return Console.ReadKey().KeyChar;
        }

        public override string ReadLine()
        {
            return Console.ReadLine();
        }

        public override void Reset()
        {
        }

        public override void Write(string s)
        {
        }

        public override void WriteLine()
        {
        }

        public override void WriteLine(string s)
        {
        }
    }
    
    public partial class PrologEngine
    {
        private FileReaderTerm currentFileReader;
        private FileWriterTerm currentFileWriter;

                // BaseReadCurrentInput. Input is read from StandardInput.
        // StandardInput is the file set by the see command, or Console if no such file exists.
        private string BaseReadLineCurrentInput() // returns null at end of file
        {
            return (currentFileReader == null) ? IO.ReadLine() : currentFileReader.ReadLine();
        }


        private BaseTerm BaseReadTermCurrentInput()
        {
            if (currentFileReader == null)
            {
                StringBuilder query = new StringBuilder();
                string line;
                PrologParser p = new PrologParser(this);

                bool first = true;

                while (true)
                {
                    IO.Write("|: ");

                    if (first) first = false; else query.AppendLine();

                    if ((line = IO.ReadLine()) == null) return FileTerm.END_OF_FILE;

                    query.Append(line = line.Trim());

                    if (line.EndsWith(".")) break;
                }

                p.StreamIn = "&reading\r\n" + query; // equal to parser ReadingSym
                BaseTerm result = p.ReadTerm;

                return (result == null) ? FileTerm.END_OF_FILE : result;
            }

            return currentFileReader.ReadTerm();
        }


        private int BaseReadCharCurrentInput() // returns -1 at end of file
        {
            return (currentFileReader == null) ? IO.ReadChar() : currentFileReader.ReadChar();
        }


        // BaseWriteCurrentOutput
        // Output from print, display, tab, put etc. is written to StandardOutput.
        // StandardOutput is the file set by the tell command, or Console if
        // no such file exists.
        private void BaseWriteCurrentOutput(string s)
        {
            if (currentFileWriter == null)
                IO.Write(s);
            else
                currentFileWriter.Write(s);
        }


        private void BaseWriteCurrentOutput(string s, object[] args)
        {
            BaseWriteCurrentOutput(string.Format(s, args));
        }
        

        
        private BaseTerm ReadTerm()
        {
            return BaseReadTermCurrentInput();
        }


        private string ReadLine()
        {
            return BaseReadLineCurrentInput();
        }


        private int ReadChar()
        {
            return BaseReadCharCurrentInput();
        }


        private void Write(BaseTerm t, bool dequote)
        {
            if (t.IsString)
                BaseWriteCurrentOutput(dequote ? t.FunctorToString : '"' + t.FunctorToString + '"');
            else if (t.IsAtom)
                BaseWriteCurrentOutput(dequote ? t.FunctorToString.Dequoted("'") : t.FunctorToString);
            else
                BaseWriteCurrentOutput(t.ToString());
        }


        public void Write(string s)
        {
            BaseWriteCurrentOutput(s);
        }


        public void Write(string s, params object[] args)
        {
            BaseWriteCurrentOutput(s, args);
        }


        public void WriteLine(string s)
        {
            BaseWriteCurrentOutput(s + Environment.NewLine);
        }


        public void WriteLine(string s, params object[] args)
        {
            BaseWriteCurrentOutput(s + Environment.NewLine, args);
        }


        public void NewLine()
        {
            BaseWriteCurrentOutput(Environment.NewLine);
        }
        
        public enum MessageKind
        {
            Consult, Runtime
        }

        // for IO *not* generated by Prolog predicates and not subject to
        // current input and current output (i.e. error messages etc.)
        public static class IO
        {
            public static BasicIo BasicIO { private get; set; }

            public static void Reset()
            {
                BasicIO.Reset();
            }

            public static bool ErrorConsult(string msg, BaseTerm o)
            {
                Log.Error(msg);
                throw new ConsultException(msg, o);

                return false;
            }

            public static bool ErrorConsult(string msg, TokenSeqToTerm.BaseToken o)
            {
                Log.Error(msg);
                throw new ConsultException(msg, symbol: o.Symbol);

                return false;
            }


            public static bool ErrorConsult(string msg, BaseParser.Symbol o)
            {
                Log.Error(msg);
                throw new ConsultException(msg, symbol: o);

                return false;
            }

            public static bool ErrorRuntime(string msg, VarStack varStack, BaseTerm term)
            {
                Log.Error(msg);
                throw new RuntimeException(msg, term, varStack);

                return false;
            }

            public static void Warning(string msg, params object[] o)
            {
                Log.Warning(string.Format(msg, o));
                BasicIO.WriteLine(string.Format("*** warning: " + msg, o));
            }


            public static void Warning(string msg)
            {
                Log.Warning(msg);
                BasicIO.WriteLine("*** warning: " + msg);
            }


            public static void Message(string msg, params object[] o)
            {
                Log.Warning(string.Format(msg, o));
                BasicIO.WriteLine(string.Format("--- " + msg, o));
            }


            public static void Message(string msg)
            {
                Log.Warning(msg);
                BasicIO.WriteLine("--- " + msg);
            }


            public static void Fatal(MessageKind messageKind, string msg, params object[] o)
            {
                Log.Error(string.Format(msg, o));
                throw new Exception("*** fatal: " + String.Format(msg, o));
            }


            public static void Fatal(MessageKind messageKind, string msg)
            {
                Log.Error(msg);
                throw new Exception("*** fatal: " + msg);
            }


            public static string ReadLine()
            {
                return BasicIO.ReadLine();
            }


            public static int ReadChar()
            {
                return BasicIO.ReadChar();
            }


            public static void Write(string s, params object[] o)
            {
                BasicIO.Write(string.Format(s, o));
            }


            public static void Write(string s)
            {
                BasicIO.Write(s);
            }


            public static void WriteLine(string s, params object[] o)
            {
                BasicIO.WriteLine(string.Format(s, o));
            }


            public static void WriteLine(string s)
            {
                BasicIO.WriteLine(s);
            }


            public static void WriteLine()
            {
                BasicIO.WriteLine();
            }


            public static void ClearScreen()
            {
                BasicIO.Clear();
            }
        }
    }
}
