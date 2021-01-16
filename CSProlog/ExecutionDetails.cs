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

using System.Collections.Generic;
using static Prolog.PrologEngine;

namespace Prolog
{
    public class ExecutionDetails
    {
        public event CurrentTerm OnCurrentTermChanged;

        public List<string> CurrentTermHistory { get; private set; } = new List<string>(1000);

        public void Reset()
        {
            this.CurrentTermHistory.Clear();
        }

        public void CurrentTermChanged(TermNode termNode)
        {
            this.CurrentTermHistory.Add(termNode.ToString());
            this.OnCurrentTermChanged?.Invoke(termNode);
        }
    }
}