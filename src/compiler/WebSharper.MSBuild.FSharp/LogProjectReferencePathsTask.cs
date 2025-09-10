// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild.FSharp
{
    public sealed class LogProjectReferencePathsTask : Task
    {
        public ITaskItem[] ResolvedNativeProjectReferencePaths { get; set; } = new ITaskItem[] { };

        public ITaskItem[] ResolvedProjectReferencePaths { get; set; } = new ITaskItem[] { };

        public ITaskItem[] ProjectReferencePaths { get; set; } = new ITaskItem[] { };

        public void LogTaskItem (string typ, ITaskItem item)
        {
            Log.LogMessage(MessageImportance.High, $"{typ}: {item.ItemSpec}");
        }

        public override bool Execute()
        {
            foreach (var item in ResolvedNativeProjectReferencePaths)
            {
                LogTaskItem("Resolved Native Project Reference Path", item);
            }
            foreach (var item in ResolvedProjectReferencePaths)
            {
                LogTaskItem("Resolved Project Reference Path", item);
            }
            foreach (var item in ProjectReferencePaths)
            {
                LogTaskItem("Project Reference Path", item);
            }
            return true;
        }
    }
}
