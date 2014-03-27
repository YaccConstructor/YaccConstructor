//   Copyright 2013, 2014 YaccConstructor Software Foundation
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

module Yard.Core.FrontendsManager

type FrontendsManager() as this =
    inherit  Yard.Core.Manager.Manager<Frontend>()

    let get_by_extension = function
        | "yrd" -> Some(this.Component "YardFrontend")
        | "g"   -> Some(this.Component "AntlrFrontend")
        | _     -> None

    member public self.GetByExtension = get_by_extension