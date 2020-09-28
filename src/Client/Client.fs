module SafeSearch.Client

open Elmish
open Elmish.React
open SafeSearch

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Update.init Update.update View.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
