module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Interpreter (Interpreter(..), merge, liftNat)
import Spork.Html as H

type Model = Unit
data Action = None
data Effect a = Effect String a
data Sub a = Sub String

app :: App.App Effect Sub Model Action
app =
  { render: const H.empty
  , update: const <<< App.purely
  , init: 
      { model: unit
      , effects: App.batch
          [ Effect "eff 1" None
          , Effect "eff 2" None
          ]
      }
  , subs: const $ App.batch
      [ Sub "sub 1"
      , Sub "sub 2"
      ]
  }

effectInterpreter :: forall i eff. Interpreter (Eff (console :: CONSOLE | eff)) Effect i
effectInterpreter = liftNat case _ of
  Effect t next -> log t $> next

subInterpreter :: forall i eff. Interpreter (Eff (console :: CONSOLE | eff)) Sub i
subInterpreter = Interpreter $ EventQueue.withAccumArray \queue -> do
  let commit :: Array (Sub i) -> Eff (console :: CONSOLE | eff) Unit
      commit new = log $ show $ new <#> \(Sub t) -> t
  pure commit

main :: Eff (App.AppEffects (console :: CONSOLE)) Unit
main = _.run =<< App.makeWithSelector (effectInterpreter `merge` subInterpreter) app "#app"

