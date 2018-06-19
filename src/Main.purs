module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HISTORY, htmlDocumentToParentNode)
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector, QuerySelector(..))
import Data.Either (Either(..))
import Data.Foldable (oneOf) as Foldable
import Data.Foreign (toForeign) as Foreign
import Data.Maybe (Maybe(..))
import React as R
import React.DOM as R
import React.DOM.Props (Props)
import React.DOM.Props as RP
import ReactDOM as ReactDOM
import Routing (match) as Routing
import Routing.Hash as RoutingHash
import Routing.Match.Class (lit, int, str, end) as Routing
import Routing.PushState (PushStateInterface, PushStateEffects)
import Routing.PushState as RoutingPushState
import Thermite hiding (defaultMain) as T

data Route = FooRoute
           | BarRoute

routing = Foldable.oneOf
          [ FooRoute <$ Routing.lit "foo"
          , BarRoute <$ Routing.lit "bar"
          ]


matchRoute :: String -> Either String Route
matchRoute = Routing.match routing

type State eff =
  { pushStateInterface :: PushStateInterface (PushStateEffects eff)
  }

data Action = RootDidMount
            | ChangeRoute Route
            | ButtonClick

withComponentDidMount pushStateInterface rs =
  rs { spec { componentDidMount = componentDidMount } }
  where
    handler this route = do
      hash <- RoutingHash.getHash
      case matchRoute hash of
        Right newRoute -> rs.dispatcher this $ ChangeRoute newRoute
        Left _ -> pure unit
    componentDidMount this = do
      void $ pushStateInterface.listen (handler this)
      rs.dispatcher this RootDidMount

spec :: T.Spec _ _ _ _
spec = T.simpleSpec performAction render
  where
    render dispatch _ state _ =
      [ R.text "Hellow world!"
      , R.button
        [ RP.onClick $ const $ dispatch ButtonClick ]
        [ R.text "Click me" ]
      ]
        
    performAction RootDidMount _ _ =
      liftEff $ log "RootDidMount"
    performAction (ChangeRoute _) _ _ =
      liftEff $ log "ChangeRoute"
    performAction ButtonClick _ { pushStateInterface } = do
      pushStateInterface.pushState (Foreign.toForeign {}) "#foo"
      pure unit
      

getElement s
  = (querySelector (QuerySelector s) <<< htmlDocumentToParentNode <=< DOM.document) =<< DOM.window

--main :: forall e. Eff (history :: HISTORY, ref :: REF, dom :: DOM, console :: CONSOLE | e) Unit
main = do
  container <- getElement ".container"

  pushStateInterface <- RoutingPushState.makeInterface

  let
    initState = { pushStateInterface }
    reactSpec = T.createReactSpec spec initState
    component = (R.createClass <<< _.spec) (withComponentDidMount pushStateInterface reactSpec)
    reactElement = R.createFactory component unit

  case container of
    Nothing ->
      log "unable to find the container"
    Just el ->
      void $ ReactDOM.render reactElement el
