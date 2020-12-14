module Main where

import Prelude
import Control.Alternative ((<|>))
import Effect (Effect)

import Concur.Core (Widget)
import Concur.Core.FRP (Signal)
import Concur.Core.FRP as F
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)

-- hello :: forall a. Widget HTML a
-- hello = do
--   void $ D.button [P.onClick] [D.text "Say Hello"]
--   D.text "Hello Sailor!"

-- hello' :: forall a. Widget HTML a
-- hello' = D.div'
--     [ D.div' [D.button' [ D.text "Ahoy Port!"]]
--     , D.div' [D.button' [ D.text "Ahoy Starboard!"]]
--     ]

-- hello'' :: forall a. Widget HTML a
-- hello'' = do
--     greeting <- D.div'
--         [ "Hello" <$ D.button [P.onClick] [D.text "Say Hello"]
--         , "Merhaba" <$ D.button [P.onClick] [D.text "Merhaba De"]
--         ]
--     D.text $ greeting <> " Sailor!"

-- data Hello''' = None | Port | Starboard
-- instance showHello :: Show Hello''' where
--     show = case _ of
--         None -> "None"
--         Port -> "Port"
--         Starboard -> "Starboard"

-- hello''' :: Widget HTML Hello'''
-- hello''' = liftEffect . log . show =<<
--     ({- None <$ D.h1' [D.text "Composing with (<|>)"]
--     <|>  -}Port <$ D.button [P.onClick] [D.text "Ahoy Port!"]
--     <|> Starboard <$ D.button [P.onClick] [D.text "Ahoy Starboard!"]
--     )
-- counter :: forall a. Int -> Widget HTML a
-- counter count = do
--   void $ D.button [P.onClick] [D.text $ show count]
--   counter (count + 1)

-- page :: Widget HTML Unit
-- page = do 
--     result <- D.div' [None <$ counter 0, None <$ hello, None <$ hello', None <$ hello'', hello''']
--     liftEffect $ log $ show result

type Form = 
        { name :: String
        , rememberMe :: Boolean
        }

data Action = Name String | RememberMe Boolean -- | Submit

formWidget :: Form -> Widget HTML Form
formWidget form = do
    res <- D.div' 
        [ Name <$> D.input [P._type "text",P.placeholder "Enter name", P.value form.name, P.unsafeTargetValue <$> P.onChange]
        , RememberMe (not form.rememberMe) <$ D.input [P._type "checkbox", P.checked form.rememberMe, P.onChange]
        -- , D.hr'
        -- , Submit <$ D.button [P.onClick] [D.text "Submit"]
        ]
    case res of
        Name n -> pure form {name = n}
        RememberMe r -> pure form {rememberMe = r}
        -- Submit -> pure form

formSignal :: Form -> Signal HTML Form
formSignal form = F.step form do
    form' <- formWidget form
    pure $ formSignal form'

page :: forall a. Widget HTML a
page = F.dyn $ F.loopS initialForm \form -> do 
    form' <- formSignal form
    F.display $ D.hr' <|> D.text (show form') <|> D.hr'
    pure form'
  where
    initialForm = {name: "", rememberMe: false}
main :: Effect Unit
main = runWidgetInDom "root" page