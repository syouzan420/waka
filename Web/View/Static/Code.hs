module Web.View.Static.Code where
import Web.View.Prelude

data CodeView = CodeView

instance View CodeView where
    html CodeView = [hsx|
        <div style="background-color: #657b83; padding: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
            <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                      {showResult} 
            </p>
         </div> 
|]

showResult :: Html
showResult = [hsx|
  <p> {res} </p>
    |]
    where res = "Hello" :: Text 
