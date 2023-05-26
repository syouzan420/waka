module Web.View.Static.Code where
import Web.View.Prelude

data CodeView = CodeView

instance View CodeView where
    html CodeView = [hsx|
        <div style="background-color: #657b83; padding: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
            <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                      {showResult} 
            </p>
            <canvas id="canvas" style="border: 1px solid khaki;" width="200" height="200"></canvas>
            {setCanvas "canvas"}
            {setDraw}
            {fillRect 100 10 20 30}
         </div> 
|]

showResult :: Html
showResult = [hsx|
  <p> {res} </p>
    |]
    where res = "Hello" :: Text 

type XYWH = Double -> Double -> Double -> Double

setCanvas :: Text -> Html
setCanvas name = [hsx|
  <script data-name={name}>
      var csd = document.currentScript.dataset;
      const canvas = document.getElementById(csd.name);
      const ctx = canvas.getContext("2d");
  </script>
  |]

setDraw :: Html
setDraw = fillRect 25 25 100 100 <>
          clearRect 45 45 60 60 <>
          strokeRect 50 50 50 50

fillRect :: Double -> Double -> Double -> Double -> Html
fillRect x y w h = rect x y w h <> [hsx|
<script>
ctx.fillRect(x,y,w,h);
</script>
|]

clearRect :: Double -> Double -> Double -> Double -> Html
clearRect x y w h = rect x y w h <> [hsx|
<script>
ctx.clearRect(x,y,w,h);
</script>
|]

strokeRect :: Double -> Double -> Double -> Double -> Html
strokeRect x y w h = rect x y w h <> [hsx|
<script>
ctx.strokeRect(x,y,w,h);
</script>
|]

rect :: Double -> Double -> Double -> Double -> Html
rect x y w h = [hsx|
<script data-x={show x} data-y={show y} data-w={show w} data-h={show h}>
  var csd = document.currentScript.dataset;
  var x = csd.x;
  var y = csd.y;
  var w = csd.w;
  var h = csd.h;
</script>
|]

