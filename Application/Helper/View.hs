module Application.Helper.View (
  module IHP.LoginSupport.Helper.View,
    generateCalenderHtml,splitYearMonthDay
                               ) where

import IHP.ViewPrelude
import IHP.LoginSupport.Helper.View
import Application.Script.TeruDays

-- Here you can add functions which are available in all your views

generateCalenderHtml :: (Text -> Text) -> Int -> Int -> Html 
generateCalenderHtml link ye mo = preEscapedToHtml (title<>"<table>"<>tbl<>"</table>\n")
  where dlst = daysList ye mo 
        title = "<a>"<>show ye<>"年  "<>show mo<>"月</a><br>"
        hdr = foldl (\acc yb -> acc<>"<th>"<>yb<>"</th>") "" ["日","月","火","水","木","金","土"]
        mdl = foldl (\acc wk -> acc<>"<tr>"<>
                foldl (\acc dy -> acc<>"<td>"<>"<a href="<>(link (show dy))<>">"
                    <>(if dy==0 then "" else show dy)<>"</a></td>") "" wk
                                           <>"</tr>") "" dlst
        tbl = hdr<>mdl 

