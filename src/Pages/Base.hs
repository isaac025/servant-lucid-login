module Pages.Base where

import Lucid

base :: Html () -> Html ()
base h =
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Website"
                link_ [rel_ "stylesheet", href_ "/static/semantic.min.css"]
            body_ $ do
                h

loginBase :: Html () -> Html ()
loginBase h =
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Website"
                link_ [rel_ "stylesheet", href_ "/static/semantic.min.css"]
                style_ "body { display: flex; justify-content: center; align-items: center; height: 100vh; background-color: #f7f7f7; }"
            body_ $ do
                h

homePage :: Html ()
homePage =
    base $ do
        div_ [class_ "ui container"] $ do
            div_ [class_ "ui two column centered grid"] $ do
                div_ [class_ "one column centered row"] $ do
                    h1_ "Home"
                div_ [class_ "one column centered row"] $ do
                    form_ [method_ "post", action_ "/home/upload", enctype_ "multipart/form-data"] $ do
                        input_ [type_ "file", name_ "file-upload", id_ "file-upload"]
                        button_ [class_ "ui secondary button", type_ "submit", for_ "file-upload"] "Submit file"
