module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    html WelcomeView = [hsx|
        <div style="background-color: #657b83; padding: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px">
          <div style="writing-mode: vertical-rl; max-width: 100%; overflow-x: scroll; margin-left: auto; margin-right: auto">
            <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white;
                       padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">
                      てるやま 
            </h1>
            <h2 style="margin-top: 0; margin-bottom: 0rem; font-weight: 900; font-size: 3rem">
                      あそべのもり 
            </h2>
            <p style="margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)">
                      いにしへと今がつむぐ未來を包む 
            </p>
            <p>
            <a href={PostsAction} style="margin-top: 2rem; background-color: #268bd2; padding: 1rem;
                                    border-radius: 3px; color: hsla(205, 69%, 98%, 1); text-decoration: none;
                                    font-weight: bold; display: inline-block;
                                    box-shadow: 0 4px 6px hsla(205, 69%, 0%, 0.08);
                                    transition: box-shadow 0.2s; transition: transform 0.2s;" target="_self">
                    ことのはくばり
            </a>
            </p>
            <p>
            <a href="https://ihp.digitallyinduced.com/Slack"
               style="margin-top: 2rem; background-color: #268bd2; padding: 1rem; border-radius: 3px;
                      color: hsla(205, 69%, 98%, 1); text-decoration: none; font-weight: bold;
                      display: inline-block; box-shadow: 0 4px 6px hsla(205, 69%, 0%, 0.08);
                      transition: box-shadow 0.2s; transition: transform 0.2s;" target="_blank">
                    To Join community on Slack!</a>
            </p>
            <p>
            <a href="https://ihp.digitallyinduced.com/Guide/your-first-project.html"
               style="margin-top: 2rem; background-color: #268bd2; padding: 1rem; border-radius: 3px;
                      color: hsla(205, 69%, 98%, 1); text-decoration: none; font-weight: bold;
                      display: inline-block; box-shadow: 0 4px 6px hsla(205, 69%, 0%, 0.08);
                      transition: box-shadow 0.2s; transition: transform 0.2s;" target="_blank">
                        Learn the IHP Steps in the Documentation
            </a>
            </p>
          </div>
        </div>
         <div style="max-width: 800px; margin-left: auto; margin-right: auto; margin-top: 4rem">
              <img src="/asakusa_zinzya_tokyo.png" alt="/asakusa_zinzya" style="width:100%;">
              <p style="color: hsla(196, 13%, 50%, 1); margin-top: 4rem; text-align: right;">
                 Yokoyama Teruhisa 2023 
              </p>
         </div> 
|]
