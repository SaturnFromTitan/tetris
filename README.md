# Tetris in elm


### What/Why?

After going through a few articles and videos on elm I got really interested in
it and wanted to start a pet project.

I started watching [this](https://www.youtube.com/playlist?list=PL7C8fMD-89DKhlerIE3BrYNd0PlhA6Zch) youtube series from [jcollard](https://github.com/jcollard) on building a Tetris game in elm, followed along and ported it to `0.19`.

Afterwards I bumped into [stil4m](https://github.com/stil4m)'s [continuation](https://github.com/stil4m/elm-tetris) of the game and followed along as here well.

Thanks a ton to both for teaching me so much about this great language!

### Deployment

The latest version of this application can be found [here]().

`index.html` can be generated via
```
elm make src/Tetris.elm
```

The `index.php` file is just needed for the deployment to Heroku. The
 description in [this](https://blog.teamtreehouse.com/deploy-static-site-heroku)
 article was the easiest way I could find.

This has the disadvantage of needing to include the index.html in the repository,
but for this toy example that's ok.
