# tidal-gui

A GUI for tidal written with the threepenny-gui library and codemirror

All the code in this repository was written as part of my google summer of code project. I would consider this the main output of the project. The main goal was to make tidal (https://hackage.haskell.org/package/tidal) more portable and easier for new users to install. 

The first step to reach this goal was to look into a way of distributing an application, that depends on the haskell 'hint' library (that wraps the GHC API in a simpler API). There was already some work done in this direction, at least for linux based operating systems, which relied on building the application in a docker container. I succeeded in extending this approach to also include tidal as a library but it became clear to me, that it wouldn't be as straightforward for the other main operating systems. We had the idea to switch to GitHub actions (which I found way easier to understand than docker), and I started working on a workflow for windows. It took me some time, but I finally got a working proof of concept. After that, I replicated the docker workflow for linux in github actions and it was luckily easy to modify for MacOS.

You can find the three finished proof of concept workflows in the following repository (I linked to my only commit):

https://github.com/tidalcycles/tidal-deploy/pull/1

After that I wanted to do soemthing a bit more creative and decided to work on a small interpreter that has a code editor and some displays for errors etc. as an interface. After some research I found the threepenny-gui library and decided to try it out and use it to build the interface. Since threepenny basically allows you to use javascript libraries I was able to use the codemirror and it was very simple to put the interface together.
As a small challange for myself I wanted to implement the code highlighting feature as it was already implemented in the feedforward editor. Otherwise I tried to implement as many of the features of the other editor plugins for tidal.

You can find a more detailed description of tidal-gui here:

https://club.tidalcycles.org/t/tidal-gui/3372

Lastly, it wasn't too difficult to combine the two things I worked on and implement workflows to make releases for tidal-gui that don't depend on GHC!

During the realisation of the project I have learned a lot about 'real world' programming, like distribution of software across multiple platforms and writing a frontend and I was also able to advance my haskell skills, which is still my favorite language to program in. I think it was a great start to learn about the world of open source.

I'm really happy with the outcome and look forward to developing further!
