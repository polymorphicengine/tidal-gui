    // hijack source init screen event because doesn't work in Electron
 hydra.s.forEach((source) => {
      source.initScreen = (index = 0) =>  desktopCapturer.getSources({types: ['window', 'screen']}).then(async (sources) => {
      console.log(sources);
        try {
            if (sources.length > index) {
              const stream = await navigator.mediaDevices.getUserMedia({
                audio: false,
                video: {
                  mandatory: {
                    chromeMediaSource: 'desktop',
                    chromeMediaSourceId: sources[index].id,
                  //  minWidth: 1280,
                    maxWidth: window.innerWidth,
                //    minHeight: 720,
                    maxHeight: window.innerHeight
                  }
                }
              })
              const video = document.createElement('video')
              video.srcObject = stream
              video.addEventListener('loadedmetadata', () => {
              video.play().then(() => {
                  source.src = video
                  source.tex = source.regl.texture(source.src)
                })
              })
            }
        } catch (error) {
          console.log('initScreen error: ', error);
          throw error
        }
    
      })
    })
