import React, { useState, useEffect } from 'react';
import ABCJS from 'abcjs';
import 'abcjs/abcjs-audio.css';
import CodeEditor from '@uiw/react-textarea-code-editor';

const App = () => {
  const [code, setCode] = useState(``);
  // abcjs.renderAbc('paper', 'X:1\nK:D\nDDAA|BBA2|\n');
  var abc =
    "T: Cooley's\n" +
    'M: 4/4\n' +
    'L: 1/8\n' +
    'R: reel\n' +
    'K: Emin\n' +
    '|:D2|EB{c}BA B2 EB|~B2 AB dBAG|FDAD BDAD|FDAD dAFD|\n' +
    'EBBA B2 EB|B2 AB defg|afe^c dBAF|DEFD E2:|\n' +
    '|:gf|eB B2 efge|eB B2 gedB|A2 FA DAFA|A2 FA defg|\n' +
    'eB B2 eBgB|eB B2 defg|afe^c dBAF|DEFD E2:|';
  useEffect(() => {
    let editor = new ABCJS.Editor('editor', { canvas_id: 'paper' });

    var visualObj = editor.tunes[0];

    // This object is the class that will contain the buffer
    var midiBuffer;

    var startAudioButton = document.querySelector('.activate-audio');
    var stopAudioButton = document.querySelector('.stop-audio');
    var explanationDiv = document.querySelector('.suspend-explanation');
    var statusDiv = document.querySelector('.status');

    startAudioButton.addEventListener('click', function () {
      startAudioButton.setAttribute('style', 'display:none;');
      explanationDiv.setAttribute('style', 'opacity: 0;');
      statusDiv.innerHTML = '<div>Testing browser</div>';
      if (ABCJS.synth.supportsAudio()) {
        stopAudioButton.setAttribute('style', '');

        // An audio context is needed - this can be passed in for two reasons:
        // 1) So that you can share this audio context with other elements on your page.
        // 2) So that you can create it during a user interaction so that the browser doesn't block the sound.
        // Setting this is optional - if you don't set an audioContext, then abcjs will create one.
        window.AudioContext =
          window.AudioContext ||
          window.webkitAudioContext ||
          navigator.mozAudioContext ||
          navigator.msAudioContext;
        var audioContext = new window.AudioContext();
        audioContext.resume().then(function () {
          statusDiv.innerHTML += '<div>AudioContext resumed</div>';
          // In theory the AC shouldn't start suspended because it is being initialized in a click handler, but iOS seems to anyway.

          // This does a bare minimum so this object could be created in advance, or whenever convenient.
          midiBuffer = new ABCJS.synth.CreateSynth();

          // midiBuffer.init preloads and caches all the notes needed. There may be significant network traffic here.
          return midiBuffer
            .init({
              visualObj: visualObj,
              audioContext: audioContext,
              millisecondsPerMeasure: visualObj.millisecondsPerMeasure(),
            })
            .then(function (response) {
              console.log('Notes loaded: ', response);
              statusDiv.innerHTML += '<div>Audio object has been initialized</div>';
              // console.log(response); // this contains the list of notes that were loaded.
              // midiBuffer.prime actually builds the output buffer.
              return midiBuffer.prime();
            })
            .then(function (response) {
              statusDiv.innerHTML +=
                '<div>Audio object has been primed (' + response.duration + ' seconds).</div>';
              statusDiv.innerHTML += '<div>status = ' + response.status + '</div>';
              // At this point, everything slow has happened. midiBuffer.start will return very quickly and will start playing very quickly without lag.
              midiBuffer.start();
              statusDiv.innerHTML += '<div>Audio started</div>';
              return Promise.resolve();
            })
            .catch(function (error) {
              if (error.status === 'NotSupported') {
                stopAudioButton.setAttribute('style', 'display:none;');
                var audioError = document.querySelector('.audio-error');
                audioError.setAttribute('style', '');
              } else console.warn('synth error', error);
            });
        });
      } else {
        var audioError = document.querySelector('.audio-error');
        audioError.setAttribute('style', '');
      }
    });

    stopAudioButton.addEventListener('click', function () {
      startAudioButton.setAttribute('style', '');
      explanationDiv.setAttribute('style', '');
      stopAudioButton.setAttribute('style', 'display:none;');
      if (midiBuffer) midiBuffer.stop();
    });
  });
  // let editor =

  return (
    <>
      <div style={{ display: 'flex' }}>
        <div>
          <CodeEditor
            id="editor"
            value={code}
            language="js"
            onChange={(evn) => setCode(evn.target.value)}
            padding={15}
            style={{
              width: '700px',
              height: '100vh',
            }}
          />
        </div>
        <div>
          <div class="container">
            <div id="paper"></div>
            <p class="suspend-explanation">
              Browsers won't allow audio to work unless the audio is started in response to a user
              action. This prevents auto-playing web sites. Therefore, the following button is
              needed to do the initialization:
            </p>
            <div class="row">
              <div>
                <button class="activate-audio">Activate Audio Context And Play</button>
                <button class="stop-audio" style={{ display: 'none' }}>
                  Stop Audio
                </button>
                <div class="audio-error" style={{ display: 'none' }}>
                  Audio is not supported in this browser.
                </div>
              </div>
              <div class="status"></div>
            </div>
          </div>
        </div>
      </div>
    </>
  );
};

export default App;
