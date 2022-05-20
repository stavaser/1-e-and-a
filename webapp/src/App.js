import React, { useState, useEffect } from 'react';
import ABCJS from 'abcjs';
import 'abcjs/abcjs-audio.css';
import CodeEditor from '@uiw/react-textarea-code-editor';
import AudioPlayer from 'react-h5-audio-player';
import 'react-h5-audio-player/lib/styles.css';
import { useDispatch, useSelector } from 'react-redux';
import { getAbc } from './redux/main.actions';

const App = () => {
  const [code, setCode] = useState(``);
  const abc = useSelector((state) => state.main.abc);
  const parse_error = useSelector((state) => state.main.parse_error);
  const runtime_error = useSelector((state) => state.main.runtime_error);
  console.log(abc);
  console.log(parse_error);
  console.log(runtime_error);
  const dispatch = useDispatch();

  useEffect(() => {
    // let editor = new ABCJS.Editor('editor', { canvas_id: 'paper' });
    var visualObj = ABCJS.renderAbc('paper', abc, {});

    // This object is the class that will contain the buffer
    var midiBuffer;

    var startAudioButton = document.querySelector('.activate-audio');
    var stopAudioButton = document.querySelector('.stop-audio');

    startAudioButton.addEventListener('click', function () {
      startAudioButton.setAttribute('style', 'display:none;');
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
          // In theory the AC shouldn't start suspended because it is being initialized in a click handler, but iOS seems to anyway.

          // This does a bare minimum so this object could be created in advance, or whenever convenient.
          midiBuffer = new ABCJS.synth.CreateSynth();

          // midiBuffer.init preloads and caches all the notes needed. There may be significant network traffic here.
          return midiBuffer
            .init({
              visualObj: visualObj[0],
              audioContext: audioContext,
              millisecondsPerMeasure: visualObj[0].millisecondsPerMeasure(),
            })
            .then(function (response) {
              console.log('Notes loaded: ', response);
              // console.log(response); // this contains the list of notes that were loaded.
              // midiBuffer.prime actually builds the output buffer.
              return midiBuffer.prime();
            })
            .then(function (response) {
              // At this point, everything slow has happened. midiBuffer.start will return very quickly and will start playing very quickly without lag.
              midiBuffer.start();
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
      stopAudioButton.setAttribute('style', 'display:none;');
      if (midiBuffer) midiBuffer.stop();
    });
  });
  const onKeyDown = (e) => {
    if ((e.metaKey && e.which === 83) || (e.shiftKey && e.which === 83)) {
      e.preventDefault();
      dispatch(getAbc(e.target.value));
    }
  };
  return (
    <>
      <div style={{ display: 'flex' }}>
        <div>
          <CodeEditor
            value=""
            language="js"
            minHeight="100vh"
            onKeyDown={(e) => onKeyDown(e)}
            padding={15}
            style={{
              width: '50vw',
              fontSize: '18px',
              overflow: 'scroll',
              fontFamily: 'monospace',
            }}
          />
        </div>
        <div style={{ height: '100vh', overflow: 'scroll' }}>
          <div className="row">
            <div>
              <button className="activate-audio">Activate Audio Context And Play</button>
              <button className="stop-audio" style={{ display: 'none' }}>
                Stop Audio
              </button>
              <div className="audio-error" style={{ display: 'none' }}>
                Audio is not supported in this browser.
              </div>
            </div>
            {/* <AudioPlayer
                autoPlay
                src="http://example.com/audio.mp3"
                onPlay={(e) => console.log('onPlay')}
                // other props here
              /> */}
          </div>
          {parse_error}
          {runtime_error}
          <div id="paper"></div>
        </div>
      </div>
    </>
  );
};

export default App;
