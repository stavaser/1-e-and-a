import React, { useState, useEffect } from 'react';
import ABCJS from 'abcjs';
import 'abcjs/abcjs-audio.css';
import CodeEditor from '@uiw/react-textarea-code-editor';
import AudioPlayer from 'react-h5-audio-player';
import 'react-h5-audio-player/lib/styles.css';
import { useDispatch, useSelector } from 'react-redux';
import { getAbc } from './redux/main.actions';
import Header from './components/Header/Header';
import { Alert, Button } from 'antd';
import 'antd/dist/antd.css';
const App = () => {
  const [code, setCode] = useState(`time: 4/4
division: 1/16
tempo: 120
title: example

pattern sixteenth_notes: [1 e + a | 2  e + a  | 3 e + a  | 4  e + a  |]

bar mybar:
  hh: [ sixteenth_notes ]
  sn: [   | 2 |   | 4 |]
  bd: [ 1 |   | 3 |   |]

render: {mybar}`);
  const [midiBuffer, setMidiBuffer] = useState();
  const [visualObj, setVisualObj] = useState([]);
  const [cursorControl, setCursorControl] = useState();
  const [audioPlaying, setAudioPlaying] = useState(false);
  const abc = useSelector((state) => state.main.abc);
  const parse_error = useSelector((state) => state.main.parse_error);
  const runtime_error = useSelector((state) => state.main.runtime_error);

  const dispatch = useDispatch();

  function CursorControl(rootSelector) {
    var self = this;

    // This demonstrates two methods of indicating where the music is.
    // 1) An element is created that is moved along for each note.
    // 2) The currently being played note is given a class so that it can be transformed.
    self.cursor = null; // This is the svg element that will move with the music.
    self.rootSelector = rootSelector; // This is the same selector as the renderAbc call uses.

    self.onStart = function () {
      // This is called when the timer starts so we know the svg has been drawn by now.
      // Create the cursor and add it to the sheet music's svg.
      var svg = document.querySelector(self.rootSelector + ' svg');
      self.cursor = document.createElementNS('http://www.w3.org/2000/svg', 'line');
      self.cursor.setAttribute('class', 'abcjs-cursor');
      self.cursor.setAttributeNS(null, 'x1', 0);
      self.cursor.setAttributeNS(null, 'y1', 0);
      self.cursor.setAttributeNS(null, 'x2', 0);
      self.cursor.setAttributeNS(null, 'y2', 0);
      svg.appendChild(self.cursor);
    };

    self.removeSelection = function () {
      // Unselect any previously selected notes.
      var lastSelection = document.querySelectorAll(self.rootSelector + ' .abcjs-highlight');
      for (var k = 0; k < lastSelection.length; k++)
        lastSelection[k].classList.remove('abcjs-highlight');
    };

    self.onEvent = function (ev) {
      // This is called every time a note or a rest is reached and contains the coordinates of it.
      if (ev.measureStart && ev.left === null) return; // this was the second part of a tie across a measure line. Just ignore it.

      self.removeSelection();

      // Select the currently selected notes.
      for (var i = 0; i < ev.elements.length; i++) {
        var note = ev.elements[i];
        for (var j = 0; j < note.length; j++) {
          note[j].classList.add('abcjs-highlight');
        }
      }

      // Move the cursor to the location of the current note.
      if (self.cursor) {
        self.cursor.setAttribute('x1', ev.left + 10);
        self.cursor.setAttribute('x2', ev.left + 10);
        self.cursor.setAttribute('y1', ev.top);
        self.cursor.setAttribute('y2', ev.top + ev.height);
      }
    };
    self.onFinished = function () {
      self.removeSelection();

      if (self.cursor) {
        self.cursor.setAttribute('x1', 0);
        self.cursor.setAttribute('x2', 0);
        self.cursor.setAttribute('y1', 0);
        self.cursor.setAttribute('y2', 0);
      }
    };
  }

  // document.querySelector('.start').addEventListener('click', startTimer);

  function onEvent(ev) {
    if (ev) cursorControl.onEvent(ev);
    else cursorControl.onFinished();
  }

  function startTimer() {
    cursorControl.onStart();

    var timingCallbacks = new ABCJS.TimingCallbacks(visualObj[0], {
      eventCallback: onEvent,
    });
    timingCallbacks.start();
  }

  const playMidi = () => {
    if (ABCJS.synth.supportsAudio()) {
      if (audioPlaying) {
        cursorControl.onFinished();
        setAudioPlaying(false);
        midiBuffer.stop();
      } else {
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

          // setMidiBuffer();

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
              console.log('midiBuffer: ', response);
              midiBuffer.start();
              return Promise.resolve();
            })
            .catch(function (error) {
              if (error.status === 'NotSupported') {
                // stopAudioButton.setAttribute('style', 'display:none;');
                var audioError = document.querySelector('.audio-error');
                audioError.setAttribute('style', '');
              } else console.warn('synth error', error);
            });
        });
        setAudioPlaying(true);
        startTimer();
      }
    } else {
      var audioError = document.querySelector('.audio-error');
      audioError.setAttribute('style', '');
      setAudioPlaying(false);
    }
  };

  useEffect(() => {
    setVisualObj(ABCJS.renderAbc('paper', abc, {}));
    setCursorControl(new CursorControl('#paper'));

    setMidiBuffer(new ABCJS.synth.CreateSynth());
  }, [abc]);
  const onKeyDown = (e) => {
    if ((e.metaKey && e.which === 83) || (e.ctrlKey && e.which === 83)) {
      e.preventDefault();

      dispatch(getAbc(e.target.value));
    }
  };
  return (
    <>
      <div style={{ display: 'flex' }}>
        <div style={{ maxHeight: '100vh', overflow: 'scroll' }}>
          {/* <Header /> */}
          <CodeEditor
            value={code}
            // value=""
            language="python"
            minHeight="100vh"
            onKeyDown={(e) => onKeyDown(e)}
            padding={15}
            style={{
              minWidth: '50vw',
              fontSize: '18px',
              fontFamily: 'monospace',
            }}
          />
        </div>
        <div style={{ height: '100vh', overflow: 'scroll', minWidth: '50vw' }}>
          <Button
            onClick={() => {
              playMidi();
            }}
            type="primary"
            // shape="round"
            ghost={audioPlaying}
            size="large"
            block
            // style={{ margin: '16px' }}
          >
            {audioPlaying ? 'Stop audio' : 'Play audio '}
          </Button>

          <div className="audio-error" style={{ display: 'none' }}>
            Audio is not supported in this browser.
          </div>
          {(parse_error || runtime_error) && (
            <Alert
              message={parse_error ? 'Parsing error' : 'Runtime error'}
              description={parse_error || runtime_error}
              type="error"
              showIcon
              style={{ maxWidth: 'calc(50vw - 32px)', margin: '16px' }}
            />
          )}
          <div id="paper"></div>
        </div>
      </div>
    </>
  );
};

export default App;
