/* global AudioContext */
// import { Elm } from './Main.elm'
import VirtualAudioGraph from './virtual-audio.js'

const context = new AudioContext()
const audio = new VirtualAudioGraph(context)

// Chrome autplay policy demans some user interaction
// takes place before the AudioContext can be resumed.
window.addEventListener('click', () => {
  if (context.state === 'suspended') context.resume()
})

const App = Elm.Main.init({
  node: document.querySelector('#app')
  ,flags: context
})

App.ports.updateAudio.subscribe(graph => {
  audio.update(graph)
})
