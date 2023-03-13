// ui.js

document.body.innerHTML += `
    <div id="container">
        <canvas></canvas>
        <ul id="toolbar">
            <li><div id="playback"><i class="material-icons">pause</i></div></li>
            <li><div id="performance" title="Performance"><i class="material-icons">bolt</i></div></li>
            <li><div id="screenshot" title="Screenshot"><i class="material-icons">screenshot_monitor</i></div></li>
            <li><div id="optionsbtn" title="Options"><i class="material-icons">settings</i></div></li>
        </ul>
        <ul id="options">
            <li><label>SIZE</label> <input id="rendersize" type="range" value="70" min="50" max="100" step="5"></li>
            <li><label>DPR</label> <input id="renderdpr" type="range" value="1.0" min="0.7" max="1.0" step="0.05"></li>
            <li class="sep"></li>
            <li><label>Max Samples: 2048 | Bounce: 4</label></li>
            <li><label>* DRAG AND DROP .HDR FILES</label></li>
        </ul>
        <div id="focuspoint"></div>
        <div id="progress"></div>
        <div id="info"></div>
    </div>
    <div id="bg"></div>
    <div id="header">
        <h1>FAST RAYMARCHED PATHTRACER</h1>
        <select id="sceneselect">
            <option value="0">Material Preview</option>
            <option value="1" selected>Menger Sponge (IBL)</option>
            <option value="2">The First Room</option>
            <option value="3">The First Room (Trap)</option>
        </select>
    </div>
    <div id="footer">
        Select Engine : <a href="index.html">THREE.js</a> | <a href="babylon.html">BABYLON.js</a><br>
        <span><a href="https://github.com/nimadez">©2023 1.1.5 Pre-Alpha</a> : <a href="https://github.com/nimadez/nray">GitHub Repository</a></span>
    </div>
    <input style="display: none" type="file" id="openfile_hdr" accept=".hdr" onclick="this.value = null">
`;

const canvas = document.getElementsByTagName('canvas')[0];
const canvasContainer = document.getElementById('container');
const domProgressBar = document.getElementById('progress');
const domFocusPoint = document.getElementById('focuspoint');
const domInfo = document.getElementById('info');
const domPlayback = document.getElementById('playback');
const domSceneSelect = document.getElementById('sceneselect');
const domRenderSize = document.getElementById('rendersize');
const domRenderDpr = document.getElementById('renderdpr');
const domPerformance = document.getElementById('performance');
const domScreenshot = document.getElementById('screenshot');
const domOptionsBtn = document.getElementById('optionsbtn');
const domOptions = document.getElementById('options');
