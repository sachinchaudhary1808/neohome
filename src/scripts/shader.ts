import * as THREE from "three";

import _frag from "./test.frag";
import _vert from "./test.vert";


// const geometry = new THREE.IcosahedronGeometry(1, 5);
// const material = new THREE.MeshStandardMaterial();

// const ico = new THREE.Mesh(geometry, material);


const width = 600;
const height = 600;

// init

const camera = new THREE.PerspectiveCamera(70, width / height, 0.01, 10);
camera.position.z = 1;

const scene = new THREE.Scene();

// const geometry = new THREE.BoxGeometry(0.2, 0.2, 0.2);
const geometry = new THREE.IcosahedronGeometry(0.2, 1);
// const material = new THREE.ShaderMaterial();
const material = new THREE.ShaderMaterial({
    vertexShader: _vert,
    fragmentShader: _frag,
    // wireframe: true,
});


const mesh = new THREE.Mesh(geometry, material);
scene.add(mesh);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(width, height);
renderer.setAnimationLoop(animate);
// document.body.appendChild(renderer.domElement);
const elem = document.getElementById("my-canvas");
if (elem !== null) {
    elem.appendChild(renderer.domElement);
}

// animation

function animate(time: number) {
    mesh.rotation.x = time / 2000;
    mesh.rotation.y = time / 1000;

    renderer.render(scene, camera);
}
