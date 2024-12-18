import * as THREE from "three";


import _frag from "./test.frag";
import _vert from "./test.vert";

import { OrbitControls } from 'three/addons/controls/OrbitControls.js';

// const geometry = new THREE.IcosahedronGeometry(1, 5);
// const material = new THREE.MeshStandardMaterial();

// const ico = new THREE.Mesh(geometry, material);


const width = 600;
const height = 600;

// init

// const camera = new THREE.PerspectiveCamera(70, width / height, 0.01, 10);
const camera = new THREE.OrthographicCamera();
camera.position.z = 1;

const scene = new THREE.Scene();

// const geometry = new THREE.IcosahedronGeometry(0.4, 2);
const geometry = new THREE.PlaneGeometry(1.5, 1.5);

const material = new THREE.ShaderMaterial({
    vertexShader: _vert,
    fragmentShader: _frag,
    // wireframe: true,
    uniforms: {
        uTime: { value: performance.now() },
    }
});


const mesh = new THREE.Mesh(geometry, material);

scene.add(mesh);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(width, height);
document.body.appendChild(renderer.domElement);
const elem = document.getElementById("my-canvas");
if (elem !== null) {
    elem.appendChild(renderer.domElement);
}

// animation

// function animate(time: number) {
//     mesh.rotation.x = time / 2000;
//     mesh.rotation.y = time / 1000;

//     renderer.render(scene, camera);
// }


renderer.setAnimationLoop(animate);
const controls = new OrbitControls(camera, renderer.domElement)
controls.enableDamping = true

// const controls = new OrbitControls(camera, renderer.domElement);
// controls.update();
// camera.position.set(0, 20, 100);

function animate() {
    requestAnimationFrame(animate); // required if controls.enableDamping or controls.autoRotate are set to true
    controls.update();
    renderer.render(scene, camera);
    mesh.material.uniforms.uTime = {value: performance.now()};
}

// renderer.render(scene, camera);
