varying vec3 vPosition;
varying vec3 vNormal;
varying vec2 vUv;

void main() {
	vPosition = position;
	vNormal = normal;
	vUv = uv;

	vec4 res = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
	gl_Position = res;
}
