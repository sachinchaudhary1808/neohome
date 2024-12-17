varying vec3 vPosition;
varying vec3 vNormal;
varying vec2 vUv;

void main() {
	vec3 c = normalize(cameraPosition - vPosition);
	float fresnel = 1. - dot(c, vNormal);

	// gl_FragColor = vec4(c, 1.0);
	gl_FragColor = vec4(vec3(fresnel), 1.0);
}
