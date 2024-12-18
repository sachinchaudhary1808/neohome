varying vec3 vPosition;
varying vec3 vNormal;
varying vec2 vUv;

void main() {
	float fresnel = dot(normalize(cameraPosition - vPosition), vNormal);

	vec3 c = vec3(vUv.x, vUv.y, 1.);
	c -= 1. - vec3(fresnel);

	// gl_FragColor = vec4(vUv, vec2(1., 1.));
	gl_FragColor = vec4(c.x, c.y, c.z, 1.);
}
