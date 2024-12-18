varying vec3 vPosition;
varying vec3 vNormal;
varying vec2 vUv;

uniform float uTime;

void main() {
	float fresnel = dot(normalize(cameraPosition - vPosition), vNormal);

	vec3 c = vec3(vUv.x, vUv.y, sin(uTime / 1000.) );
	// c -= 1. - vec3(fresnel);
	c = c - (1. - fresnel)*.2;



	// gl_FragColor = vec4(vUv, vec2(1., 1.));
	gl_FragColor = vec4(c.x, c.y, c.z, 1.);
}
