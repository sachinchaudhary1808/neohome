varying vec3 vPosition;
varying vec3 vNormal;

void main() {
	// gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
	// float c = sin(pos.x * 16.) / 2.0;
	vec3 c = normalize(vNormal * vNormal);

	gl_FragColor = vec4(c, 1.0);
	// gl_FragColor = vec4(vec3(1., 1., 1.), 1.0);
}
