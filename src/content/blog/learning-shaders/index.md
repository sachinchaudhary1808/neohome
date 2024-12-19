---
title: Learning OpenGL Shaders
pubDate: 2024-12-19T14:35:08Z
draft: true
summary: ...
---

I've been learning shaders this week, and I wanted to share my impressions as a
beginner -- you might find it useful too.

A shader is a program that can transform data from an input object, like the
coordinates of its vertices, to a rasterized image that can be displayed on the
screen. It is one of the components that makes the graphics pipeline that allows
you to see stuff on your computer screen.

## The GLSL programming language

Shaders are written in a language which is very similar to C, and are compiled
by the GPU libraries. It includes built-in types to do linear algebra, like
vector and matrices, as well as functions that are useful in the context of
graphics, like
[`smoothstep`](https://registry.khronos.org/OpenGL-Refpages/gl4/html/smoothstep.xhtml).

```glsl
varying vec2 vUv;
uniform float uTime;

void main() {
	float value = 0.0;
	value += sin(uTime / 100. + vUv.x*100.);
	value += 5.*sin(uTime / 1000. + vUv.y*300.);
	gl_FragColor = vec4(value, value, value, 1.);
}
```

When writing a shader, the inputs and outputs are defined in the **global scope**.
The shader compiler checks if the global are available or not. In the previous
example, `vUv` and `uTime` are inputs to the shader, and it sets the
`gl_FragColor` "global". Each shader runs the `void main()` function, but you
can write your own functions like in C.

## Vertex and fragment shaders

There are two different types of shaders that will be run by our graphics
library:

- **Vertex shaders** maps the coordinates of our object's geometry, depending on
  the camera, perspective, or any other transformation.
- **Fragment shaders** calculate the color of each pixel in the triangle of the
  geometry.

