#version 330

varying vec3 v_position;

layout(location = 0) out float fragmentDepth;

void main() {
  fragmentDepth = gl_FragCoord.z;
}