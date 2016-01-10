#version 330

varying vec3 v_position;

// TODO Common uniforms
const float NEAR = 1.0f;
const float FAR = 100.0f;

layout(location = 0) out float fragmentDepth;

float linearizeDepth(float depth)
{
  float z = depth * 2.0 - 1.0;
  return (2.0 * NEAR * FAR) / (FAR + NEAR - z * (FAR - NEAR));
}

void main() {
  fragmentDepth = linearizeDepth(gl_FragCoord.z);
}
