#version 330

in vec3 a_position;

out vec3 v_position;

uniform mat4 u_view;
uniform mat4 u_model;
uniform mat4 u_proj;

void main() {
    gl_Position = u_proj * u_view * u_model * vec4(a_position, 1);
}